/// Implemented by Arman Fidanoglu (af5918)
module BusWire

open System
open CommonTypes
open Symbol
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Helpers

/// Bounding box to be used with wire segments. We need the "previous" field to keep track of the
/// segments when using List.fold, because sometimes we need to swap corners to make sure Box.P1
/// is always the top left corner.
type WireBoundingBox = { Box: BoundingBox; Prev: XYPos }

type Wire =
    { Id: ConnectionId
      SourcePort: PortId
      TargetPort: PortId
      IsError: bool
      Width: int
      IsDragging: bool
      BoundingBoxes: WireBoundingBox list
      Corners: XYPos list
      DraggedCornerIndex: int
      LastDragPos: XYPos
      IsHighlighted: bool }

type Model =
    { Symbols: Symbol.Model
      Wires: Wire list
      Colour: HighLightColor }

/// Messages, all except for Symbol sent from Sheet and handled in BusWire.update
type Msg =
    /// Symbol messages
    | Symbol of Symbol.Msg
    /// Message to create connection between two given ports
    | CreateConnection of PortId * PortId
    /// Message to set the wire colour to the given HighlightColor
    | SetColor of HighLightColor
    /// Message to mark the start of dragging from a wire and the position on the wire
    | StartDragging of wireId: ConnectionId * pagePos: XYPos
    /// Message to handle changes while still dragging a wire
    | Dragging of wireId: ConnectionId * pagePos: XYPos
    /// Message to mark the end of dragging of a wire
    | EndDragging of wireId: ConnectionId
    /// Message with list of wires that have been selected
    | SelectWires of wireIds: ConnectionId list
    /// Message with list of wires that are no longer selected
    | DeselectWires of wireIds: ConnectionId list
    /// Message with list of wires to be deleted
    | DeleteWires of wireIds: ConnectionId list
    /// Message with list of symbols that have been deleted, so that the wires connected to them can also be deleted
    | DeleteSymbols of sIds: ComponentId list
    /// Message with a list of wires to be added to the model
    | NewWires of wires: Wire list
    /// Message with a mouse action
    | MouseMsg of MouseT
    
/// Un-highlights all wires
let unhighlightWires (wires: Wire list) =
    wires
    |> List.map (fun wr ->
        { wr with
            IsHighlighted = false
        })

/// Tries to find the specified port, along with its parent symbol's bounding box for wire routing purposes.
/// Returns an option of the port.
let findPortData (symbols: Symbol.Model) (portId: PortId) : (Port * BoundingBox * ComponentId) option =
    symbols
    |> List.tryFind (fun symbol ->
        symbol.Ports
        |> List.map (fun port -> port.Id)
        |> List.contains portId)
    |> Option.bind (fun symbol ->
        Some
            (symbol.Ports
            |> List.map (fun port -> port.Id, port)
            |> List.find (fun (id, _) -> id = portId)
            |> snd
            , symbol.BoundingBox, symbol.Id))

/// Automatically finds corners given source and target port positions and their parent symbol bounding boxes
let findCorners (source: Port) (target: Port) (sourceBox: BoundingBox) (targetBox: BoundingBox) : XYPos list =
    // Source coordinates
    let xSource = source.Pos.X
    let ySource = source.Pos.Y

    // Target coordinates
    let xTarget = target.Pos.X
    let yTarget = target.Pos.Y
    
    // Padding around symbols
    let padding = 10.
    
    // Distance above and below the source and target ports, based on the parent bounding boxes
    let distAboveSource = sourceBox.P2.Y - ySource + padding
    let distBelowSource = ySource - sourceBox.P1.Y + padding
    
    let distAboveTarget = targetBox.P2.Y - yTarget + padding
    let distBelowTarget = yTarget - targetBox.P1.Y + padding
    
    let sourceWidth = sourceBox.P2.X - sourceBox.P1.X
    let targetWidth = targetBox.P2.X - targetBox.P1.X
    
    // Is the source on the left of the target?
    let xPositive = xTarget > xSource
    // Is the source below the target?
    let yPositive = yTarget > ySource
    
    // Midpoints
    let xMid =
        match xPositive with
        | true -> (xSource + xTarget) / 2.0
        | false -> ((xSource - sourceWidth) + (xTarget + targetWidth)) / 2.0
    let yMid =
        match yPositive with
        | true -> ((ySource + distAboveSource) + (yTarget - distBelowTarget)) / 2.0
        | false -> ((ySource - distBelowSource) + (yTarget + distAboveTarget)) / 2.0

    // x and y distances between ports
    let xDiff = Math.Abs(xTarget - xSource)
    let yDiff = Math.Abs(yTarget - ySource)

    /// Position of the first corner: If there is enough space then xMid,
    /// otherwise minimum distance from source port
    let xCorner1 =
        /// If target is above and on the right of the source, and the x and y difference conditions are satisfied
        if xPositive && yPositive && (yDiff < distAboveSource + distBelowTarget || xDiff >= 2. * padding) then
            xMid
        /// If target is below and on the right of the source, and the x and y difference conditions (different
        /// from the ones above) are satisfied
        elif xPositive && (yDiff < distBelowSource + distAboveTarget || xDiff >= 2. * padding) then
            xMid
        /// If port locations are inverted
        else
            xSource + padding
            
    /// Same logic as xCorner1 above
    let xCorner2 =
        if xPositive && yPositive && (yDiff < distAboveSource + distBelowTarget || xDiff >= 2. * padding) then
            xMid
        elif xPositive && (yDiff < distBelowSource + distAboveTarget || xDiff >= 2. * padding) then
            xMid
        else
            xTarget - padding

    /// Horizontal line segments y coordinates, above or below symbols
    let yCorner1 =
        match yPositive with
        | true -> ySource + distAboveSource
        | false -> ySource - distBelowSource

    let yCorner2 =
        match yPositive with
        | true -> yTarget - distBelowTarget
        | false -> yTarget + distAboveTarget

    /// Choose between the vertical midpoint and adaptive heights based on conditions
    let yMidAdaptive1, yMidAdaptive2 =
        if (    yPositive && yDiff >= distAboveSource + distBelowTarget) ||
           (not yPositive && yDiff >= distBelowSource + distAboveTarget)
        then yMid, yMid
        else yCorner1, yCorner2


    /// Corner list, number of corners may vary depending on shape of wire.
    if xCorner1 = xCorner2 then
        [ { X = xSource; Y = ySource } // Source port
          { X = xMid; Y = ySource } // Midpoint, source-height
          { X = xMid; Y = yTarget } // Midpoint, target-height
          { X = xTarget; Y = yTarget } // Target port
          ]
    elif yMidAdaptive1 = yMidAdaptive2 then
        [ { X = xSource; Y = ySource } // Source port
          { X = xCorner1; Y = ySource } // Source port padding
          { X = xCorner1; Y = yMidAdaptive1 } // Mid-horizontal, source-side
          { X = xCorner2; Y = yMidAdaptive2 } // Mid-horizontal, target-side
          { X = xCorner2; Y = yTarget } // Target port padding
          { X = xTarget; Y = yTarget } // Target port
          ]
    else
        [ { X = xSource; Y = ySource } // Source port
          { X = xCorner1; Y = ySource } // Source port padding
          { X = xCorner1; Y = yMidAdaptive1 } // Source symbol corner
          { X = xMid; Y = yMidAdaptive1 } // Midpoint, source-height
          { X = xMid; Y = yMidAdaptive2 } // Midpoint, target-height
          { X = xCorner2; Y = yMidAdaptive2 } // Target symbol corner
          { X = xCorner2; Y = yTarget } // Target port padding
          { X = xTarget; Y = yTarget } // Target port
          ]

/// Chooses top left and bottom right corners out of the given two corners, for bounding boxes
let chooseCorners firstCorner secondCorner =
    let s1 = if (firstCorner.X < secondCorner.X && firstCorner.Y = secondCorner.Y)
                || (firstCorner.X = secondCorner.X && firstCorner.Y < secondCorner.Y)
             then firstCorner
             else secondCorner
    let s2 = if s1 = firstCorner then secondCorner else firstCorner
    s1, s2

/// Generates a list of bounding boxes for segments of the wire, based on its corners, using List.fold.
/// These boxes are in the same order as the corners, which makes figuring out the clicked segment easy.
/// Out of any given two corners, topLeft is always the top-left one, and bottomRight is always the bottom-right.
/// However, Prev always keeps the most recent corner - This is used as a reference when creating the chain of boxes.
let createBoundingBoxes (corners: XYPos list): WireBoundingBox list =
    // Determines the width of the bounding boxes. In the current case, each bounding box is 10px wide/tall.
    let diff = { X = 5.; Y = 5. }
    // Since there will always be at least three corners on any given wire, there is no need to match other cases.
    let (firstCorner :: secondCorner :: rest) = corners
    
    // Switching corners if necessary
    let s1, s2 = chooseCorners firstCorner secondCorner
    
    // This will work even if "rest" was empty (if we had two segments, which we won't), as we have a start state.
    rest
    |> List.fold (fun boxes currentCorner ->
        (let previousBox = List.head boxes
         let p1 = previousBox.Prev
         let p2 = currentCorner
         
         // Switching corners if necessary
         let topLeft, bottomRight = chooseCorners p1 p2

         // Appending box to list of boxes so far
         { Box =
               { P1 = posDiff topLeft diff
                 P2 = posAdd bottomRight diff }
           Prev = p2 }
         :: boxes

        ))
           // I can define the first box like this since it will always emanate from an output, meaning that it will
           // always face to the right. I will have to change this when we add different symbol orientations.
           [ { Box =
                   { P1 = posDiff s1 diff
                     P2 = posAdd s2 diff }
               Prev = secondCorner } ]
    |> List.rev

/// The props required to render a wire
type WireRenderProps =
    { key: ConnectionId
      Wire: Wire
      Source: Port
      Target: Port
      WireColour: string
      WireWidth: int
      Dispatch: Dispatch<Msg> }

/// Renders a single wire on the screen
let singleWireView =
    FunctionComponent.Of(fun (props: WireRenderProps) ->
        let corners = props.Wire.Corners
        
        // If the widths don't match, display a text. Otherwise display the wire width
        let widthText =
            if props.Wire.IsError then str "Different widths" else str <| sprintf "%d" props.WireWidth

        // Use to draw bounding boxes for debugging purposes
        let boxes: ReactElement list =
            createBoundingBoxes corners
            |> List.map (fun box ->
                (polygon [ SVGAttr.Points
                               (sprintf
                                   "%0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f"
                                    box.Box.P1.X  // First corner
                                    box.Box.P1.Y
                                    box.Box.P2.X  // Second corner
                                    box.Box.P1.Y
                                    box.Box.P2.X  // Third corner
                                    box.Box.P2.Y
                                    box.Box.P1.X  // Fourth corner
                                    box.Box.P2.Y)
                           SVGAttr.Stroke "blue"
                           SVGAttr.Fill "lightblue"
                           SVGAttr.Opacity 0.5 ] []))
            
        // Create the string that defines the wire path, based on the number of segments.
        // Can generate the string for 3, 5 or 7 segments, depending on the wire position.
        let drawCorners =
            match corners.Length with
            | 4 ->
                sprintf
                    "%0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f" corners.[0].X corners.[0].Y corners.[1].X
                    corners.[1].Y corners.[2].X corners.[2].Y corners.[3].X corners.[3].Y
            | 6 ->
                sprintf "%0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f
                                            %0.2f, %0.2f %0.2f, %0.2f" corners.[0].X corners.[0].Y corners.[1].X
                    corners.[1].Y corners.[2].X corners.[2].Y corners.[3].X corners.[3].Y corners.[4].X corners.[4].Y
                    corners.[5].X corners.[5].Y
            | 8 ->
                sprintf "%0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f
                                            %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f %0.2f, %0.2f " corners.[0].X
                    corners.[0].Y corners.[1].X corners.[1].Y corners.[2].X corners.[2].Y corners.[3].X corners.[3].Y
                    corners.[4].X corners.[4].Y corners.[5].X corners.[5].Y corners.[6].X corners.[6].Y corners.[7].X
                    corners.[7].Y
            | _ -> failwithf "Invalid corners."
        
        g
            []
            (
                [
                    // The wire
                    polyline [
                        SVGAttr.Points drawCorners
                        SVGAttr.Stroke props.WireColour
                        SVGAttr.StrokeWidth
                          (str (sprintf "%d" <| props.Wire.Width))
                        SVGAttr.FillOpacity "0"
                        SVGAttr.Custom ("stroke-linejoin", "round")
                         ] []
                    // Width legend, displayed if width <> 1
                    if props.Wire.Width <> 1 then
                        text [
                            SVGAttr.X(corners.[0].X + 15.)
                            SVGAttr.Y(corners.[0].Y - 6.)
                            SVGAttr.Stroke props.WireColour
                            SVGAttr.Fill props.WireColour
                            SVGAttr.FontSize 10
                            Style [UserSelect UserSelectOptions.None] ] [
                            widthText
                        ] 
                ]
                // @ boxes // Uncomment to display bounding boxes, for debugging purposes
            )
    )

/// BusWire view function, used to display wires on top of Symbol.view
let view (model: Model) (dispatch: Msg -> unit) =
    let wires =
        model.Wires
        |> List.map (fun wire ->
            let source = findPort model.Symbols wire.SourcePort
            let target = findPort model.Symbols wire.TargetPort
            
            let wireColour =
                if wire.IsHighlighted then "lightskyblue"
                elif wire.IsError then Red.Text()
                else model.Colour.Text()
                
            let props =
                { key = wire.Id
                  Wire = wire
                  Source = source
                  Target = target
                  WireColour = wireColour
                  WireWidth = wire.Width
                  Dispatch = dispatch }

            singleWireView props)

    let symbols =
        Symbol.view model.Symbols (fun sMsg -> dispatch (Symbol sMsg))

    g [] [ (g [] wires); symbols ]

/// Returns true if the source and target widths do not match
let calcIsError symbols sourcePortId targetPortId =
    let outPort = findPort symbols sourcePortId
    let inPort = findPort symbols targetPortId
    let result =
        match outPort.Width with 
            | Some width ->
                match inPort.InferenceRule with
                    | GreaterThan x when x >= width -> true
                    | GreaterThan _ -> false
                    | LessThan x when x <= width -> true
                    | LessThan _ -> false
                    | EqualTo x when x <> width -> true
                    | EqualTo _ -> false
            | None -> false
    result
    
/// Creates connection given the source and target port ids, and the symbol model.
let createWire (sourcePort: PortId) (targetPort: PortId) (symbols: Symbol.Model) : Wire =
    // Try to find the ports. If not found (shouldn't happen because they were created and do exist but still) throw an error.
    let sourcePort, sBox, targetPort, tBox =
        match findPortData symbols sourcePort, findPortData symbols targetPort with
        | Some (sp, sb, _), Some (tp, tb, _) -> sp, sb, tp, tb
        | _ -> failwithf "Ports not found"
        
    let corners = findCorners sourcePort targetPort sBox tBox
    let isError = calcIsError symbols sourcePort.Id targetPort.Id
    
    { Id = ConnectionId(uuid ())
      SourcePort = sourcePort.Id
      TargetPort = targetPort.Id
      IsError = isError
      Width = 
        match isError, sourcePort.Width with
        | false, Some w -> w // There is no error and there is a width
        | false, None -> 1 // There is no error and the width is 1
        | true, _ -> 3 // There is an error. Generate thick red wire.
      IsDragging = false
      BoundingBoxes = createBoundingBoxes corners
      Corners = corners
      DraggedCornerIndex = 1 // This is just an initial value, does not change anything.
      LastDragPos = { X = 0.; Y = 0. }
      IsHighlighted = false}

let init () =
    let symbols, cmd = Symbol.init ()
    { Wires = []
      Symbols = symbols
      Colour = Blue },
    cmd

/// Given a wire, tries to find and return an option of the index of the segment that was clicked. If there are none, returns None.
let tryFindClickedSegment (pagePos: XYPos) (wire: Wire): int option =
    wire.BoundingBoxes
    |> List.mapi (fun index boundingBox -> (index, boundingBox))
    |> List.tryFind (fun (_, boundingBox) -> containsPoint boundingBox.Box pagePos)
    |> Option.bind (fun segment -> Some <| fst segment)

/// Given two ports, checks whether a wire already exists between them.
let connectionExists (model: Model) (source: PortId) (target: PortId) : bool =
    model.Wires
    |> List.tryFind (fun wire -> wire.SourcePort = source && wire.TargetPort = target)
    |> Option.isSome

/// Orientation of wire segments, used to choose direction to drag segment in
type SegmentOrientation =
    | Vertical
    | Horizontal

/// Determines orientation of given segment
let segmentOrientation (corners: XYPos list) (index: int): SegmentOrientation =
    let corner1 = corners.[index]
    let corner2 = corners.[index + 1]

    if corner1.X = corner2.X && corner1.Y <> corner2.Y
    then Vertical
    else Horizontal

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | Symbol sMsg ->
        match sMsg with
        // This is called by symbol when a width has been set on an input
        // Uses function in Symbol to work out width of each output port
        // Sets the output width of each port
        // Checks if the port on the other side of the output port now needs updating
        // If so call setPortWidths in Symbol for the connected Symbol
        | UpdateConnections (symbolId: ComponentId) ->
            let symbol = List.find (fun (sym: Symbol) ->  symbolId.Equals sym.Id) model.Symbols

            let inputPorts =
                symbol.Ports
                |> List.filter (fun port -> port.PortType = PortType.Input)

            let inputWidths = 
                inputPorts
                |> List.map (fun port -> port.Width)

            let updatedOutputPorts =
                symbol.Ports
                |> List.filter (fun port -> port.PortType = PortType.Output)
                |> List.mapi (fun index port -> {port with Width = Symbol.calculateOutputPort symbol.ComponentType index inputWidths})

            let updatedSymbol =
                {symbol with Ports = (inputPorts @ updatedOutputPorts)}
            
            let updatedOutputPortIds = 
                List.map (fun (port : Port)-> port.Id) updatedOutputPorts
            
            let updatedSymbols =
                model.Symbols
                |> List.map (fun sym ->
                    if sym.Id = updatedSymbol.Id then
                        updatedSymbol
                    else
                        sym)

            //update the wires if some are now errors
            let updatedWires =
                model.Wires
                |> List.map (fun wire ->
                    let updatedPort =
                        updatedOutputPortIds
                        |> List.tryFind (fun portId -> portId = wire.SourcePort) 

                    match updatedPort with
                    | Some portId ->                    
                        let outPort = findPort updatedSymbols portId

                        match outPort.Width with
                        | Some width ->
                            //outPort has been updated and has fixed width
                            let isError = calcIsError updatedSymbols portId wire.TargetPort
                            {wire with
                                
                                Width = if isError then 3 else width
                                IsError = isError
                            }
                        | None -> 
                            //outPort has been updated and has a variable width
                            let isError = calcIsError updatedSymbols portId wire.TargetPort
                            {wire with
                               
                                Width = if isError then 3 else 1
                                IsError = isError}
                        
                    | None -> wire                    

                )

            //go through the updated wires, get the ones that have been affected, check if IsError is false, update target ports if variable
            let inputPortsToUpdate = 
                ([],updatedWires) 
                ||> List.fold (fun outList wire ->
                    let outPort = findPort updatedSymbols wire.SourcePort
                    let inPort = findPort updatedSymbols wire.TargetPort
                    match outPort.Width, inPort.Width, wire.IsError with
                        | Some width, None, false -> (wire.TargetPort,Some width) :: outList
                        | None, Some _, false -> (wire.TargetPort,None) :: outList
                        | _,_,_ -> outList
                
                )

          
            {model with 
                Wires = updatedWires
                Symbols = updatedSymbols} 

            , Cmd.ofMsg <| Symbol (SetPortWidths inputPortsToUpdate)
        | _ ->
            let sm, sCmd = Symbol.update sMsg model.Symbols
            
            /// Every time the symbol model gets updated, we need to update the bounding boxes of all wires. If we don't
            /// do this, the wire gets drawn again but the boxes don't get calculated.
            { model with
                Symbols = sm
                Wires =
                    model.Wires
                    |> List.map (fun wire ->
                        (let corners =
                            let source, sBox, target, tBox =
                                match findPortData sm wire.SourcePort, findPortData sm wire.TargetPort with
                                | Some (sp, sb, _), Some (tp, tb, _) -> sp, sb, tp, tb
                                | _ -> failwithf "Ports not found"
                                
                            /// If one of the ports has changed its location, we need to recalculate corners.
                            /// If not, we can keep the current corners. We have to do this because the wire might
                            /// be manually adjusted, in which case we won't want to recalculate corners/auto-route.
                            if source.IsPositionModified || target.IsPositionModified
                            then findCorners source target sBox tBox
                            else wire.Corners
                         { wire with
                               Corners = corners
                               BoundingBoxes = createBoundingBoxes wire.Corners  }))
                     }, Cmd.map Symbol sCmd
    /// Handles creating a connection between source and target
    | CreateConnection (source, target) ->
        if not (connectionExists model source target) then
            // If sourcePort is fixed and destination port is variable and no error, then call symbol update port in order to propagate the value down
            let cmdMessage =
                match (findPort model.Symbols source).Width,(findPort model.Symbols target).Width,(calcIsError model.Symbols source target) with
                | Some width, None, false -> Cmd.ofMsg <| Symbol (SetPortWidths [target,Some width])
                | _,_,_ -> Cmd.none
            { model with
                  Wires = (createWire source target model.Symbols) :: model.Wires },
            cmdMessage
        else model, Cmd.none
    /// Handles setting wire colours
    | SetColor c -> { model with Colour = c }, Cmd.none
    /// Handles the start of wire dragging:
    /// First decides on the Figures out which wire segment was clicked, decides the current position
    | StartDragging (wireId, pagePos) ->
        { model with
            Wires =
                model.Wires
                |> List.map (fun wire ->
                let source, sBox, target, tBox =
                    match findPortData model.Symbols wire.SourcePort, findPortData model.Symbols wire.TargetPort with
                    | Some (sp, sb, _), Some (tp, tb, _) -> sp, sb, tp, tb
                    | _ -> failwithf "Ports not found"

                // If any of the symbols have been moved, then auto-route the wire. Otherwise, keep the last shape.
                let corners =
                  if source.IsPositionModified || target.IsPositionModified
                  then findCorners source target sBox tBox
                  else wire.Corners

                let boundingBoxes = createBoundingBoxes corners

                // If this isn't the wire being dragged, we have to still update the corners or else its position will
                // be reset to its initial position.
                if wireId <> wire.Id then
                    { wire with
                        LastDragPos = pagePos
                        Corners = corners
                        BoundingBoxes = boundingBoxes }
                else
                    let i = // Index of the clicked segment on the clicked wire
                        tryFindClickedSegment
                            pagePos
                            { wire with
                                Corners = corners
                                BoundingBoxes = boundingBoxes }

                    match i with
                    | Some i ->
                        // If the end segments are clicked, select the wire but do not let the segment be dragged
                        if i = 0 || i = corners.Length - 2 then
                            { wire with
                                LastDragPos = pagePos
                                Corners = corners
                                BoundingBoxes = boundingBoxes
                                IsHighlighted = true }
                        // Otherwise segment can be dragged    
                        else
                            { wire with
                                LastDragPos = pagePos
                                IsDragging = true
                                Corners = corners
                                DraggedCornerIndex = i
                                IsHighlighted = true
                                BoundingBoxes = boundingBoxes }
                    // If no segment has been clicked, update the corners and leave the wire as is (Not dragging)
                    | None ->
                        { wire with
                            LastDragPos = pagePos
                            Corners = corners
                            BoundingBoxes = boundingBoxes })
            // We have to modify the symbol list every time a wire is dragged. This is because when we start dragging
            // a wire, we need to state that its ports' positions are no longer modified. This is required to keep the
            // dragged positions of the wires even after letting go after them, but to reset their positions (auto-route
            // again) whenever one of its ports is dragged.
            Symbols =
                model.Symbols
                |> List.map (fun symbol ->
                    let wire =
                        model.Wires
                        |> List.find (fun wire -> wireId = wire.Id)

                    let sId, tId =
                        match findPortData model.Symbols wire.SourcePort, findPortData model.Symbols wire.TargetPort with
                        | Some (_, _, sid), Some (_, _, tid) -> sid, tid
                        | _ -> failwithf "Ports not found"

                    if (symbol.Id <> sId && symbol.Id <> tId) then
                        symbol
                    else
                        // In Symbol.update, we set IsPositionModified to true when a symbol with this port is dragged.
                        { symbol with
                            Ports = List.map (fun port -> { port with IsPositionModified = false }) symbol.Ports }) },
        Cmd.none
    /// Handles all actions while dragging a wire:
    /// Modifies the corner list to allow the dragging of a wire segment in the opposite direction of its orientation.
    /// Also prevents the end segments from being dragged through the ports.
    | Dragging (wireId, pagePos) ->
        { model with
              Wires =
                  model.Wires
                  |> List.map (fun wire ->
                      if wireId <> wire.Id || not wire.IsDragging then
                          wire
                      else
                          // Get information about which corners are dragged, and the orientation of the segment
                          // between them.
                          let i = wire.DraggedCornerIndex
                          let diff = posDiff pagePos wire.LastDragPos
                          let orientation = segmentOrientation wire.Corners i
                          // The start of the last vertical segment before the target port
                          let lastVerticalCornerIndex = List.length wire.Corners - 3
                          // Last corner, so target position
                          let lastCornerIndex = lastVerticalCornerIndex + 2

                          // If a segment is vertical, only drag it in the horizontal direction.
                          // If it is horizontal, only drag it in the vertical direction.
                          // If the segment is the first vertical segment, prevent it from being dragged past 10 pixels to the source port.
                          // If the segment is the last vertical segment, prevent it from being dragged past 10 pixels to the target port.
                          // If there are only three segments, prevent the vertical segment from being dragged past 10 pixels from each port.
                          let adjusted =
                              match orientation with
                              | Vertical ->
                                  match i with
                                  // Only three segments
                                  | 1 when i = lastVerticalCornerIndex ->
                                      if // No problems, allowed drag position
                                          pagePos.X > wire.Corners.[0].X + 10. &&
                                          pagePos.X < wire.Corners.[lastCornerIndex].X - 10.
                                      then
                                          { wire.Corners.[i] with
                                                X = wire.Corners.[i].X + diff.X },
                                          { wire.Corners.[i + 1] with
                                                X = wire.Corners.[i + 1].X + diff.X }
                                      elif pagePos.X > wire.Corners.[0].X + 10. // Dragged past target port
                                      then
                                          { wire.Corners.[i] with
                                                X = wire.Corners.[lastCornerIndex].X - 10. },
                                          { wire.Corners.[i + 1] with
                                                X = wire.Corners.[lastCornerIndex].X - 10. }
                                      else // Dragged past source port
                                          { wire.Corners.[i] with
                                                X = wire.Corners.[0].X + 10. },
                                          { wire.Corners.[i + 1] with
                                                X = wire.Corners.[0].X + 10. }
                                  | 1 -> // More than three segments but first segment
                                      if pagePos.X > wire.Corners.[0].X + 10. // No problems
                                      then
                                          { wire.Corners.[i] with
                                                X = wire.Corners.[i].X + diff.X },
                                          { wire.Corners.[i + 1] with
                                                X = wire.Corners.[i + 1].X + diff.X }
                                      else // Being dragged past source port
                                          { wire.Corners.[i] with
                                                X = wire.Corners.[0].X + 10. },
                                          { wire.Corners.[i + 1] with
                                                X = wire.Corners.[0].X + 10. }
                                  | _ when i = lastVerticalCornerIndex -> // More than three segments but last segment
                                      if pagePos.X < wire.Corners.[lastCornerIndex].X - 10. // No problems
                                      then
                                          { wire.Corners.[i] with
                                                X = wire.Corners.[i].X + diff.X },
                                          { wire.Corners.[i + 1] with
                                                X = wire.Corners.[i + 1].X + diff.X }
                                      else // Being dragged past target port
                                          { wire.Corners.[i] with
                                                X = wire.Corners.[lastCornerIndex].X - 10. },
                                          { wire.Corners.[i + 1] with
                                                X = wire.Corners.[lastCornerIndex].X - 10. }
                                  | _ -> // Intermediate vertical segment, don't worry about dragging past ports
                                      { wire.Corners.[i] with
                                            X = wire.Corners.[i].X + diff.X },
                                      { wire.Corners.[i + 1] with
                                            X = wire.Corners.[i + 1].X + diff.X }
                              | Horizontal ->
                                  { wire.Corners.[i] with
                                        Y = wire.Corners.[i].Y + diff.Y },
                                  { wire.Corners.[i + 1] with
                                        Y = wire.Corners.[i + 1].Y + diff.Y }
                          
                          /// Keep the corner list the same, except for the modified corners.
                          let corners =
                              wire.Corners.[..i - 1]
                              @ [ fst adjusted; snd adjusted ]
                                @ wire.Corners.[i + 2..]
                          
                          { wire with
                                Corners = corners
                                BoundingBoxes = createBoundingBoxes corners
                                LastDragPos = pagePos }) },
        Cmd.none
    /// Handles the ending of wire dragging, by setting the dragged wire's "IsDragging" to false.
    | EndDragging wireId ->
        { model with
            Wires =
                model.Wires
                |> List.map (fun wire -> if wireId <> wire.Id then wire else { wire with IsDragging = false }) },
        Cmd.none
    /// Selects given list of wires.
    | SelectWires wireIds ->
        { model with
            Wires =
                model.Wires
                |> List.map (fun wire -> if List.contains wire.Id wireIds then { wire with IsHighlighted = true } else wire) },
        Cmd.none
    /// Deselects given list of wires.
    | DeselectWires wireIds ->
        { model with
            Wires =
                model.Wires
                |> List.map (fun wire -> if List.contains wire.Id wireIds then { wire with IsHighlighted = false } else wire) },
        Cmd.none
    /// Deletes given list of wires
    | DeleteWires wireIds ->
        // When deleting a wire, detect if on creation it decided on the width of the target port
        // We can tell this by looking at the inference rule of the target port, if it is variable (i.e. GreaterThan _ etc)
        // then the width was originally None. If so, set it back to None by calling the setPortWidths message in Symbol
        let portsToUpdate = 
            let wires =
                model.Wires
                |> List.filter (fun wire -> List.contains wire.Id wireIds)
            ([],wires) ||> List.fold (fun outList wire ->
                let targetPort = findPort model.Symbols wire.TargetPort
                match targetPort.InferenceRule, targetPort.Width with
                | GreaterThan _ , Some _ 
                | LessThan _ , Some _ -> (wire.TargetPort,None)::outList
                | _,_ -> outList

            )

        { model with
            Wires =
                model.Wires
                |> List.filter (fun wire -> not <| List.contains wire.Id wireIds) },
        Cmd.ofMsg <| Symbol (SetPortWidths portsToUpdate)
    /// Handle deletion of symbols by first deleting all wires connected to the "to be deleted" symbols.
    | DeleteSymbols sIds ->
        /// Collect all ports and return their IDs
        let ports : PortId list =
            sIds
            |> List.map (getSymbol model.Symbols)
            |> List.collect (fun symbol -> symbol.Ports)
            |> List.map (fun port -> port.Id)

        //all wires that have a source or target on this symbol
        let wireIdsToDelete =
            model.Wires
            |> List.filter (fun wire ->
                (List.contains wire.SourcePort ports) ||
                (List.contains wire.TargetPort ports))   
            |> List.map (fun wire -> wire.Id)
        
        // To make things easier with width inference, DeleteSymbol simply calls a DeleteWires 
        // message for all the wires connected to the symbol. Width inference is handled there
        model
        , Cmd.batch [
            Cmd.ofMsg <| DeleteWires wireIdsToDelete
            Cmd.ofMsg <| Symbol (Symbol.DeleteSymbols sIds)]
    /// Adds the given list of wires to the model.
    | NewWires wires ->
        { model with
            Wires = wires@model.Wires
        }
        , Cmd.none
    | MouseMsg mMsg -> model, Cmd.ofMsg (Symbol(Symbol.MouseMsg mMsg))
    | _ -> model, Cmd.none
    
//----------------------interface to Issie-----------------------------//
let mapCornersToVertices (corners: XYPos list) : (float * float) list =
    corners
    |> List.map (fun corner -> (corner.X, corner.Y))

let mapWireToConnection (symbols: Symbol list) (wire: Wire): Connection =
    let sourcePort, sourceHostId = findPortWithSymbolId symbols wire.SourcePort
    let targetPort, targetHostId = findPortWithSymbolId symbols wire.TargetPort
    {
        Id = string wire.Id
        Source = mapSymbolPortToIssiePort sourcePort.Id None sourcePort.PortType sourceHostId
        Target = mapSymbolPortToIssiePort targetPort.Id None targetPort.PortType targetHostId
        Vertices = mapCornersToVertices wire.Corners
    }

let extractWire (wModel: Model) (wId: ConnectionId) : Connection = 
    wModel.Wires
    |> List.find (fun wire -> wire.Id = wId)
    |> mapWireToConnection wModel.Symbols

let extractWires (wModel: Model) : Connection list = 
    wModel.Wires
    |> List.map (mapWireToConnection wModel.Symbols)