/// Implemented by Lukas Baliunas (lb4418)
module Sheet

open CommonTypes
open Electron
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Helpers

/// D.U. combining all three interactable element types
type SheetElement =
    | SymbolElement of Symbol.Symbol
    | PortElement of Symbol.Port
    | WireElement of BusWire.Wire
    | CanvasElement

/// Type for holding the dragging state with
type DragType =
    | Symbol of ComponentId list
    | Connection of srcPort: Symbol.Port * srcPos: XYPos * lastDragPos: XYPos
    | Wire of ConnectionId
    | Canvas of srcPos: XYPos * lastDragPos: XYPos
    
/// Record for holding copied symbols, wires and ports
type Clipboard = {
    Symbols: Symbol.Symbol list
    Wires: BusWire.Wire list
    Ports: Symbol.Port list
}

/// Record for the lists of selected symbol and wire ids.
type Selection = {
    Symbols: ComponentId list
    Wires: ConnectionId list
}

type CursorType = | DefaultCursor | MoveCursor

type Model = {
    Wire: BusWire.Model
    DragType: DragType option
    IsCtrlPressed: bool
    GuidelineNearby: ((float * float) * GuidelineOrientation) option
    Clipboard: Clipboard
    Selection: Selection
    PastWireModels: BusWire.Model list
    FutureWireModels: BusWire.Model list
    ClickToDrop: (ComponentType * string) option
    CursorType: CursorType
    Zoom: float
}

type KeyboardMsg =
    | CtrlS | AltC | AltV | AltShiftZ | DEL | CtrlA | CtrlC| CtrlV | CtrlZ | CtrlY | Ctrl of KeyOp

type Msg =
    | Wire of BusWire.Msg
    | AddSymbol of cType: ComponentType * label: string
    | KeyPress of KeyboardMsg
    | MouseMsg of MouseT
    | AdjustZoom of amount: float
    
/// Helper for better readability, when sending messages from Sheet to Symbol 
let getSymbolMsg (msg:Symbol.Msg) = 
    Cmd.ofMsg <| Wire (BusWire.Symbol msg)
    
/// Helper for better readability, when sending messages from Sheet to BusWire 
let getWireMsg (msg:BusWire.Msg) =
    Cmd.ofMsg <| Wire msg

/// The threshold for how far from a symbol to show/hide its ports.
let threshold = 20.

/// The maximum number of BusWire models that can be in PastWireModels and FutureWireModels
let historySize = 15;

let displaySvgWithZoom (svgReact: ReactElement) (dispatch: Dispatch<Msg>) (model:Model) =
    let zoom = model.Zoom

    let sizeInPixels = sprintf "%.2fpx" ((1000. * zoom))
    let mDown (ev:Types.MouseEvent) = 
        if ev.buttons <> 0. then true else false

    let mouseOp (op:MouseOp) (ev:Types.MouseEvent) =
        dispatch <| MouseMsg {Op = op ; Pos = { X = ev.pageX / zoom ; Y = ev.pageY / zoom}}

    let keyOp (op:KeyOp) (ev:Types.KeyboardEvent) =
        match ev.key, op, model.IsCtrlPressed with
            | "a", KeyDown, true | "A", KeyDown, true -> dispatch <| KeyPress CtrlA
            | "c", KeyDown, true | "C", KeyDown, true -> dispatch <| KeyPress CtrlC
            | "v", KeyDown, true | "V", KeyDown, true -> dispatch <| KeyPress CtrlV
            | "Control", KeyDown, false | "Control", KeyUp, true -> dispatch <| KeyPress (Ctrl op)
            | _ -> ()
        
    /// Creates the React dragging element (connection line or multi-select rectangle) or nothing
    let draggingSvgElement =
          match model.DragType with
            | Some (Connection (_,srcPos,lastDragPos)) ->
                [
                    line [
                        X1 srcPos.X
                        Y1 srcPos.Y
                        X2 lastDragPos.X
                        Y2 lastDragPos.Y
                        Style [
                            Stroke "blue"
                            StrokeWidth "0.5px"
                            StrokeDasharray "5 5"
                        ]
                    ] []
                ]
            | Some (Canvas (srcPos, lastDragPos)) ->
                let x1 = srcPos.X
                let y1 = srcPos.Y
                let x2 = lastDragPos.X
                let y2 = lastDragPos.Y
                [
                    polygon [
                        SVGAttr.Points <| sprintf "%f,%f %f,%f %f,%f %f,%f" x1 y1 x1 y2 x2 y2 x2 y1
                        SVGAttr.StrokeWidth "0.5px"
                        SVGAttr.Stroke "blue"
                        SVGAttr.StrokeDasharray "5 5"
                        SVGAttr.Fill "none"
                    ] []
                ]
            | _ -> List.empty
    
    /// Create the guideline element
    let guidelineSVGElement =
        match model.GuidelineNearby with
        | Some ((_, lineCoordinate), side) ->
            let sym = Symbol.getSymbol model.Wire.Symbols (List.exactlyOne model.Selection.Symbols)
            let point1, point2 =
                match side with
                | GuidelineLeft -> {X = lineCoordinate; Y = sym.BoundingBox.P1.Y - 100.0}, {X  = lineCoordinate; Y = sym.BoundingBox.P2.Y + 100.0} // left
                | GuidelineRight -> {X = lineCoordinate; Y = sym.BoundingBox.P1.Y - 100.0}, {X  = lineCoordinate; Y = sym.BoundingBox.P2.Y + 100.0} // right
                | GuidelineTop -> {X = sym.BoundingBox.P1.X - 100.0; Y = lineCoordinate}, {X  = sym.BoundingBox.P2.X + 100.0; Y = lineCoordinate} // top
                | GuidelineBottom -> {X = sym.BoundingBox.P1.X - 100.0; Y = lineCoordinate}, {X  = sym.BoundingBox.P2.X + 100.0; Y = lineCoordinate} // bottom

            [
                line[
                    X1 point1.X
                    Y1 point1.Y
                    X2 point2.X
                    Y2 point2.Y
                    Style [
                        Stroke "blue"
                        StrokeWidth "0.5px"
                        StrokeDasharray "5 5"
                    ]
                ] []
            ]
            
        | None -> List.empty

    let finalSvg = List.append (svgReact::draggingSvgElement) guidelineSVGElement

    div [
          TabIndex -1
          OnKeyDown (fun ev -> keyOp KeyDown ev)
          OnKeyUp (fun ev -> keyOp KeyUp ev)
          OnMouseDown (fun ev -> (mouseOp Down ev))
          OnMouseUp (fun ev -> (mouseOp Up ev))
          OnMouseMove (fun ev -> mouseOp (if mDown ev then Drag else MouseOp.Move) ev)
        ]
        [ svg
            [ Style 
                [
                    Border "3px solid green"
                    Height sizeInPixels
                    Width sizeInPixels
                    Cursor (match model.CursorType with | DefaultCursor -> "default" | MoveCursor -> "move")
                ]
            ]
            [ g // group list of elements with list of attributes
                [ Style [Transform (sprintf "scale(%f)" zoom)]] // top-level transform style attribute for zoom
                finalSvg
            ]
        ]

let view (model:Model) (dispatch : Msg -> unit) = 
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    displaySvgWithZoom wireSvg dispatch model
    
/// Returns the opposite of the given PortType, e.g. Input returns Output, Output returns Input
let getOppositePortType (portType:PortType):PortType =
    match portType with
        | PortType.Input -> PortType.Output
        | PortType.Output -> PortType.Input
        
/// Finds the Port that is in the location of the mouse, returns None if no such Port was found
let tryFindClickedPort (mousePos:XYPos) (symbols:Symbol.Symbol list):Symbol.Port option = 
    symbols
    |> List.collect (fun x -> x.Ports)
    |> List.tryFindBack (fun port -> containsPoint port.BoundingBox mousePos)
        
/// Finds the Port element that is in the location of the mouse, returns None if no such Port was found
let tryFindClickedPortElement (mousePos:XYPos) (symbols:Symbol.Symbol list):SheetElement option = 
    tryFindClickedPort mousePos symbols
    |> Option.bind (fun p -> Some <| PortElement p)
    
/// Finds the Symbol element that is in the location of the mouse, returns None if no such Symbol was found
let tryFindClickedSymbolElement (mousePos:XYPos) (symbols:Symbol.Symbol list):SheetElement option = 
    symbols
    |> List.tryFindBack (fun sm -> containsPoint sm.BoundingBox mousePos)
    |> Option.bind (fun sm -> Some <| SymbolElement sm)
    
/// Finds the Wire element that is in the location of the mouse, returns None if no such Wire was found
let tryFindClickedWireElement (mousePos:XYPos) (wires: BusWire.Wire list):SheetElement option =
    let isSegmentClicked (wire: BusWire.Wire) =
        wire.BoundingBoxes
        |> List.exists (fun bb -> containsPoint bb.Box mousePos)

    wires
    |> List.tryFindBack isSegmentClicked 
    |> Option.bind (fun wr -> Some <| WireElement wr)
    
/// Gets the element that is in the location of the mouse
let getClickedElement (mousePos:XYPos) (symbols:Symbol.Symbol list) (wires:BusWire.Wire list):SheetElement =
    let canvasOrWireThunk () =
        tryFindClickedWireElement mousePos wires
        |> Option.defaultValue SheetElement.CanvasElement

    let wireOrSymbolThunk () =
        tryFindClickedSymbolElement mousePos symbols
        |> Option.defaultWith canvasOrWireThunk
    
    tryFindClickedPortElement mousePos symbols
    |> Option.defaultWith wireOrSymbolThunk

/// Finds the symbols that are fully contained inside of the given bounding box and returns a list containing their ids.
let getSymbolsInsideBoundingBox (bBox:BoundingBox) (symbols:Symbol.Symbol list):ComponentId list = 
    symbols
    |> List.filter (fun sm -> containsBox sm.BoundingBox bBox)
    |> List.map (fun sm -> sm.Id)
    
/// Finds the wires that are fully contained inside of the given bounding box and returns a list containing their ids.
let getWiresInsideBoundingBox (bBox:BoundingBox) (wires:BusWire.Wire list):ConnectionId list =
    let isAllSegmentsInside (wr:BusWire.Wire) =
        wr.BoundingBoxes
        |> List.forall (fun segment ->
            containsBox segment.Box bBox)

    wires
    |> List.filter isAllSegmentsInside
    |> List.map (fun wr -> wr.Id)

/// Returns a tuple containing the list of symbol ids within and outside a threshold (20px) from the mouse
let getToShowAndToHidePortsSymbols (mousePos:XYPos) (symbols:Symbol.Symbol list) = 
    let partitionedSymbols =
        symbols
        |> List.partition (fun sm -> distanceFromPoint sm.BoundingBox mousePos < threshold)
    
    let toShowPortsSymbolIds =
        fst partitionedSymbols
        |> List.filter (fun sm -> not sm.IsShowingPorts)
        |> List.map (fun sm -> sm.Id)
        
    let toHidePortsSymbolIds =
        snd partitionedSymbols
        |> List.filter (fun sm -> sm.IsShowingPorts)
        |> List.map (fun sm -> sm.Id)

    toShowPortsSymbolIds, toHidePortsSymbolIds
    
// Returns an empty list if the list is empty, else returns the command. Used so that no messages are sent with empty parameter lists
let getCmdIfNotEmpty (cmd:Cmd<Msg>) list =
    if List.isEmpty list then
        List.empty
    else
        [cmd]

/// Returns the commands for showing and hiding ports.
let getShowPortsAndHidePortsCmds (mousePos:XYPos) (symbols:Symbol.Symbol list) (specificPortType:PortType option):Cmd<Msg> list = 
    let toShowPortsSymbols, toHidePortsSymbols = getToShowAndToHidePortsSymbols mousePos symbols
                
    let showPortsCmd = getCmdIfNotEmpty (getSymbolMsg <| Symbol.ShowPorts (toShowPortsSymbols, specificPortType)) toShowPortsSymbols
    let hidePortsCmd = getCmdIfNotEmpty (getSymbolMsg <| Symbol.HidePorts toHidePortsSymbols) toHidePortsSymbols

    showPortsCmd@hidePortsCmd

/// helper function to unzip list of tuples with 4 elements
let rec unzip4 lst =
    match lst with
    | [] -> ([], [], [], [])
    | (a, b, c, d) :: rst->
        let (aRst, bRst, cRst, dRst) = unzip4 rst
        (a::aRst, b::bRst, c::cRst, d::dRst);;

/// returns the guidelines
let getNearGuidelines (symId: CommonTypes.ComponentId) (model: Model): ((float * float) * GuidelineOrientation) option =
    let currentSymbol = Symbol.getSymbol model.Wire.Symbols symId
    let threshold = 15.0
    let distanceList =
        model.Wire.Symbols
        |> List.filter (fun sm -> sm.Id <> symId)
        |> List.map (fun sm -> sm.BoundingBox.P1, sm.BoundingBox.P2)
        |> List.map (fun (x, y) ->
            (x.X - currentSymbol.BoundingBox.P1.X, x.X), // left
            (x.Y - currentSymbol.BoundingBox.P1.Y, x.Y), // top
            (y.X - currentSymbol.BoundingBox.P2.X, y.X), // right
            (y.Y - currentSymbol.BoundingBox.P2.Y, y.Y)) // bottom 
        |> List.filter (fun (dist1, dist2, dist3, dist4) -> abs (fst dist1) < threshold || abs (fst dist2) < threshold || abs (fst dist3) < threshold || abs (fst dist4) < threshold) // prefilter on the entire list first to avoid iterating through four lists
        
    if not (List.isEmpty distanceList)
    then
        let (left, top, right, bottom) =
            distanceList
            |> unzip4
        
        let minDistanceFromLeft = 
            left
            |> List.sortBy (fst)
            |> List.head
        
        let minDistanceFromRight =
            right
            |> List.sortBy (fst)
            |> List.head
        
        let minDistanceFromTop =
            top
            |> List.sortBy (fst)
            |> List.head
            
        let minDistanceFromBottom =
            bottom
            |> List.sortBy (fst)
            |> List.head
            
        [(minDistanceFromLeft, GuidelineLeft); (minDistanceFromRight, GuidelineRight); (minDistanceFromTop, GuidelineTop); (minDistanceFromBottom, GuidelineBottom)]
        |> List.sortBy (fst)
        |> List.filter (fun x -> abs (fst (fst x)) < threshold)
        |> List.tryHead
                    
    else None

/// Unhighlight every element in the BusWire model
let unhighlightWireModel (wModel:BusWire.Model) =
    { wModel with
        Symbols = Symbol.unhighlightSymbols wModel.Symbols
        Wires = BusWire.unhighlightWires wModel.Wires
    }
    
/// Add BusWire model to the list of models. Truncate the list size according to historySize.
let addToWireModels (wModel: BusWire.Model) (wireModels: BusWire.Model list) =
    List.truncate historySize ((unhighlightWireModel wModel)::wireModels)

let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    /// hold often used values as constants for better readability
    let symbols = model.Wire.Symbols
    let wires = model.Wire.Wires
    match msg with
    | Wire wMsg -> 
        let wModel, wCmd = BusWire.update wMsg model.Wire
        {model with Wire = wModel}, Cmd.map Wire wCmd
    | AdjustZoom amount ->
        // Adjust zoom by the given amount
        { model with
            Zoom = model.Zoom + amount
        }
        , Cmd.none
    | AddSymbol (cType, label) ->
        // Switch to click-to-drop mode with the given component
        { model with
            ClickToDrop = Some (cType, label)
            CursorType = MoveCursor
        }
        , Cmd.none
    | KeyPress (Ctrl keyOp) ->
        // Set IsCtrlPressed
        { model with
            IsCtrlPressed = match keyOp with | KeyDown -> true | KeyUp -> false
        }
        , Cmd.none
    | KeyPress CtrlA ->
        // Select all symbols and wires
        let toSelectSymbolIds =
            symbols
            |> List.map (fun sm -> sm.Id)
            
        let toSelectWireIds =
            wires
            |> List.map (fun wr -> wr.Id)
        { model with
            Selection = {
                Symbols = toSelectSymbolIds
                Wires = toSelectWireIds
            }
        }
        , Cmd.batch [
            getSymbolMsg <| Symbol.SelectSymbols toSelectSymbolIds
            getWireMsg <| BusWire.SelectWires toSelectWireIds
        ]
    | KeyPress CtrlC ->
        // Copy the selected symbols, their ports and their interconnecting wires to the clipboard
        let clipboardSymbols =
            symbols
            |> List.filter (fun sm -> List.contains sm.Id model.Selection.Symbols)
        
        let clipboardPorts =
            clipboardSymbols
            |> List.collect (fun sm -> sm.Ports)

        let clipboardPortIds =
            clipboardPorts
            |> List.map (fun port -> port.Id)
            
        let clipboardWires =
            wires
            |> List.filter (fun wire ->
                (List.contains wire.SourcePort clipboardPortIds) &&
                (List.contains wire.TargetPort clipboardPortIds))

        { model with
            Clipboard = {
                Symbols = clipboardSymbols
                Wires = clipboardWires
                Ports = clipboardPorts
            }
        }
        , Cmd.none
    | KeyPress CtrlV ->
        // Paste the selected symbols with generated labels and their interconnecting wires
        let toCopyPortIdAliases =
            model.Clipboard.Ports
            |> List.map (fun port -> (port.Id, PortId <| uuid()))
            |> Map.ofList

        let newSymbols =
            ([], model.Clipboard.Symbols)
            ||> List.fold (fun state sm ->
                let newLabel = Symbol.generateUniqueCopiedLabel sm.Label sm.ComponentType (symbols@state)
                let newSymbol =
                    { sm with
                        Id = ComponentId (uuid())
                        Label = newLabel
                        IsTransparent = false
                        IsHighlighted = true
                        Ports =
                            sm.Ports
                            |> List.map (fun port ->
                                { port with
                                    Id = Map.find port.Id toCopyPortIdAliases
                                })
                    }
                state@[newSymbol]
                )
            |> List.map (fun sm -> Symbol.displaceSymbol sm {X = 20.; Y = 20.})

        let createConnectionCmds =
            model.Clipboard.Wires
            |> List.map (fun wr ->
                getWireMsg <| BusWire.CreateConnection (Map.find wr.SourcePort toCopyPortIdAliases, Map.find wr.TargetPort toCopyPortIdAliases))
        
        let newSymbolIds =
            newSymbols
            |> List.map (fun sm -> sm.Id)
            
        let deselectSymbolsCmd = getCmdIfNotEmpty (getSymbolMsg <| Symbol.DeselectSymbols model.Selection.Symbols) model.Selection.Symbols
        let deselectWiresCmd = getCmdIfNotEmpty (getWireMsg <| BusWire.DeselectWires model.Selection.Wires) model.Selection.Wires
        
        { model with
            Selection = {
                Symbols = newSymbolIds
                Wires = List.empty
            }
            PastWireModels = addToWireModels model.Wire model.PastWireModels
            FutureWireModels = List.empty
        }
        , Cmd.batch <|
            getCmdIfNotEmpty (getSymbolMsg <| Symbol.NewSymbols newSymbols) newSymbols
            @createConnectionCmds
            @deselectSymbolsCmd
            @deselectWiresCmd
    | KeyPress CtrlZ ->
        // Set the BusWire model to the last BusWire model. Move the current BusWire model to the FutureWireModels list
        if List.isEmpty model.PastWireModels then
            model
        else
            { model with
                Wire = List.head model.PastWireModels
                FutureWireModels = addToWireModels model.Wire model.FutureWireModels
                PastWireModels = List.tail model.PastWireModels
                Selection = {
                    Symbols = List.empty
                    Wires = List.empty
                }
            }
        , Cmd.none
    | KeyPress CtrlY ->
        // Set the BusWire model to the "future" BusWire model. Move the current BusWire model to the PastWireModels list
        if List.isEmpty model.FutureWireModels then
            model
        else
            { model with
                Wire = List.head model.FutureWireModels
                FutureWireModels = List.tail model.FutureWireModels
                PastWireModels = addToWireModels model.Wire model.PastWireModels
                Selection = {
                    Symbols = List.empty
                    Wires = List.empty
                }
            }
        , Cmd.none
    | KeyPress DEL ->
        // Delete all selected symbols and wires
        let deleteSymbolsCmd = getCmdIfNotEmpty (getWireMsg <| BusWire.DeleteSymbols model.Selection.Symbols) model.Selection.Symbols
        let deleteWiresCmd = getCmdIfNotEmpty (getWireMsg <| BusWire.DeleteWires model.Selection.Wires) model.Selection.Wires
        
        { model with
            Selection = {
                Symbols = List.empty
                Wires = List.empty
            }
            PastWireModels = addToWireModels model.Wire model.PastWireModels
            FutureWireModels = List.empty
        }
        , Cmd.batch <| deleteSymbolsCmd@deleteWiresCmd
    | KeyPress AltShiftZ ->
        printStats()
        model, Cmd.none
    | KeyPress s ->
        match s with
            | AltC -> model, Cmd.ofMsg (Wire <| BusWire.SetColor Blue)
            | AltV -> model, Cmd.ofMsg (Wire <| BusWire.SetColor Green)
            | _ -> model, Cmd.ofMsg (Wire <| BusWire.SetColor Grey)
    | MouseMsg mouseT ->
        match mouseT.Op with
            | Move ->
                // Show and hide ports for specific symbols, while moving mouse
                model, Cmd.batch <| getShowPortsAndHidePortsCmds mouseT.Pos symbols None
            | Down ->
                let clickedElement = getClickedElement mouseT.Pos symbols wires
                
                match clickedElement with
                    | PortElement port ->
                        // Deselect all symbols and wires. Start dragging a connection
                        let deselectSymbolsCmd = getCmdIfNotEmpty (getSymbolMsg <| Symbol.DeselectSymbols model.Selection.Symbols) model.Selection.Symbols
                        let deselectWiresCmd = getCmdIfNotEmpty (getWireMsg <| BusWire.DeselectWires model.Selection.Wires) model.Selection.Wires

                        { model with
                            DragType = Some <| Connection (port, port.Pos, port.Pos)
                            Selection = {
                                Symbols = List.empty
                                Wires = List.empty
                            }
                            ClickToDrop = None
                            CursorType = DefaultCursor
                        }
                        ,  Cmd.batch <| deselectSymbolsCmd@deselectWiresCmd
                    | SheetElement.SymbolElement symbol ->
                        // Other symbols and wires are deselected only if Ctrl is not pressed. Start dragging the symbol
                        let selectedSymbolIds, deselectSymbolsCmd, selectedWireIds, deselectWiresCmd =
                            if model.IsCtrlPressed || List.contains symbol.Id model.Selection.Symbols then
                                appendIfAbsent symbol.Id model.Selection.Symbols, List.empty, model.Selection.Wires, List.empty
                            else
                                [symbol.Id], getCmdIfNotEmpty (getSymbolMsg <| Symbol.DeselectSymbols model.Selection.Symbols) model.Selection.Symbols,
                                List.empty, getCmdIfNotEmpty (getWireMsg <| BusWire.DeselectWires model.Selection.Wires) model.Selection.Wires
                        
                        let startDraggingCmd = [
                            getSymbolMsg <| Symbol.StartDragging (selectedSymbolIds, mouseT.Pos)
                            getSymbolMsg <| Symbol.SelectSymbols (selectedSymbolIds)
                        ]

                        { model with
                            DragType = Some <| Symbol selectedSymbolIds
                            Selection = {
                                Symbols = selectedSymbolIds
                                Wires = selectedWireIds
                            }
                            PastWireModels = addToWireModels model.Wire model.PastWireModels
                            FutureWireModels = List.empty
                            ClickToDrop = None
                            CursorType = DefaultCursor
                        }
                        , Cmd.batch <| deselectSymbolsCmd@deselectWiresCmd@startDraggingCmd
                    | SheetElement.WireElement connection ->
                        // Other wires and symbols are deselected if Ctrl is not pressed. Start dragging the wire
                        let selectedSymbolIds, deselectSymbolsCmd, selectedWireIds, deselectWiresCmd =
                            if model.IsCtrlPressed then
                                model.Selection.Symbols, List.empty, appendIfAbsent connection.Id model.Selection.Wires, List.empty
                            else
                                List.empty, getCmdIfNotEmpty (getSymbolMsg <| Symbol.DeselectSymbols model.Selection.Symbols) model.Selection.Symbols,
                                [connection.Id], getCmdIfNotEmpty (getWireMsg <| BusWire.DeselectWires model.Selection.Wires) model.Selection.Wires 

                        let startDraggingCmd = [getWireMsg <| BusWire.StartDragging (connection.Id, mouseT.Pos)]
                        
                        { model with
                            DragType = Some <| DragType.Wire connection.Id
                            Selection = {
                                Symbols = selectedSymbolIds
                                Wires = selectedWireIds
                            }
                            PastWireModels = addToWireModels model.Wire model.PastWireModels
                            FutureWireModels = List.empty
                            ClickToDrop = None
                            CursorType = DefaultCursor
                        }
                        , Cmd.batch <| deselectSymbolsCmd@deselectWiresCmd@startDraggingCmd
                    | SheetElement.CanvasElement ->
                        // Deselect all symbols and wires. Start dragging a multi-select rectangle
                        let deselectSymbolsCmd = getCmdIfNotEmpty (getSymbolMsg <| Symbol.DeselectSymbols model.Selection.Symbols) model.Selection.Symbols
                        let deselectWiresCmd = getCmdIfNotEmpty (getWireMsg <| BusWire.DeselectWires model.Selection.Wires) model.Selection.Wires

                        match model.ClickToDrop with
                            | Some (cType, label) ->
                                let newLabel = Symbol.generateUniqueCopiedLabel label cType symbols
                                { model with
                                    DragType = None
                                    Selection = {
                                        Symbols = List.empty
                                        Wires = List.empty
                                    }
                                    GuidelineNearby = None
                                    ClickToDrop = None
                                    CursorType = DefaultCursor
                                    PastWireModels = addToWireModels model.Wire model.PastWireModels
                                    FutureWireModels = List.empty
                                }
                                , Cmd.batch <| deselectSymbolsCmd@deselectWiresCmd@[getSymbolMsg <| Symbol.AddSymbol (cType, mouseT.Pos, newLabel)]
                            | None ->
                                { model with
                                    DragType = Some <| Canvas (mouseT.Pos, mouseT.Pos)
                                    Selection = {
                                        Symbols = List.empty
                                        Wires = List.empty
                                    }
                                    GuidelineNearby = None
                                    ClickToDrop = None
                                    CursorType = DefaultCursor
                                }
                                , Cmd.batch <| deselectSymbolsCmd@deselectWiresCmd
            | Drag ->
                match model.DragType with
                    | Some (Symbol sIds) ->
                        // Dragging Symbol
                        { model with
                            GuidelineNearby =
                                if List.length sIds = 1 then
                                    getNearGuidelines (List.exactlyOne sIds) model
                                else
                                    None
                        }
                        , getSymbolMsg <| Symbol.Dragging (sIds, mouseT.Pos)

                    | Some (Connection (srcPort, srcPos, _)) ->
                        // Dragging Connection
                        { model with
                            DragType = Some <| Connection (srcPort,srcPos, mouseT.Pos)
                        }
                        , Cmd.batch <| getShowPortsAndHidePortsCmds mouseT.Pos symbols (Some <| getOppositePortType srcPort.PortType)
                    | Some (DragType.Wire cId) ->
                        // Dragging Wire
                        model, getWireMsg <| BusWire.Dragging (cId, mouseT.Pos)
                    | Some (Canvas (srcPos, _)) ->
                        // Dragging multi-select rectangle
                        { model with
                            DragType = Some <| Canvas (srcPos, mouseT.Pos)
                        }
                        , Cmd.batch <| getShowPortsAndHidePortsCmds mouseT.Pos symbols None
                    | None ->
                        // Will never happen, because DragType is always set by during MouseMsg Down
                        model, Cmd.none
            | Up ->
                match model.DragType with
                    | Some (Symbol sIds) ->
                        //End symbol dragging
                        let snapPosition =
                            model.GuidelineNearby
                            |> Option.bind (fun ((dist, _), orientation) ->
                                match orientation with
                                    | GuidelineLeft | GuidelineRight -> Some {X = dist; Y = 0.0}
                                    | _ -> Some {X = 0.0; Y = dist}
                                )
                        { model with
                            DragType = None
                            GuidelineNearby = None
                        },                            
                        getSymbolMsg <| Symbol.EndDragging (sIds, snapPosition)
                    | Some (Connection (srcPort,_,_)) ->
                        //End connection dragging. If on a valid port, create a connection
                        let clickedPort = tryFindClickedPort mouseT.Pos symbols
                        
                        match clickedPort with
                            | Some targetPort ->
                                match srcPort.PortType, targetPort.PortType with
                                    | PortType.Input, PortType.Output ->
                                        { model with
                                            DragType = None
                                            PastWireModels = addToWireModels model.Wire model.PastWireModels
                                            FutureWireModels = List.empty
                                        },
                                        Cmd.ofMsg (Wire <| BusWire.CreateConnection (targetPort.Id, srcPort.Id))
                                    | PortType.Output, PortType.Input ->
                                        { model with
                                            DragType = None
                                            PastWireModels = addToWireModels model.Wire model.PastWireModels
                                            FutureWireModels = List.empty
                                        },
                                        Cmd.ofMsg (Wire <| BusWire.CreateConnection (srcPort.Id, targetPort.Id))
                                    | _ ->
                                        { model with
                                            DragType = None  
                                        },
                                        Cmd.none
                            | None ->
                                { model with
                                    DragType = None  
                                },
                                Cmd.none
                    | Some (Canvas (srcPos, lastDragPos)) ->
                        //Select symbols inside of rectangle
                        let rectangle = alignBoundingBox {P1=srcPos; P2=lastDragPos}
    
                        let toSelectSymbolIds = getSymbolsInsideBoundingBox rectangle symbols
                        let toSelectWireIds = getWiresInsideBoundingBox rectangle wires
                        
                        let selectSymbolsCmd = getCmdIfNotEmpty (getSymbolMsg <| Symbol.SelectSymbols toSelectSymbolIds) toSelectSymbolIds
                        let selectWiresCmd = getCmdIfNotEmpty (getWireMsg <| BusWire.SelectWires toSelectWireIds) toSelectWireIds

                        { model with
                            DragType = None
                            Selection = {
                                Symbols = toSelectSymbolIds
                                Wires = toSelectWireIds
                            }
                        }
                        , Cmd.batch <| selectSymbolsCmd@selectWiresCmd
                    | Some (DragType.Wire cId) ->
                        //End wire dragging
                        { model with
                            DragType = None
                        },
                        getWireMsg <| BusWire.EndDragging cId
                    | None ->
                        model, Cmd.none

let init() = 
    let model, cmds = (BusWire.init)()
    {        
        Wire = model
        DragType = None
        IsCtrlPressed = false
        GuidelineNearby = None
        Clipboard = {
            Symbols = List.empty
            Wires = List.empty
            Ports = List.empty
        }
        Selection = {
            Symbols = List.empty
            Wires = List.empty
        }
        PastWireModels = List.empty
        FutureWireModels = List.empty
        ClickToDrop = None
        CursorType = DefaultCursor
        Zoom = 1.0
    }, Cmd.map Wire cmds

//----------------------interface to Issie-----------------------------//
let extractCanvasState (model: Model) : CanvasState =
    (Symbol.extractComponents model.Wire.Symbols, BusWire.extractWires model.Wire)