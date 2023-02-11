module Symbol

open System.Runtime.Serialization
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers

//------------------------------------------------------------------------//
//-------------------------------Symbol Types-----------------------------//
//------------------------------------------------------------------------//

/// Used to define what width wire the port expects
type InferenceRule =
    | LessThan of int
    | GreaterThan of int
    | EqualTo of int

type Port =
    { Id: CommonTypes.PortId
      PortType: CommonTypes.PortType
      Pos: XYPos
      BoundingBox: BoundingBox
      Width: int option
      InferenceRule: InferenceRule
      IsHighlighted: bool
      IsPositionModified: bool
      IsInverted: bool
      Label: string }


type Symbol =
    { Id: CommonTypes.ComponentId
      BoundingBox: BoundingBox
      Ports: Port list
      Pos: XYPos
      LastDragPos: XYPos
      IsShowingPorts: bool
      ShowingPortsType: CommonTypes.PortType option
      IsHighlighted: bool
      IsTransparent: bool
      NoOfInputPorts: int
      NoOfOutputPorts: int
      ComponentType: CommonTypes.ComponentType
      Label: string }


type Model = Symbol list

//----------------------------Message Type-----------------------------------//

/// Messages to update symbol model
type Msg =
    | MouseMsg of MouseT
    | ShowPorts of sIds: CommonTypes.ComponentId list * portType: CommonTypes.PortType option
    | HidePorts of sIds: CommonTypes.ComponentId list
    | StartDragging of sIds: CommonTypes.ComponentId list * pagePos: XYPos
    | Dragging of sIds: CommonTypes.ComponentId list * pagePos: XYPos
    | EndDragging of sIds: CommonTypes.ComponentId list * snapPosition: XYPos option
    | SelectSymbols of sIds: CommonTypes.ComponentId list
    | DeselectSymbols of sIds: CommonTypes.ComponentId list
    | AddSymbol of cType: CommonTypes.ComponentType * pagePos: XYPos * label: string
    | DeleteSymbols of sIds: CommonTypes.ComponentId list
    | SetPortWidths of portIdAndWidths: (CommonTypes.PortId * int option) list
    | UpdateConnections of symbolId: CommonTypes.ComponentId
    | EditSymbolLabel of symbolId: CommonTypes.ComponentId * label: string
    | NewSymbols of symbols: Symbol list

// ---------------- Grid System------------------------//

type Grid =
    { Rows: float list //List of heights of each row in px
      Columns: float list } //List of widths of each column in px

//Where object should be aligned within a cell, works horizontally and vertically
type GridAlignment =
    | Start
    | Middle
    | End

let gridWidth (grid: Grid) : float = (0., grid.Columns) ||> List.fold (+)

let gridHeight (grid: Grid) : float = (0., grid.Rows) ||> List.fold (+)

let gridAddCol (width: float) (grid: Grid) : Grid =
    match grid.Columns with
    | x -> { grid with Columns = width :: x }

let gridAddRow (height: float) (grid: Grid) : Grid =
    match grid.Rows with
    | x -> { grid with Rows = height :: x }

let gridRemoveCol (grid: Grid) : Grid =
    match grid.Columns with
    | [] -> { grid with Columns = [] }
    | _ :: ls -> { grid with Columns = ls }

let gridRemoveRow (grid: Grid) : Grid =
    match grid.Rows with
    | [] -> { grid with Rows = [] }
    | _ :: ls -> { grid with Rows = ls }

/// Calculates 'distance' of a child depending on its row/col. used because it abstracts out gridX and gridY.
let gridDistance (list: float list) (index: int) (alignment: GridAlignment) (span: int) : float =
    let endIndex =
        match alignment with
        | Start
        | Middle -> index
        | End -> index + span

    let filtered =
        list
        |> List.indexed
        |> List.filter (fun (i, _) -> i < endIndex)

    let distance =
        (0., filtered)
        ||> List.fold (fun sum (_, el) -> sum + el)

    match alignment with
    | Start
    | End -> distance
    | Middle ->
        let spanDistance =
            //not exception safe
            (0., list.[index..index + span - 1])
            ||> List.fold (+)
        //align with midpoint of span
        distance + spanDistance / 2.

/// Get X coords from col
let gridX (grid: Grid) (col: int) (alignment: GridAlignment) (span: int) : float =
    gridDistance grid.Columns col alignment span

/// Get Y coords from row
let gridY (grid: Grid) (row: int) (alignment: GridAlignment) (span: int) : float =
    gridDistance grid.Rows row alignment span


//---------------------------------helper types and functions----------------//
/// Returns a port from its id
let findPort (model: Model) (portId: CommonTypes.PortId) : Port =
    model
    |> List.collect (fun symbol -> symbol.Ports)
    |> List.filter (fun port -> port.Id = portId)
    |> List.head

/// Returns a tuple containing the port and its symbol parent's sId
let findPortWithSymbolId (model: Model) (portId: CommonTypes.PortId) =
    model
    |> List.pick
        (fun symbol ->
            symbol.Ports
            |> List.tryFind (fun port -> port.Id = portId)
            |> Option.bind (fun port -> Some(port, symbol.Id)))

/// Gets a symbol from its sId
let getSymbol (sModel: Model) (sId: CommonTypes.ComponentId) : Symbol =
    sModel |> List.find (fun sm -> sm.Id = sId)

/// Displaces a symbol position by a certain amount
let displaceSymbol (symbol: Symbol) (diff: XYPos) : Symbol =
    { symbol with
          Pos = posAdd symbol.Pos diff
          BoundingBox = updateBoundingBox symbol.BoundingBox diff
          Ports =
              symbol.Ports
              |> List.map
                  (fun port ->
                      { port with
                            Pos = posAdd port.Pos diff
                            BoundingBox = updateBoundingBox port.BoundingBox diff }) }

/// Removes the number after the last underscore, e.g "decode_1" -> "decode"; "decode_1_2" -> "decode_1".
/// If there is no underscore, returns the label.
/// If the substring after the underscore is not a valid number, returns the label
let trimLabelAfterUnderscore (label: string) =
    let underscoreIndex = label.LastIndexOf "_"

    match underscoreIndex with
    | -1 -> label
    | index ->
        let afterUnderscore = label.[index + 1..label.Length]

        if numberRegex.IsMatch afterUnderscore then
            label.[0..(index - 1)]
        else
            label

/// Returns the number after the last underscore, if there is no number returns 0.
let getNumberAfterUnderscore (label: string) =
    let index = label.LastIndexOf "_"
    let afterUnderscoreStr = label.[index + 1..label.Length]

    match System.Int32.TryParse afterUnderscoreStr with
    | true, i -> i
    | false, _ -> 0

/// Generates a unique copied label. Finds the maximum existing number suffix for a symbol of the same component type and same label,
/// and returns the label with a suffix of one higher.
let generateUniqueCopiedLabel (label: string) (cType: CommonTypes.ComponentType) (symbols: Symbol list) =
    let trimmedLabel = trimLabelAfterUnderscore label

    let sameLabels =
        symbols
        |> List.filter (fun sm -> sm.ComponentType = cType)
        |> List.map (fun sm -> sm.Label)
        |> List.filter (fun lbl -> trimLabelAfterUnderscore lbl = trimmedLabel)

    match sameLabels with
    | [] -> trimmedLabel
    | labels ->
        labels
        |> List.map getNumberAfterUnderscore
        |> List.max
        |> (fun i -> trimmedLabel + "_" + (string <| (i + 1)))


let unhighlightSymbols (symbols: Symbol list) =
    symbols
    |> List.map (fun sm -> { sm with IsHighlighted = false })

let getHeightBoundingBox bBox : float = bBox.P1.Y - bBox.P2.Y

let getWidthBoundingBox bBox : float = bBox.P2.X - bBox.P1.X

let getSymbolFromPortId (model: Model) (portId: CommonTypes.PortId) : Symbol option =
    (None, model)
    ||> List.fold
            (fun out symbol ->
                let portIds =
                    List.map (fun (port: Port) -> port.Id) symbol.Ports

                if List.contains portId portIds then
                    Some symbol
                else
                    out)


/// Takes a port list and splits it into InputPorts OutputPorts and Enable and Clk ports
let private splitPorts
    (ports: Port list)
    : {| InputPorts: Port list
         OutputPorts: Port list
         EnClkPorts: Port list |} =
    {| OutputPorts = List.filter (fun port -> port.PortType = CommonTypes.PortType.Output) ports
       InputPorts =
           List.filter
               (fun port ->
                   (port.PortType = CommonTypes.PortType.Input)
                   && port.Label <> "EN"
                   && port.Label <> "Clk")
               ports
       EnClkPorts =
           List.filter
               (fun port ->
                   (port.PortType = CommonTypes.PortType.Input)
                   && (port.Label = "EN" || port.Label = "Clk"))
               ports |}

/// Returns the label displayed for each ComponentType, the width of the string determines the width of the symbol (reason ROM & RAM are padded)
let getComponentName (componentType: CommonTypes.ComponentType) : string =
    match componentType with
    | CommonTypes.ComponentType.Input _ -> ""
    | CommonTypes.ComponentType.Output _ -> ""
    | CommonTypes.ComponentType.IOLabel -> ""
    | CommonTypes.ComponentType.BusSelection _ -> "Bus Select"
    | CommonTypes.ComponentType.Constant _ -> ""
    | CommonTypes.ComponentType.Not -> "1"
    | CommonTypes.ComponentType.And -> "&"
    | CommonTypes.ComponentType.Or -> "≥1"
    | CommonTypes.ComponentType.Xor -> "=1"
    | CommonTypes.ComponentType.Nand -> "&"
    | CommonTypes.ComponentType.Nor -> "≥1"
    | CommonTypes.ComponentType.Xnor -> "=1"
    | CommonTypes.ComponentType.Decode4 -> "Decode"
    | CommonTypes.ComponentType.Mux2 -> "Mux"
    | CommonTypes.ComponentType.Demux2 -> "Demux"
    | CommonTypes.ComponentType.NbitsAdder width -> sprintf "Adder (%i..0)" (width - 1)
    | CommonTypes.ComponentType.Custom customComponentType -> customComponentType.Name
    | CommonTypes.ComponentType.MergeWires -> ""
    | CommonTypes.ComponentType.SplitWire _ -> ""
    | CommonTypes.ComponentType.DFF -> "DFF"
    | CommonTypes.ComponentType.DFFE -> "DFFE"
    | CommonTypes.ComponentType.Register _ -> " Register "
    | CommonTypes.ComponentType.RegisterE _ -> "RegisterE"
    | CommonTypes.ComponentType.AsyncROM _ -> "Async ROM"
    | CommonTypes.ComponentType.ROM _ -> " ROM "
    | CommonTypes.ComponentType.RAM _ -> "   RAM   "


//------------------------- Bounding Box & Position functions --------------------------//
/// Calculates a ports bounding box from its pos
let calcPortBoundingBox (portPos: XYPos) =
    { P1 =
          { X = portPos.X - 10.
            Y = portPos.Y - 10. }
      P2 =
          { X = portPos.X + 10.
            Y = portPos.Y + 10. } }


/// used to calculate the Bounding Box of an automatic Symbol (not ports)
let calcSymbolBoundingBox (componentType: CommonTypes.ComponentType) (ports: Port list) (pos: XYPos) : BoundingBox =
    let splitPorts = splitPorts ports
    let compNameLength = (getComponentName componentType).Length
    let titleHeight = if compNameLength < 3 then 5. else 40.

    let height =
        titleHeight
        + 40.
          * (float)
              (
                  max splitPorts.InputPorts.Length splitPorts.OutputPorts.Length
                  + splitPorts.EnClkPorts.Length
              )

    let width = 60. + 10. * (float) compNameLength

    updateBoundingBox
        { P1 = { X = 0.; Y = 0. }
          P2 = { X = width; Y = height } }
        pos

/// this function creates a grid object that matches the one that is rendered. Used for calculating automatic Port locations
let private initGrid
    (inputPorts: Port list)
    (outputPorts: Port list)
    (enClkPorts: Port list)
    (compName: string)
    : Grid =
    let grid : Grid =
        let titleHeight = if compName.Length < 3 then 5. else 40.

        { Columns =
              [ 30.
                (float) compName.Length * 10.
                30. ]
          Rows =
              [ titleHeight
                (float) (max inputPorts.Length outputPorts.Length)
                * 40.
                (float) enClkPorts.Length * 40. ] }

    grid

/// This is used for the automatic symbols, calculates the position of a port by index
let calcPortPos
    (portIndex: int)
    (portType: CommonTypes.PortType)
    (portLabel: string)
    (isInverted: bool)
    (symbol: Symbol)
    : XYPos =
    let splitPorts = splitPorts symbol.Ports

    let portGrid =
        match portType, portLabel with
        | CommonTypes.PortType.Input, "EN"
        | CommonTypes.PortType.Input, "Clk" ->
            ({ Columns = [ 30. ]; Rows = [] }, splitPorts.EnClkPorts) // i feel this is dodgy, done so that i dont have to define a whole new port just to add a line
            ||> List.fold (fun grid _ -> gridAddRow 40. grid)
        | CommonTypes.PortType.Input, _ ->
            ({ Columns = [ 30. ]; Rows = [] }, splitPorts.InputPorts)
            ||> List.fold (fun grid _ -> gridAddRow 40. grid)
        | CommonTypes.PortType.Output, _ ->
            ({ Columns = [ 30. ]; Rows = [] }, splitPorts.OutputPorts)
            ||> List.fold (fun grid _ -> gridAddRow 40. grid)

    let offsetX =
        match isInverted, portType with
        | true, CommonTypes.PortType.Output -> 14.
        | true, CommonTypes.PortType.Input -> -14.
        | false, _ -> 0.

    let alignment =
        match portType with
        | CommonTypes.PortType.Input -> GridAlignment.Start
        | CommonTypes.PortType.Output -> GridAlignment.End

    let posPortg =
        { X = gridX portGrid 0 alignment 1 + offsetX
          Y = gridY portGrid portIndex GridAlignment.Middle 1 }


    let parentGrid =
        initGrid
            splitPorts.InputPorts
            splitPorts.OutputPorts
            splitPorts.EnClkPorts
            (getComponentName symbol.ComponentType)

    let posParentg =
        match portType, portLabel with
        | CommonTypes.PortType.Input, "EN"
        | CommonTypes.PortType.Input, "Clk" ->
            { X = (gridX parentGrid 0 GridAlignment.Start 1)
              Y = (gridY parentGrid 2 GridAlignment.Start 1) }
        | CommonTypes.PortType.Input, _ ->
            { X = (gridX parentGrid 0 GridAlignment.Start 1)
              Y =
                  (gridY parentGrid 1 GridAlignment.Middle 1
                   - (float) splitPorts.InputPorts.Length * 40. / 2.) }
        | CommonTypes.PortType.Output, _ ->
            { X = (gridX parentGrid 2 GridAlignment.Start 1)
              Y =
                  (gridY parentGrid 1 GridAlignment.Middle 1
                   - (float) splitPorts.OutputPorts.Length * 40. / 2.) }

    { X = symbol.Pos.X + posParentg.X + posPortg.X
      Y = symbol.Pos.Y + posParentg.Y + posPortg.Y }


/// updates a port with its calculated pos and Bbox (used in manual symbols)
let updatePorts (positions: XYPos list) (ports: Port list) (symbolPos: XYPos) : Port list =
    List.map2
        (fun portPos port ->
            let finalPortPos = posAdd symbolPos portPos

            { port with
                  Pos = finalPortPos
                  BoundingBox = calcPortBoundingBox finalPortPos })
        positions
        ports

/// calculates the locations of bounding box and pos for each port for each symbol
let calculatePortsPosAndBoundingBoxes (symbol: Symbol) : Symbol =
    match symbol.ComponentType with
    //these symbols are rendered manually, so bounding boxes are defined manually
    | CommonTypes.ComponentType.Input _ ->
        let symbolBBox =
            updateBoundingBox
                { P1 = { X = 0.; Y = 0. }
                  P2 = { X = 71.; Y = 42. } }
                symbol.Pos

        let portPositions = [ { X = 71.; Y = 21. } ]

        { symbol with
              BoundingBox = symbolBBox
              Ports = updatePorts portPositions symbol.Ports symbol.Pos }

    | CommonTypes.ComponentType.Output _ ->
        let symbolBBox =
            updateBoundingBox
                { P1 = { X = 0.; Y = 0. }
                  P2 = { X = 72.; Y = 43. } }
                symbol.Pos

        let portPositions = [ { X = 0.; Y = 21. } ]

        { symbol with
              BoundingBox = symbolBBox
              Ports = updatePorts portPositions symbol.Ports symbol.Pos }

    | CommonTypes.ComponentType.IOLabel ->
        let symbolBBox =
            updateBoundingBox
                { P1 = { X = 0.; Y = 0. }
                  P2 = { X = 63.; Y = 43. } }
                symbol.Pos

        let portPositions =
            [ { X = 0.; Y = 21. }
              { X = 63.; Y = 21. } ]

        { symbol with
              BoundingBox = symbolBBox
              Ports = updatePorts portPositions symbol.Ports symbol.Pos }

    | CommonTypes.ComponentType.Constant _ ->
        let symbolBBox =
            updateBoundingBox
                { P1 = { X = 0.; Y = 0. }
                  P2 = { X = 70.; Y = 43. } }
                symbol.Pos

        let portPositions = [ { X = 70.; Y = 21. } ]

        { symbol with
              BoundingBox = symbolBBox
              Ports = updatePorts portPositions symbol.Ports symbol.Pos }

    | CommonTypes.ComponentType.MergeWires ->
        let symbolBBox =
            updateBoundingBox
                { P1 = { X = 0.; Y = 0. }
                  P2 = { X = 63.; Y = 51. } }
                symbol.Pos

        let portPositions =
            [ { X = 0.; Y = 0. }
              { X = 0.; Y = 51. }
              { X = 63.; Y = 25. } ]

        { symbol with
              BoundingBox = symbolBBox
              Ports = updatePorts portPositions symbol.Ports symbol.Pos }

    | CommonTypes.ComponentType.SplitWire _ ->
        let symbolBBox =
            updateBoundingBox
                { P1 = { X = 0.; Y = 0. }
                  P2 = { X = 63.; Y = 51. } }
                symbol.Pos

        let portPositions =
            [ { X = 0.; Y = 25. }
              { X = 63.; Y = 0. }
              { X = 63.; Y = 51. } ]

        { symbol with
              BoundingBox = symbolBBox
              Ports = updatePorts portPositions symbol.Ports symbol.Pos }

    //all other symbols are rendered automatically, bbox calculations are as follows
    | _ ->
        let splitPorts = splitPorts symbol.Ports

        let updatePosAndBound (i: int) (port: Port) =
            let pos =
                calcPortPos i port.PortType port.Label port.IsInverted symbol

            let bBox = calcPortBoundingBox pos

            { port with
                  Pos = pos
                  BoundingBox = bBox }

        let updatedInputs =
            List.mapi updatePosAndBound splitPorts.InputPorts

        let updatedOutputs =
            List.mapi updatePosAndBound splitPorts.OutputPorts

        let updatedEnClk =
            List.mapi updatePosAndBound splitPorts.EnClkPorts

        { symbol with
              Ports = updatedInputs @ updatedOutputs @ updatedEnClk }


//----------------------- Functions to create symbols -----------------------//

/// Creates a port object, adds it to the symbol and recalculates all pos and bounding boxes (they change when symbol changes shape)
let addPort
    (portType: CommonTypes.PortType)
    (width: int option)
    (label: string)
    (isInverted: bool)
    (inferenceRule: InferenceRule)
    (symbol: Symbol)
    : Symbol =
    let port =
        { Id = CommonTypes.PortId(uuid ())
          PortType = portType
          Pos = { X = 0.; Y = 0. } //calculate this
          BoundingBox =
              { P1 = { X = 0.; Y = 0. }
                P2 = { X = 0.; Y = 0. } }
          Width = width
          IsHighlighted = true
          IsPositionModified = false
          Label = label
          IsInverted = isInverted
          InferenceRule = inferenceRule }

    let ports = symbol.Ports @ [ port ]

    //when adding a port, recalculate all port bounding boxes.
    let newSymbol =
        { symbol with
              Ports = ports
              BoundingBox = calcSymbolBoundingBox symbol.ComponentType ports symbol.Pos }

    calculatePortsPosAndBoundingBoxes newSymbol

/// This is the function that is used to actually create a symbol. Defines the boundingBoxes and positions of ports and symbol etc
let createNewSymbol (pos: XYPos) (label: string) (componentType: CommonTypes.ComponentType) : Symbol =
    let defaultSymbol =
        { Id = CommonTypes.ComponentId(uuid ())
          BoundingBox =
              { P1 = { X = 0.; Y = 0. }
                P2 = { X = 0.; Y = 0. } }
          Ports = []
          Pos = pos
          LastDragPos = { X = 0.; Y = 0. } // initial value can always be this
          IsShowingPorts = false
          ShowingPortsType = None
          IsHighlighted = false
          IsTransparent = false
          NoOfInputPorts = 0 //this is calculated after all ports have been added
          NoOfOutputPorts = 0 //this is calculated after all ports have been added
          ComponentType = componentType
          Label = label }

    let symbolWithPorts =
        match componentType with
        | CommonTypes.ComponentType.Input width ->
            defaultSymbol
            |> addPort CommonTypes.PortType.Output (Some width) "" false (EqualTo width)

        | CommonTypes.ComponentType.Output width ->
            defaultSymbol
            |> addPort CommonTypes.PortType.Input (Some width) "" false (EqualTo width)

        | CommonTypes.ComponentType.IOLabel ->
            defaultSymbol
            |> addPort CommonTypes.PortType.Input None "" false (GreaterThan 0)
            |> addPort CommonTypes.PortType.Output None "" false (GreaterThan 0)

        | CommonTypes.ComponentType.Constant (width, _) ->
            defaultSymbol
            |> addPort CommonTypes.PortType.Output (Some width) "" false (EqualTo width)

        | CommonTypes.ComponentType.BusSelection (outWidth, outLSB) ->
            defaultSymbol
            |> addPort CommonTypes.PortType.Input None "In" false (GreaterThan(outWidth + outLSB - 1))
            |> addPort
                CommonTypes.PortType.Output
                (Some outWidth)
                (if outWidth > 1 then
                     sprintf "[%i..%i]" (outLSB + outWidth - 1) outLSB
                 else
                     sprintf "[%i]" (outLSB + outWidth - 1))
                false
                (EqualTo outWidth)

        | CommonTypes.ComponentType.Not ->
            defaultSymbol
            |> addPort CommonTypes.PortType.Input (Some 1) "" false (EqualTo 1)
            |> addPort CommonTypes.PortType.Output (Some 1) "" true (EqualTo 1)

        | CommonTypes.ComponentType.And
        | CommonTypes.ComponentType.Or
        | CommonTypes.ComponentType.Xor ->
            defaultSymbol
            |> addPort CommonTypes.PortType.Input (Some 1) "" false (EqualTo 1)
            |> addPort CommonTypes.PortType.Input (Some 1) "" false (EqualTo 1)
            |> addPort CommonTypes.PortType.Output (Some 1) "" false (EqualTo 1)

        | CommonTypes.ComponentType.Nand
        | CommonTypes.ComponentType.Nor
        | CommonTypes.ComponentType.Xnor ->
            defaultSymbol
            |> addPort CommonTypes.PortType.Input (Some 1) "" false (EqualTo 1)
            |> addPort CommonTypes.PortType.Input (Some 1) "" false (EqualTo 1)
            |> addPort CommonTypes.PortType.Output (Some 1) "" true (EqualTo 1)

        | CommonTypes.ComponentType.Decode4 ->
            defaultSymbol
            |> addPort CommonTypes.PortType.Input (Some 2) "Sel" false (EqualTo 2)
            |> addPort CommonTypes.PortType.Input (Some 1) "Data" false (EqualTo 1)
            |> addPort CommonTypes.PortType.Output (Some 1) "0" false (EqualTo 1)
            |> addPort CommonTypes.PortType.Output (Some 1) "1" false (EqualTo 1)
            |> addPort CommonTypes.PortType.Output (Some 1) "2" false (EqualTo 1)
            |> addPort CommonTypes.PortType.Output (Some 1) "3" false (EqualTo 1)

        | CommonTypes.ComponentType.Mux2 ->
            defaultSymbol
            |> addPort CommonTypes.PortType.Input None "0" false (GreaterThan 0)
            |> addPort CommonTypes.PortType.Input None "1" false (GreaterThan 0)
            |> addPort CommonTypes.PortType.Input (Some 1) "Sel" false (EqualTo 1)
            |> addPort CommonTypes.PortType.Output None "" false (GreaterThan 0)

        | CommonTypes.ComponentType.Demux2 ->
            defaultSymbol
            |> addPort CommonTypes.PortType.Input None "In" false (GreaterThan 0)
            |> addPort CommonTypes.PortType.Input (Some 1) "Sel" false (EqualTo 1)
            |> addPort CommonTypes.PortType.Output None "0" false (GreaterThan 0)
            |> addPort CommonTypes.PortType.Output None "1" false (GreaterThan 0)

        | CommonTypes.ComponentType.NbitsAdder width ->
            defaultSymbol
            |> addPort CommonTypes.PortType.Input (Some 1) "Cin" false (EqualTo 1)
            |> addPort CommonTypes.PortType.Input (Some width) "A" false (EqualTo width)
            |> addPort CommonTypes.PortType.Input (Some width) "B" false (EqualTo width)
            |> addPort CommonTypes.PortType.Output (Some width) "Sum" false (EqualTo width)
            |> addPort CommonTypes.PortType.Output (Some 1) "Cout" false (EqualTo 1)

        | CommonTypes.ComponentType.Custom customComponentType ->
            let symWithInputs =
                (defaultSymbol, customComponentType.InputLabels)
                ||> List.fold
                        (fun sym newPort ->
                            let width = (snd newPort)

                            sym
                            |> addPort CommonTypes.PortType.Input (Some width) (fst newPort) false (EqualTo width))

            (symWithInputs, customComponentType.OutputLabels)
            ||> List.fold
                    (fun sym newPort ->
                        let width = (snd newPort)

                        sym
                        |> addPort CommonTypes.PortType.Output (Some width) (fst newPort) false (EqualTo width))

        | CommonTypes.ComponentType.MergeWires ->
            defaultSymbol
            |> addPort CommonTypes.PortType.Input None "" false (GreaterThan 0)
            |> addPort CommonTypes.PortType.Input None "" false (GreaterThan 0)
            |> addPort CommonTypes.PortType.Output None "" false (GreaterThan 0)

        | CommonTypes.ComponentType.SplitWire topWidth ->
            defaultSymbol
            |> addPort CommonTypes.PortType.Input None "" false (GreaterThan topWidth)
            |> addPort CommonTypes.PortType.Output (Some topWidth) "" false (EqualTo topWidth)
            |> addPort CommonTypes.PortType.Output None "" false (GreaterThan 0)

        | CommonTypes.ComponentType.DFF ->
            defaultSymbol
            |> addPort CommonTypes.PortType.Input (Some 1) "D" false (EqualTo 1)
            |> addPort CommonTypes.PortType.Input (Some 1) "Clk" false (EqualTo 1)
            |> addPort CommonTypes.PortType.Output (Some 1) "Q" false (EqualTo 1)

        | CommonTypes.ComponentType.DFFE ->
            defaultSymbol
            |> addPort CommonTypes.PortType.Input (Some 1) "D" false (EqualTo 1)
            |> addPort CommonTypes.PortType.Input (Some 1) "EN" false (EqualTo 1)
            |> addPort CommonTypes.PortType.Input (Some 1) "Clk" false (EqualTo 1)
            |> addPort CommonTypes.PortType.Output (Some 1) "Q" false (EqualTo 1)

        | CommonTypes.ComponentType.Register width ->
            defaultSymbol
            |> addPort CommonTypes.PortType.Input (Some width) "Data-in" false (EqualTo width)
            |> addPort CommonTypes.PortType.Input (Some 1) "Clk" false (EqualTo 1)
            |> addPort CommonTypes.PortType.Output (Some width) "Data-out" false (EqualTo width)

        | CommonTypes.ComponentType.RegisterE width ->
            defaultSymbol
            |> addPort CommonTypes.PortType.Input (Some width) "Data-in" false (EqualTo width)
            |> addPort CommonTypes.PortType.Input (Some 1) "EN" false (EqualTo 1)
            |> addPort CommonTypes.PortType.Input (Some 1) "Clk" false (EqualTo 1)
            |> addPort CommonTypes.PortType.Output (Some width) "Data-out" false (EqualTo width)

        | CommonTypes.ComponentType.AsyncROM memory ->
            defaultSymbol
            |> addPort CommonTypes.PortType.Input (Some memory.AddressWidth) "Addr" false (EqualTo memory.AddressWidth)
            |> addPort CommonTypes.PortType.Output (Some memory.WordWidth) "Data" false (EqualTo memory.WordWidth)

        | CommonTypes.ComponentType.ROM memory ->
            defaultSymbol
            |> addPort CommonTypes.PortType.Input (Some memory.AddressWidth) "Addr" false (EqualTo memory.AddressWidth)
            |> addPort CommonTypes.PortType.Input (Some 1) "Clk" false (EqualTo 1)
            |> addPort CommonTypes.PortType.Output (Some memory.WordWidth) "Data" false (EqualTo memory.WordWidth)

        | CommonTypes.ComponentType.RAM memory ->
            defaultSymbol
            |> addPort CommonTypes.PortType.Input (Some memory.AddressWidth) "Addr" false (EqualTo memory.AddressWidth)
            |> addPort CommonTypes.PortType.Input (Some memory.WordWidth) "Data-in" false (EqualTo memory.WordWidth)
            |> addPort CommonTypes.PortType.Input (Some 1) "Write" false (EqualTo 1)
            |> addPort CommonTypes.PortType.Input (Some 1) "Clk" false (EqualTo 1)
            |> addPort CommonTypes.PortType.Output (Some memory.WordWidth) "Data-out" false (EqualTo memory.WordWidth)

    let symbolWithBBoxes =
        calculatePortsPosAndBoundingBoxes symbolWithPorts

    let splitPorts = splitPorts symbolWithBBoxes.Ports

    { symbolWithBBoxes with
          NoOfInputPorts =
              splitPorts.InputPorts.Length
              + splitPorts.EnClkPorts.Length
          NoOfOutputPorts = splitPorts.OutputPorts.Length }

/// Init function
let init () = [], Cmd.none

/// update function which displays symbols
let update (msg: Msg) (model: Model) : Model * Cmd<'a> =
    match msg with
    | AddSymbol (cType, pagePos, label) -> (createNewSymbol pagePos label cType) :: model, Cmd.none
    | DeleteSymbols sIds -> List.filter (fun sym -> not <| List.contains sym.Id sIds) model, Cmd.none
    | StartDragging (sIds, pagePos) ->
        model
        |> List.map
            (fun sym ->
                if List.contains sym.Id sIds then
                    { sym with
                          LastDragPos = pagePos
                          Ports =
                              sym.Ports
                              |> List.map (fun port -> { port with IsPositionModified = true }) }
                else
                    sym),
        Cmd.none
    | Dragging (sIds, pagePos) ->
        model
        |> List.map
            (fun sym ->
                if List.contains sym.Id sIds then
                    let diff = posDiff pagePos sym.LastDragPos

                    { sym with
                          Pos = posAdd sym.Pos diff
                          IsTransparent = true
                          LastDragPos = pagePos
                          BoundingBox = updateBoundingBox sym.BoundingBox diff
                          Ports =
                              sym.Ports
                              |> List.map
                                  (fun port ->
                                      { port with
                                            Pos = posAdd port.Pos diff
                                            BoundingBox = updateBoundingBox port.BoundingBox diff }) }
                else
                    sym),
        Cmd.none

    | EndDragging (sIds, snap) ->
        model
        |> List.map
            (fun sym ->
                if List.contains sym.Id sIds then
                    match snap with
                    | None -> { sym with IsTransparent = false }
                    | Some diff ->
                        { sym with
                              Pos = posAdd sym.Pos diff
                              IsTransparent = false
                              BoundingBox = updateBoundingBox sym.BoundingBox diff
                              Ports =
                                  sym.Ports
                                  |> List.map
                                      (fun port ->
                                          { port with
                                                Pos = posAdd port.Pos diff
                                                BoundingBox = updateBoundingBox port.BoundingBox diff }) }
                else
                    sym),
        Cmd.none
    | SelectSymbols sIds ->
        model
        |> List.map
            (fun sym ->
                if List.contains sym.Id sIds then
                    { sym with IsHighlighted = true }
                else
                    sym),
        Cmd.none
    | DeselectSymbols sIds ->
        model
        |> List.map
            (fun sym ->
                if List.contains sym.Id sIds then
                    { sym with IsHighlighted = false }
                else
                    sym),
        Cmd.none
    | ShowPorts (sIds, portType) ->
        model
        |> List.map
            (fun sym ->
                if List.contains sym.Id sIds then
                    let updatedPorts =
                        List.map
                            (fun (port: Port) ->
                                match portType with
                                | None -> { port with IsHighlighted = true }
                                | Some portType when portType = port.PortType -> { port with IsHighlighted = true }
                                | Some _ -> port)
                            sym.Ports

                    { sym with
                          IsShowingPorts = true
                          ShowingPortsType = portType
                          Ports = updatedPorts }
                else
                    sym),
        Cmd.none
    | HidePorts sIds ->
        model
        |> List.map
            (fun sym ->
                if List.contains sym.Id sIds then
                    let updatedPorts =
                        List.map (fun (port: Port) -> { port with IsHighlighted = false }) sym.Ports

                    { sym with
                          IsShowingPorts = false
                          ShowingPortsType = None
                          Ports = updatedPorts }
                else
                    sym),
        Cmd.none
    | SetPortWidths portIdAndWidths ->
        let updatedSymbols =
            portIdAndWidths
            |> List.map (fun (portId, _) -> getSymbolFromPortId model portId)

        let updatedModel =
            model
            |> List.map
                (fun sym ->
                    let updatedPorts =
                        sym.Ports
                        |> List.map
                            (fun port ->
                                (port, portIdAndWidths)
                                ||> List.fold
                                        (fun out (portId, width) ->
                                            if portId = port.Id then
                                                { port with Width = width }
                                            else
                                                port))

                    { sym with Ports = updatedPorts })
        updatedModel,
        Cmd.batch (
            ([], updatedSymbols)
            ||> List.fold
                    (fun out symbol ->
                        match symbol with
                        | Some sy -> (Cmd.ofMsg <| UpdateConnections sy.Id) :: out
                        | None -> out)
        )
    | EditSymbolLabel (sId, label) ->
        model
        |> List.map
            (fun sym ->
                if sym.Id = sId then
                    { sym with Label = label }
                else
                    sym),
        Cmd.none
    | NewSymbols symbols -> model @ symbols, Cmd.none
    | _ -> model, Cmd.none

//----------------------------View Function for Symbols----------------------------//

let text_Style dominantBaseline font fill =
    [
      DominantBaseline dominantBaseline // auto/middle/hanging: vertical alignment vs (X,Y)
      FontSize font
      Fill fill
      UserSelect UserSelectOptions.None ]

let text_Style_With_Anchor dominantBaseline font fill textAnchor =
    [
      DominantBaseline dominantBaseline // auto/middle/hanging: vertical alignment vs (X,Y)
      FontSize font
      Fill fill
      UserSelect UserSelectOptions.None
      TextAnchor textAnchor ]
//---------------------Text Styles ---------------//
let textStyle = text_Style "middle" "20px" "Black"

let compNameStyle (isCompact: bool) =
    let baseline =
        if isCompact then
            "hanging"
        else
            "middle"

    text_Style_With_Anchor baseline "20px" "Black" "middle"

let portTextStyle = text_Style "middle" "17px" "Black"

let textStyleAnchor textAnchor =
    text_Style_With_Anchor "middle" "20px" "Black" textAnchor

let labelTextStyle =
    text_Style_With_Anchor "auto" "15px" "Black" "end"

let labelTextStyleCenter =
    text_Style_With_Anchor "auto" "15px" "Black" "middle"


/// Input to react component (which does not re-evaluate when inputs stay the same)
/// This generates View (react virtual DOM SVG elements) for one symbol
type private RenderSymbolProps =
    { Symbol: Symbol
      Dispatch: Dispatch<Msg>
      key: string } // special field used by react to detect whether lists have changed, set to symbol Id

//------------------- Render Functions --------------------//

let the_style x y opacity =
    Style [ Transform(sprintf "translate(%Apx,%Apx)" x y)
            Opacity opacity ]

let a_polygon coordinates opacity borderColor backgroundColor =
    polygon [ SVGAttr.Points coordinates
              SVGAttr.StrokeWidth "1px"
              SVGAttr.Stroke borderColor
              SVGAttr.FillOpacity opacity
              SVGAttr.Fill backgroundColor ] []

let some_text x y style content =
    text [ X x; Y y; Style style ] [
        str <| content
    ]

let a_circle x y r color stroke stroke_width =
    circle [ Cx x
             Cy y
             R r
             SVGAttr.Fill color
             SVGAttr.Stroke stroke
             SVGAttr.StrokeWidth stroke_width ] []

let a_line x1 y1 x2 y2 strokewidth stroke =
    line [ SVGAttr.X1 x1
           SVGAttr.Y1 y1
           SVGAttr.X2 x2
           SVGAttr.Y2 y2
           SVGAttr.StrokeWidth strokewidth
           SVGAttr.Stroke stroke ] []

let a_polyline points strokeWidth stroke opacity fill =
    polyline [ SVGAttr.Points points
               SVGAttr.StrokeWidth strokeWidth
               SVGAttr.Stroke stroke
               SVGAttr.FillOpacity opacity
               SVGAttr.Fill fill ] []

/// renders the port label
let private renderPortLabel grid row label alignment offsetX =
    some_text (gridX grid 0 alignment 1 + offsetX) (gridY grid row GridAlignment.Middle 1) portTextStyle label

/// renders the circle indicating port connection
let private renderPortCircle grid row alignment (isInverted: bool) =
    let color = "grey"

    let offsetX =
        match isInverted, alignment with
        | true, GridAlignment.End -> 14.
        | true, GridAlignment.Start -> -14.
        | false, _ -> 0.
        | _, _ -> 0.

    a_circle (gridX grid 0 alignment 1 + offsetX) (gridY grid row GridAlignment.Middle 1) 7. color "black" 1

/// renders an invert circle to indicate the port is Inverted (For example Nand and Not but works also for inputs)
let private renderInvertCircle grid row alignment =
    let color = "lightgrey"

    let offsetX =
        match alignment with
        | GridAlignment.End -> 7.5
        | GridAlignment.Start -> -7.5
        | _ -> 0.

    a_circle (gridX grid 0 alignment 1 + offsetX) (gridY grid row GridAlignment.Middle 1) 7. color "black" 1

/// renders the triangle indicating the port is a clk
let private renderClkTriangle grid row =
    let color = "lightgrey"

    a_polygon
        (sprintf
            "0,%f 8.66,%f 0,%f"
            (gridY grid row GridAlignment.Middle 1 + 5.)
            (gridY grid row GridAlignment.Middle 1)
            (gridY grid row GridAlignment.Middle 1 - 5.))
        0.
        "black"
        color

/// renders the Component name
let private renderCompName (grid: Grid) (name: string) : ReactElement =
    let isCompact = name.Length < 3
    some_text (gridX grid 1 GridAlignment.Middle 1) (gridY grid 0 GridAlignment.Middle 1) (compNameStyle isCompact) name

/// renders the floating label unique (hopefully, not that is matters for rendering) to symbol
let private renderLabel (grid: Grid) (label: string) : ReactElement =
    some_text (gridX grid 2 GridAlignment.End 1) (-15) labelTextStyle label

/// renders a port given an alignment (left or right of symbol)
let private renderPort (portGrid: Grid) (port: Port) (index: int) : ReactElement =
    let alignment =
        if port.PortType = CommonTypes.PortType.Input then
            GridAlignment.Start
        else
            GridAlignment.End

    let offsetX =
        if port.PortType = CommonTypes.PortType.Input then
            10.
        else
            -10.

    g [] [
        renderPortLabel portGrid index port.Label alignment offsetX
        if port.IsInverted then
            renderInvertCircle portGrid index alignment
        if port.Label = "Clk" then
            renderClkTriangle portGrid index
        if port.IsHighlighted then
            renderPortCircle portGrid index alignment port.IsInverted
    ]

/// render the input ports
let private renderInputs (parentGrid: Grid) (inputPorts: Port list) : ReactElement =
    let inputGrid =
        ({ Columns = [ 30. ]; Rows = [] }, inputPorts)
        ||> List.fold (fun grid _ -> gridAddRow 40. grid)

    let inputs : ReactElement list =
        List.mapi (fun i (port: Port) -> renderPort inputGrid port i) inputPorts

    g
        [ Style [ Transform(
                      sprintf
                          "translate(%Apx,%Apx)"
                          (gridX parentGrid 0 GridAlignment.Start 1)
                          (gridY parentGrid 1 GridAlignment.Middle 1
                           - (float) inputPorts.Length * 40. / 2.)
                  )
                  TextAnchor "start" ] ]
        inputs

/// render the output ports
let private renderOutputs (parentGrid: Grid) (outputPorts: Port list) : ReactElement =
    let outputGrid =
        ({ Columns = [ 30. ]; Rows = [] }, outputPorts)
        ||> List.fold (fun grid _ -> gridAddRow 40. grid)

    let outputs : ReactElement list =
        List.mapi (fun i (port: Port) -> renderPort outputGrid port i) outputPorts

    g
        [ Style [ Transform(
                      sprintf
                          "translate(%Apx,%Apx)"
                          (gridX parentGrid 2 GridAlignment.Start 1)
                          (gridY parentGrid 1 GridAlignment.Middle 1
                           - (float) outputPorts.Length * 40. / 2.)
                  )
                  TextAnchor "end" ] ]
        outputs

/// render the Enable and CLK ports (placed separately)
let private renderEnClk (parentGrid: Grid) (ports: Port list) : ReactElement =
    let enClkGrid =
        ({ Columns = [ 30. ]; Rows = [] }, ports)
        ||> List.fold (fun grid _ -> gridAddRow 40. grid)

    let enClk : ReactElement list =
        List.mapi (fun i (port: Port) -> renderPort enClkGrid port i) ports

    g
        [ Style [ Transform(
                      sprintf
                          "translate(%Apx,%Apx)"
                          (gridX parentGrid 0 GridAlignment.Start 1)
                          (gridY parentGrid 2 GridAlignment.Start 1)
                  )
                  TextAnchor "start" ] ]
        enClk

/// Displays a red rectangle in location of Bounding Box, used for testing and demo purposes
let renderBoundingBox (bBox: BoundingBox) (parentSymbol: Symbol) =
    polygon [ Class "bBox"
              //default hidden, shown in demo using button
              SVGAttr.Opacity 0
              SVGAttr.Points(
                  sprintf
                      "%f,%f %f,%f %f,%f %f,%f"
                      (bBox.P1.X - parentSymbol.Pos.X)
                      (bBox.P1.Y - parentSymbol.Pos.Y)
                      (bBox.P2.X - parentSymbol.Pos.X)
                      (bBox.P1.Y - parentSymbol.Pos.Y)
                      (bBox.P2.X - parentSymbol.Pos.X)
                      (bBox.P2.Y - parentSymbol.Pos.Y)
                      (bBox.P1.X - parentSymbol.Pos.X)
                      (bBox.P2.Y - parentSymbol.Pos.Y)
              )
              SVGAttr.StrokeWidth "1px"
              SVGAttr.Stroke "red"
              SVGAttr.FillOpacity "0.2"
              SVGAttr.Fill "red" ] []

/// This function handles overall rendering for all Symbols (manual first then automatic)
let private renderSymbol =
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
            let backgroundColor =
                    if props.Symbol.IsHighlighted then
                        "lightblue"
                    else
                        "lightgrey"

            let borderColor = //added in case changes made later
                "black" 
            
            let opacity = 
                if props.Symbol.IsTransparent then
                    "0.8"
                else
                    "1"

            match props.Symbol.ComponentType with
            | CommonTypes.ComponentType.Input _ -> 
                    g [
                        the_style props.Symbol.Pos.X props.Symbol.Pos.Y opacity
                    ] [                       
                            a_polygon  "0 21 0 0 52 0 71 21 52 42 0 42 0 21" opacity borderColor backgroundColor                                        
                            some_text 25 -15 labelTextStyleCenter props.Symbol.Label
                            renderBoundingBox (props.Symbol.BoundingBox) props.Symbol
                            renderBoundingBox (props.Symbol.Ports.Item 0).BoundingBox props.Symbol
                            
                            if (props.Symbol.Ports.Item 0).IsHighlighted then     
                                a_circle 71 21 7. "grey" "black" 1
                        ]
            | CommonTypes.ComponentType.Output _ ->
                    g [  
                        the_style props.Symbol.Pos.X props.Symbol.Pos.Y opacity
                    ][
                        a_polygon "72 21 72 0 19 0 0 21 19 43 72 43 72 21" opacity borderColor backgroundColor
                        some_text 45 -15 labelTextStyleCenter props.Symbol.Label
                        renderBoundingBox (props.Symbol.BoundingBox) props.Symbol
                        renderBoundingBox (props.Symbol.Ports.Item 0).BoundingBox props.Symbol
                        if (props.Symbol.Ports.Item 0).IsHighlighted then   
                            a_circle 0 21 7. "grey" "black" 1
                    ]
            | CommonTypes.ComponentType.IOLabel ->
                    g [  
                        the_style props.Symbol.Pos.X props.Symbol.Pos.Y opacity
                    ][            
                        a_polygon "32 0 19 0 0 21 19 43 32 43 32 43 44 43 63 21 44 0 32 0 32 0" opacity borderColor backgroundColor
                        some_text 31 -15 labelTextStyleCenter props.Symbol.Label
                        renderBoundingBox (props.Symbol.BoundingBox) props.Symbol
                        renderBoundingBox (props.Symbol.Ports.Item 0).BoundingBox props.Symbol  
                        renderBoundingBox (props.Symbol.Ports.Item 1).BoundingBox props.Symbol    

                        if (props.Symbol.Ports.Item 0).IsHighlighted then  
                            a_circle 0 21 7. "grey" "black" 1
                        if (props.Symbol.Ports.Item 1).IsHighlighted then    
                            a_circle 63 21 7. "grey" "black" 1
                    ]
            | CommonTypes.ComponentType.Constant (_, constValue)->
                    g [  
                        the_style props.Symbol.Pos.X props.Symbol.Pos.Y opacity
                    ][
                        a_polygon "37 21 0 0 0 43 37 21" opacity borderColor backgroundColor
                        a_line "37" "21" "70" "21" "1px" borderColor
                        some_text 35 -15 labelTextStyleCenter props.Symbol.Label
                        some_text 30 35 (text_Style_With_Anchor "hanging" "17px" "Black" "start") (sprintf "%i" constValue)
                        renderBoundingBox (props.Symbol.BoundingBox) props.Symbol
                        renderBoundingBox (props.Symbol.Ports.Item 0).BoundingBox props.Symbol
                        if (props.Symbol.Ports.Item 0).IsHighlighted then  
                            a_circle 70 21 7. "grey"  "black" 1     
                        ]
            | CommonTypes.ComponentType.MergeWires _ ->
                    g [
                        the_style props.Symbol.Pos.X props.Symbol.Pos.Y opacity
                    ][
                        a_polyline ("0 51 30 51 30 25") "1px" borderColor 0 backgroundColor
                        a_polyline ("0 0 30 0 30 25") "1px" borderColor 0 backgroundColor
                        a_line "63" "25" "30" "25" "1px" borderColor
                        some_text 30 -15 labelTextStyleCenter props.Symbol.Label          
                        if (props.Symbol.Ports.Item 0).IsHighlighted then 
                            a_circle 0 0 7. "grey" "black" 1
                        if (props.Symbol.Ports.Item 1).IsHighlighted then 
                            a_circle 0 51 7. "grey" "black" 1
                        if (props.Symbol.Ports.Item 2).IsHighlighted then      
                            a_circle 63 25 7. "grey" "black" 1
                        renderBoundingBox (props.Symbol.BoundingBox) props.Symbol
                        renderBoundingBox (props.Symbol.Ports.Item 0).BoundingBox props.Symbol  
                        renderBoundingBox (props.Symbol.Ports.Item 1).BoundingBox props.Symbol 
                        renderBoundingBox (props.Symbol.Ports.Item 2).BoundingBox props.Symbol       
                              
                        ]

                                
            | CommonTypes.ComponentType.SplitWire _ ->
            g [
                the_style props.Symbol.Pos.X props.Symbol.Pos.Y opacity
            ][
                a_polyline ("63 51 33 51 33 25") "1px" borderColor 0 backgroundColor
                a_polyline ("63 0 33 0 33 25") "1px" borderColor 0 backgroundColor
                a_line "0" "25" "33" "25" "1px" borderColor
                some_text 30 -15 labelTextStyleCenter props.Symbol.Label
                if (props.Symbol.Ports.Item 0).IsHighlighted then      
                    a_circle 0 25 7. "grey" "black" 1
                if (props.Symbol.Ports.Item 1).IsHighlighted then    
                    a_circle 63 0 7. "grey" "black" 1
                if (props.Symbol.Ports.Item 2).IsHighlighted then   
                    a_circle 63 51 7. "grey" "black" 1
                renderBoundingBox (props.Symbol.BoundingBox) props.Symbol
                renderBoundingBox (props.Symbol.Ports.Item 0).BoundingBox props.Symbol  
                renderBoundingBox (props.Symbol.Ports.Item 1).BoundingBox props.Symbol 
                renderBoundingBox (props.Symbol.Ports.Item 2).BoundingBox props.Symbol      
            ]
            | _ ->
                g            
                    [ 
                        the_style props.Symbol.Pos.X props.Symbol.Pos.Y opacity
                    ]
                    [ 
                        let symbol = props.Symbol
                        let portsSplit = splitPorts symbol.Ports
                        
                        let compName = getComponentName props.Symbol.ComponentType
                        //grid rows and columns and their sizes are created here
                        let grid = initGrid portsSplit.InputPorts portsSplit.OutputPorts portsSplit.EnClkPorts compName
                        
                        a_polygon (sprintf "0,0 %f,0 %f,%f 0,%f" 
                            (getWidthBoundingBox props.Symbol.BoundingBox)
                            (getWidthBoundingBox props.Symbol.BoundingBox)
                            (-getHeightBoundingBox props.Symbol.BoundingBox)
                            (-getHeightBoundingBox props.Symbol.BoundingBox)) 
                            opacity borderColor backgroundColor
                        
                        renderLabel grid symbol.Label
                        renderCompName grid compName
                        renderInputs grid portsSplit.InputPorts
                        renderOutputs grid portsSplit.OutputPorts
                        renderEnClk grid portsSplit.EnClkPorts

                        //This shows the bounding boxes, used in testing and demo. In demo turned off with button
                        renderBoundingBox (props.Symbol.BoundingBox) props.Symbol
                        let bBoxes =
                            [0..(props.Symbol.Ports.Length - 1)]
                            |> List.map (fun i -> renderBoundingBox (props.Symbol.Ports.Item i).BoundingBox props.Symbol)
                        g [] bBoxes                           
                    ]
    , "Symbol"
    , equalsButFunctions
    )


/// View function for symbol layer of SVG
let view (model: Model) (dispatch: Msg -> unit) =
    model
    |> List.map
        (fun symbol ->
            match symbol with
            | { Id = CommonTypes.ComponentId id } as symbol ->
                renderSymbol
                    { Symbol = symbol
                      Dispatch = dispatch
                      key = id })
    |> ofList


//---------------Other interface functions--------------------//

let symbolPos (symModel: Model) (sId: CommonTypes.ComponentId) : XYPos =
    List.find (fun sym -> sym.Id = sId) symModel
    |> (fun sym -> sym.Pos)


/// Return the output Buswire width (in bits) if this can be calculated based on known
/// input wire widths, for the component type.
let calculateOutputPort
    (componentType: CommonTypes.ComponentType)
    (outputPortNumber: int)
    (inputPortWidths: int option list)
    : int option =
    match componentType with
    | CommonTypes.ComponentType.SplitWire width ->
        match outputPortNumber with
        | 0 -> Some width
        | 1 ->
            inputPortWidths.[0]
            |> Option.map (fun inWidth -> inWidth - width)
        | _ -> failwithf "calculateOutputPort in Symbol was implemented incorrectly, not such port exists"
    | CommonTypes.ComponentType.MergeWires ->
        match outputPortNumber with
        | 0 ->
            match inputPortWidths.[0], inputPortWidths.[1] with
            | Some a, Some b -> Some(a + b)
            | _ -> None
        | _ -> failwithf "calculateOutputPort in Symbol was implemented incorrectly, not such port exists"
    | CommonTypes.ComponentType.IOLabel ->
        match outputPortNumber with
        | 0 -> inputPortWidths.[0] |> Option.map id
        | _ -> failwithf "calculateOutputPort in Symbol was implemented incorrectly, not such port exists"
    | CommonTypes.ComponentType.Mux2 ->
        match outputPortNumber with
        | 0 -> inputPortWidths.[0] |> Option.map id
        | _ -> failwithf "calculateOutputPort in Symbol was implemented incorrectly, not such port exists"
    | CommonTypes.ComponentType.Demux2 ->
        match outputPortNumber with
        | 0 -> inputPortWidths.[0] |> Option.map id
        | 1 -> inputPortWidths.[0] |> Option.map id
        | _ -> failwithf "calculateOutputPort in Symbol was implemented incorrectly, not such port exists"
    | _ -> None







//----------------------interface to Issie-----------------------------//
let mapSymbolPortToIssiePort
    (id: CommonTypes.PortId)
    (portNumber: int option)
    (portType: CommonTypes.PortType)
    (hostId: CommonTypes.ComponentId)
    : CommonTypes.Port =
    { Id = string id
      PortNumber = portNumber
      PortType = portType
      HostId = string hostId }

let getIssiePortList
    (symbolPorts: Port list)
    (symbolId: CommonTypes.ComponentId)
    (portType: CommonTypes.PortType)
    : CommonTypes.Port list =
    symbolPorts
    |> List.filter (fun port -> port.PortType = portType)
    |> List.mapi (fun index port -> mapSymbolPortToIssiePort port.Id (Some index) port.PortType symbolId)

let mapSymbolToComponent (symbol: Symbol) : CommonTypes.Component =
    { Id = string symbol.Id
      Type = symbol.ComponentType
      Label = symbol.Label
      InputPorts = getIssiePortList symbol.Ports symbol.Id CommonTypes.PortType.Input
      OutputPorts = getIssiePortList symbol.Ports symbol.Id CommonTypes.PortType.Output
      X = int (symbol.Pos.X)
      Y = int (symbol.Pos.Y)
      H = int (getHeightBoundingBox symbol.BoundingBox)
      W = int (getWidthBoundingBox symbol.BoundingBox) }

let extractComponent (symModel: Model) (sId: CommonTypes.ComponentId) : CommonTypes.Component =
    symModel
    |> List.find (fun symbol -> symbol.Id = sId)
    |> mapSymbolToComponent

let extractComponents (symModel: Model) : CommonTypes.Component list =
    symModel |> List.map mapSymbolToComponent
