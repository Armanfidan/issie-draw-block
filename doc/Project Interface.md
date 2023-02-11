- [Team 8 HLP Group Project Interface](#team-8-hlp-group-project-interface)
  * [Basic Structure](#basic-structure)
  * [Interfacing with Issie](#interfacing-with-issie)
  * [Symbol](#symbol)
    + [Types](#types)
      - [Model](#model)
      - [Symbol](#symbol-1)
      - [Port](#port)
      - [Inference Rule](#inference-rule)
    + [Messages](#messages)
  * [BusWire](#buswire)
    + [Types](#types-1)
      - [Model](#model-1)
      - [Wire](#wire)
      - [WireBoundingBox](#wireboundingbox)
    + [Messages](#messages-1)
  * [Sheet](#sheet)
    + [Types](#types-2)
      - [Model](#model-2)
      - [DragType](#dragtype)
      - [Selection](#selection)
      - [Clipboard](#clipboard)
    + [CursorType](#cursortype)
    + [Messages](#messages-2)
    + [Clarification of Sheet Messages](#clarification-of-sheet-messages)
      - [Keypress](#keypress)
      - [MouseMsg](#mousemsg)
  * [Extra Types Used](#extra-types-used)
    + [BoundingBox](#boundingbox)
    + [PortId](#portid)
    + [KeyOp](#keyop)
  * [Common Functions Defined in Helpers.fs](#common-functions-defined-in-helpersfs)
    + [BoundingBox calculations](#boundingbox-calculations)
    + [XYPos calculations](#xypos-calculations)

# Team 8 HLP Group Project Interface

## Basic Structure

```
Sheet -> BusWire -> Symbol
```

## Interfacing with Issie

To interface with Issie the Draw2dWrapper module should be replaced by the Sheet module, add the Helper.fs file and replace the Issie CommonTypes file by the one from this project.

Sheet controls mouse interactions internally by using event listeners on the canvas, thus no external interface or connections needs to be added. Sheet also listens for Ctrl+C and Ctrl+Y internally, so no interface is required for that.

Sheet also internally controls element selecting, dragging, copy and paste, undo and redo. These functionalities should be removed from Issie, since there is no need to connect them externally.

Width inference is also done internally, and is triggered whenever a wire is connected or deleted, thus no external interfaces are needed to trigger these actions, and can thus be removed from Issie.

The external interface that needs to be done is message communication, i.e. sending messages from Issie's update function to Sheet. These messages are `AddSymbol`, `KeyPress CtrlZ`, `KeyPress CtrlY`, `KeyPress DEL`, `AdjustZoom` (Check Sheet [messages](#messages-2) and [message clarifications](#clarification-of-sheet-messages) for more info on when to send them.)

Any state that needs to be accessed can be accessed directly from the Sheet module. Access functions for getting `CanvasState`, `Connection` (single or list) and `Component` (single or list) have been implemented to link Issie's types with the draw library implementations.

## Symbol

### Types

#### Model

```
Symbol list
```

#### Symbol

```
Id: CommonTypes.ComponentId
BoundingBox: BoundingBox
Ports: Port list
Pos: XYPos
LastDragPos: XYPos
IsShowingPorts: bool
ShowingPortsType: CommonType.PortType option
IsHighlighted: bool
IsTransparent: bool
NoOfInputPorts: int
NoOfOutputPorts: int
ComponentType: CommonTypes.ComponentType
Label: string
```

#### Port

```
Id: CommonTypes.PortId
PortType: CommonTypes.PortType
Pos: XYPos
BoundingBox: BoundingBox
Width: int
InferenceRule: InferenceRule
IsHighlighted: bool
IsPositionModified: bool
IsInverted: bool
Label: string
```

#### Inference Rule
Used to define what width wire the port expects.
```
| LessThan of int 
| GreaterThan of int 
| EqualTo of int
```

### Messages

|Message |Parameters |From Module|Action| When
|-------------------|-----------|------|-----------|------|
|ShowPorts          |sIds: ComponentId list </br> portType: PortType option | Sheet | Show ports for the specified symbols. If portType is None show all ports of default size, if portType is Some, show ports of that type enlarged.|When the pointer gets within 20px of symbols that are not showing their ports.
|HidePorts          |sIds: ComponentId list|Sheet|Hide the ports of the specified symbols.|When the pointer gets further than 20px of symbols that are showing their ports.
|StartDragging      |sIds: ComponentId list, </br> pagePos: XYPos|Sheet|Set last dragging positions and highlight the symbols. Turn symbols transparent.|When the mouse goes down on a symbol.
|Dragging           |sIds: ComponentId list, </br> pagePos: XYPos|Sheet|Update the symbol positions.|When the symbols are being dragged.
|EndDragging        |sIds: ComponentId list, </br> snapPosition: XYPos option|Sheet|Stop dragging. If a snap position is given, snap to it. Keep the symbol highlighted.|When the mouse is released after dragging
|SelectSymbols      |sIds: ComponentId list| Sheet|Highlight the specified symbols.| When the symbol is selected (with a single click, click with ctrl pressed or with a multi-select rectangle)
|DeselectSymbols    |sIds: ComponentId list|Sheet|Unhighlight the specified symbols.|When a port is clicked, when a wire or other symbol is clicked without Strl pressed, or when an empty spot on the canvas is clicked.
|AddSymbol          |cType: ComponentType, </br> pagePos: XYPos, </br> label: string |Sheet|Add a new symbol to the symbol list based on the component type and with the label provided.| (For demo purposes) When button in menu item to 'Add symbol' is clicked.
|DeleteSymbols      |sIds: ComponentId list|BusWire|Delete the specified symbols.| When DEL is pressed while the symbols are selected.
|SetPortWidths      |portIdAndWidths: (PortId * int option) list|BusWire| Update the given ports to the given widths and send a UpdateConnections message to BusWire. | When a wire is added or deleted. 
|EditSymbolLabel      |symbolId: ComponentId, </br> label: string|Sheet|Edit the label of given the symbol.| (Future implementation) when label of a symbol is changed through UI.
|NewSymbols      |symbols: Symbol list|Sheet|Append the given symbols to the current Symbol list.| When Ctrl+V is pressed and copied symbols are generated in Sheet.

## BusWire

### Types

#### Model

```
WX: Wire list
Symbols: Symbol.Model
Color: HighlightColor
```

#### Wire

```
Id: ConnectionId
SourcePort: PortId
TargetPort: PortId
IsError: bool
Width: int
IsDragging: bool
BoundingBoxes: WireBoundingBox list
Corners: XYPos list
DraggedCornerIndex: int
LastDragPos: XYPos
IsHighlighted: bool
```

#### WireBoundingBox

```
Box: BoundingBox
Prev: XYPos
```

### Messages

|Message            |Parameters       |From Module|Action|When|
|-------------------|-----------------|------|-----------|----|
|Symbol | symbolMsg: Symbol.Msg| Sheet| Send the given message to Symbol.|When a symbol message is sent from Sheet.
|UpdateConnections | symbolId: ComponentId | Symbol | Update all of the connections connected to that symbol with correct widths and send a SetPortWidths if any port widths need to be updated on any connected symbols. | When an input port width is set on a symbol. |
| SetColor | color: HighlightColor| Sheet| Set the wires to the specified color.| When the color is changed in the UI.
|CreateConnection | srcPortId: PortId, </br> targetPortId: PortId | Sheet | Create a wire between the given source and target ports. Assume that the source is an output and target an input.|When a valid connection (from input to output port, or the other way) is drawn between two ports.
|StartDragging | wireId: ConnectionId </br> pagePos: XYPos | Sheet | Set last dragging position and highlight the wire.|When the mouse goes down on a wire.
|Dragging | wireId: ConnectionId, </br> pagePos: XYPos | Sheet | Update corners of the wire to drag only the given segment of the wire.| When the wire segment is being dragged|
|EndDragging | wireId: ConnectionId | Sheet | Stop dragging the wire. Keep the wire highlighted.| When the mouse is released after dragging
|SelectWires |wireIds: ConnectionId list |Sheet| Highlight the specified wires.| When the symbol is selected (with a single click, click with ctrl pressed or with a multi-select rectangle)
|DeselectWires |wireIds: ConnectionId list |Sheet| Unhighlight the specified wires.| When a port is clicked, when a symbol or other wire is clicked without Ctrl pressed, or when an empty spot on the canvas is clicked.
|DeleteWires |wireIds: ConnectionId list |Sheet| Delete the specified wires.| When DEL is pressed while the wires are selected.
|DeleteSymbols |sIds: ComponentId list|Sheet|Delete the connections with the symbol and send a message to Symbol to delete the specified symbol.| When DEL is pressed while the symbols are selected.
|NewWires      |wires: Wire list|Sheet|Append the given wires to the current Wire list.| When Ctrl+V is pressed and the copied wires are generated in Sheet.

## Sheet

### Types

#### Model

```
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
```

#### DragType

Discriminated Union

```
Symbol of ComponentId list 
Connection of Port * XYPos * XYPos
Wire of ConnectionId
Canvas of XYPos * XYPos
```

#### Selection

```
Symbols: ComponentId list
Wires: ConnectionId list
```

#### Clipboard

```
Symbols: Symbol list
Wires: Wire list
Ports: Port list
```

#### CursorType

```
| DefaultCursor | MoveCursor
```

#### GuidelineOrientation

```
| GuidelineTop | GuidelineBottom | GuidelineRight | GuidelineLeft
```

### Messages

|Message            |Parameters |From Module|Description|
|-------------------|-----------|------|-----------|
| KeyPress | KeyboardMsg | Sheet | Keyboard key is pressed|
| MouseMsg | MouseT | Sheet | Mouse action has occured.|
| AddSymbol | cType: ComponentType, </br> label: string| Renderer | Go into click-to-drop mode for the given component.
| AdjustZoom | amount: float | Renderer | Adjust zoom by the given amount.

### Clarification of Sheet Messages


#### Keypress

|KeyBoardMsg |Action |
|----------- |-----------|
| AltC  | Set color to Blue |
| AltV  | Set color to Green |
| DEL  | Delete selected symbols and/or wires |
| CtrlA | Select all symbols and wires |
| CtrlC | Put selected symbols and their interconnecting wires in the clipboard |
| CtrlV | Paste symbols and their wires from the clipboard |
| CtrlZ | Put the current BusWire Model to FutureWireModels and set the BusWire model to the head of PastWireModels.
| CtrlY | Put the current BusWire Model to PastWireModels and set the BusWire model to the head of FutureWireModels.
| Ctrl KeyUp| Set IsCtrlPressed to false. Subsequent actions in sheet will be on single elements.|
| Ctrl KeyDown| Set IsCtrlPressed to true. Subsequent actions in sheet will be on multiple elements.|

#### MouseMsg

|MouseMsg |Action |
|----------- |-----------|
| Move | Check if there is a symbol within 20px of the mouse position, which is not showing its ports. Send a message to show the ports. </br> Check if there are symbols further than 20px of the mouse position, which are showing their ports. Send a message to hide the ports.|
| Down  | If Port is clicked, start drawing a connection. </br> If Symbol is clicked, select it and start dragging the symbol. If ctrl is not pressed, deselect other wires and symbols. </br> If Wire is clicked, select the wire. If ctrl is not pressed, deselect other wires and symbols. </br> If Canvas is clicked, check if click-to-drop is set, if yes, add the symbol to the mouse position, if no start drawing a rectangle for multi-select. Deselect symbols and wires.
| Drag  | If a connection is being dragged, update the mouse position for redraw. </br> If a symbol is being dragged, send Dragging message to Symbol. </br> If a wire is being dragged, send Dragging message to BusWire. </br> If a multi-select rectangle is being dragged, update the last dragging position for redraw. </br> In all cases, show ports for symbols close to pointer.|
| Up  | If a connection is released, check if it is released on a valid port, if yes, then make a wire connection. </br> If a dragged symbol is released, then send a message to Symbol to end dragging. </br>  If a dragged wire is released, then send a message to BusWire to end dragging. </br> If a multi-select rectangle is released, find symbols and wires inside of the rectangle and select them.|




## Extra Types Used

### BoundingBox

Defined in Helpers.fs.

Defined as two points. P1 is the top-left point, P2 is the bottom-right point.

```
P1: XYPos
P2: XYPos
```

### PortId

Defined in CommonTypes.fs

Used as the Id of a Port.
```
PortId of string
```

### KeyOp
Discriminated Union defined in  Helpers.fs
```
| KeyUp | KeyDown
```

## Common Functions Defined in Helpers.fs

### BoundingBox calculations

```
/// Update the bounding box position given the difference vector
updateBoundingBox (bBox:BoundingBox) (diff:XYPos):BoundingBox

/// Checks if a point is inside of a bounding box
containsPoint (bBox:BoundingBox) (pos:XYPos):bool

/// Check if bBox1 is fully inside of bBox2
containsBox (bBox1:BoundingBox) (bBox2:BoundingBox):bool

/// Finds the distance between the point and the closest edge of the bounding box
distanceFromPoint (bBox:BoundingBox) (pos:XYPos):float
```

### XYPos calculations

```
/// Returns the difference between points a and b, e.g. a - b
let posDiff (a:XYPos) (b:XYPos):XYPos =

/// Returns the addition between points a and b, e.g. a + b
let posAdd (a:XYPos) (b:XYPos):XYPos =

/// Returns the point defined by the two coordinate values, x and y
let posOf (x:float) (y:float):XYPos =
```

