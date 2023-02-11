# HLP 21 Group Project Team 8

# Features To Be Demonstrated

## Port interactions
* **Ports are shown when mouse gets close to a symbol(s)**
* **Ports are hidden when mouse moves away from the symbol(s)**

## Adding a symbol
* **After pressing 'Add Symbol', cursor changes, symbol is placed in the clicked canvas position.**
* **Adding another symbol with same label generates a unique copied label**

## Individual symbol interactions
* **Can move an individual symbol and guidelines appear to symbols in same horizontal or vertical axis**
* **Can snap a symbol to the shown guideline**. If mouse is released when guideline is show, the symbol will snap to that position.
* **Can select and deselect individual symbol**. Select by clicking on the symbol. Deselect by clicking on empty canvas.
* **Can delete a selected symbol**. Symbol is deleted after pressing Del button.
* **Can copy and paste a selected symbol**. The copied symbol has a new generated symbol with an underscore and a number.

## Multiple Symbol Interactions
* **Can select multiple symbols with Ctrl**. Multiple symbols are selected by mouse clicks, when Ctrl button is pressed.
* **Can move multiple selected symbols**
* **Can deselect multiple selected symbols by clicking on empty canvas**
* **Can select multiple symbols by dragging on the canvas**. A dashed-line rectangle appears, symbols that are fully inside the rectangle are selected.
* **Can delete multiple symbols that are selected**
* **Can copy and paste multiple symbols, generating unique labels for each**

## Creating a Connection
* **Draws line during drag-and-drop connection**
* **Ports that can be connected to are shown while drawing line**
* **Creates a connection.**

## Width inference
* **When a connection is made between an element of known width and an element of variable width, the width is set**. The widths are propagated through all of the connected elements.
* **When a connection is removed between an element of known width and an element of variable width, the width is set**. The widths are propagated through all of the connected elements.

## Individual Wire interactions
* **Can manually drag wire segments**. Wire is highlighted and can be dragged. Routing around symbols is sensitive to port position on symbols. Can have a variable number of segments. Segment dragging in correct orientation.
* **Can select and deselect individual individual wire**. Wire is selected and highlighted on mouse click.
* **Can delete a selected wire**

## Multiple Wire Interactions
* **Can select multiple wires with Ctrl**. Multiple wires are selected by mouse clicks, when Ctrl button is pressed.
* **Can deselect multiple selected wires by clicking on empty canvas**
* **Can delete multiple wires that are selected**

## Multiple Symbol and Wire interactions
* **Can select all elements in the canvas using Ctrl+A**
* **Can select multiple elements (symbols and wires), with Ctrl**
* **Can select multiple elements (symbols and wires), with multi-select rectangle**
* **Can move multiple selected elements (symbols and wires)**
* **Can copy and paste multiple selected elements**. The symbols that are selected and their interconnecting wires will be copied. Symbols will have unique generated labels.
* **Can delete multiple selected elements (symbols and wires)**

## Zooming
* **Horizontal and vertical scroll bars always visible, and changing depending on the zoom**. Ctrl+= zooms in, Ctrl+- zooms out.
* **All actions working fine under different zoom**

## Timeline interactions
* **Pressing Ctrl+Z undoes the last action.**
* **Pressing Ctrl+Y redoes the last undone action.**
* **Actions done during Undo will overwrite "future" actions**. Ctrl+Y will no longer redo anything, when an action was done during Undo.
