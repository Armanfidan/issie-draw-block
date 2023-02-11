module Renderer

    open CommonTypes
    open Elmish
    open Elmish.React
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.React
    open Fable.React.Props
    open Browser
    
    open Helpers
    open Electron

    open Sheet

    // Editor Keybindings (also items on Edit menu)
    // Use Elmish subscriptions to attach external source of events such as keyboard
    // shortcuts. According to electron documentation, the way to configure keyboard
    // shortcuts is by creating a menu.
    let editMenu dispatch =
        let menuSeparator =
           let sep = createEmpty<MenuItemOptions>
           sep.``type`` <- MenuItemType.Separator
           sep
        let makeRoleItem (role:MenuItemRole) =
            jsOptions<MenuItemOptions> <| fun item ->
                item.role <- role
        let makeKeyItem (label:string) (accelerator : string) (action : unit -> unit) =
            jsOptions<MenuItemOptions> <| fun item ->
                item.label <- label
                item.accelerator <- accelerator
                item.click <- fun _ _ _ -> action()

    
        jsOptions<MenuItemOptions> <| fun invisibleMenu ->
            invisibleMenu.``type`` <- MenuItemType.SubMenu
            invisibleMenu.label <- "Edit"
            invisibleMenu.visible <- true // false if you want keys but no "Edit" menu
            invisibleMenu.submenu <-
                [| makeKeyItem "Default" "CmdOrCtrl+S" (fun () -> dispatch (KeyPress KeyboardMsg.CtrlS))
                   makeKeyItem "Blue" "Alt+C" (fun () -> dispatch (KeyPress KeyboardMsg.AltC))
                   makeKeyItem "Green" "Alt+V" (fun () -> dispatch (KeyPress KeyboardMsg.AltV))
                   menuSeparator
                   makeKeyItem "Undo" "CmdOrCtrl+Z" (fun () -> dispatch (KeyPress KeyboardMsg.CtrlZ))
                   makeKeyItem "Redo" "CmdOrCtrl+Y" (fun () -> dispatch (KeyPress KeyboardMsg.CtrlY))
                   makeKeyItem "Delete"  "delete" (fun () -> dispatch (KeyPress KeyboardMsg.DEL))
                   menuSeparator
                   makeKeyItem "Print Statistics" "Alt+Shift+Z" (fun () -> dispatch (KeyPress KeyboardMsg.AltShiftZ))
                   makeKeyItem "Zoom In" "CmdOrCtrl+=" (fun () -> dispatch (AdjustZoom 0.1))
                   makeKeyItem "Zoom Out" "CmdOrCtrl+-" (fun () -> dispatch (AdjustZoom -0.1))
                   makeRoleItem MenuItemRole.ForceReload
                   makeRoleItem MenuItemRole.Reload
                   makeRoleItem MenuItemRole.ToggleDevTools|]
                |> U2.Case1
                
    let addSymbolMenu dispatch =
        let addSymbolMsg cType label () =
            dispatch <| AddSymbol (cType, label)
    
        let menuSeparator =
           let sep = createEmpty<MenuItemOptions>
           sep.``type`` <- MenuItemType.Separator
           sep

        let makeKeyItem (label:string) (action : unit -> unit) =
            jsOptions<MenuItemOptions> <| fun item ->
                item.label <- label
                item.click <- fun _ _ _ -> action()
                
        let memory =
            {
                AddressWidth = 4
                WordWidth = 4
                Data = Map.empty
            }
    
        jsOptions<MenuItemOptions> <| fun invisibleMenu ->
            invisibleMenu.``type`` <- MenuItemType.SubMenu
            invisibleMenu.label <- "Add Symbols"
            invisibleMenu.visible <- true // false if you want keys but no "Edit" menu
            invisibleMenu.submenu <-
                [| makeKeyItem "Input 7" (addSymbolMsg (ComponentType.Input 7) "Input 7") 
                   makeKeyItem "Output 1" (addSymbolMsg (ComponentType.Output 1) "Output 1")
                   menuSeparator
                   makeKeyItem "IOLabel" (addSymbolMsg ComponentType.IOLabel "IOLabel")
                   makeKeyItem "Constant" (addSymbolMsg (ComponentType.Constant (1, 0)) "Constant") 
                   makeKeyItem "BusSelection" (addSymbolMsg (ComponentType.BusSelection (1, 0)) "BusSelection") 
                   menuSeparator
                   makeKeyItem "And" (addSymbolMsg ComponentType.And "And") 
                   makeKeyItem "Or" (addSymbolMsg ComponentType.Or "Or")
                   makeKeyItem "Xor" (addSymbolMsg ComponentType.Xor "Xor")
                   menuSeparator
                   makeKeyItem "Nand" (addSymbolMsg ComponentType.Nand "Nand") 
                   makeKeyItem "Nor" (addSymbolMsg ComponentType.Nor "Nor")
                   makeKeyItem "Xnor" (addSymbolMsg ComponentType.Xnor "Xnor")
                   menuSeparator
                   makeKeyItem "Decode4" (addSymbolMsg ComponentType.Decode4 "Decode4")
                   menuSeparator
                   makeKeyItem "Mux2" (addSymbolMsg ComponentType.Mux2 "Mux2")
                   makeKeyItem "Demux2" (addSymbolMsg ComponentType.Demux2 "Demux2")
                   menuSeparator
                   makeKeyItem "NbitsAdder 5" (addSymbolMsg (ComponentType.NbitsAdder 5) "NbitsAdder 5")
                   menuSeparator
                   makeKeyItem "MergeWires" (addSymbolMsg ComponentType.MergeWires "MergeWires") 
                   makeKeyItem "SplitWire 2" (addSymbolMsg (ComponentType.SplitWire 2) "SplitWire 2")
                   menuSeparator
                   makeKeyItem "DFF" (addSymbolMsg ComponentType.DFF "DFF")
                   makeKeyItem "DFFE" (addSymbolMsg ComponentType.DFF "DFFE")
                   menuSeparator
                   makeKeyItem "Register 5" (addSymbolMsg (ComponentType.Register 5) "Register 5")
                   makeKeyItem "RegisterE 5" (addSymbolMsg (ComponentType.RegisterE 5) "RegisterE 5") 
                   makeKeyItem "ROM 4x4" (addSymbolMsg (ComponentType.ROM memory) "ROM 4x4")
                   makeKeyItem "RAM 4x4" (addSymbolMsg (ComponentType.RAM memory) "RAM 4x4")
                |]
                |> U2.Case1
    
    let attachMenusAndKeyShortcuts dispatch =
        let sub dispatch =
            let menu = 
                [| editMenu dispatch
                   addSymbolMenu dispatch
                |]          
                |> Array.map U2.Case1
                |> electron.remote.Menu.buildFromTemplate   
            menu.items.[0].visible <- Some true
            electron.remote.app.applicationMenu <- Some menu
    
        Cmd.ofSub sub 

    let update' = fun msg -> recordExecutionTimeStats "Update" (Sheet.update msg)
    let view'  = recordExecutionTimeStats "View" Sheet.view
    let printMsg (msg:Msg) =
        match msg with
        | Wire (BusWire.Msg.MouseMsg busWireMouseMsg) -> sprintf "BusWireMsg:%A" busWireMouseMsg.Op
        | KeyPress key -> sprintf "%A" key
        | Wire (BusWire.Msg.Symbol (Symbol.Msg.MouseMsg symMouseMsg)) -> sprintf "SymbolMsg:%A"  symMouseMsg.Op
        | x -> sprintf "Other:%A" x

    let traceFn (msg:Msg) model =

//        printfn "Msg=%A\n\n" (printMsg msg) //uncomment if you want to print mouse messages
            match msg with
            | MouseMsg _ -> ()
            | x -> printfn "Msg=%A\n\n" (printMsg x)
    // App
    Program.mkProgram Sheet.init update' view'
    |> Program.withReactBatched "app"
    |> Program.withSubscription attachMenusAndKeyShortcuts
    |> Program.withTrace traceFn
    //|> Program.withConsoleTrace
    |> Program.run

