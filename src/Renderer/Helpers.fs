module Helpers
open Browser.Types
open Fable.Core
open Fable.Core.JsInterop
open Electron
open Fable.React
open Helpers

//-------------------------------------------------------------------------//
//------------------------------Types--------------------------------------//
//-------------------------------------------------------------------------//

/// position on SVG canvas
type XYPos =
    {
        X : float
        Y : float
    }

type BoundingBox =
    {
        P1 : XYPos //upper left point
        P2 : XYPos //lower right point
    }
type MouseOp = 
    /// button up
    | Up
    /// button down
    | Down
    /// Move with button up
    | Move 
    /// Move with button Down
    | Drag
    
type KeyOp =
    | KeyUp
    | KeyDown

type MouseT = {
    Pos: XYPos
    Op: MouseOp}

type GuidelineOrientation =
    | GuidelineTop
    | GuidelineBottom
    | GuidelineRight
    | GuidelineLeft

//--------------------------------------------------------------------------//
//-----------------------------Helpers--------------------------------------//
//--------------------------------------------------------------------------//

let posDiff a b =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}
    
/// Checks if a point is inside of a bounding box
let containsPoint (bBox:BoundingBox) (pos:XYPos):bool =
    (pos.X > bBox.P1.X) && (pos.X < bBox.P2.X) && (pos.Y > bBox.P1.Y) && (pos.Y < bBox.P2.Y)

/// Check if bBox1 is fully inside of bBox2
let containsBox (bBox1:BoundingBox) (bBox2:BoundingBox):bool =
    (containsPoint bBox2 bBox1.P1) && (containsPoint bBox2 bBox1.P2)
    
/// Finds the distance between the point and the closest edge of the bounding box
let distanceFromPoint (bBox:BoundingBox) (pos:XYPos):float =
    let cx = max (min pos.X bBox.P2.X) bBox.P1.X
    let cy = max (min pos.Y bBox.P2.Y) bBox.P1.Y
    sqrt <| (pos.X - cx) ** 2. + (pos.Y - cy) ** 2.
    
/// Updates the bounding box position given the difference vector
let updateBoundingBox (bBox:BoundingBox) (diff:XYPos):BoundingBox =
    {
        P1 = posAdd bBox.P1 diff
        P2 = posAdd bBox.P2 diff
    }
    
///Aligns bounding box, so that P1 is the top-left point and P2 is the bottom-right point.
let alignBoundingBox (bBox:BoundingBox):BoundingBox =
    let x1, x2 =
        if bBox.P1.X < bBox.P2.X then 
            bBox.P1.X, bBox.P2.X
        else
            bBox.P2.X, bBox.P1.X

    let y1, y2 =
        if bBox.P1.Y < bBox.P2.Y then 
            bBox.P1.Y, bBox.P2.Y
        else
            bBox.P2.Y, bBox.P1.Y
            
    {P1 = {X = x1; Y = y1}; P2 = {X = x2; Y = y2}}

/// Append the given item to the list, if it is not already in the list. Otherwise, return the list.
let appendIfAbsent item list =
    if List.contains item list then
        list
    else
        item::list
        
/// Regex for matching valid positive numbers
let numberRegex = System.Text.RegularExpressions.Regex("^[0-9]+$")

/// return a v4 (random) universally unique identifier (UUID)
let uuid():string = import "v4" "uuid"


//-----------------Code to record and print execution time statistics-------//

let timeNowInMicroS() = 
    System.DateTime.Now.Ticks
    |> (fun t -> t /10L)

type Stats = {
    Min: float
    Max: float
    Av: float
    Num: float
    }

/// add time t to st
let addTimeToStats (t:float) (st:Stats) =
    {
        Min = min st.Min t
        Max = max st.Max t
        Av = (st.Av*st.Num + t)/(st.Num+1.)
        Num = st.Num + 1.
    }

/// execution time stats indexed by name in recordExecutionStats
let mutable executionStats = Map<string,Stats> []

/// Run (f arg) recording its time in executionStats under name.
/// NB - this will run f multiple times if needed to estimate average speed more accurately.
/// If an execution time of 5ms for this function is too long reduce timeLimit.
/// The multiple time execution will not work, and will give lower than real results, if
/// f is memoised. In that case set timeLimit to 0. for only one execution.
let recordExecutionTimeStats (name: string) (f: 'a -> 'b) (arg: 'a) : 'b =
    let timeLimit = 0. // time in ms to execute f for.
    let t1 = timeNowInMicroS()
    let execTime() = float (timeNowInMicroS() - t1) / 1000.
    let res = f arg // do f
    let mutable iterations = 1
    while execTime() < timeLimit do // do f multiple times if it is fast to get more accurate speed statistics
        iterations <- iterations + 1
        f arg |> ignore // do f again
    let t = execTime() / float iterations
    executionStats <-
        Map.tryFind name executionStats
        |> Option.map (addTimeToStats t)
        |> Option.defaultValue {Min=t;Max=t;Av=t;Num=1.}  
        |> (fun st -> Map.add name st executionStats)
    res

/// print
let printStats() =
    executionStats
    |> Map.toList
    |> List.iter (fun (name,st) -> 
        printfn "%s time: min=%.3fms max=%.3fms av=%.3fms samples:%d" name st.Min st.Max st.Av (int st.Num))
    executionStats <- Map [] // reset stats

//--------------------------------Constants----------------------------------//

/// these determine the size of the canvas relative to the objects on it.
let canvasUnscaledDimensions : XYPos = 
    {X = 1000. ; Y = 1000.}




    

