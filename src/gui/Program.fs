open Stmt.Parser
open CoreParser
open Interpreter
open System.Drawing
open System.Windows.Forms

let programInput =
  let textBox = new TextBox()
  textBox.Location   <- System.Drawing.Point(0, 0)
  textBox.Multiline  <- true
  textBox.ScrollBars <- ScrollBars.Vertical
  textBox.Height <- 300
  textBox.Width  <- 300
  textBox

let programLabel =
  let lbl = new Label()
  lbl.Location <- System.Drawing.Point(programInput.Width, 0)
  lbl.AutoSize <- true
  lbl

let mutable env     : string -> Option<int> = fun (s : string) -> None
let mutable program : Option<Stmt.t> = None 
let mutable state : Stmt.t list = []

let prevStepAction (but : Button) args =
  match state with 
  | [] -> but.Enabled <- false
  | a :: list -> 
    program <- Some a
    state <- list
    if list = [] then but.Enabled <- false
  programLabel.Text <- sprintf "%A" program

let prevStepButton =
  let but = new Button()
  but.Text     <- "Previous Step"
  but.Location <- System.Drawing.Point(programInput.Width - 2 * but.Width - 10, programInput.Height)
  but.Enabled  <- false
  but.Click.Add (prevStepAction but)
  but

let nextStepAction (but : Button) args =
  match program with 
  | None   -> but.Enabled <- false
  | Some p ->
    let (nenv, np) = ss env p
    env     <- nenv
    state   <- p :: state
    program <- np
    if program = None then but.Enabled <- false
    programLabel.Text <- sprintf "%A" program
    prevStepButton.Enabled <- true

let nextStepButton =
  let but = new Button()
  but.Text     <- "Next Step"
  but.Location <- System.Drawing.Point(programInput.Width - but.Width, programInput.Height)
  but.Enabled  <- false
  but.Click.Add (nextStepAction but)
  prevStepButton.Click.Add (fun f -> but.Enabled <- true)
  but

let interpretAction args =
  let parseResult = &programInput.Text |> parse ()
  try
    program <- parseResult |> List.head |> fst |> Some
    env <- (fun s -> None)
    nextStepButton.Enabled <- true
    programLabel.Text <- sprintf "%A" program
  with
  | _ -> failwith "test"

let interpretButton =
  let but = new Button()
  but.Text <- "Interpret"
  but.Location <- System.Drawing.Point(0, programInput.Height)
  but.MouseClick.Add interpretAction 
  but

let mainForm =
  let form = new Form(Visible = false, TopMost = true)
  form.Controls.Add(interpretButton)
  form.Controls.Add(nextStepButton)
  form.Controls.Add(prevStepButton)
  form.Controls.Add(programInput)
  form.Controls.Add(programLabel)
  form

[<EntryPoint>]
let main argv = 
  mainForm.Visible <- true
  Application.Run()
  0
