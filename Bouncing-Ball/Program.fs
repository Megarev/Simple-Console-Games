open System
open System.Threading

type App(w: int, h: int) =
    let radius = 4.0
    let mutable pos, dir = (w/2-(int radius+1), 0), 1
    let buffer = [| for i in 0..w*h-1 -> if i > w*(h-2) then '\u2593' else '.' |]

    let Index(x, y) =
        let index = y*w+x
        (index, index < w*h)

    let DrawCircle(pos: int*int, radius: float, isFilled: bool,  bufferCopy: outref<char[]>) =
        let thickness = 0.5
        for i in -radius..radius do
            for j in -radius..radius do
                let distanceSq = j ** 2.0 + i ** 2.0

                if (distanceSq <= (radius + thickness) ** 2.0) then
                    if (isFilled || distanceSq >= (radius - thickness) ** 2.0) then
                        let index, isIndexInBounds = Index((fst pos + int j + int radius), (snd pos + int i + int radius))
                        if isIndexInBounds then bufferCopy.[index] <- '\u2588'

    let Input(key: char) =
        match key with
        | 'a' -> pos <- (fst pos - 1, snd pos)
        | 'd' -> pos <- (fst pos + 1, snd pos)
        | _ -> printf ""

    let Update() =
        pos <- (fst pos, snd pos + dir)

        let posInBuffer, isPosInBounds = Index(fst pos, (snd pos + 2 * int radius + 1))

        match dir with
        |  1 when (isPosInBounds && buffer.[posInBuffer] = '\u2593') -> (dir <- -1)
        | -1 when (snd pos - int radius - 1 < 0)                     -> (dir <-  1)
        | _ -> printf ""

    let Draw() =
        let mutable bufferCopy = Array.copy buffer
        DrawCircle(pos, radius, true, &bufferCopy)

        let SetColor(color: ConsoleColor) = Console.ForegroundColor <- color

        bufferCopy
        |> Array.iteri(fun i a -> 

            match a with
            | '\u2593' -> SetColor(ConsoleColor.Cyan)
            | '\u2588' -> if i % 2 = 0 then SetColor(ConsoleColor.Yellow) else SetColor(ConsoleColor.White) 
            | _ -> SetColor(ConsoleColor.White)

            if i % w = 0 then printfn ""
            else printf $"{a}"
        )

    member this.Run() =
        let mutable isRunning = true

        let mutable key = ' '
        let InputMain() = while isRunning do key <- Console.ReadKey(true).KeyChar

        let inputThread = Thread(InputMain)
        inputThread.Start()

        while isRunning do
            Console.SetCursorPosition(0, 0)

            Input(key)
            Update()
            Draw()

            if key = 'z' then isRunning <- false
            Thread.Sleep(TimeSpan.FromMilliseconds(200.0))
        inputThread.Join()

[<EntryPoint>]
let main argv = 
    let app = App(30, 20)
    app.Run()
    0
