open System
open System.Threading

type Paddle(x: int, y: int, size: int) =
    let mutable pos, paddleSize = (x, y), size + Convert.ToInt32 (size % 2 = 0)
    let mutable state = true

    member this.Input(key: char, bufferSize: int*int, buffer: inref<char[]>) =
        let px, py = fst pos, snd pos

        match key with
        | 'a' when px - ((paddleSize-1)/2) > 1 ->                pos <- (px-1, py)
        | 'd' when px + ((paddleSize-1)/2) < fst bufferSize-1 -> pos <- (px+1, py)
        | _ -> printf ""

    member this.Render(bufferSize: int*int, buffer: outref<char[]>) =
        let px, py = fst pos, snd pos
        for i in -(paddleSize-1)/2..(paddleSize-1)/2 do
            buffer.[py*fst bufferSize+(px+i)] <- '#'

    member this.Pos(coord: char) =
        match coord with
        | 'x' -> fst pos
        | 'y' -> snd pos
        | _ -> 0

    member this.GetState(nState: int) = 
        if nState % 2 = 0 then state <- not state
        state

    member this.Size() = (paddleSize-1)/2

type Ball(x: int, y: int) =
    let mutable pos, velocity = (x, y), (0, 0)
    let mutable state = true
    do 
        let rng = Random()
        let dirX, dirY = rng.Next(-1, 2), rng.Next(-1, 2)
        velocity <- (
            (if dirX = 0 then 1 else dirX),
            (if dirY = 0 then 1 else dirY)
        )

    member this.Logic(bufferSize: int*int, buffer: byref<char[]>, paddle: inref<Paddle>) =
        let px, py = (fst pos + fst velocity, snd pos + snd velocity)
        if px < 1 || px > fst bufferSize-1 then velocity <- (-fst velocity,  snd velocity)
        if py < 1 || py > snd bufferSize-1 then velocity <- ( fst velocity, -snd velocity)

        let paddleSize, paddlePos = paddle.Size(), (paddle.Pos('x'), paddle.Pos('y'))
        let isBounce = [| for i in -paddleSize..paddleSize -> (fst paddlePos + i, snd paddlePos) |]
                       |> Array.contains ((px, py))
        if isBounce then velocity <- (fst velocity, -snd velocity)

        pos <- (fst pos + fst velocity, snd pos)
        if Array.contains (Char.ToLower(buffer.[snd pos*fst bufferSize+fst pos])) [| 'a'; 'b'; |] then
            buffer.[snd pos*fst bufferSize+fst pos] <- '.'
            velocity <- (-fst velocity, snd velocity)

        pos <- (fst pos, snd pos + snd velocity)
        if Array.contains (Char.ToLower(buffer.[snd pos*fst bufferSize+fst pos])) [| 'a'; 'b' |] then
            buffer.[snd pos*fst bufferSize+fst pos] <- '.'
            velocity <- (fst velocity, -snd velocity)

    member this.GetState(nState: int) = 
        if nState % 2 = 0 then state <- not state 
        state

    member this.Render(bufferSize: int*int, buffer: outref<char[]>) =
        let px, py = fst pos, snd pos
        buffer.[py*fst bufferSize+px] <- 'O'

type GameMain(w: int, h: int) =
    let playerPaddle, ball = Paddle(w/2, h-3, 5), Ball(w/2, h/2)
    let mutable buffer = Array.create (w*h) '.'
    let mutable nState, isPaused = 0, false

    let mutable key, isThreadRunning = '.', true
    let keyPress() = 
        while isThreadRunning do 
            key <- Console.ReadKey(true).KeyChar

    let loadMap(filename: string) =
        let lines = IO.File.ReadAllLines(filename) 
                    |> String.concat ""
        buffer <- lines.ToCharArray()

    let render() =
        let mutable drawBuffer = Array.copy buffer
        playerPaddle.Render((w, h), &drawBuffer)
        ball.Render((w, h), &drawBuffer)
        
        let SetColor(color: ConsoleColor) = Console.ForegroundColor <- color
        
        drawBuffer
        |> Array.iteri (fun i a ->
            // Colorize tiles
            match a with
            | 'A' -> SetColor(ConsoleColor.Red)
            | 'a' -> SetColor(ConsoleColor.DarkRed)
            | 'B' -> SetColor(ConsoleColor.Blue)
            | 'b' -> SetColor(ConsoleColor.DarkBlue) 
            | '#' -> if playerPaddle.GetState(nState) then SetColor(ConsoleColor.Cyan) else SetColor(ConsoleColor.DarkCyan)
            | 'O' -> if ball.GetState(nState) then SetColor(ConsoleColor.Yellow) else SetColor(ConsoleColor.DarkYellow)
            | _   -> SetColor(ConsoleColor.White)
            
            // Drawing tiles
            match a with
            | 'A' | 'B' | 'a' | 'b' -> printf "#"
            | _ ->                     printf "%c" (if i%w = 0 then '\n' else a)
        )

    let logic() =
        // Animations
        nState <- nState + 1
        if nState % 2 = 0 then
            nState <- 0
            buffer <- buffer
                      |> Array.map (fun a -> 
                        match a with
                        | 'a' -> 'A'
                        | 'b' -> 'B'
                        | 'A' -> 'a'
                        | 'B' -> 'b'
                        | _ -> a
                      )

    member this.Run() =
        let inputThread = Thread(keyPress)
        inputThread.Start()

        loadMap("level.txt")

        let rec iterate() =

            Console.SetCursorPosition(0, 0)
            Thread.Sleep(TimeSpan.FromMilliseconds(100.0))            
            
            logic()

            if key = ' ' then 
                isPaused <- not isPaused

            if not isPaused then
                playerPaddle.Input(key, (w, h), &buffer)
                ball.Logic((w, h), &buffer, &playerPaddle)
            
            render()

            key <- '.' // Reset key

            if key <> 'q' then iterate()
            else inputThread.Join()
        iterate()

[<EntryPoint>]
let main argv =
    let game = GameMain(20, 15)
    game.Run()
    0
