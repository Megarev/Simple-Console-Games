open System

type Game (x: int, y: int, w: int, h: int) = 
    let rng = Random()
    let mutable px, py, pointX, pointY, score = x, y, 0, 0, 0
    let mutable grid, color = [| for i in 0..w*h -> if i % w = 0 then '\n' else '.' |], ConsoleColor.White
    
    member this.RandomizePoint() =
        pointX <- rng.Next(0, w-2)
        pointY <- rng.Next(0, h-1)

    member this.RandomizeGrid() =
        grid <- grid |> Array.mapi (fun i _ -> if i % w = 0 then '\n' else '.') // Reset grid
                     |> Array.mapi (fun i a -> 
                        match i with
                        | _ when a <> '\n' && rng.Next(0, 10) < 2 -> '#'
                        | _ -> a
                     ) // Modify grid
        this.RandomizePoint()
        while grid.[pointY*w+pointX] = '#' do this.RandomizePoint()

    member this.ChangeColor() =
        [| ConsoleColor.Red; ConsoleColor.Green; ConsoleColor.Blue |]
        |> (fun colors -> 
            let initColor = Console.ForegroundColor
            while Console.ForegroundColor = initColor do Console.ForegroundColor <- colors.[rng.Next(0, colors.Length)]
            color <- Console.ForegroundColor
        )

    member this.Run() =
        this.RandomizeGrid()
        let rec game(isRun: bool) = 
            Console.SetCursorPosition(0, 0) // Refresh console buffer
            let key = Console.ReadKey().KeyChar
            printf $" (Score: {score})"

            this.Input(key)
            this.Logic()
            this.Draw()

            match key with
            | 'z' -> this.RandomizeGrid()
            | 'e' -> this.ChangeColor()
            | ' ' -> game(false)
            | _ -> printf ""

            if isRun then game(isRun)
        game(true)

    member this.Input (key: char) =
        match key with
        | 'd' when px < (w-1) && grid.[(py+0)*w+(px+1)] <> '#' -> px <- px + 1
        | 's' when py < (h-1) && grid.[(py+1)*w+(px+0)] <> '#' -> py <- py + 1
        | 'a' when px > 1     && grid.[(py+0)*w+(px-1)] <> '#' -> px <- px - 1
        | 'w' when py > 0     && grid.[(py-1)*w+(px+0)] <> '#' -> py <- py - 1
        | _ -> printf ""

    member this.Logic() =
        if px = pointX && py = pointY then
            score <- score + 1
            this.RandomizePoint()
            while grid.[pointY*w+pointX] = '#' do this.RandomizePoint()

    member this.Draw() =
        let renderGrid = grid

        let colorizeTile col = Console.ForegroundColor <- col

        renderGrid
        |> Array.mapi (fun i a -> 
            match i with
            | _ when i = (py*w+px) -> '@'
            | _ when i = (pointY*w+pointX) -> 'O'
            | _ -> a
        )
        |> Array.iter (fun a -> 
            match a with
            | '@' -> colorizeTile ConsoleColor.Yellow
            | '#' -> colorizeTile ConsoleColor.DarkYellow
            | 'O' -> colorizeTile ConsoleColor.Cyan
            | _   -> colorizeTile color
            printf "%c" a
        )

[<EntryPoint>]
let main argv =
    let game = Game(1, 1, 25, 25)
    game.Run()
    0