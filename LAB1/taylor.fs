// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    
    let e = Math.E

    let factorial n =
        let rec loop n acc =
            if n <= 1 then acc
            else loop (n - 1) (acc * n)
        loop n 1

    let main_func x = e ** (x ** 2.)
    
    let a = 0.
    let b = 1.
    let n = 10
    
    let eps = 0.0000001
    
    
    let main (x, n) = (x ** (2. * float n)) / float(factorial n)
    
    let rec taylor_naive x =
        let rec loop cnt sum =
            let current_term = main(x, int cnt)
            if abs current_term > eps then
                loop (cnt + 1.) (sum + current_term)
            else
                sum
        loop 0. 0.
    
    
    let taylor_smart x =
        let initialTerm = 1.0
        let rec loop n term sum =
            if abs term < eps then sum
            else 
                let nextTerm = term * x ** 2.0 / n
                loop (n + 1.) nextTerm (sum + nextTerm)
        loop 1. initialTerm (initialTerm)
    
    // Пример использования:
    let final =
        for i = 0 to int n do
            let x = a + float i / float n * (b - a)
            printfn "%5.2f  %10.6f  %10.6f %10.6f " x (main_func x) (taylor_naive x) (taylor_smart x)

   
    0 // return an integer exit code
