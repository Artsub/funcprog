open System

[<EntryPoint>]
let main argv =
    
    let f1 x = cos(2./x) - 2.* sin(1./x) + 1./x
    let f2 x = Math.Sqrt(1.-0.4*x**2.) - asin(x)
    let f3 x = Math.Exp(x) - Math.Exp(-x) - 2.0
    
    let f1' x = Math.Sin(2.0 / x) * (2.0 / (x ** 2.0)) - 2.0 * Math.Cos(1.0 / x) * (1.0 / (x ** 2.0)) - (1.0 / (x ** 2.0))
    let f2' x = (-0.4 * x) / Math.Sqrt(1.0 - 0.4 * x ** 2.0) - 1.0 / Math.Sqrt(1.0 - x ** 2.0)
    let f3' x = Math.Exp(x) + Math.Exp(-x)
    
    let phi1 x = x - f1 x / f1' x
    let phi2 x = x - f2 x / f2' x
    let phi3 x = x - f3 x / f3' x
    
    let eps =0.0001
    
    let rec dichotomy f a b =
        let c = (a+b) / 2.0 
        if abs(f c) < eps then c
        else
            if abs((f a)*(f c)) = ((f a)*(f c)) then
                dichotomy f c b
            else
                dichotomy f a c
    
    let rec iterations phi a =
        if abs((phi a) - a) < eps then a
            else
                iterations phi (phi a)
        
    
    let newthon f f' b =
        let newt b = b - (f b)/(f' b)
        iterations newt b
    
    let final = 
        printfn " |%10.5f | %10.5f | %10.5f |" (dichotomy f1 1. 2.) (iterations phi1 1.8) (newthon f1 f1' 1.8)
        printfn " |%10.5f  | %10.5f | %10.5f|" (dichotomy f2 0. 1.) (iterations phi2 0.5) (newthon f2 f2' 0.5)
        printfn " |%10.5f  | %10.5f | %10.5f|" (dichotomy f3 0. 1.) (iterations phi3 0.5) (newthon f3 f3' 0.5)

   
    0 // return an integer exit code
