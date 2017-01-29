namespace FsMathLib
open System
open Microsoft.FSharp.Math
open System.Threading.Tasks
//open MathNet.Numerics.LinearAlgebra
//open MathNet.Numerics
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
//open Microsoft.Office.Interop.Excel
//open Microsoft.Office.Interop
open FSharp.ExcelProvider


//open office


module A=
/// <summary> ----------------------------------------------------------
/// overloading operator for parallel matrix multiplication
/// </summary>
    let inline (.*) (a:float[,]) (b:float[,]) =
        let rowsA, colsA = Array2D.length1 a, Array2D.length2 a
        let rowsB, colsB = Array2D.length1 b, Array2D.length2 b
        let result = Array2D.create rowsA colsB 0.0
        Parallel.For(0, rowsA, (fun i->
            for j = 0 to colsB - 1 do
                for k = 0 to colsA - 1 do
                    result.[i,j] <- result.[i,j] + a.[i,k] * b.[k,j]))  
        |> ignore
        result
/// <summary> -------------------------------------------------------
/// Asynchronous Matrix Transpose
/// <param name="b initial matrix"></param>
/// </summary>
    let inline AtransAsync  (b:float[,]) =
        let rowsB, colsB = Array2D.length1 b, Array2D.length2 b
        if(colsB <=0 || rowsB <= 0) then raise (new ArgumentOutOfRangeException("matrix shape error"))
        let result = Array2D.create colsB rowsB  0.0
        Async.Parallel  [ for i in 0 .. colsB - 1 ->async {for k in 0 .. rowsB - 1 do result.[i,k] <- b.[k,i]
            } ]
       // |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
        result
/// <summary> --------------------------------------------------------------------------
/// Asynchronous multiplication of a vector and a matrix
/// </summary>
/// <param name="a vector"></param>
/// <param name="b matrix"></param>
    let inline VxAasync (a:float[]) (b:float[,]) =
        let colsA = a.Length
        let rowsB, colsB = Array2D.length1 b, Array2D.length2 b
        if(colsA <> rowsB || rowsB < 0) then raise (new ArgumentOutOfRangeException("vector shape error"))
        let result = Array.create colsA 0.0
        Async.Parallel [ for i in 0 .. colsA - 1 -> async { for k in 0 .. colsA - 1 do result.[i] <- result.[i] + a.[k] * b.[k,i]} ]
       // |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
        result
/// <summary> ------------------------------------------------------------------------------------
/// Asynchronous multiplication of two matrices a and b. The results matrix is rowsA, columnsB
/// </summary>
/// <param name="a matrix with rowsA and columnsA"></param>
/// <param name="b matrix with rowsB and columnsB"></param>
    let inline AxBasync (a:float[,]) (b:float[,])  =
        let rowsA, colsA = Array2D.length1 a, Array2D.length2 a
        let rowsB, colsB = Array2D.length1 b, Array2D.length2 b
        if(rowsA <> colsB || rowsB < 0) then raise (new ArgumentOutOfRangeException("matrix shape error"))
        let result = Array2D.create rowsA colsB 0.0
//        let result=a*b
        Async.Parallel  [ for i in 0 .. rowsA - 1 -> async { for j in 0 .. colsB - 1 do for k in 0 .. colsA - 1 do result.[i,j] <- result.[i,j] + a.[i,k] * b.[k,j] } ]
     //   |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore 
        result
///<sumary> -------------------------------------------------------------------------------
/// Parallel Multiplication of two matrices
///</summary>
    let inline AxBParallel (a:float[,]) (b:float[,]) =
        let rowsA, colsA = Array2D.length1 a, Array2D.length2 a
        let rowsB, colsB = Array2D.length1 b, Array2D.length2 b
        if(rowsA <> colsB || rowsB < 0) then raise (new ArgumentOutOfRangeException("matrix shape error"))
        let result = Array2D.create rowsA colsB 0.0
        Parallel.For(0, rowsA, (fun i->
            for j = 0 to colsB - 1 do
                for k = 0 to colsA - 1 do
                    result.[i,j] <- result.[i,j] + a.[i,k] * b.[k,j]))  
        |> ignore
        result
//------------------------------------------------------------------------------------
    let gravity=9.806

    let ToMetre  =function
            |"cm" -> 0.01
            |"inch" -> 1./39.37
            |"ft" -> 1./3.28
            |"mm" -> 0.001
            |"m" ->1.0
            |_  -> 1.0
    let ToNewton  =function
            |"kN" -> 1000.
            |"lbf" -> 4.448
            |"kip" -> 4448.
            |"kgf" -> gravity
            |"tonf" -> 8896.
            |"tonnef" -> 1000.*gravity
            |"N" -> 1.0
            |_  -> 1.0
    let ToNperM  =function
            |"kN/m" -> 1000.
            |"lbf/ft" -> 14.594
            |"kip/inch" -> 8275000.
            |"kgf/m" -> gravity
            |"tonf/ft" -> 29190.
            |"tonnef/m" -> 1000.*gravity
            |"N/m" -> 1.0
            |_  -> 1.0
    let ToNperM2  =function
            |"kN/m2" -> 1000.
            |"lbf/sqf" -> 47.88
            |"ksi" -> 6895000.
            |"kgf/m2" -> gravity
            |"tonf/sqf" -> 95760.
            |"tonnef/m2" -> 1000.*gravity
            |"N/m" -> 1.0
            |_  -> 1.0
    let ToNperM3  =function
            |"kN/m3" -> 1000.
            |"lbf/cuf" -> 4.448
            |"kip/inch3" -> 2.714*10.**8.
            |"kgf/m3" -> gravity
            |"tonf/cuf" -> 31420.
            |"tonnef/m3" -> 1000.*gravity
            |"N/m3" -> 1.0
            |_  -> 1.0
    //let inline ReadWF(fullpath:string) = 
        //let xlApp= new Excel.ApplicationClass(Visible=false)      //"B2:G28">
        
        //let WFInput=xlApp.Workbooks.Open(@"C:\Users\Ernesto\iCloudDrive\GitRep3\FsMathLib\WFTables.xlsx")
        //let sheet1 = WFInput.Worksheets.["Sheet1"] :?> Excel.Worksheet
        //let nrow=sheet1.Rows.Count
        //let ncol=sheet1.Columns.Count
        //let sectable=Array2D.zeroCreate nrow ncol
        //let names =Array.create  nrow "empty"
        
        //for r in 0 .. nrow-1 do
            //names.[r]<- sheet1.Cells.[r,0] :?> string
            //for c in 0 .. ncol-1 do
               //sectable.[r,c] <- sheet1.Cells.[r,c] :?> float
        //|> ignore
        //sectable
               
module Excel =               
    type SelectWF (fileFullpath:string) = 
        member val File=fileFullpath
        //inherit Base ExcelFile(fileFullpath) 
    
       //type wflib=ExcelFile<"fileFullpath">
    type wflib=ExcelFile<"WFTables.xlsx">
    //type ExcelFile<fileFullpath:string> = 
        //member this.File =fileFullpath 
    let xlfile=new wflib()
    let wftable=xlfile.Data |> Seq.toArray
    let nrow=wftable.GetUpperBound(0)
    let sectable =Array2D.zeroCreate nrow 10
    let names =Array.create  nrow "empty"
    for i in 0 .. nrow-1 do
    sectable.[i,0] <- wftable.[i].Ax
    sectable.[i,2] <- wftable.[i].J
    sectable.[i,3] <- wftable.[i].Iy
    sectable.[i,4] <- wftable.[i].Ix
    sectable.[i,5] <- wftable.[i].Weight
    sectable.[i,6] <- wftable.[i].Sx
    sectable.[i,7] <- wftable.[i].Sy
    sectable.[i,8] <- wftable.[i].rx
    sectable.[i,9] <- wftable.[i].ry
    names.[i] <- wftable.[i].Designation
    
module Q =
    type Quaternion=struct 
        val X:float
        val Y:float
        val Z:float
        val W:float
        new(w,x,y,z)={W=w;X=x;Y=y;Z=z}

        static member (+)(a:Quaternion, b:Quaternion)=
            new Quaternion(a.W + b.W,a.X + b.X,a.Y + b.Y,a.Z+b.Z)
        static member (-)(a:Quaternion, b:Quaternion)=
            new Quaternion(a.W - b.W,a.X - b.X,a.Y - b.Y,a.Z-b.Z )
        static member (*)(a:Quaternion, b:Quaternion)=
            let r0=b.X
            let r1=b.Y
            let r2=b.Z
            let r3=b.W
            let q0=a.X
            let q1=a.Y
            let q2=a.Z
            let q3=a.W
            let t0 =(r0*q0-r1*q1-r2*q2-r3*q3)
            let t1 = (r0*q1+r1*q0-r2*q3+r3*q2)
            let t2 = (r0*q2+r1*q3+r2*q0-r3*q1)
            let t3 = (r0*q3-r1*q2+r2*q1+r3*q0)            
            new Quaternion(t0,t1,t2,t3 )
        static member (~-)(a:Quaternion)=
            new Quaternion (a.W,-a.X,-a.Y,-a.Z)
    end 
module M=
    type Vec3D()=
        let mutable x=0.0
        let mutable y=0.0
        let mutable z=0.0
        member this.X with get()=x and set(v)=x <- v
        member this.Y with get()=y and set(v)=y <- v
        member this.Z with get()=z and set(v)=z <- v
        member this.Distance (point:Vec3D)=
            let xdif=this.X-point.X
            let ydif=this.Y-point.Y
            let zdif=this.Z-point.Z
            let Distance=sqrt(xdif**2.+ydif**2.+zdif**2.)
            Distance
        static member (.*) (point1:Vec3D,point2:Vec3D)=
            let dotp=point1.X*point2.X+point1.Y*point2.Y+point1.Z*point2.Z
            dotp
        static member (*)(a:Vec3D,b:Vec3D) =
            let u1=a.X
            let u2=a.Y
            let u3=a.Z
            let v1=b.X
            let v2=b.Y
            let v3=b.Z
            let s1=u2*v3-u3*v2
            let s2=u3*v1-u1*v3
            let s3=u1*v2-u2*v1
            let result= (s1,s2,s3)
            result 
