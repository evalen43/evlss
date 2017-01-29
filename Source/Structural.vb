Option Strict Off
Option Explicit On
Imports System.Math

Imports FsMathLib
Public Class Structural
    Dim myfslib As A

#Region "Properties----------------------------"
    Private unitLen, unitF As String
    Private scaleLen, scaleFor As Double
    Sub New()

    End Sub
    Sub New(ByVal UNITL As String, ByVal SCALEL As Double, UNITF As String, ByVal SCALEF As Double)
        Me.unitLen = UNITL
        Me.scaleLen = SCALEL
        Me.unitF = UNITF
        Me.scaleFor = SCALEF
    End Sub
    Public Property UnitL() As String
        Get
            Return unitLen
        End Get
        Set(ByVal Value As String)
            unitLen = Value
        End Set
    End Property
    Public Property UnitFor() As String
        Get
            Return unitF
        End Get
        Set(ByVal Value As String)
            unitF = Value
        End Set
    End Property
    Public Property ScaleL() As Double
        Set(ByVal Value As Double)
            scaleLen = Value
        End Set
        Get
            Return ScaleL
        End Get
    End Property
    Public Property ScaleF() As Double
        Set(ByVal Value As Double)
            scaleFor = Value
        End Set
        Get
            Return scaleFor
        End Get
    End Property
#End Region

    Dim Debugfile As System.IO.StreamWriter

#Region "Function Rotmat -------------------------------"
    Public Overloads Function Rotmat(ByRef rot(,) As Double, ByVal c() As Double, ByVal ndf As Integer, ByRef struct As String) As Double(,)
        Dim CXZ, COSBET, SINBET, beta As Double
        Dim cx, cy, cz As Double
        cx = c(0)
        cy = c(1)
        cz = c(2)
        beta = c(3)
        CXZ = Math.Sqrt(cx * cx + cz * cz)
        Select Case struct
            Case "2DTruss"
                '----
                '2D TRUSS
                '----
                rot(0, 0) = cx
                rot(0, 1) = cy
                rot(1, 0) = -cy
                rot(1, 1) = cx
            Case "3DTruss"
                '----
                'Space TRUSS
                '----
                If (CXZ <> 0#) Then
                    rot(0, 0) = cx
                    rot(0, 1) = cy
                    rot(0, 2) = cz
                    rot(1, 0) = -cx * cy / CXZ
                    rot(1, 1) = CXZ
                    rot(1, 2) = -cy * cz / CXZ
                    rot(2, 0) = -cz / CXZ
                    rot(2, 1) = 0#
                    rot(2, 2) = cx / CXZ
                ElseIf (CXZ = 0#) Then
                    rot(0, 0) = 0#
                    rot(0, 1) = cy
                    rot(0, 2) = 0#
                    rot(1, 0) = -cy
                    rot(1, 1) = 0#
                    rot(1, 2) = 0#
                    rot(2, 0) = 0#
                    rot(2, 1) = 0#
                    rot(2, 2) = 1.0#
                End If
            Case "2DFrame"
                '------------
                'PLANE FRAMES
                '------------
                rot(0, 0) = cx
                rot(0, 1) = cy
                rot(1, 0) = -cy
                rot(1, 1) = cx
                rot(2, 2) = 1.0
            Case "Grid"
                '------------
                'Grids
                '------------
                rot(0, 0) = cx
                rot(0, 1) = cy
                rot(1, 0) = -cy
                rot(1, 1) = cx
                rot(2, 2) = 1.0
            Case "3DFrame"
                '----
                'Space Frames
                '----
                COSBET = Math.Cos(beta)
                SINBET = Math.Sin(beta)
                If (CXZ <> 0#) Then
                    rot(0, 0) = cx
                    rot(0, 1) = cy
                    rot(0, 2) = cz
                    rot(1, 0) = (-cx * cy * COSBET - cz * SINBET) / CXZ
                    rot(1, 1) = CXZ * COSBET
                    rot(1, 2) = (-cy * cz * COSBET + cx * SINBET) / CXZ
                    rot(2, 0) = (cx * cy * SINBET - cz * COSBET) / CXZ
                    rot(2, 1) = -CXZ * SINBET
                    rot(2, 2) = (cy * cz * SINBET + cx * COSBET) / CXZ
                ElseIf (CXZ = 0#) Then
                    '----------------------------------------------
                    'THE MEMBER Is VERTICAL And PERPENDICULAR TO XZ
                    '----------------------------------------------
                    rot(0, 1) = 1.0
                    rot(1, 0) = -COSBET
                    rot(1, 2) = SINBET
                    rot(2, 0) = SINBET
                    rot(2, 2) = COSBET
                End If
        End Select
        For I = 0 To ndf - 1
            For J = 0 To ndf - 1
                rot(I + ndf, J + ndf) = rot(I, J)
            Next J
        Next I
        Return rot
    End Function

#End Region



#Region "Function K Local Stiffness Matrix------------------"
    Public Overloads Function Klocal(ByRef elst(,) As Double, ByVal AX As Double, ByVal YZ As Double, ByVal D As Double, ByVal E As Double) As Double(,)
        '----
        '2D Frames
        '----
        elst(0, 0) = E * AX / D
        elst(0, 3) = -elst(0, 0)
        elst(1, 1) = 12.0# * E * YZ / (D ^ 3)
        elst(1, 2) = 6.0# * E * YZ / (D * D)
        elst(1, 4) = -elst(1, 1)
        elst(1, 5) = elst(1, 2)
        elst(2, 1) = elst(1, 2)
        elst(2, 2) = 4.0# * E * YZ / D
        elst(2, 4) = -elst(1, 2)
        elst(2, 5) = 2.0# * E * YZ / D
        elst(3, 0) = elst(0, 3)
        elst(3, 3) = elst(0, 0)
        elst(4, 1) = elst(1, 4)
        elst(4, 2) = elst(2, 4)
        elst(4, 4) = elst(1, 1)
        elst(4, 5) = elst(2, 4)
        elst(5, 1) = elst(1, 5)
        elst(5, 2) = elst(2, 5)
        elst(5, 4) = elst(4, 5)
        elst(5, 5) = elst(2, 2)
        Return elst
    End Function
    Public Overloads Function Klocal(ByRef elst(,) As Double, ByVal YX As Double, ByVal YZ As Double, ByVal D As Double, ByVal E As Double, G As Double) As Double(,)
        '----
        'Grids
        '----
        elst(0, 0) = G * YX / D
        elst(0, 3) = -G * YX / D
        elst(1, 1) = 4.0# * E * YZ / D
        elst(1, 2) = -6.0# * E * YZ / (D * D)
        elst(1, 4) = 2.0# * E * YZ / D
        elst(1, 5) = 6.0# * E * YZ / (D * D)
        elst(2, 1) = elst(1, 2)
        elst(2, 2) = 12.0# * E * YZ / (D ^ 3)
        elst(2, 4) = elst(1, 2)
        elst(2, 5) = -12.0# * E * YZ / (D ^ 3)
        elst(3, 0) = -elst(0, 0)
        elst(3, 3) = elst(0, 0)
        elst(4, 1) = 2.0# * E * YZ / D
        elst(4, 2) = -6.0# * E * YZ / (D * D)
        elst(4, 4) = 4.0# * E * YZ / D
        elst(4, 5) = 6.0# * E * YZ / (D * D)
        elst(5, 1) = 6.0# * E * YZ / (D * D)
        elst(5, 2) = -12.0# * E * YZ / (D ^ 3)
        elst(5, 4) = 6.0# * E * YZ / (D * D)
        elst(5, 5) = 12.0# * E * YZ / (D ^ 3)
        Return elst
    End Function
    Public Overloads Function Klocal(ByRef elst(,) As Double, ByVal AX As Double, ByVal D As Double, ByVal E As Double) As Double(,)
        '----
        '3D Truss
        '----
        elst(0, 0) = E * AX / D
        elst(3, 0) = -E * AX / D
        elst(0, 3) = -E * AX / D
        elst(3, 3) = E * AX / D
        Return elst
    End Function
    Public Overloads Function K2DT(ByRef elst(,) As Double, ByVal AX As Double, ByVal D As Double, ByVal E As Double) As Double(,)
        '-------------
        '2D Truss
        '------------
        elst(0, 0) = E * AX / D
        elst(1, 0) = 0#
        elst(0, 1) = -E * AX / D
        elst(2, 2) = 0#
        Return elst
    End Function
    Public Overloads Function Klocal(ByRef elst(,) As Double, ByVal AX As Double, ByVal YX As Double, ByVal YY As Double,
                                     ByVal YZ As Double, ByVal D As Double, ByVal E As Double, ByVal G As Double) As Double(,)
        '----
        'Space Frames
        '----
        elst(0, 0) = E * AX / D
        elst(6, 0) = -E * AX / D
        elst(1, 1) = 12.0# * E * YZ / D ^ 3
        elst(5, 1) = 6.0# * E * YZ / D ^ 2
        elst(7, 1) = -12.0# * E * YZ / D ^ 3
        elst(11, 1) = 6.0# * E * YZ / D ^ 2
        elst(2, 2) = 12.0# * E * YY / D ^ 3
        elst(4, 2) = -6.0# * E * YY / D ^ 2
        elst(8, 2) = -12.0# * E * YY / D ^ 3
        elst(10, 2) = -6.0# * E * YY / D ^ 2
        elst(3, 3) = G * YX / D
        elst(9, 3) = -G * YX / D
        elst(2, 4) = -6.0# * E * YY / D ^ 2
        elst(4, 4) = 4.0# * E * YY / D
        elst(8, 4) = 6.0# * E * YY / D ^ 2
        elst(10, 4) = 2.0# * E * YY / D
        elst(1, 5) = 6.0# * E * YZ / D ^ 2
        elst(5, 5) = 4.0# * E * YZ / D
        elst(7, 5) = -6.0# * E * YZ / D ^ 2
        elst(11, 5) = 2.0# * E * YZ / D
        elst(0, 6) = -E * AX / D
        elst(6, 6) = E * AX / D
        elst(1, 7) = -12.0# * E * YZ / D ^ 3
        elst(5, 7) = -6.0# * E * YZ / D ^ 2
        elst(7, 7) = 12.0# * E * YZ / D ^ 3
        elst(11, 7) = -6.0# * E * YZ / D ^ 2
        elst(2, 8) = -12.0# * E * YY / D ^ 3
        elst(4, 8) = 6.0# * E * YY / D ^ 2
        elst(8, 8) = 12.0# * E * YY / D ^ 3
        elst(10, 8) = 6.0# * E * YY / D ^ 2
        elst(3, 9) = -G * YX / D
        elst(9, 9) = G * YX / D
        elst(2, 10) = -6.0# * E * YY / D ^ 2
        elst(4, 10) = 2.0# * E * YY / D
        elst(8, 10) = 6.0# * E * YY / D ^ 2
        elst(10, 10) = 4.0# * E * YY / D
        elst(1, 11) = 6.0# * E * YZ / D ^ 2
        elst(5, 11) = 2.0# * E * YZ / D
        elst(7, 11) = -6.0# * E * YZ / D ^ 2
        elst(11, 11) = 4.0# * E * YZ / D
        Return elst
    End Function
#End Region

#Region "Sub STIFF Element Stiffness Matrix -------------------"
    '----------------------------------------------------
    'NEL=	CURRENT ELEMENT NUMBER
    'AX=   AREA OF ELEMENT CROSS SECTION
    'YX,YY,YZ=   MOMENT OF INERTIA OF ELEMENT CROSS SECTION
    '-----------------------------------------------------
    Public Sub STIFF(ByVal NEL As Integer, ByRef PROP() As Double, ByVal ndf As Integer,
        ByRef ELST(,) As Double, ByVal nprop As Integer, ByVal NTAB As Integer, ByRef struct As String)
        'Debugfile = My.Computer.FileSystem.OpenTextFileWriter("LSS-Debug.txt", False)
        Dim AX, YX, YY, YZ, D, DX, DY, DZ, E, G, BETA, C(3) As Double
        Dim NDFEL, KDSP, I, J As Integer
        NDFEL = ELST.GetUpperBound(0)
        Dim ROT(NDFEL, NDFEL), theta As Double
        KDSP = nprop * (NEL - 1)
        D = PROP(KDSP + 5 + NTAB - 1)
        E = PROP(KDSP + 2 + NTAB - 1)
        G = PROP(KDSP + 3 + NTAB - 1)
        BETA = 0#
        AX = PROP(KDSP)
        YX = PROP(KDSP + 2)
        YY = PROP(KDSP + 3)
        YZ = PROP(KDSP + 4)
        '--------------------------------------------------------------
        'COMPUTE Director Cosines
        '--------------------------------------------------------------
        DX = PROP(KDSP + 6 + NTAB - 1)
        DY = PROP(KDSP + 7 + NTAB - 1)
        DZ = PROP(KDSP + 8 + NTAB - 1)
        C = {DX / D, DY / D, DZ / D, BETA}
        For I = 0 To NDFEL
            For J = 0 To NDFEL
                ELST(I, J) = 0#
            Next J
        Next I
        '--------------------------------------
        'COMPUTE ELEMENT LOCAL STIFFNESS MATRIX
        '--------------------------------------
        Select Case struct
            Case "2DTruss"
                ROT = Rotmat(ROT, C, ndf, struct)
                ELST = K2DT(ELST, AX, D, E)
            Case "2DFrame"
                ROT = Rotmat(ROT, C, ndf, struct)
                ELST = Klocal(ELST, AX, YZ, D, E)
            Case "Grid"
                ROT = Rotmat(ROT, C, ndf, struct)
                ELST = Klocal(ELST, YX, YZ, D, E, G)
            Case "3DTruss"
                ROT = Rotmat(ROT, C, ndf, struct)
                ELST = Klocal(ELST, AX, D, E)
            Case "3DFrame"
                ROT = Rotmat(ROT, C, ndf, struct)
                ELST = Klocal(ELST, AX, YX, YY, YZ, D, E, G)
        End Select
        theta = Math.Atan2(DY, DX)
        '------------------------------------------------------
        'ROTATE ELEMENT STIFFNESS MATRIX TO GLOBAL COORDINATES
        '------------------------------------------------------
        ELST = A.AxBasync(A.AtransAsync(ROT), A.AxBasync(ELST, ROT))
        'ELST = BTAB31(ELST, ROT)
        Return
    End Sub
#End Region

#Region "Function BTAB31 Rotate from Local to Global"

    'Function BTAB31(ByRef A(,) As Double, ByRef B(,) As Double) As Double(,)
    '------------------------------------------
    'THIS PROGRAM COMPUTES THE MATRIX OPERATION 
    'A = TRANSPOSE(B) * A * B)
    '------------------------------------------
    'A = Matrix.Multiply(Matrix.Transpose(B), Matrix.Multiply(A, B))
    'Return A
    'End Function
#End Region

#Region "Function BTAB3"
    'Function BTAB3(ByVal A(,) As Double, ByVal B(,) As Double) As Double(,)
    'Dim N, I, J, K As Integer
    '   N = A.GetUpperBound(0)
    'Dim V(N) As Double
    '------------------------------------------------
    'C	THIS PROGRAM COMPUTES THE MATRIX OPERATION 
    'A = TRANSPOSE(B) * A * B
    'N	=	ACTUAL ORDER OF A AND B
    'V	=	AUXILIARY VECTOR
    'COMPUTE A*B AND STORE IN A
    '----------------------------------------------
    'For I = 0 To N 'DO 10 I=1,N
    'For J = 0 To N 'DO 5 J=1,N
    '           V(J) = 0.0#
    'For K = 0 To N 'DO 5 K=1,N
    '5:                  V(J) = V(J) + A(I, K) * B(K, J)
    '   Next K
    '  Next J
    ' For J = 0 To N 'DO 10 J=1,N
    '10:             A(I, J) = V(J)
    '   Next J
    '  Next I
    '---------------------------------------
    'COMPUTE TRANSPOSE(B)*A AND STORE IN A
    '-------------------------------------------
    ' For J = 0 To N 'DO 20 J=1,N
    'For I = 0 To N 'DO 15 I=1,N
    '           V(I) = 0.0#
    'For K = 0 To N 'DO 15 K=1,N
    '15:                 V(I) = V(I) + B(K, I) * A(K, J)
    '   Next K
    '  Next I
    ' For I = 0 To N 'DO 20 I=1,N
    '20:             A(I, J) = V(I)
    '   Next I
    '  Next J
    '     BTAB3 = A
    'End Function
#End Region

#Region "Sub ELASS-Assembly of Stiffness Matrix"
    Sub ELASS(ByVal NEL As Integer, ByRef CON() As Integer, ByRef ELST(,) As Double, ByRef TK(,) As Double,
              ByVal NNE As Integer, ByVal ndf As Integer)
        '----------------------------------------------------------
        'NEL	=number of the current node
        'N1	=number of the start node
        'N2	=number of the END node
        '-----------------------------------------------------------
        Dim L1, L2, N1, N2, I, I1, J1, I2, J2, KI, KR, KC, IC, K1, K2,
            K, J, L As Integer
        Try
            L1 = NNE * (NEL - 1)
            For I = 0 To NNE - 1 'DO 50 I=1,NNE
                L2 = L1 + I
                N1 = CON(L2)
                I1 = ndf * I '- 1) '- 1 'Modified zero-based --------------
                J1 = ndf * (N1 - 1) '- 1 'Modified zero-based------------
                For J = 0 To NNE - 1  'DO 50 J=I,NNE
                    L2 = L1 + J
                    N2 = CON(L2)
                    I2 = ndf * J '- 1) '- 1 'Modified zero-based----------
                    J2 = ndf * (N2 - 1) '- 1 'Modified zero-based----
                    For K = 0 To ndf - 1  'DO 50 K=1,NDF 'Modified-------
                        KI = 0
                        'IF(N1-N2) 20,10,30
                        If (N1 > N2) Then
                            GoTo 30
                        ElseIf (N1 < N2) Then
                            GoTo 20
                        ElseIf (N1 = N2) Then
                            'STORE A DIAGONAL Submatrix      
10:                         KI = K
                        End If
                        'STORE AN OFF DIAGONAL Submatrix
20:                     KR = J1 + K
                        IC = J2 - KR + 1 '- 1 'Modified zero-based----
                        K1 = I1 + K
                        GoTo 40
                        'STORE THE TRANSPOSE OF AN OFF DIAGONAL MATRIX
30:                     KR = J2 + K
                        IC = J1 - KR + 1 '- 1 'Modified zero-based----
                        K2 = I2 + K
40:                     For L = KI To ndf '- 1 'DO 50 L=KI,NDF
                            KC = IC + L '- 1 'Modified zero-based-----------------
                            'IF(N1-N2) 45,45,46
                            If (N1 <= N2) Then
45:                             K2 = I2 + L
                                GoTo 50
                            Else
46:                             K1 = I1 + L
                            End If
50:                         TK(KR, KC) = TK(KR, KC) + ELST(K1, K2)
                        Next L
                    Next K
                Next J
            Next I
        Catch ex As Exception
            Debug.WriteLine("Program Exception " & N1.ToString & "-" & N2.ToString)
            Debug.WriteLine("Element " & NEL.ToString)
            Debug.WriteLine("KR=" & KR.ToString)
            Debug.WriteLine("KC=" & KC.ToString)
            Debug.WriteLine("K= " & K.ToString & " L= " & L.ToString)
            Debug.WriteLine(ex.ToString)
        End Try
    End Sub 'subroutine elass

#End Region

#Region "Sub ELASS1-Assembly of Stiffness Matrix"

    Sub ELASS1(ByVal NEL As Integer, ByRef CON() As Integer, ByRef ELST(,) As Double, ByRef TK(,) As Double,
              ByVal NNE As Integer, ByVal ndf As Integer)
        '----------------------------------------------------------
        'NEL	=number of the current node
        'N1	=number of the start node
        'N2	=number of the END node
        '(i,j) ----> (i,j-i+1) for base 1
        '(i,j) ----> (i,j-i) for base 0
        '-----------------------------------------------------------
        Dim L1, L2, N1, N2, I, J1, J2, KR, KC, IC, J, ndim, nsubm As Integer
        ndim = ELST.GetUpperBound(0) + 1
        nsubm = (ndim / 2) - 1
        Dim subm(nsubm, nsubm) As Double ', submT(nsubm, nsubm) As Double
        L1 = NNE * (NEL - 1)
        L2 = L1 + 1
        N1 = CON(L1)
        N2 = CON(L2)
        J1 = ndf * (N1 - 1)
        J2 = ndf * (N2 - 1)
        Try
            subm = Submatrix(ELST, "N1")
            For I = 0 To ndf - 1
                KR = J1 + I
                For J = I To ndf - 1
                    KC = J - I
                    TK(KR, KC) = TK(KR, KC) + subm(I, J)
                Next J
            Next I
            subm = Submatrix(ELST, "N2")
            For I = 0 To ndf - 1
                KR = J2 + I
                For J = I To ndf - 1
                    KC = J - I
                    TK(KR, KC) = TK(KR, KC) + subm(I, J)
                Next J
            Next I
            If (N1 < N2) Then
                subm = Submatrix(ELST, "N1<N2")
                For I = 0 To ndf - 1
                    KR = J1 + I
                    IC = J2 - KR
                    For J = 0 To ndf - 1
                        KC = IC + J
                        TK(KR, KC) = TK(KR, KC) + subm(I, J)
                    Next J
                Next I
            End If
            If (N1 > N2) Then
                subm = Submatrix(ELST, "N1>N2")
                'submT = Matrix.Transpose(subm)
                For I = 0 To ndf - 1
                    KR = J2 + I
                    IC = J1 - KR
                    For J = 0 To ndf - 1
                        KC = IC + J
                        TK(KR, KC) = TK(KR, KC) + subm(I, J)
                    Next J
                Next I
            End If
            'MatPrint(TK)
        Catch ex As Exception
            Debug.WriteLine("Program Exception " & N1.ToString & "-" & N2.ToString)
            Debug.WriteLine("Element " & NEL.ToString)
            Debug.WriteLine("KR=" & KR.ToString)
            Debug.WriteLine("KC=" & KC.ToString)
            Debug.WriteLine(ex.ToString)
        End Try

    End Sub 'subroutine elass

#End Region

#Region "Sub Bound Boundary Conditions"
    Sub BOUND(ByVal TK(,) As Double, ByRef AL(,) As Double, ByRef REAC(,) As Double,
              ByVal IB() As Integer, ByVal NLC As Integer, ByVal NDF As Integer,
              ByVal NBN As Integer)
        Dim I, J, ICOL, L1, NO, K1, L2, L, KR, KV, N, MS As Integer
        N = TK.GetUpperBound(0) + 1
        MS = TK.GetUpperBound(1) + 1
        '--------------------------
        'INTRODUCTION OF THE BOUNDARY CONDITIONS
        '-------------------------
        'double precision :: REAC(N,NLC),AL(N,NLC),TK(N,MS)
        'integer*4 :: IB(NIB)
        '
        For L = 1 To NBN 'DO 100 L=1,NBN
            '----------------
            'NO=NUMBER OF THE CURRENT BOUNDARY
            '---------------------
            L1 = (NDF + 1) * (L - 1) + 1
            NO = IB(L1 - 1)
            K1 = NDF * (NO - 1)
            For I = 1 To NDF 'DO 100 I=1,NDF
                L2 = L1 + I
                'IF(IB(L2)) 100,10,100
                If (IB(L2 - 1) < 0 Or IB(L2 - 1) > 0) Then
                    GoTo 100
                End If
                '--------------------------------
                'SET DIAGONAL COEFFICIENT OF TK EQUAL TO 1
                'AND PLACE PRESCRIBED VALUE IN AL
                '-----------------------------------
10:             KR = K1 + I
                For J = 2 To MS 'DO 50 J=2,MS
                    KV = KR + J - 1
                    'IF(N-KV)30,20,20
                    If (N < KV) Then
                        GoTo 30
                    End If
                    'MODIFY ROW OF TK AND CORRESPONDING ELEMENTS IN AL
                    '------------------------------
20:                 For ICOL = 1 To NLC 'DO 60 ICOL=1,NLC
60:                     AL(KV - 1, ICOL - 1) = AL(KV - 1, ICOL - 1) - TK(KR - 1, J - 1) * REAC(KR - 1, ICOL - 1)
                    Next ICOL
                    TK(KR - 1, J - 1) = 0.0#
30:                 KV = KR - J + 1
                    'IF(KV)50,50,40
                    If (KV <= 0) Then
                        GoTo 50
                    End If
                    '------------------
                    'MODIFY COLUMN IN TK AND CORRESPONDING ELEMENT IN AL
                    '---------------------
40:                 For ICOL = 1 To NLC 'DO 70 ICOL=1,NLC
70:                     AL(KV - 1, ICOL - 1) = AL(KV - 1, ICOL - 1) - TK(KV - 1, J - 1) * REAC(KR - 1, ICOL - 1)
                    Next ICOL
                    TK(KV - 1, J - 1) = 0.0#
50:                 'Continue Do
                    TK(KR - 1, 1 - 1) = 1.0#
                Next J
                For ICOL = 1 To NLC 'DO 200 ICOL=1,NLC
200:                AL(KR - 1, ICOL - 1) = REAC(KR - 1, ICOL - 1)
                Next ICOL
100:        Next I 'Continue Do
        Next L
        Return
    End Sub
#End Region

#Region "Sub BGAUSS Solve Banded System"
    Sub BGAUSS(ByRef A(,) As Double, ByRef B(,) As Double, ByRef KERR As Integer)
        '---------------------------------------------------
        'N:     ROW DIMENSION OF A AND B
        'MS:     COLUMN DIMENSION OF A
        'LC:     COLUMN DIMENSION OF B
        '(D) : AUXILIARY(VECTOR)
        '---------------------------------------------------
        'double precision :: A(N,MS),B(N,NLC),D(N)
        Dim N1, N, K1, K, MS, NLC, NI, L, I, J, K2, ICOL, K3 As Integer
        Dim C As Double
        N = A.GetUpperBound(0) + 1
        MS = A.GetUpperBound(1) + 1
        NLC = B.GetUpperBound(1) + 1
        Dim D(N - 1) As Double
        KERR = 0
        N1 = N - 1
        For K = 1 To N1 'DO 100 K=1,N1
            C = A(K - 1, 1 - 1)
            K1 = K + 1
            If (Math.Abs(C) <= 0.000001) Then ' 1,1,3
                '1       WRITE(IOUT,2) K
                '2       FORMAT(1HO,'**** SINGULARITY IN ROW',I5,1X,'****')
                KERR = KERR + 1
                GoTo 300
            Else
                '--------------------------------------------
                'DIVIDE ROW BY DIAGONAL COEFFICIENT
3:              NI = K1 + MS - 2
            End If
            L = Math.Min(NI, N)
            For J = 2 To MS 'DO 11 J=2,MS
11:             D(J - 1) = A(K - 1, J - 1)
            Next J
            For J = K1 To L 'DO 4 J=K1,L
                K2 = J - K + 1
4:              A(K - 1, K2 - 1) = A(K - 1, K2 - 1) / C
            Next J
            For ICOL = 1 To NLC 'DO 12 ICOL=1,NLC
                B(K - 1, ICOL - 1) = B(K - 1, ICOL - 1) / C
12:         Next ICOL
            '------------------------------------------
            'ELIMINATE UNKNOWN X(K) FROM ROW I
            '------------------------------------------
            For I = K1 To L 'DO 10 I=K1,L
                K2 = I - K1 + 2
                C = D(K2 - 1)
                For J = I To L 'DO 5 J=I,L
                    K2 = J - I + 1
                    K3 = J - K + 1
5:                  A(I - 1, K2 - 1) = A(I - 1, K2 - 1) - C * A(K - 1, K3 - 1)
                Next J
                For ICOL = 1 To NLC 'DO 10 ICOL=1,NLC
10:                 B(I - 1, ICOL - 1) = B(I - 1, ICOL - 1) - C * B(K - 1, ICOL - 1)
                Next ICOL
            Next I
100:    Next K
        '------------------------------
        'COMPUTE LAST UNKNOWN
        '------------------------------
        If (Math.Abs(A(N - 1, 1 - 1)) <= 0.000001) Then '17,17,101
            '17      WRITE(IOUT,2) N
            KERR = KERR + 1
            GoTo 300
        Else
101:        For ICOL = 1 To NLC 'DO 102 ICOL=1,NLC
102:            B(N - 1, ICOL - 1) = B(N - 1, ICOL - 1) / A(N - 1, 1 - 1)
            Next ICOL
        End If
        '----------------------------------------
        'APPLY BACKSUBSTITUTION PROCESS TO COMPUTE REMAINING UNKNOWNS
        '------------------------------------------
        For I = 1 To N1 'DO 200 I=1,N1
            K = N - I
            K1 = K + 1
            NI = K1 + MS - 2
            L = Math.Min(NI, N)
            For J = K1 To L 'DO 200 J=K1,L
                K2 = J - K + 1
                For ICOL = 1 To NLC 'DO 200 ICOL=1,NLC
200:                B(K - 1, ICOL - 1) = B(K - 1, ICOL - 1) - A(K - 1, K2 - 1) * B(J - 1, ICOL - 1)
                Next ICOL
            Next J
        Next I
300:    Return
    End Sub
#End Region

#Region "Sub Force Internal Forces--------------------------------------------------------------"
    Sub Force(ByVal con() As Integer, ByVal prop() As Double, ByRef forc() As Double, ByRef reac(,) As Double,
              ByRef al(,) As Double, ByVal ndf As Integer, ByVal ndfel As Integer, ByVal NE As Integer,
              ByVal NNE As Integer, ByVal NTAB As Integer, ByVal NPROP As Integer, ByVal FEM() As Double, ByRef struct As String, ByRef loadflag() As Integer)

        Dim K1, K2, I, J1, J2, L, L2, N1, N2, NEL, kLC, I1, I2, I3, LC As Integer
        Dim AX, YX, YY, YZ, E, G, D, DX, DY, DZ, CX, CY, CZ, BETA, C(3), angle As Double
        Dim rot(ndfel - 1, ndfel - 1), ELST(ndfel - 1, ndfel - 1), U(ndfel - 1), UL(ndfel - 1), f(ndfel - 1), fg(ndfel - 1), f1(ndfel - 1) As Double

        LC = al.GetUpperBound(1) + 1
        For kLC = 1 To LC
            For NEL = 1 To NE
                L = NNE * (NEL - 1)
                L2 = NPROP * (NEL - 1)
                N1 = con(L)
                N2 = con(L + 1)
                AX = prop(L2)
                YX = prop(L2 + 2)
                YY = prop(L2 + 3)
                YZ = prop(L2 + 4)
                E = prop(L2 + 1 + NTAB)
                G = prop(L2 + 2 + NTAB)
                D = prop(L2 + 4 + NTAB)
                DX = prop(L2 + 5 + NTAB)
                DY = prop(L2 + 6 + NTAB)
                DZ = prop(L2 + 7 + NTAB)
                angle = Atan2(DY, DX)
                CX = DX / D
                CY = DY / D
                CZ = DZ / D
                BETA = 0
                C = {Cos(angle), Sin(angle), CZ, BETA}
                rot = Rotmat(rot, C, ndf, struct)
                Select Case struct
                    Case "2DTruss"
                        ELST = K2DT(ELST, AX, D, E)
                    Case "2DFrame"
                        ELST = Klocal(ELST, AX, YZ, D, E)
                    Case "3DTruss"
                        ELST = Klocal(ELST, AX, D, E)
                    Case "3DFrame"
                        ELST = Klocal(ELST, AX, YX, YY, YZ, D, E, G)
                End Select
                K1 = ndf * (N1 - 1)
                K2 = ndf * (N2 - 1)
                For I = 1 To ndf
                    J1 = K1 + I
                    J2 = K2 + I
                    U(I - 1) = al(J1 - 1, kLC - 1) 'Modified zero-base
                    U(I + ndf - 1) = al(J2 - 1, kLC - 1) 'Modified zero-base
                Next I
                UL = A.VxAasync(U, rot)
                'UL = Matrix.MulMatVec(rot, U)
                '----------------------------------------------
                'COMPUTE MEMBER END ForceS IN LOCAL COORDINATES
                '----------------------------------------------
                f = A.VxAasync(UL, ELST)
                'f = Matrix.MulMatVec(ELST, UL)
                '----------------------------------------
                'STORE MEMBER END ForceS IN ARRAY Force
                '--------------------------------------
                I1 = ndfel * (NEL - 1) + NE * ndfel * (kLC - 1)
                I3 = ndfel * (NEL - 1)
                If (loadflag(kLC - 1) = 0) Then
                    For I = 0 To ndfel - 1
                        I2 = I1 + I
                        forc(I2) = f(I)
                        f1(I) = f(I)
                    Next I
                ElseIf (loadflag(kLC - 1) = 1) Then
                    For I = 0 To ndfel - 1
                        I2 = I1 + I
                        forc(I2) = f(I) + FEM(I3 + I)
                        f1(I) = f(I) + FEM(I3 + I)
                    Next I
                End If
                '------------------------------------------------------------------------
                'ROTATE MEMBER ForceS TO THE GLOBAL REFERENCE FRAME AND STORE IN ARRAY FG
                '------------------------------------------------------------------------
                fg = A.VxAasync(f1, A.AtransAsync(rot))
                'fg = Matrix.MulMatVec(Matrix.Transpose(rot), f1)
                '----------------------------------------------------------
                'ADD ELEMENT CONTRIBUTION TO NODAL RESULTANTS IN ARRAY REAC
                '----------------------------------------------------------
                For I = 1 To ndf
                    J1 = K1 + I
                    J2 = K2 + I
                    reac(J1 - 1, kLC - 1) = reac(J1 - 1, kLC - 1) + fg(I - 1)
                    reac(J2 - 1, kLC - 1) = reac(J2 - 1, kLC - 1) + fg(I + ndf - 1)
                Next I
            Next NEL
        Next kLC
        Return
    End Sub
#End Region

#Region "Sub Dead Load -------------------------------------"
    Sub DLOAD(ByVal CON() As Integer, ByRef AL(,) As Double, ByVal PROP() As Double, ByVal NPROP As Integer, ByVal LC As Integer, ByVal NTAB As Integer,
     ByVal NNE As Integer, ByVal NDF As Integer, ByVal NE As Integer, ByRef FEM() As Double, ByVal NDFEL As Integer, ByRef struct As String)

        Dim K1, K2, J1, J2, L, L2, N1, N2, NEL, KLC, I2 As Integer
        Dim VG(NDFEL - 1), VL(NDFEL - 1), W, RX, RY, MomentZ, AX, MomentY,
            D, DX, DY, DZ, BETA, rot(NDFEL - 1, NDFEL - 1), CX, CY, CZ, CXZ, WX, WY, WZ, C(4) As Double
        LC = AL.GetUpperBound(1) + 1
        For NEL = 1 To NE
            L = NNE * (NEL - 1)
            L2 = NPROP * (NEL - 1)
            N1 = CON(L)
            N2 = CON(L + 1)
            K1 = NDF * N1
            K2 = NDF * N2
            AX = PROP(L2)
            D = PROP(L2 + 4 + NTAB) : DX = PROP(L2 + 5 + NTAB) : DY = PROP(L2 + 6 + NTAB) : DZ = PROP(L2 + 7 + NTAB)
            W = PROP(L2 + 5)
            If (W = 0#) Then W = AX * PROP(L2 + 3 + NTAB)
            If (W = 0#) Then GoTo 100
            CX = DX / D : CY = DY / D : CZ = DZ / D : CXZ = Math.Sqrt(CX ^ 2 + CZ ^ 2)
            BETA = 0#
            C = {CX, CY, CZ, BETA}
            Select Case struct
#Region "2DTruss"
                Case "2DTruss"
                    WX = -CY * W
                    WY = -CX * W
                    RX = WX * D / 2.0#
                    RY = WY * D / 2.0#
                    VL(0) = RX
                    VL(1) = 0#
                    VL(2) = RX
                    VL(3) = 0#
                    rot = Rotmat(rot, C, NDF, struct)
                    VG = A.VxAasync(VL, A.AtransAsync(rot))
                        'VG = Matrix.MulMatVec(Matrix.Transpose(rot), VL)
#End Region
#Region "2DFrame"
                Case "2DFrame"
                    WX = CY * W
                    WY = CX * W
                    RX = WX * D / 2.0#
                    RY = WY * D / 2.0#
                    MomentZ = WY * D * D / 12.0#
                    VL(0) = -RX
                    VL(1) = -RY
                    VL(2) = -MomentZ
                    VL(3) = -RX
                    VL(4) = -RY
                    VL(5) = MomentZ
                    rot = Rotmat(rot, C, NDF, struct)
                    VG = A.VxAasync(VL, A.AtransAsync(rot))
                        'VG = Matrix.MulMatVec(Matrix.Transpose(rot), VL)
#End Region
#Region "3DTruss"
                Case "3DTruss"
                    If (CXZ > 0) Then
                        WX = -CZ * W
                        WY = CY * CZ * W / CXZ
                        WZ = -CX * W / CXZ
                    ElseIf (CXZ = 0) Then
                        WX = 0
                        WY = 0
                        WZ = -W
                    End If
                    VL(0) = WX * D * 0.5
                    VL(3) = WX * D * 0.5
                    rot = Rotmat(rot, C, NDF, struct)
                    VG = A.VxAasync(VL, A.AtransAsync(rot))
                        'VG = Matrix.MulMatVec(Matrix.Transpose(rot), VL)
#End Region
#Region "3DFrame"
                Case "3DFrame"
                    If (CXZ > 0) Then
                        WX = -CZ * W
                        WY = CY * CZ * W / CXZ
                        WZ = -CX * W / CXZ
                    ElseIf (CXZ = 0) Then
                        WX = 0
                        WY = 0
                        WZ = -W
                    End If
                    MomentZ = WX * D * D / 12.0#
                    MomentY = WZ * D * D / 12.0#
                    VL(0) = WX * D * 0.5
                    VL(1) = WY * D * 0.5
                    VL(2) = WZ * D * 0.5
                    VL(3) = 0#
                    VL(4) = -MomentY
                    VL(5) = -MomentZ
                    VL(6) = WX * D * 0.5
                    VL(7) = WY * D * 0.5
                    VL(8) = WZ * D * 0.5
                    VL(9) = 0#
                    VL(10) = MomentY
                    VL(11) = MomentZ
                    rot = Rotmat(rot, C, NDF, struct)
                    VG = A.VxAasync(VL, A.AtransAsync(rot))
                    'VG = Matrix.MulMatVec(Matrix.Transpose(rot), VL)
#End Region
            End Select
            '----------------------------------------
            'STORE MEMBER Fixed END ForceS IN ARRAY FEM
            '--------------------------------------
            For I = 0 To NDFEL - 1
                I2 = NDFEL * (NEL - 1) + I
                FEM(I2) = VL(I)
            Next I
            For KLC = 1 To LC
                For I = 1 To NDF
                    J1 = K1 + I
                    J2 = K2 + I
                    AL(J1 - 1, KLC - 1) = AL(J1 - 1, KLC - 1) + VG(I - 1)
                    AL(J2 - 1, KLC - 1) = AL(J2 - 1, KLC - 1) + VG(I + NDF - 1)
                Next I
            Next KLC
100:    Next NEL
        Return
    End Sub
#End Region

#Region "Sub OUTPUT----------------------------------------------"
    '''<summary>
    '''PROGRAM TO OUTPUT JOINT DISPLACEMENT, NODAL REACTION AND MEMBER ForceS
    '''</summary>
    Sub OUTPT(ByVal AL(,) As Double, ByVal REAC(,) As Double, ByVal FORC() As Double, ByVal NLC As Integer,
            ByVal PROP() As Double, ByVal nprop As Integer, ByVal ntab As Integer, ByRef TITLE As String,
            ByVal IB() As Integer, ByVal N As Integer, ByVal NN As Integer, ByVal NDF As Integer, ByRef fout As String,
            ByVal NBN As Integer, ByVal NE As Integer, ByVal NDFEL As Integer, ByVal NNE As Integer,
            ByVal CON() As Integer, ByVal KITER As Integer,
            ByRef PNAME As String, ByRef PTITLE As String, ByVal KSTRUC As Integer, ByRef nodestring() As String, ByRef elemtable() As String)
        Dim file As System.IO.StreamWriter
        Dim nodename(NN), LINE, eqt As String
        Dim Ulen(NDF), Ufor(NDF), rmom(NDFEL), sLen, sFor, unityratio, fyield As Double
        Dim ialpha = 0, I, J, K, K1, K2, KLC, L1, NO, NEL, N1, count As Integer
        Dim TestDateTime As Date = #1/27/2001 5:04:23 PM#
        Dim TestStr, TimeStr, Tab As String
        Dim dispheader As String = String.Format("{0,10} {1,10} {2,10} {3,10}", "Node", "Dx", "Dy", "ROTz")
        Dim dispheader2 As String = String.Format("{0,10} {1,10} {2,10} {3,10}", "    ", "<" & UnitL & ">", "<" & UnitL & ">", "<rad>")
        Dim forheader As String = String.Format("{0,10} {1,10} {2,10} {3,10}", "Node", "Px", "Py", "Mz")
        Dim forheader2 As String = String.Format("{0,10} {1,10} {2,10} {3,10}", "    ", "<" & unitF & ">", "<" & unitF & ">", "<" & unitF & UnitL & ">")
        Dim memheader As String = String.Format("{0,10} {1,10} {2,10} {3,10} {4,10}", "Element", "Node", "Px", "Py", "Mz")
        Dim memheader2 As String = String.Format("{0,10} {1,10} {2,10} {3,10} {4,10}", "  ", "    ", "<" & unitF & ">", "<" & unitF & ">", "<" & unitF & UnitL & ">")
        TestStr = Format(Now(), "D")
        TimeStr = Format(Now(), "hh:mm:ss.fff tt")
        Tab = "       "
        eqt = " "
        '--------------------------------------------------------------------
        'WRITE NODAL DISPLACEMENTS
        '---------------------------------------------------------------------
        file = My.Computer.FileSystem.OpenTextFileWriter(fout, False)
        KITER = 1
        file.WriteLine("{0,20}", PTITLE)
        LINE = TITLE
        file.WriteLine(TITLE)
        file.WriteLine(Format(Now(), "D"))
        file.WriteLine(TimeStr)
        file.WriteLine("-----------------------------------------------------")
        If (KITER > 0) Then
            file.WriteLine("Number of Iterations " & KITER.ToString())
        End If
        sLen = A.ToMetre(UnitL)
        sFor = A.ToNewton(UnitFor)
        fyield = 36.0# * A.ToNperM2("ksi")
        Ulen(0) = 1.0# / sLen
        Ulen(1) = 1.0# / sLen
        Ulen(2) = 1.0#
        Ufor(0) = 1.0# / sFor
        Ufor(1) = 1.0# / sFor
        Ufor(2) = 1.0# / (sFor * sLen)
        For KLC = 1 To NLC
            file.WriteLine("Nodal Displacements for Loading " & KLC.ToString())
            file.WriteLine("Active Units: " & UnitL)
            If (KSTRUC = 4) Then
                file.WriteLine("NODE" & Tab & "DX" & Tab & "DY" & Tab & "DZ" & "ROTX" & "ROTY" & "ROTZ")
            ElseIf (KSTRUC = 3) Then
                file.WriteLine(dispheader)
                file.WriteLine(dispheader2)
            End If
            For I = 1 To NN
                K1 = NDF * (I - 1) + 1
                K2 = K1 + NDF - 1
                file.Write("{0,10}", nodestring(I - 1))
                count = 0
                For J = K1 To K2
                    file.Write("{0,12}", Format(AL(J - 1, KLC - 1) * Ulen(count), "0.00000"))
                    count = count + 1
                Next J
                file.WriteLine("")
            Next I
        Next KLC
        '------------------------------------------------------
        'WRITE NODAL REACTIONS
        '------------------------------------------------------
        file.WriteLine("-----------------------------------------------------")
        For KLC = 1 To NLC
            file.WriteLine("Nodal Reactions for Loading " & KLC.ToString())
            file.WriteLine("Active Units: " & UnitFor & " " & UnitL)
            If (KSTRUC = 4) Then
                file.WriteLine("NODE" & Tab & "PX" & Tab & "PY" & Tab & "PZ" & "MX" & Tab & "MY" & Tab & "MZ")
            ElseIf (KSTRUC = 3) Then
                file.WriteLine(forheader)
                file.WriteLine(forheader2)
            End If
            For I = 1 To NBN
                L1 = (NDF + 1) * (I - 1) + 1
                NO = IB(L1 - 1)
                K1 = NDF * (NO - 1) + 1
                K2 = K1 + NDF - 1
                file.Write("{0,10}", nodestring(NO - 1))
                count = 0
                For J = K1 To K2
                    file.Write("{0,12}", Format(REAC(J - 1, KLC - 1) * Ufor(count), "#.00"))
                    count = count + 1
                Next J
                file.WriteLine("")
            Next I
        Next KLC
        file.WriteLine("-----------------------------------------------------")
        '---------------------------------------------------
        'OUTPUT MEMBER END ForceS
        '---------------------------------------------------
        '103:    'IF(KGO.LT.4) RETURN
        For KLC = 1 To NLC
            file.WriteLine("Member End Forces for Loading " & KLC.ToString())
            file.WriteLine("Active Units: " & UnitL)
            If (KSTRUC = 4) Then
                file.WriteLine("ELEMENT" & Tab & "NODE     " & "FX" & Tab & "FY" & Tab & "FZ" & Tab & "MX" & Tab & "MY" & Tab & "MZ")
            ElseIf (KSTRUC = 3) Then
                file.WriteLine(memheader)
                file.WriteLine(memheader2)
            End If
            For NEL = 1 To NE
                K1 = NDFEL * (NEL - 1) + NE * NDFEL * (KLC - 1)
                K2 = K1 + 2
                N1 = NNE * (NEL - 1)
                file.Write("{0,10}", Format(elemtable(NEL - 1)))
                file.Write("{0,10}", Format(nodestring(CON(N1) - 1)))
                count = 0
                For K = K1 To K2
                    file.Write("{0,12}", Format(FORC(K) * Ufor(count), "#.00"))
                    rmom(count) = FORC(K)
                    count = count + 1
                Next K
                'rmom(0) = FORC(K1)
                'rmom(1) = FORC(K2)
                file.WriteLine("")
                LINE = NEL.ToString
                K1 = K2 + 1
                K2 = K1 + 2
                file.Write("{0,10}", Format(elemtable(NEL - 1)))
                file.Write("{0,10}", Format(nodestring(CON(N1 + 1) - 1)))
                count = 0
                For K = K1 To K2
                    file.Write("{0,12}", Format(FORC(K) * Ufor(count), "#.00"))
                    rmom(count + NDF) = FORC(K)
                    count = count + 1
                Next K
                'rmom(3) = FORC(K1)
                'rmom(4) = FORC(K2)
                unityratio = AISC(NEL, PROP, nprop, ntab, rmom, fyield, eqt)
                file.WriteLine("")
                file.WriteLine("{0,10} {1,10} {2,10} {3,12} {4,12}", elemtable(NEL - 1), "AISC", "URatio", Format(unityratio, "0.000"), eqt)
            Next NEL
            file.WriteLine("-----------------------------------------------------")
        Next KLC
        TestStr = Format(Now(), "D")
        TimeStr = Format(Now(), "hh:mm:ss.fff tt")
        file.WriteLine(Format(Now(), "D"))
        file.WriteLine(TimeStr)
        file.Close()
    End Sub
#End Region

#Region "Sub MatPrint ----------------"
    Sub MatPrint(ByVal A(,) As Double)
        Dim Debugfile As System.IO.StreamWriter
        Debugfile = My.Computer.FileSystem.OpenTextFileWriter("LSS-Debug.txt", True)
        Dim irow, icol As Integer

        irow = A.GetUpperBound(0)
        icol = A.GetUpperBound(1)
        Debugfile.WriteLine("irow " & irow.ToString())
        Debugfile.WriteLine("icol " & icol.ToString())
        Debug.WriteLine("irow " & irow.ToString())
        Debug.WriteLine("icol " & icol.ToString())
        For i = 0 To irow
            For j = 0 To icol
                Debugfile.Write("{0,12}", Format(A(i, j), "#.00"))
            Next j
            Debugfile.WriteLine("")
        Next i
        Debugfile.Close()
    End Sub
#End Region

#Region "Function Submatrix---------------------------------------------"
    Public Function Submatrix(ByVal ELST(,) As Double, ByRef LOC As String)
        Dim N, m, i, j, k1, k2 As Integer
        N = ELST.GetUpperBound(0) + 1
        m = N / 2
        Dim subM(m - 1, m - 1) As Double
        Select Case LOC
            Case "N1"
                For i = 0 To m - 1
                    For j = 0 To m - 1
                        subM(i, j) = ELST(i, j)
                    Next j
                Next i

            Case "N2"
                For i = 0 To m - 1
                    k1 = i + m
                    For j = 0 To m - 1
                        k2 = j + m
                        subM(i, j) = ELST(k1, k2)
                    Next j
                Next i
            Case "N1<N2"
                For i = 0 To m - 1
                    For j = 0 To m - 1
                        k2 = j + m
                        subM(i, j) = ELST(i, k2)
                    Next j
                Next i
            Case "N1>N2"
                For i = 0 To m - 1
                    k1 = i + m
                    For j = 0 To m - 1
                        k2 = j
                        subM(i, j) = ELST(k1, k2)
                    Next j
                Next i
        End Select
        Submatrix = subM
    End Function
#End Region

#Region "AISC Check-------------------------------------"
    '''<summary>
    '''Verify stress ratio For tube members according To API RP 2A
    '''OD=tube diameter; TH= wall thickness.
    '''</summary>
    Function AISC(ByVal NEL As Integer, ByVal PROP() As Double, ByVal nprop As Integer, ByVal NTAB As Integer,
                  ByVal rmom() As Double, ByVal fyield As Double, ByRef eqtn As String) As Double
        Dim PI, cmx, cmy, r, yiner, xiner, area, rmomx, rmomy, sx, sy, rx, ry, p, fx, fy,
        faw, slendy, fb, cc, Fey, Fa, ratio1, uratio, blngth, E As Double
        Dim KDSP As Integer
        PI = 3.14159
        cmx = 0.85
        cmy = 0.85
        KDSP = nprop * (NEL - 1)
        blngth = PROP(KDSP + 5 + NTAB - 1)
        E = PROP(KDSP + 2 + NTAB - 1)
        area = PROP(KDSP)
        yiner = PROP(KDSP + 3)
        xiner = PROP(KDSP + 4)
        sx = PROP(KDSP + 6)
        sy = PROP(KDSP + 7)
        rx = PROP(KDSP + 8)
        ry = PROP(KDSP + 9)
        r = Min(rx, ry)
        p = rmom(0)
        rmomx = Max(rmom(2), rmom(5))
        'rmomy = Max(rmom(2), rmom(4))
        rmomy = 0#
        '     -----
        'Compute working stresses
        '     -----
        fx = rmomx / sx
        fy = rmomy / sy
        faw = p / area
        slendy = blngth / r
        '     -----
        'Compute allowable stresses
        '     -----
        fb = 0.66 * fyield
        cc = Sqrt(19.7392 * E / fyield)
        Fey = 5.149359 * E / (slendy ^ 2)
        If (slendy < cc) Then
            Fa = fyield * (1.0# - 0.5 * (slendy / cc) ^ 2) / (1.66667 + 0.375 * (slendy / cc) -
            0.125 * (slendy / cc) ^ 3)
        Else
            Fa = Fey
        End If
        '     -----
        'Compute Unity Ratio
        '     -----
        If (faw < 0) Then
            If (Abs(faw / Fa) <= 0.15) Then
                uratio = Abs(faw / Fa) + Sqrt(fx ^ 2 + fy ^ 2) / fb
                eqtn = "(2.5.2-8)"
            Else
                ratio1 = Abs(faw / (0.6 * fyield)) + Sqrt(fx ^ 2 + fy ^ 2) / fb
                uratio = Abs(faw / Fa) + cmx * Sqrt(fx ^ 2 + fy ^ 2) / ((1.0# - Abs(faw / Fey)) * fb)
                eqtn = "(2.5.2-6)"
                If (uratio < ratio1) Then
                    uratio = ratio1
                    eqtn = "(2.5.2-7)"
                End If
            End If
        Else
            uratio = Abs(faw / (0.6 * fyield)) + Sqrt(fx ^ 2 + fy ^ 2) / fb
            eqtn = "(2.5.2-7a)"
        End If
        AISC = uratio
    End Function

#End Region

End Class
