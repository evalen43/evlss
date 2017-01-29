using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
//using StructLib_VB;
using myfsMod = FsMathLib.A;
using excelMod = FsMathLib.Excel;
using System.Windows.Forms;
//using ExcelProvider;

namespace StructInput_Cs
{
    public class StructInput
    {
        //Structural myvb = new Structural();

 #region Variables and Properties ---------------------------------------
        /// <summary>
        /// <ntab=Number of elements in section table></ntab>
        /// nprop=ntab+9
        /// </summary>
        public int nn = 0, ne = 0, nbn = 0, ms = 0, ndf, nne = 2, ncon, nproptot, nib, n, lc = 0, KSTRUC,
            ntab = 10, nprop = 19, nfem;
        public double[] x, y, z, prop, fem;
        public double[,] al, reac;
        public int[] con, ib, loadflag;

        public string title = " ";
        int nln = 0, nmat = 0, ntitle = 0, nsec = 0, ic1 = 0, ic2 = 0,
             ioffelem = 5, itab = 0, imat = 0, n1, n2, ioffsec, ioffmat = 4, ntabmat = 3,
             jn, kdsp, nconst = 0, vlength = 0, items = 0;
        int[] ic;
        double[] w;
        double[,] sectable, mattable;
        string[] nodeboundname;
        public string unitL, structure, unitF;
        double d, dx, dy, scaleL, scaleF;

        ArrayList vector = new ArrayList();
        public List<string> nodelist = new List<string>();
        public List<string> seclist = new List<string>();
        public List<string> matlist = new List<string>();
        public List<string> elemlist = new List<string>();
        // public readonly object MessageBox;
        #endregion

 #region Method Input ----------------------------------------------
        public void Input(string fullPathname)
        {
            loadflag = new int[10];
            #region Input Title-------------------------------                   
            string unitl = "";
            string unitf = "";
            Linecomp(fullPathname, "title", vector, ref ntitle, ref unitl, ref unitf);
            string[] array = vector.ToArray(typeof(string)) as string[];
            foreach (string str in array) title += str + " ";
            //          source.Text += title + "\n";
            #endregion

            #region Input Constants-----------------------------------------------                   
            Linecomp(fullPathname, "constants", vector, ref nconst, ref unitl, ref unitf);
            string[] array1 = vector.ToArray(typeof(string)) as string[];
            for (int i = 0; i < array1.Length; i++)
            {
                if (array1[i] == "UnitL") unitL = array1[i + 1];
                if (array1[i] == "UnitF") unitF = array1[i + 1];
                if (array1[i] == "Structure") structure = array1[i + 1];
            }
            switch (structure)
            {
                case ("2DFrame"):
                    KSTRUC = 3;
                    ndf = 3;
                    break;
                case ("3DFrame"):
                    KSTRUC = 4;
                    ndf = 6;
                    break;
                case ("3DTrust"):
                    KSTRUC = 2;
                    ndf = 3;
                    break;
                case ("2DTrust"):
                    KSTRUC = 1;
                    ndf = 2;
                    break;
                default:
                    KSTRUC = 0;
                    break;

            }

            //          source.Text += title + "\n";
            #endregion

            #region Input of Nodes -------------------
            vector = new ArrayList();
            Linecomp(fullPathname, "nodes", vector, ref nn, ref unitl, ref unitf);

            scaleL = myfsMod.ToMetre(unitl);
            n = nn * ndf;
            x = new double[nn];
            y = new double[nn];
            array = vector.ToArray(typeof(string)) as string[];
            vlength = array.Length;
            items = vlength / nn;
            if (items < 4)
            {
                for (int i = 0; i < nn; i++)
                {

                    nodelist.Add(array[i * ndf]);
                    x[i] = Convert.ToDouble(array[i * ndf + 1]) * scaleL;
                    y[i] = Convert.ToDouble(array[i * ndf + 2]) * scaleL;
                    //         Parsenumber(array[i * ndf + 1], ref x[i]);
                    //Parsenumber(array[i * ndf + 2], ref y[i]);
                    //              source.Text += nodelist[i] + "\t" + x[i].ToString("F1") + "\t\t" + y[i].ToString("F1") + "\n";
                    //}
                }
            }
            else
            {
                for (int i = 0; i < nn; i++)
                {

                    nodelist.Add(array[i * ndf]);
                    x[i] = Convert.ToDouble(array[i * ndf + 1]) * scaleL;
                    y[i] = Convert.ToDouble(array[i * ndf + 2]) * scaleL;
                    z[i] = Convert.ToDouble(array[i * ndf + 3]) * scaleL;
                    //Parsenumber(array[i * ndf + 1], ref x[i]);
                    //Parsenumber(array[i * ndf + 2], ref y[i]);
                    //Parsenumber(array[i * ndf + 3], ref z[i]);
                    //              source.Text += nodelist[i] + "\t" + x[i].ToString("F1") + "\t\t" + y[i].ToString("F1") + "\n";
                }
            }

            #endregion

            #region Input of Material Table----------------------------------------------------------------------------
            vector = new ArrayList();
            Linecomp(fullPathname, "material", vector, ref nmat, ref unitl, ref unitf);
            double Esteel = 200000000000.0;//N/m2
            double poisson = 0.3;
            double denSteel = 76980.0;//N/m3
            double scalemod = myfsMod.ToNperM2(unitl);
            double scaleden = myfsMod.ToNperM3(unitf);
            scaleF = myfsMod.ToNewton(unitf);
            mattable = new double[nmat, ntabmat];
            array = vector.ToArray(typeof(string)) as string[];
            for (int i = 0; i < nmat; i++)
            {
                matlist.Add(array[i * ioffmat]);
                if (array[i * ioffmat+1] == "STEEL")
                {
                    mattable[i, 0] = Esteel;
                    mattable[i, 1] = Esteel / 2 * (1 + poisson);
                    mattable[i, 2] = denSteel;
                }
                else if (array[i * ioffmat+1] == "General")
                {
                    mattable[i, 0] = Convert.ToDouble(array[i * ioffmat + 0 + 2]) * scalemod;
                    mattable[i, 1] = Convert.ToDouble(array[i * ioffmat + 0 + 3]) * scalemod;
                    mattable[i, 2] = Convert.ToDouble(array[i * ioffmat + 0 + 4]) * scaleden;
                }
                else
                { for (int j = 0; j < ntabmat; j++) Parsenumber(array[i * ioffmat + j + 1], ref mattable[i, j]); }
                //                source.Text += matlist[i] + "\t" + mattable[i, 0].ToString("F1") + "\t" + mattable[i, 1].ToString("F1") + "\t" +
                //                  mattable[i, 2].ToString("F1") + "\n";
            }
            #endregion


            #region Input of Sections--Table--------------------------------------------------------------------
            ///<summary>
            ///sectable[i, 0]=AX;sectable[i, 1]=AY;sectable[i, 2]=IX;sectable[i, 3]=IY;sectable[i, 4]=IZ;
            ///sectable[i, 5]=w
            ///</summary>
            vector = new ArrayList();
            double[,] wftable = excelMod.sectable;
            string[] wfDesignation = excelMod.names;
            ioffsec = ntab + 2;
            Linecomp(fullPathname, "section", vector, ref nsec, ref unitl, ref unitf);
            sectable = new double[nsec, ntab];
            array = vector.ToArray(typeof(string)) as string[];
            //myunit.fromUnitL = unitl;
            scaleL = myfsMod.ToMetre(unitl);
            scaleF = myfsMod.ToNperM(unitf);
             for (int i = 0; i < nsec; i++)
            {
                seclist.Add(array[i * ioffsec]);
                if (array[ioffsec * i + 1] == "General")
                {
                    sectable[i, 0] = Convert.ToDouble(array[i * ioffsec + 0 + 2]) * scaleL * scaleL;
                    sectable[i, 1] = Convert.ToDouble(array[i * ioffsec + 1 + 2]) * scaleL * scaleL;
                    sectable[i, 2] = Convert.ToDouble(array[i * ioffsec + 2 + 2]) * Math.Pow(scaleL, 4);
                    sectable[i, 3] = Convert.ToDouble(array[i * ioffsec + 3 + 2]) * Math.Pow(scaleL, 4);
                    sectable[i, 4] = Convert.ToDouble(array[i * ioffsec + 4 + 2]) * Math.Pow(scaleL, 4);
                    sectable[i, 5] = Convert.ToDouble(array[i * ioffsec + 5 + 2]) * scaleF;
                    sectable[i, 6] = Convert.ToDouble(array[i * ioffsec + 6 + 2]) * Math.Pow(scaleL, 3);
                    sectable[i, 7] = Convert.ToDouble(array[i * ioffsec + 7 + 2]) * Math.Pow(scaleL, 3);
                    sectable[i, 8] = Convert.ToDouble(array[i * ioffsec + 8 + 2]) * scaleL;
                    sectable[i, 9] = Convert.ToDouble(array[i * ioffsec + 9 + 2]) * scaleL;
                    //                for (int j = 0; j < ntab; j++) Parsenumber(array[i * ioffsec + j + 2], ref sectable[i, j]);
                }
                else if (array[ioffsec * i + 1] == "Tube")
                {
                    double od = Convert.ToDouble(array[ioffsec * i + 2]) * scaleL;
                    double wt = Convert.ToDouble(array[ioffsec * i + 3]) * scaleL;
                    double id = od - 2 * wt;
                    double area = 0.25 * Math.PI * (od * od - id * id);
                    double iner = Math.PI * (od * od * od * od - id * id * id * id) / 64;
                    sectable[i, 0] = area;
                    sectable[i, 1] = area;
                    sectable[i, 2] = 2 * iner;
                    sectable[i, 3] = iner;
                    sectable[i, 4] = iner;
                    sectable[i, 5] = 0;
                    sectable[i, 6] = iner/od;
                    sectable[i, 7] = iner/od;
                    sectable[i, 8] = Math.Sqrt(iner/area);
                    sectable[i, 9] = Math.Sqrt(iner/area);

                }
                else if (array[ioffsec * i + 1] == "AISC")
                {
                    scaleL = myfsMod.ToMetre("inch");
                    scaleF = myfsMod.ToNperM("lbf/ft");
                    String searchstring=array[ioffsec * i + 2];
                    int nsec = Array.IndexOf(wfDesignation, searchstring);
                    sectable[i, 0] = wftable[nsec, 0] * scaleL*scaleL;
                    sectable[i, 1] = wftable[nsec, 1] * scaleL * scaleL;
                    sectable[i, 2] = wftable[nsec, 2] * scaleL * scaleL * scaleL * scaleL;
                    sectable[i, 3] = wftable[nsec, 3] * scaleL * scaleL * scaleL * scaleL;
                    sectable[i, 4] = wftable[nsec, 4] * scaleL * scaleL * scaleL * scaleL;
                    sectable[i, 5] = wftable[nsec, 5]*scaleF;
                    sectable[i, 6] = wftable[nsec, 6] * scaleL * scaleL * scaleL;
                    sectable[i, 7] = wftable[nsec, 7] * scaleL * scaleL * scaleL;
                    sectable[i, 8] = wftable[nsec, 8] * scaleL;
                    sectable[i, 9] = wftable[nsec, 9] * scaleL;
                }
                //                source.Text += seclist[i] + "\t" + sectable[i, 0].ToString("F1") + "\t" + sectable[i, 1].ToString("F1") + "\t" +
                //                  sectable[i, 2].ToString("F1") + "\t" + sectable[i, 3].ToString("F1") + "\t" + sectable[i, 4].ToString("F1") + "\t" +
                //                  sectable[i, 5].ToString("F1") + "\n";
                
            }
            #endregion ------


            #region Input of Elements-----------------------------------------------------------------------------
            ///<summary>
            ///<prop[n2 + 0]=AX;prop[n2 + 1]=AY;prop[n2 + 2]=IX;prop[n2 + 3]=IY;prop[n2 + 4]=;prop[n2 + 5]=w;></prop>
            ///<prop[n2 + 6]=0;prop[n2 + 7]=Emod;prop[n2 + 8]=Gmod;prop[n2 + 9]=den;prop[n2 + 10]=len;></prop>
            ///<prop[n2 + 11]=dx;prop[n2 + 12]=dy;prop[n2 + 13]=dz;prop[n2 + 14]=0></prop>
            ///</summary>
            vector = new ArrayList();
            Linecomp(fullPathname, "elements", vector, ref ne, ref unitl, ref unitf);
            array = vector.ToArray(typeof(string)) as string[];
            ncon = nne * ne;
            nfem = ne * ndf * nne;
            fem = new double[nfem];
            nproptot = nprop * ne;
            int L;

            prop = new double[nproptot];
            con = new int[ncon];
            for (int i = 0; i < ne; i++)
            {
                n1 = nne * i;
                n2 = nprop * i;
                elemlist.Add(array[i * ioffelem]);
                ic1 = nodelist.IndexOf(array[i * ioffelem + 1]);
                ic2 = nodelist.IndexOf(array[i * ioffelem + 2]);
                itab = seclist.IndexOf(array[i * ioffelem + 3]);
                imat = matlist.IndexOf(array[i * ioffelem + 4]);
                L = Math.Abs(ic2 - ic1);
                if (ms < L) ms = L;
                con[n1] = ic1;
                con[n1 + 1] = ic2;
                dx = x[ic2] - x[ic1];
                dy = y[ic2] - y[ic1];
                d = Math.Sqrt(dx * dx + dy * dy);
                for (int j = 0; j < ntab; j++) prop[n2 + j] = sectable[itab, j];
                prop[n2 + 2 + ntab - 1] = mattable[imat, 0];
                prop[n2 + 3 + ntab - 1] = mattable[imat, 1];
                prop[n2 + 4 + ntab - 1] = mattable[imat, 2];
                prop[n2 + 5 + ntab - 1] = d;
                prop[n2 + 6 + ntab - 1] = dx;
                prop[n2 + 7 + ntab - 1] = dy;
                //               source.Text += elemlist[i] + "\t\t" + nodelist[ic1] + "\t\t" + nodelist[ic2] + "\t\t" + seclist[itab] + "\t\t" + matlist[imat] + "\n";
                //        source.Text += prop[n2 + 5 + ntab - 1].ToString("F1") + "\t" + prop[n2 + 2 + ntab - 1].ToString("F1") + "\n";
            }
            ms = ndf * (ms + 1);
            //Femloc();// Compute Fixed End Moments
            //            source.Text += "Band of Stiffness MatrixMath " + ms.ToString() + "\n";
            #endregion

            #region Input of Loading Conditions---------------------------------------------------------------------------------
            vector = new ArrayList();
            Linecomp(fullPathname, "loading", vector, ref nln, ref unitl, ref unitf);
            array = vector.ToArray(typeof(string)) as string[];
            //myunit.fromUnitL = unitl;
            //myunit.fromUnitF = unitf;
            scaleL = myfsMod.ToMetre(unitl);
            scaleF = myfsMod.ToNewton(unitf);
            //if (unitl != " ")
            //{
            //    scaleL = myunit.ToMeters();
            //}
            //if (unitf != " ")
            //{
            //    scaleF = myunit.ToNewton();
            //}
            int klc = -1;
            double p1 = 0.0, p3 = 0.0;
            double p2 = 0.0;
            foreach (var name in array) if (name == "<case>") lc++;

            al = new double[n, lc];
            for (int k = 0; k <= vector.Count - 1; k++)
            {
                switch (array[k])
                {
                    case ("<case>"):
                        //                       source.Text += "Case " + array[k + 1] + "\n";
                        klc++;
                        break;
                    case ("<loadednodes>"):
                        k++;
                        while (array[k] != "</loadednodes>")
                        {
                            jn = nodelist.IndexOf(array[k]);
                            kdsp = ndf * jn;
                            p1 = Convert.ToDouble(array[k + 1]) * scaleF;
                            p2 = Convert.ToDouble(array[k + 2]) * scaleF;
                            p3 = Convert.ToDouble(array[k + 3]) * scaleF * scaleL;
                            //   Parsenumber(array[k + 1], ref p1);
                            // Parsenumber(array[k + 2], ref p2);
                            //   Parsenumber(array[k + 3], ref p3);
                            al[kdsp, klc] = p1;
                            al[kdsp + 1, klc] = p2;
                            al[kdsp + 2, klc] = p3;
                            //                           source.Text += array[k] + "\t" + array[k + 1] + "\t" + array[k + 2] + "\t" + array[k + 3] + "\n";
                            k = k + (ndf + 1);
                        }//End while
                        break;
                    case ("<loadedmembers>"):
                        k++;
                        while (array[k] != "</loadedmembers>")
                        {
                            //if (array[k] == "Deadload") Deadload(klc);
                            k++;
                        }//End while
                        break;
                    default:
                        //              source.Text +="Default case";
                        break;
                }
            }//End For
            #endregion

            #region Input of Boundary Conditions------------------------------------------------------------------------------

            int ioffbound = 2;
            int l1, l2;
            ic = new int[ndf];
            w = new double[ndf];
            vector = new ArrayList();
            Linecomp(fullPathname, "boundary", vector, ref nbn, ref unitl, ref unitf);// Reads all boundary data into vector
            nib = (ndf + 1) * nbn;
            ib = new int[nib];
            nodeboundname = new string[2 * nbn];
            reac = new double[n, lc];
            array = vector.ToArray(typeof(string)) as string[];
            for (int i = 0; i < nbn; i++)
            {
                l1 = (ndf + 1) * i;
                jn = nodelist.IndexOf(array[i * ioffbound]);
                l2 = ndf * jn;
                ib[l1] = jn;
                Supptype(array[i * ioffbound + 1], ndf, ref ic, ref w);
                for (int k = 1; k <= ndf; k++)
                {
                    n1 = l1 + k;
                    n2 = l2 + k - 1;
                    ib[n1] = ic[k - 1];
                    for (int j = 0; j < lc; j++) reac[n2, j] = w[k - 1];
                }
                nodeboundname[i * ioffbound] = array[i * ioffbound];
                nodeboundname[i * ioffbound + 1] = array[i * ioffbound + 1];
                //              source.Text += nodeboundname[i * ioffbound] + "\t\t" + nodeboundname[i * ioffbound + 1] + "\n";

            }
            #endregion
        }//End Method Input
#endregion


 #region Method to read a line and split it  into its components--------------------------------------------------
        private void Linecomp(string fullPathname, string param, ArrayList vector, ref int number, ref string Unitl, ref string unitF)
        {
            using (TextReader reader = new StreamReader(fullPathname))
            {

                string[] paramDelims = { " ", "\t", "=",Environment.NewLine };
                string stop = "";
                int count = 0,nline=0;
                string line = "";

                char endcharacter = '>';
                while ((line = reader.ReadLine()) != null)
                {
                    string[] values = line.Split(paramDelims, StringSplitOptions.RemoveEmptyEntries);
                    stop = values[0];

                    if (stop == "<" + param + ">" || stop == "<" + param)
                    {
                        int vlen = values.Length;
                        if (vlen > 1) { Unitl = values[2].Trim(endcharacter); }
                        if (vlen > 3) { unitF = values[4].Trim(endcharacter); }
                        while (stop != "</" + param + ">")
                        {
                            line = "";
                            line = reader.ReadLine();

                            values = line.Split(paramDelims, StringSplitOptions.RemoveEmptyEntries);
                            stop = values[0];
                            vlen = values.Length;
                            if (stop != "</" + param + ">")
                            {
                                count++;
                                foreach (var value in values)
                                    vector.Add(value);
                                switch (param)
                                {
                                    case "nodes":
                                        nline = 3;
                                        break;
                                    case "material":
                                        nline = 5;
                                        break;
                                    case "section":
                                        nline = ioffsec;
                                        break;
                                    default:
                                        nline = 0;
                                        break;
                                }
                                if (vlen < nline) { for (int i = vlen; i < nline; i++) { vector.Add("0.0 "); } }
                            }
                            else
                            {
                                number = count;
                                //                                source.Text += "Number of " + param + " " + count.ToString() + "\n";

                            }
 
                        //}
                        } // End While
                    } //EndIf
                    
                } //End While
            } // End Using Reader
            
        } //End Linecomp
        #endregion

 #region Method to parse a real or integer string to a number-----------------------------------------------
        private void Parsenumber(string value, ref double x)
        {

            if (Double.TryParse(value, out x))
            {
                return;
            }
            else
            {
                MessageBox.Show("Error in real number ", value, MessageBoxButtons.OK, MessageBoxIcon.Question);
                //                source.Text += "Error in real number " + value + "\n";
            }
        }
        private void Parsenumber(string value, ref int x)
        {

            if (Int32.TryParse(value, out x))
            {
                return;
            }
            else
            {
                MessageBox.Show("Error in integer number ", value, MessageBoxButtons.OK, MessageBoxIcon.Question);
                //    source.Text += value + "  Error in integer number" + "\n ";
            }

        }
        #endregion

 #region Method to retrieve the index of an element in an array---------------------------------------------------------
        private void Retrievepos(string[] names, ref int ipos, string param)
        {
            ipos = -1;
            int icount = -1;
            foreach (var name in names)
            {
                icount++;
                if (name == param) ipos = icount;
            }
        }
        #endregion

 #region Method Support Type -----------------------------------------------------------------------------------
        private void Supptype(string bname, int ndf, ref int[] ic, ref double[] w)
        {
            ic = new int[ndf];
            w = new double[ndf];
            switch (bname)
            {
                case ("ENCASTRE"):
                    break;
                case ("ENCASTRE-SX"):
                    ic[0] = 1;
                    break;
                case ("ENCASTRE-SY"):
                    ic[1] = 1;
                    break;
                case ("PINNED"):
                    ic[2] = 1;
                    break;
                case ("PINNED-SX"):
                    ic[0] = 1;
                    ic[2] = 1;
                    break;
                case ("PINNED-SY"):
                    ic[1] = 1;
                    ic[2] = 1;
                    break;
                default:
                    break;
            }
        }
        #endregion


 #region Method Femloc - Local Fixed End Moments for Dead Load--------------------------
        //public void Femloc()
        //{
        //    int L, L2, I = 0, I2, N1, N2;
        //    double AX, D, W, DX, DY, DZ, CX, CY, CZ, CXZ, WX = 0, WY = 0, WZ = 0, RX, RY, MomentZ, MomentY;

        //    double[,] rot1 = new double[nne * ndf, nne * ndf];
        //    double[] VL = new double[nne * ndf];
        //    for (int NEL = 1; NEL <= ne; NEL++)
        //    {
        //        L = nne * (NEL - 1);
        //        L2 = nprop * (NEL - 1);
        //        N1 = con[L];
        //        N2 = con[L + 1];
        //        AX = prop[L2];
        //        D = prop[L2 + 4 + ntab]; DX = prop[L2 + 5 + ntab]; DY = prop[L2 + 6 + ntab]; DZ = prop[L2 + 7 + ntab];
        //        W = prop[L2 + 5];
        //        CX = DX / D; CY = DY / D; CZ = DZ / D;
        //        CXZ = Math.Sqrt(CX * CX + CZ * CZ);
        //        if (W == 0) W = AX * prop[L2 + 3+ntab];
        //        if (W == 0) continue;
        //        switch (structure)
        //        {
        //            case ("2DFrame"):
        //                WX = CY * W;
        //                WY = CX * W;
        //                RX = WX * D / 2;
        //                RY = WY * D / 2.0;
        //                MomentZ = WY * D * D / 12.0;
        //                VL[0] = -RX; VL[1] = -RY; VL[2] = -MomentZ; VL[3] = -RX; VL[4] = -RY; VL[5] = MomentZ;
        //                break;
        //            case ("2DTruss"):
        //                WX = CY * W;
        //                WY = CX * W;
        //                RX = WX * D / 2.0;
        //                RY = WY * D / 2.0;
        //                VL[0] = -RX;
        //                VL[1] = 0;
        //                VL[2] = -RX;
        //                VL[3] = 0;
        //                break;
        //            case ("3DTruss"):
        //                if (CXZ > 0)
        //                {
        //                    WX = CZ * W;
        //                    WY = CY * CZ * W / CXZ;
        //                    WZ = CX * W / CXZ;
        //                }
        //                else if (CXZ == 0)
        //                {
        //                    WX = 0;
        //                    WY = 0;
        //                    WZ = W;
        //                }
        //                VL[0] = -WX * D * 0.5;
        //                VL[3] = -WX * D * 0.5;
        //                break;
        //            case ("3DFrame"):
        //                if (CXZ > 0)
        //                {
        //                    WX = -CZ * W; WY = CY * CZ * W / CXZ; WZ = -CX * W / CXZ;
        //                }
        //                else if (CXZ == 0)
        //                {
        //                    WX = 0; WY = 0; WZ = -W;
        //                }
        //                MomentZ = WX * D * D / 12.0;
        //                MomentY = WZ * D * D / 12.0;
        //                VL[0] = WX * D * 0.5;
        //                VL[1] = WY * D * 0.5;
        //                VL[2] = WZ * D * 0.5;
        //                VL[3] = 0;
        //                VL[4] = -MomentY;
        //                VL[5] = -MomentZ;
        //                VL[6] = WX * D * 0.5;
        //                VL[7] = WY * D * 0.5;
        //                VL[8] = WZ * D * 0.5;
        //                VL[9] = 0;
        //                VL[10] = MomentY;
        //                VL[11] = MomentZ;
        //                break;
        //            default:
        //                break;
        //        }
        //        //----------------------------------------
        //        //STORE MEMBER Fixed END FORCES IN ARRAY FEM
        //        //--------------------------------------

        //        for (I = 0; I <= nne * ndf - 1; I++)
        //        {
        //            I2 = nne * ndf * (NEL - 1) + I;
        //            fem[I2] = -VL[I];
        //        }
        //    }
        //}
        #endregion

 #region Method Deadload-------------------------
        //public void Deadload(int klc)
        //{
        //    int L, L2, I = 0, I2, N1, N2, K1, K2, J1, J2;
        //    double[,] rot = new double[nne * ndf, nne * ndf];
        //    double[] VL = new double[nne * ndf];
        //    double[] VG = new double[nne * ndf];
        //    double D, DX, DY, DZ, BETA;
        //    loadflag[klc] = 1;
        //    for (int NEL = 1; NEL <= ne; NEL++)
        //    {
        //        L = nne * (NEL - 1);
        //        L2 = nprop * (NEL - 1);
        //        N1 = con[L];
        //        N2 = con[L + 1];
        //        K1 = ndf * N1;
        //        K2 = ndf * N2;
        //        BETA = 0;
        //        //----------------------------------------
        //        //Retrieve MEMBER Fixed END FORCES IN ARRAY VL
        //        //--------------------------------------
        //        for (I = 0; I <= nne * ndf - 1; I++)
        //        {
        //            I2 = nne * ndf * (NEL - 1) + I;
        //            VL[I] = -fem[I2];
        //        }
        //        D = prop[L2 + 4 + ntab]; DX = prop[L2 + 5 + ntab]; DY = prop[L2 + 6 + ntab]; DZ = prop[L2 + 7 + ntab];

        //        double[] C = new double[] { DX / D, DY / D, DZ / D, BETA };
        //        //------------------------------------------
        //        //Rotate local to global 
        //        //------------------------------------------
        //        rot = myvb.Rotmat(ref rot, C, ndf, ref structure);
        //        VG = myfsMod.VxAasync(VL, myfsMod.AtransAsync(rot));
        //        //VG = Matrix.MulMatVec(Matrix.Transpose(rot), VL);
        //        for (I = 1; I <= ndf; I++)
        //        {
        //            J1 = K1 + I;
        //            J2 = K2 + I;
        //            al[J1 - 1, klc] = al[J1 - 1, klc] + VG[I - 1];
        //            al[J2 - 1, klc] = al[J2 - 1, klc] + VG[I + ndf - 1];
        //        }
        //    }
        //    //    }

        //}
        #endregion

    }
}
