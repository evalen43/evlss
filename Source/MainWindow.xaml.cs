using System;
using System.IO;
using System.Windows;
using System.Windows.Forms;
using System.Diagnostics;
using StructInput_Cs;
using VBLib;
using myfsMod = FsMathLib.A;
//using ExcelProvider;

namespace EVCI_WpfBasicScreen
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        private string filein;
        public string FileName;
        public string Progname
        {
            set { this.Pname.Content = value; }
        }
        public string Struct
        {
            set { this.Structure.Content = value; }
        }
        public string Stat
        {
            set { this.Status.Content = value; }
        }

        public MainWindow()
        {
            InitializeComponent();
        }

        private void OpenFile_Click(object sender, RoutedEventArgs e)
        {
            // Create an instance of the open file dialog box.
            OpenFileDialog dlg = new OpenFileDialog()
            {
                Filter = "Text Files (.txt)|*.txt|All Files (*.*)|*.*",
                FilterIndex = 1,
                Multiselect = true,
                ShowReadOnly = true,
                Title = "Open File"
            };
            dlg.ShowDialog();
            if (!string.IsNullOrEmpty(dlg.FileName))
            {
                filein = dlg.FileName;
 
                InputFile.Text = filein;
                int index = filein.IndexOf(".");
                string fileOut = filein.Substring(0, index) + "-out.txt";
                OutputFile.Text = fileOut;

           }
            else System.Windows.MessageBox.Show("Input File not Specified", "Analysis cannot start");
        }

        private void RunProgram_Click(object sender, RoutedEventArgs e)
        {

            #region Part1 - CS ---------------------------------------------------------

            int index = filein.IndexOf(".");
            string fileOut = filein.Substring(0, index) + "-out.txt";
            int n2 = fileOut.Length;
            StructInput mylibcs = new StructInput();
            mylibcs.Input(filein);//Call Input 
            int[] con, ib;
            int nproptot = mylibcs.nproptot;
            int nib = mylibcs.nib;
            con = new int[mylibcs.ncon];
            ib = new int[nib];
            string[] nodetable = mylibcs.nodelist.ToArray();
            string[] elemtable = mylibcs.elemlist.ToArray();
            string nodestring = "";
           
            foreach (string name in nodetable) nodestring += name + ",";

            Array.Copy(mylibcs.ib, ib, nib);
            for (int i = 0; i < mylibcs.ncon; i++) con[i] = mylibcs.con[i] + 1;

            for (int i = 0; i < mylibcs.nbn; i++) ib[(mylibcs.ndf + 1) * i] = mylibcs.ib[(mylibcs.ndf + 1) * i] + 1;
            #endregion

            #region Part2 - VB ---------------------------------------------------------
            try
            {

                int NDFEL, NFORC, KERR = 0, KITER = 1;
                NDFEL = mylibcs.ndf * mylibcs.nne;
                double[,] ELST = new double[NDFEL, NDFEL];

                double ToScaleunitL = myfsMod.ToMetre(mylibcs.unitL); 
                double ToScaleunitF = myfsMod.ToNewton(mylibcs.unitF);
                double[,] TK = new double[mylibcs.n, mylibcs.ms];
                string PNAME = "LSSolver";
                string PTITLE = "Linear Static Solver (LSS)";
                NFORC = mylibcs.ne * NDFEL * mylibcs.lc;
                double[] FORC = new double[NFORC];
                double[] FEM = new double[NFORC];
                this.Pname.Content = PNAME;
                this.Structure.Content = mylibcs.structure;
                Structural myvb = new Structural(mylibcs.unitL, ToScaleunitL, mylibcs.unitF, ToScaleunitF);

                this.Status.Content = "CALLING DLOAD";
                myvb.DLOAD(con, ref mylibcs.al, mylibcs.prop, mylibcs.nprop, mylibcs.lc, mylibcs.ntab, mylibcs.nne, mylibcs.ndf, mylibcs.ne, ref mylibcs.fem, NDFEL, ref mylibcs.structure);

                for (int i = 1; i <= mylibcs.ne; i++)
                {

                    this.Status.Content = "CALLING STIFF";
                    myvb.STIFF(i, ref mylibcs.prop, mylibcs.ndf, ref ELST, mylibcs.nprop, mylibcs.ntab, ref mylibcs.structure);
                    this.Status.Content = "CALLING ELASS";
                    myvb.ELASS1(i, ref con, ref ELST, ref TK, mylibcs.nne, mylibcs.ndf);

                }
                // myvb.MatPrint(TK);

                this.Status.Content = "CALLING BOUND";

                myvb.BOUND(TK, ref mylibcs.al, ref mylibcs.reac, ib, mylibcs.lc, mylibcs.ndf, mylibcs.nbn);

                this.Status.Content = "CALLING BGAUSS";

                myvb.BGAUSS(ref TK, ref mylibcs.al, ref KERR);

                this.Status.Content = "CALLING FORCE";

                myvb.Force(con, mylibcs.prop, ref FORC, ref mylibcs.reac, ref mylibcs.al, mylibcs.ndf, NDFEL,
                       mylibcs.ne, mylibcs.nne, mylibcs.ntab, mylibcs.nprop, mylibcs.fem, ref mylibcs.structure, ref mylibcs.loadflag);

                this.Status.Content = "CALLING OUTPUT";

                myvb.OUTPT(mylibcs.al, mylibcs.reac, FORC, mylibcs.lc, mylibcs.prop, mylibcs.nprop, mylibcs.ntab,ref mylibcs.title,
                ib, mylibcs.n, mylibcs.nn, mylibcs.ndf, ref fileOut, mylibcs.nbn, mylibcs.ne, NDFEL, mylibcs.nne, con,
                KITER, ref PNAME, ref PTITLE, mylibcs.KSTRUC, ref nodetable, ref elemtable);

                this.Status.Content = "Program Finished!";
                //               this.Close();
            }
            catch (Exception ex)

            {
                
                System.Windows.MessageBox.Show(ex.Message);
            }
            #endregion
        }

        private void ViewFile_Click(object sender, RoutedEventArgs e)
        {
            if (filein != null)
                Process.Start("notepad.exe", filein);
                    else System.Windows.MessageBox.Show("Input File not Specified","Analysis cannot start");
        }

        private void ViewResults_Click(object sender, RoutedEventArgs e)
        {
            if (filein != null)
            {
                int index = filein.IndexOf(".");
                string fileOut = filein.Substring(0, index) + "-out.txt";
                Process.Start("notepad.exe", fileOut);
            }
            else System.Windows.MessageBox.Show("No results file found","Open an Input File First");
        }

        private void Exit_Click(object sender, RoutedEventArgs e)
        {
            if (System.Windows.Forms.Application.MessageLoop)
            {
                // WinForms app
                System.Windows.Forms.Application.Exit();
            }
            else
            {
                // Console app
                System.Environment.Exit(1);
            }
        }
    }
}
