program MineSweeper3D;

uses
  Forms,
  Main in 'Main.pas' {frmMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Minesweeper 3D';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
