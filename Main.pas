unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, OpenGL12,
  GLScene, GLObjects, GLMisc, Geometry, ExtCtrls, GLTexture, GLCadencer, Contnrs,
  ComCtrls, ToolWin, GLSkydome, Functions, StdCtrls, GLGraph, GLFireFX,
  GLWin32Viewer, GLSpaceText, GLHUDObjects, GLBitmapFont, JPeg;

type
  TGLCustomSceneObjectClass = class of TGLCustomSceneObject;
  TColorArray = Array [1..26] of TColorVector;

  TMyDodecahedron = class (TGLSceneObject)
  public
    procedure BuildList(var rci : TRenderContextInfo); override;
  end;
  
  // Gets rid of typecasting all over the place
  TGLObjectList = class (TObjectList)
  private
    function GetGLItem(Index: Integer): TGLCustomSceneObject;
    procedure SetGLItem(Index: Integer; AObject: TGLCustomSceneObject);
  public
    property GLItems[Index: Integer]: TGLCustomSceneObject read GetGLItem write SetGLItem;
            default;
  end;
  
  THUDTextArea = class (TObject)
  private
    FBottomRight: TPoint;
    FHUDText: THUDText;
    FTopLeft: TPoint;
    function IsPointInside(X, Y: Integer): Boolean;
  public
    constructor Create(AHUDText: THUDText);
  end;
  
  TfrmMain = class (TForm)
    bmpCopyright: TBitmapFont;
    bmpFontBlue: TBitmapFont;
    bmpFontGold: TBitmapFont;
    cadExplosion: TGLCadencer;
    dcCam: TDummyCube;
    dcGame: TDummyCube;
    dcInGameOptions: TDummyCube;
    dcMain: TDummyCube;
    dcMainMenu: TDummyCube;
    dcOptions: TDummyCube;
    fxExplosion: TGLFireFXManager;
    GLCam: TGLCamera;
    GLLight: TGLLightSource;
    GLScene: TGLScene;
    GLSceneViewer: TGLSceneViewer;
    matLib: TGLMaterialLibrary;
    tmrClock: TTimer;
    tmrSpin: TTimer;
    txtBack: THUDText;
    txtCopyright: THUDText;
    txtDensity: THUDText;
    txtDensityLess: THUDText;
    txtDensityMore: THUDText;
    txtDensityValue: THUDText;
    txtDepthLess: THUDText;
    txtDepthMore: THUDText;
    txtDepthValue: THUDText;
    txtFieldDepth: THUDText;
    txtFieldHeight: THUDText;
    txtFieldWidth: THUDText;
    txtGiveUp: THUDText;
    txtHeightLess: THUDText;
    txtHeightMore: THUDText;
    txtHeightValue: THUDText;
    txtInfoOptions: THUDText;
    txtInGameOptions: THUDText;
    txtLoser: THUDText;
    txtMinefieldSize: THUDText;
    txtMinesweeper: THUDText;
    txtOK: THUDText;
    txtQuit: THUDText;
    txtRemaining: THUDText;
    txtResetView: THUDText;
    txtRestart: THUDText;
    txtShapes: THUDText;
    txtShapesLeft: THUDText;
    txtShapesRight: THUDText;
    txtShapeValue: THUDText;
    txtStart: THUDText;
    txtTime: THUDText;
    txtWidthLess: THUDText;
    txtWidthMore: THUDText;
    txtWidthValue: THUDText;
    txtWinner: THUDText;
    procedure cadExplosionProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
            MousePos: TPoint; var Handled: Boolean);
    procedure FormResize(Sender: TObject);
    procedure GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
            X, Y: Integer);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
            Y: Integer);
    procedure tmrClockTimer(Sender: TObject);
    procedure tmrSpinTimer(Sender: TObject);
  private
    FElapsedTime: Integer;
    FEndGameText: THUDText;
    FGameMineCount: Integer;
    FGameSizeX: Integer;
    FGameSizeY: Integer;
    FGameSizeZ: Integer;
    FGameStarted: Boolean;
    FHighlights: TColorArray;
    FLostGame: Boolean;
    FMineClass: TGLCustomSceneObjectClass;
    FMineCount: Integer;
    FMines: TGLObjectList;
    FMinesToFind: Integer;
    FMoving: Boolean;
    FNewGame: Boolean;
    FOptions: Boolean;
    FPicked: TGLCustomSceneObject;
    FRevealedCount: Integer;
    FShapeIndex: Integer;
    FShapes: TStringList;
    FSize: Integer;
    FSizeX: Integer;
    FSizeY: Integer;
    FSizeZ: Integer;
    FStartX: Integer;
    FStartY: Integer;
    FTempMineCount: Integer;
    FTempShapeIndex: Integer;
    FTempSizeX: Integer;
    FTempSizeY: Integer;
    FTempSizeZ: Integer;
    FTextAreas: TObjectList;
    FValues: TGLObjectList;
    FWonGame: Boolean;
    procedure CreateMines;
    procedure ExplodeMine(const AMine: TGLCustomSceneObject);
    function GetMine(const X, Y, Z: Integer): TGLCustomSceneObject;
    procedure GetMineCoordinates(const AObject: TGLCustomSceneObject; var X, Y, Z: Integer);
    function IsHighlighted(const AGLObject: TGLCustomSceneObject): Boolean;
    function IsRevealed(const AGLObject: TGLCustomSceneObject): Boolean;
    function MenuOptionClicked(X, Y: Integer): Boolean;
    function OnMenuOption(X, Y: Integer): THUDText;
    procedure OrientateValues;
    procedure PlaceMines(const AStartingObject: TGLCustomSceneObject);
    procedure ProcessObject(const AGLObject: TGLCustomSceneObject);
    procedure ProcessSurround(const AGLObject: TGLCustomSceneObject);
    procedure RefreshMenuRectangles;
    procedure ResetPositions;
    procedure ResizeMines(ASize: Integer);
    procedure Reveal(const AObject: TGLCustomSceneObject);
    procedure RevealMines;
    procedure SetVisiblility;
    procedure StartGame;
    function UpdateValue(AHUDText: THUDText; AValue, AMin, AMax, ADirection: Integer; 
            ACharCount: Integer = 2): Integer;
    property Mines[const X, Y, Z: Integer]: TGLCustomSceneObject read GetMine;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

//==================================================================================================
implementation

{$R *.DFM}

const
  FACTOR = 10;
  ALPHA_FACTOR = 0.4;
  END_MSG_SCALE = 1.9;

  MIN_DIMENSION = 3;
  MAX_DIMENSION = 11;

  MAT_NORMAL = 'matBlue';

//  RES_NORMAL = 'Textures\metal_shred.jpg';           // All
//  RES_NORMAL = 'Textures\Burnstone.jpg';             // All
  RES_NORMAL = 'Textures\cubicsbricks.jpg';          // Cube
//  RES_NORMAL = 'Textures\Wood_Misc_2.jpg';           // Teapot

  COPYRIGHT =
      ' Freeware ©2005 - written by Eric Salmon using delphi and GLScene (glscene.org)'#13 +
      'Bitmap fonts from http://www.algonet.se/~guld1/freefont.htm';

  MSG_WIN  = 'YEAH! YOU WON!'#13#13'NICE ONE!';
  MSG_LOSE = 'HA HA! LOSER!'#13#13'YOU''RE RUBBISH!';

//==================================================================================================
//==================================================================================================
procedure PopulateColorArray(var AColorArray: TColorArray; AColors: array of TColorVector);
var liIdx: Integer;
begin
  for liIdx := 0 to High(AColors) do
    AColorArray[liIdx + 1] := AColors[liIdx];
end;  // PopulateColorArray

{-==============================================================================
    TfrmMain
===============================================================================}
{-------------------------------------------------------------------------------
}
{-==============================================================================
    TfrmMain
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor TfrmMain.Create(AOwner: TComponent);

  procedure FillTextAreaList(AHUDs: Array of THUDText);
  var
    i: Integer;
  begin
    for i := 0 to High(AHUDs) do
      FTextAreas.Add(THUDTextArea.Create(AHUDs[i]));
  end;
  
begin
  inherited Create(AOwner);
  // Initialise Cubes list
  FMines := TGLObjectList.Create;
  FValues:= TGLObjectList.Create;
  
  PopulateColorArray(FHighlights, [clrBlue, clrGreen, clrRed, clrPurple, clrBakersChoc, clrTeal,
                                   clrCoral, clrLime, clrNavy, clrFuchsia, clrAqua, clrMaroon,
                                   clrScarlet, clrDarkGreen, clrOrchid, clrAquamarine, clrViolet,
                                   clrSkyBlue, clrSeaGreen, clrSienna, clrSlateBlue, clrOrange,
                                   clrTan, clrThistle, clrSteelBlue, clrTurquoise]);
  
  FShapes := TStringList.Create;
  FShapes.CommaText := '"CUBE  ","SPHERE","FUNNY ","BARREL","PIPE  ","CRAZY "';
  
  FSize  := 9;
  FSizeX := 5;
  FSizeY := 5;
  FSizeZ := 5;
  FMineCount := 1;
  FShapeIndex := 0;
  txtShapeValue.Text := FShapes[FShapeIndex];
  txtLoser.Text := MSG_LOSE;
  txtWinner.Text := MSG_WIN;

  FTextAreas := TObjectList.Create;
  FillTextAreaList([txtStart, txtInfoOptions, txtQuit,
                    txtRestart, txtResetView, txtInGameOptions, txtGiveUp,
                    txtWidthLess, txtWidthValue, txtWidthMore,
                    txtHeightLess, txtHeightValue, txtHeightMore,
                    txtDepthLess, txtDepthValue, txtDepthMore,
                    txtDensityLess, txtDensityValue, txtDensityMore,
                    txtShapesLeft, txtShapeValue, txtShapesRight, txtBack, txtOK]);
  FormResize(nil);
  SetVisiblility;
  txtCopyright.Text := COPYRIGHT;
  
  with TImage.Create(nil) do
    try
      Picture.LoadFromFile(RES_NORMAL);
      matLib.Materials[0].Material.Texture.Image.Assign(Picture);
      Picture.LoadFromFile(RES_NORMAL);
      matLib.Materials[1].Material.Texture.Image.Assign(Picture);
      Picture.LoadFromFile('Textures\metal_shred Red.jpg');
      matLib.Materials[2].Material.Texture.Image.Assign(Picture);
    finally
      Free;
    end;
end;  // TfrmMain.Create 

{-------------------------------------------------------------------------------
}
destructor TfrmMain.Destroy;
var
  i: Integer;
begin
  // Remove objects from scene
  RemoveObjects(dcMain, nil);

  // ObjectList is in charge of freeing its objects, by default
  FTextAreas.Free;
  FMines.Free;
  FValues.Free;
  inherited Destroy;
end;  // TfrmMain.Destroy 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.cadExplosionProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  if not fxExplosion.Disabled then begin
    // Fire explosion
    fxExplosion.IsotropicExplosion(8, 10, 2);
    //GLFireFX.RingExplosion(8, 10, 2, XVector, ZVector);
    // And stop it from happening again and again and again...
    fxExplosion.Disabled := True;
  end;
  // If no fire to animate, stop the cadencer.
  if fxExplosion.ParticleCount = 0 then begin
    fxExplosion.Reference.Effects.Clear;
    cadExplosion.Enabled := False;
    // Reveal the other mines once the explosion has finished
    if not tmrSpin.Enabled and (FLostGame or FWonGame) then RevealMines;
  end;
end;  // TfrmMain.cadExplosionProgress 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.CreateMines;
var
  x, y, z: Integer;
  
  //-------------------------------------------------------------------------------------------
  function MakeMine: TGLCustomSceneObject;
  begin
    // Create a mine
    Result := FMineClass.Create(nil);

    // Set additional properties
    with Result do begin
      Position.AsVector := PointMake(FACTOR * (x + 0.5 - FGameSizeX / 2),
                                     FACTOR * (y + 0.5 - FGameSizeY / 2),
                                     FACTOR * (z + 0.5 - FGameSizeZ / 2));
      with Material do begin
        MaterialLibrary := matLib;
        LibMaterialName := MAT_NORMAL;
        FrontProperties.Diffuse.Color := clrBronze2;  // Adds "something" to the transparency.
        BlendingMode := bmTransparency;
      end;
    end;
  end;  // MakeMine

begin
  case FShapeIndex of
    1: FMineClass := TSphere;
    2: FMineClass := TMyDodecahedron;
    3: FMineClass := TCylinder;
    4: FMineClass := TAnnulus;
    5: FMineClass := TTeapot;
  else
    FMineClass := TCube;
  end;
  
  // Set List capacity to stop having to grow it everytime it fills up.
  FMines.Capacity  := FGameSizeX * FGameSizeY * FGameSizeZ;

  for x := 0 to FGameSizeX - 1 do
    for y := 0 to FGameSizeY - 1 do
      for z := 0 to FGameSizeZ - 1 do begin
        FMines.Add(MakeMine);
      end;

  // Send the objects to the main dummy cube
  AssignObjects(dcMain, FMines);

  // Size all objects
  ResizeMines(FSize);
end;  // TfrmMain.CreateMines 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.ExplodeMine(const AMine: TGLCustomSceneObject);
begin
  // Explosion won't appear if object is invisible, so make it very small instead
  with AMine do begin
    Scale.SetVector(0.01, 0.01, 0.01);
    StructureChanged;
  end;
  TGLBFireFX.Create(AMine.Effects);
  TGLBFireFX(AMine.Effects[0]).Manager := fxExplosion;
  fxExplosion.Reference := AMine;
  fxExplosion.Disabled := False;
  fxExplosion.FireInit;
  // Get the explosion going, cadencer deals with it
  cadExplosion.Enabled := True;
end;  // TfrmMain.ExplodeMine

{-------------------------------------------------------------------------------
}
procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    if not dcMainMenu.Visible then begin
      if FNewGame then PlaceMines(nil);
      RevealMines;
    end else
      Close;
end;  // TfrmMain.FormKeyDown 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; 
        MousePos: TPoint; var Handled: Boolean);
var
  loPick: TGLCustomSceneObject;
  lDirection: Integer;
begin
  if WheelDelta < 0 then lDirection := -1
                    else lDirection := 1;
  if dcOptions.Visible then begin
    MousePos := Application.MainForm.ScreenToClient(MousePos);
    loPick := OnMenuOption(MousePos.X, MousePos.Y);
    if loPick = txtWidthValue then
      FTempSizeX := UpdateValue(txtWidthValue, FTempSizeX,
                                MIN_DIMENSION, MAX_DIMENSION, 1 * lDirection)
    else
    if loPick = txtHeightValue then
      FTempSizeY := UpdateValue(txtHeightValue, FTempSizeY,
                                MIN_DIMENSION, MAX_DIMENSION, 1 * lDirection)
    else
    if loPick = txtDepthValue then
      FTempSizeZ := UpdateValue(txtDepthValue, FTempSizeZ,
                                MIN_DIMENSION, MAX_DIMENSION, 1 * lDirection)
    else
    if loPick = txtDensityValue then
      FTempMineCount := UpdateValue(txtDensityValue, FTempMineCount, 1,
                                    FTempSizeX * FTempSizeY * FTempSizeZ * 9 div 10,
                                    lDirection, 3);
    if loPick = txtShapeValue then begin
      with FShapes do
        FTempShapeIndex := (FTempShapeIndex + Count + lDirection) mod Count;
      txtShapeValue.Text := FShapes[FTempShapeIndex];
      RefreshMenuRectangles;
    end;
  
    // In case too many for given dimensions, need to drop down to new max.
    FTempMineCount := UpdateValue(txtDensityValue, FTempMineCount, 1,
                                  FTempSizeX * FTempSizeY * FTempSizeZ * 9 div 10, 0, 3);
    Handled := True;
  end else
  if ssCtrl in Shift then begin
    ResizeMines(FSize - lDirection);
    Handled := True;
  end else
    // Zoom in and out, to a point
    with GLCam do begin
      if ((VectorLength(VectorSubtract(AbsolutePosition, TargetObject.AbsolutePosition)) > 10) and
          (WheelDelta < 0)) or
         ((VectorLength(VectorSubtract(AbsolutePosition, TargetObject.AbsolutePosition)) < 350) and
          (WheelDelta > 0)) then
      begin
        AdjustDistanceToTarget(Power(1.1, lDirection));
        GLLight.Position.AsVector := Position.AsVector;
      end;
    end;
end;  // TfrmMain.FormMouseWheel 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.FormResize(Sender: TObject);
var
  lCenter: TPoint;
begin
  lCenter.X := GLSceneViewer.Width div 2;
  lCenter.Y := GLSceneViewer.Height div 2;
  // Always showing
  txtTime.Position.X := GLSceneViewer.Width - 15;
  
  // Main menu, showing when no gane in progress and Options menu not showing.
  txtMinesweeper.Position.AsVector := VectorMake(lCenter.X, lCenter.Y - 300, 0);
  txtStart.Position.AsVector       := VectorMake(lCenter.X, lCenter.Y - 150, 0);
  txtInfoOptions.Position.AsVector := VectorMake(lCenter.X, lCenter.Y -   0, 0);
  txtQuit.Position.AsVector        := VectorMake(lCenter.X, lCenter.Y + 150, 0);
  txtCopyright.Position.AsVector   := VectorMake(lCenter.X, GLSceneViewer.Height - 30, 0);
  
  // Showing during game in progress.
  txtRestart.Position.X       := lCenter.X;
  txtResetView.Position.AsVector := VectorMake(lCenter.X, GLSceneViewer.Height - 15, 0);
  txtInGameOptions.Position.Y := GLSceneViewer.Height - 15;
  txtGiveUp.Position.AsVector := VectorMake(GLSceneViewer.Width - 15, GLSceneViewer.Height - 15, 0);
  
  // Options menu, showing whenever requested, game in progress or not.
  txtMinefieldSize.Position.AsVector := VectorMake(lCenter.X, lCenter.Y - 300, 0);
  
  txtFieldWidth.Position.AsVector    := VectorMake(lCenter.X +  10, lCenter.Y - 160, 0);
  txtWidthLess.Position.AsVector     := VectorMake(lCenter.X +  30, lCenter.Y - 160, 0);
  txtWidthValue.Position.AsVector    := VectorMake(lCenter.X + 110, lCenter.Y - 160, 0);
  txtWidthMore.Position.AsVector     := VectorMake(lCenter.X + 160, lCenter.Y - 160, 0);
  
  txtFieldHeight.Position.AsVector   := VectorMake(lCenter.X +  10, lCenter.Y - 110, 0);
  txtHeightLess.Position.AsVector    := VectorMake(lCenter.X +  30, lCenter.Y - 110, 0);
  txtHeightValue.Position.AsVector   := VectorMake(lCenter.X + 110, lCenter.Y - 110, 0);
  txtHeightMore.Position.AsVector    := VectorMake(lCenter.X + 160, lCenter.Y - 110, 0);
  
  txtFieldDepth.Position.AsVector    := VectorMake(lCenter.X +  10, lCenter.Y - 60, 0);
  txtDepthLess.Position.AsVector     := VectorMake(lCenter.X +  30, lCenter.Y - 60, 0);
  txtDepthValue.Position.AsVector    := VectorMake(lCenter.X + 110, lCenter.Y - 60, 0);
  txtDepthMore.Position.AsVector     := VectorMake(lCenter.X + 160, lCenter.Y - 60, 0);
  
  txtDensity.Position.AsVector       := VectorMake(lCenter.X +  10, lCenter.Y - 10, 0);
  txtDensityLess.Position.AsVector   := VectorMake(lCenter.X +  30, lCenter.Y - 10, 0);
  txtDensityValue.Position.AsVector  := VectorMake(lCenter.X + 110, lCenter.Y - 10, 0);
  txtDensityMore.Position.AsVector   := VectorMake(lCenter.X + 160, lCenter.Y - 10, 0);
  
  txtShapes.Position.AsVector        := VectorMake(lCenter.X +  10, lCenter.Y + 40, 0);
  txtShapesLeft.Position.AsVector    := VectorMake(lCenter.X +  30, lCenter.Y + 40, 0);
  txtShapeValue.Position.AsVector    := VectorMake(lCenter.X +  63, lCenter.Y + 40, 0);
  txtShapesRight.Position.AsVector   := VectorMake(lCenter.X + 256, lCenter.Y + 40, 0);
  
  txtOK.Position.AsVector            := VectorMake(lCenter.X - 100, lCenter.Y + 140, 0);
  txtBack.Position.AsVector          := VectorMake(lCenter.X +  25, lCenter.Y + 140, 0);
  
  txtLoser.Position.AsVector         := VectorMake(lCenter.X, lCenter.Y, 0);
  txtWinner.Position.AsVector        := VectorMake(lCenter.X, lCenter.Y, 0);
  
  RefreshMenuRectangles;
  OnMenuOption(-1, -1);
end;  // TfrmMain.FormResize 

{-------------------------------------------------------------------------------
}
function TfrmMain.GetMine(const X, Y, Z: Integer): TGLCustomSceneObject;
var
  liIndex: Integer;
begin
  if (X in [0..FGameSizeX - 1])
  and (Y in [0..FGameSizeY - 1])
  and (Z in [0..FGameSizeZ - 1]) then
  begin
    liIndex := X * FGameSizeY * FGameSizeZ + Y * FGameSizeZ + Z;
    Result := FMines[liIndex];
  end else
    Result := nil;
end;  // TfrmMain.GetMine 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.GetMineCoordinates(const AObject: TGLCustomSceneObject; var X, Y, Z: 
        Integer);
begin
  X := 0;
  Y := 0;
  Z := FMines.IndexOf(AObject);
  if Z <> -1 then begin
    while Z >= (FGameSizeY * FGameSizeZ) do begin
      Inc(X);
      Dec(Z, (FGameSizeY * FGameSizeZ));
    end;
    while Z >= FGameSizeZ do begin
      Inc(Y);
      Dec(Z, FGameSizeZ);
    end;
  end;
end;  // TfrmMain.GetMineCoordinates 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: 
        TShiftState; X, Y: Integer);
begin
  FStartX := X;
  FStartY := Y;
  FMoving := False;
  
  MenuOptionClicked(X, Y);
end;  // TfrmMain.GLSceneViewerMouseDown 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  lAngle: Single;
begin
  if OnMenuOption(X, Y) <> nil then
    GLSceneViewer.Cursor := crHandPoint
  else begin
    // No text, remove highlight, from all.
    GLSceneViewer.Cursor := crDefault;
    // Move camera
    if ssRight in Shift then begin
      lAngle := 5;
      FMoving := True;
      // Spin left/right
      if X < FStartX then begin
        GLCam.MoveAroundTarget(0, lAngle);
      end else
      if X > FStartX then begin
        GLCam.MoveAroundTarget(0, -lAngle);
      end;
      // Tilt up/down
      if Y < FStartY then begin
        GLCam.MoveAroundTarget(lAngle, 0);
      end else
      if Y > FStartY then begin
        GLCam.MoveAroundTarget(-lAngle, 0);
      end;
      FStartX := X;
      FStartY := Y;
      GLLight.Position.AsVector := GLCam.Position.AsVector;
      // Get the digits to face the camera.
      OrientateValues;
    end else begin
      if FGameStarted and not (FWonGame or FLostGame) then begin
        FPicked := (GLSceneViewer.Buffer.GetPickedObject(X, Y) as TGLCustomSceneObject);
        if Assigned(FPicked) then GLSceneViewer.Cursor := crHandPoint
                             else GLSceneViewer.Cursor := crDefault;
      end;
    end;
  end;
end;  // TfrmMain.GLSceneViewerMouseMove 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.GLSceneViewerMouseUp(Sender: TObject; Button: TMouseButton; Shift: 
        TShiftState; X, Y: Integer);
begin
  if not Assigned(FPicked) and not FMoving then
    FPicked := (GLSceneViewer.Buffer.GetPickedObject(X, Y) as TGLCustomSceneObject);
  
  if FGameStarted and not FOptions then begin
    if Assigned(FPicked) then
      if FPicked is THUDText then Exit;
  
    // Process surround
    if not FMoving and
       (((Button = mbLeft) and (ssRight in Shift)) or
        ((Button = mbRight) and (ssLeft in Shift))) then
    begin
      if Assigned(FPicked) and IsRevealed(FPicked) then
        ProcessSurround(FPicked);
      txtRemaining.Text := Format('%.3d', [FMinesToFind]);
    end else
    // Center view on clicked object.
    if Button = mbMiddle then begin
      if Assigned(FPicked) then
        if not (FPicked is THUDText) then
          dcCam.Position := FPicked.Position;
      GLCam.TransformationChanged;
      GLLight.Position.AsVector := GLCam.Position.AsVector;
      // Get the digits to face the camera.
      OrientateValues;
    end else
    // Highlight
    if (Button = mbRight) and not FMoving then
    begin
      if Assigned(FPicked) then begin
        if not IsRevealed(FPicked) then
          if IsHighlighted(FPicked) then begin
            FPicked.Material.LibMaterialName := 'matBlue';
            Inc(FMinesToFind);
          end else begin
            FPicked.Material.LibMaterialName := 'matYellow';
            Dec(FMinesToFind);
          end
        else
          ProcessSurround(FPicked);
      end;
      txtRemaining.Text := Format('%.3d', [FMinesToFind]);
    end else
    // Stop moving
    if FMoving then
      FMoving := False
    else
    // Process clicked
    if not (FLostGame or FWonGame) then begin
      // Is object revealed? If not, carry on
      if Assigned(FPicked) and not IsRevealed(FPicked) and not IsHighlighted(FPicked) then
        ProcessObject(FPicked);
    end;
  end;
  FPicked := nil;
end;  // TfrmMain.GLSceneViewerMouseUp 

{-------------------------------------------------------------------------------
}
function TfrmMain.IsHighlighted(const AGLObject: TGLCustomSceneObject): Boolean;
begin
  Result := (AGLObject.Material.LibMaterialName = 'matYellow');
  //  Result := VectorEquals(clrBronze2, AGLObject.Material.FrontProperties.Emission.Color);
end;  // TfrmMain.IsHighlighted 

{-------------------------------------------------------------------------------
}
function TfrmMain.IsRevealed(const AGLObject: TGLCustomSceneObject): Boolean;
begin
  Result := AGLObject.Material.FrontProperties.Diffuse.Alpha < 1;
end;  // TfrmMain.IsRevealed 

{-------------------------------------------------------------------------------
}
function TfrmMain.MenuOptionClicked(X, Y: Integer): Boolean;
var
  loPick: TGLCustomSceneObject;
begin
  loPick := OnMenuOption(X, Y);
  Result := loPick <> nil;
  if not Result then Exit;
  
  if (loPick = txtStart) or (loPick = txtRestart) then begin
    StartGame;
    FMoving := True;
  end else
  if (loPick = txtInfoOptions) or (loPick = txtInGameOptions) then begin
    FOptions := True;
    FTempSizeX := FSizeX;
    FTempSizeY := FSizeY;
    FTempSizeZ := FSizeZ;
    FTempMineCount := FMineCount;
    FTempShapeIndex := FShapeIndex;
    SetVisiblility;
  end else
  if loPick = txtQuit then
    Close
  else
  if (loPick = txtResetView) then begin
    ResetPositions;
    FMoving := True;
  end else
  if (loPick = txtGiveUp) then begin
    if FNewGame then PlaceMines(nil);
    RevealMines;
  end else
  // Options menu
  if loPick = txtWidthLess then
    FtempSizeX := UpdateValue(txtWidthValue, FTempSizeX, MIN_DIMENSION, MAX_DIMENSION, -1)
  else
  if loPick = txtWidthMore then
    FtempSizeX := UpdateValue(txtWidthValue, FTempSizeX, MIN_DIMENSION, MAX_DIMENSION, 1)
  else
  if loPick = txtHeightLess then
    FtempSizeY := UpdateValue(txtHeightValue, FTempSizeY, MIN_DIMENSION, MAX_DIMENSION, -1)
  else
  if loPick = txtHeightMore then
    FtempSizeY := UpdateValue(txtHeightValue, FTempSizeY, MIN_DIMENSION, MAX_DIMENSION, 1)
  else
  if loPick = txtDepthLess then
    FtempSizeZ := UpdateValue(txtDepthValue, FTempSizeZ, MIN_DIMENSION, MAX_DIMENSION, -1)
  else
  if loPick = txtDepthMore then
    FtempSizeZ := UpdateValue(txtDepthValue, FTempSizeZ, MIN_DIMENSION, MAX_DIMENSION, 1)
  else
  if loPick = txtDensityLess then
    FTempMineCount := UpdateValue(txtDensityValue, FTempMineCount, 1,
                                  FTempSizeX * FTempSizeY * FTempSizeZ * 9 div 10, -1, 3)
  else
  if loPick = txtDensityMore then
    FTempMineCount := UpdateValue(txtDensityValue, FTempMineCount, 1,
                                  FTempSizeX * FTempSizeY * FTempSizeZ * 9 div 10, 1, 3)
  else
  if loPick = txtShapesLeft then begin
    with FShapes do
      FTempShapeIndex := (FTempShapeIndex + Count - 1) mod Count;
    txtShapeValue.Text := FShapes[FTempShapeIndex];
    RefreshMenuRectangles;
  end else
  if loPick = txtShapesRight then begin
    with FShapes do
      FTempShapeIndex := (FTempShapeIndex + Count + 1) mod Count;
    txtShapeValue.Text := FShapes[FTempShapeIndex];
    RefreshMenuRectangles;
  end else
  if loPick = txtBack then begin
    FOptions := False;
    txtShapeValue.Text := FShapes[FShapeIndex];
    SetVisiblility;
    FMoving := True;
  end else
  if loPick = txtOK then begin
    FOptions := False;
    FSizeX := FTempSizeX;
    FSizeY := FTempSizeY;
    FSizeZ := FTempSizeZ;
    FMineCount := FTempMineCount;
    FShapeIndex := FTempShapeIndex;
    SetVisiblility;
    FMoving := True;
  end;
  // In case too many for given dimensions, need to drop down to new max.
  if dcOptions.Visible then
    FTempMineCount := UpdateValue(txtDensityValue, FTempMineCount, 1,
                                  FTempSizeX * FTempSizeY * FTempSizeZ * 9 div 10, 0, 3);
end;  // TfrmMain.MenuOptionClicked 

{-------------------------------------------------------------------------------
}
function TfrmMain.OnMenuOption(X, Y: Integer): THUDText;
var
  i: Integer;
  lTextArea: THUDTextArea;
  lCube: TGLBaseSceneObject;
begin
  Result := nil;
  
  for i := 0 to FTextAreas.Count - 1 do begin
    lTextArea := THUDTextArea(FTextAreas[i]);
    // Got some nested objects, so need to get back up to containing "cube"
    lCube := lTextArea.FHUDText.Parent;
    while not (lCube is TDummyCube) do lCube := lCube.Parent;
    if lCube.Visible then
      if lTextArea.IsPointInside(X, Y) then begin
        lTextArea.FHUDText.BitmapFont := bmpFontGold;
        Result := lTextArea.FHUDText;
        Break;
      end else
      if lTextArea.FHUDText.Tag = 0 then
        lTextArea.FHUDText.BitmapFont := bmpFontBlue;
  end;
end;  // TfrmMain.OnMenuOption 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.OrientateValues;
var
  i: Integer;
begin
  for i := 0 to FValues.Count - 1 do
    with FValues[i] do begin
      // Face the camera
      Direction.AsVector := GLCam.Position.AsVector;
      // But stay same way up
      Up.AsVector := YHmgVector;
    end;
end;  // TfrmMain.OrientateValues 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.PlaceMines(const AStartingObject: TGLCustomSceneObject);
var
  liMines: Integer;
  loMine: TGLCustomSceneObject;
  ltfFound: Boolean;
  lx, ly, lz: Integer;
  li, lj, lk: Integer;
  lCursor: TCursor;
  
  //-------------------------------------------------------
  procedure IncrementTag(const AMine: TGLCustomSceneObject);
  begin
     if (AMine <> nil) and (AMine.Tag <> -1) then
       AMine.Tag := AMine.Tag + 1;
  end;
  //-------------------------------------------------------

begin
  lCursor := Screen.Cursor;
  Screen.Cursor := crAppStart;
  try
    loMine := nil;
    lx := 0;
    ly := 0;
    lz := 0;
    for liMines := 0 to FGameMineCount - 1 do begin
      ltfFound := False;
      while not ltfFound do begin
        lx := Trunc(Random * FGameSizeX);
        ly := Trunc(Random * FGameSizeY);
        lz := Trunc(Random * FGameSizeZ);

        loMine := Mines[lx, ly, lz];

        // Always allow first pick to be OK
        ltfFound := (loMine.Tag <> -1) and (loMine <> AStartingObject);
      end;
      loMine.Tag := -1;
      // Set values around the mine. Don't change existing ones
      for li := -1 to 1 do
        for lj := -1 to 1 do
          for lk := -1 to 1 do
            IncrementTag(Mines[lx + li, ly + lj, lz + lk]);
    end;
  finally
    Screen.Cursor := lCursor;
  end;
end;  // TfrmMain.PlaceMines 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.ProcessObject(const AGLObject: TGLCustomSceneObject);
var
  i: Integer;
  lCursor: TCursor;
begin
  lCursor := Screen.Cursor;
  Screen.Cursor := crAppStart;
  try
    // find what's under the mouse
    i := FMines.IndexOf(AGLObject);
    if i <> -1 then begin
      // If new game, allow safe first pick
      if FNewGame then begin
        FNewGame := False;
        PlaceMines(AGLObject);
      end;

      if AGLObject.Tag = -1 then begin
        FLostGame := True;
        FEndGameText := txtLoser;
        ExplodeMine(AGLObject);
      end else begin
        Reveal(AGLObject);
        if FRevealedCount >= ((FGameSizeX * FGameSizeY * FGameSizeZ) - FGameMineCount) then
        begin
          FWonGame := True;
          FEndGameText := txtWinner;
          RevealMines;
        end;
      end;
    end;
  finally
    Screen.Cursor := lCursor;
  end;
end;  // TfrmMain.ProcessObject 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.ProcessSurround(const AGLObject: TGLCustomSceneObject);
var
  i, liTotalAround: Integer;
  lx, ly, lz: Integer;
  li, lj, lk: Integer;
  loMine: TGLCustomSceneObject;
begin
  i := FMines.IndexOf(AGLObject);
  if i <> -1 then begin
    GetMineCoordinates(AGLObject, lx, ly, lz);
    if lz <> -1 then begin
      // Count the number of surrounding mines
      liTotalAround := 0;
      for li := lx - 1 to lx + 1 do
        for lj := ly - 1 to ly + 1 do
          for lk := lz - 1 to lz + 1 do begin
            loMine := Mines[li, lj, lk];
            // If it's a marked mine, count it
            if Assigned(loMine) and (dcMain.IndexOfChild(loMine) <> -1)
            and IsHighlighted(loMine) then
              Inc(liTotalAround);
          end;
      // If number of marked mines is same as value of selected object, try and uncover all around
      if liTotalAround = AGLObject.Tag then begin
        for li := lx - 1 to lx + 1 do
          for lj := ly - 1 to ly + 1 do
            for lk := lz - 1 to lz + 1 do begin
              loMine := Mines[li, lj, lk];
              // If it's a marked mine, don't touch it, but try the others
              if Assigned(loMine) and (dcMain.IndexOfChild(loMine) <> -1)
              and not IsHighlighted(loMine) then
                if (loMine.Tag <> -1) then
                  Reveal(loMine)
                else begin
                  ProcessObject(loMine);
                  Exit;
                end;
            end;
      end;
    end;
    ProcessObject(AGLObject);
  end;
end;  // TfrmMain.ProcessSurround 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.RefreshMenuRectangles;
var
  i: Integer;
  lWidth, lHeight: Extended;
begin
  for i := 0 to FTextAreas.Count - 1 do
    with THUDTextArea(FTextAreas[i]) do
      with FHUDText do begin
        lWidth  := Length(Text) * Scale.X * (BitmapFont.CharWidth + BitmapFont.GlyphsIntervalX);
        lHeight := Scale.Y * (BitmapFont.CharHeight + BitmapFont.GlyphsIntervalY);
        case Alignment of
          taLeftJustify:
            begin
              FTopLeft.X     := Round(Position.X);
              FBottomRight.X := Round(Position.X + lWidth);
            end;
          taCenter:
            begin
              FTopLeft.X     := Round(Position.X - (lWidth / 2));
              FBottomRight.X := Round(Position.X + (lWidth / 2));
            end;
          taRightJustify:
            begin
              FTopLeft.X     := Round(Position.X - lWidth);
              FBottomRight.X := Round(Position.X);
            end;
        end;
        case Layout of
          tlTop:
            begin
              FTopLeft.Y     := Round(Position.Y);
              FBottomRight.Y := Round(Position.Y + lHeight);
            end;
          tlCenter:
            begin
              FTopLeft.Y     := Round(Position.Y - (lHeight / 2));
              FBottomRight.Y := Round(Position.Y + (lHeight / 2));
            end;
          tlBottom:
            begin
              FTopLeft.Y     := Round(Position.Y - lHeight);
              FBottomRight.Y := Round(Position.Y);
            end;
        end;
      end;
end;  // TfrmMain.RefreshMenuRectangles 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.ResetPositions;
begin
  with dcMain do begin
    Position.AsVector := PointMake(0, 0, 0);
    PitchAngle := 0;
    RollAngle  := 0;
    TurnAngle  := 0;
  end;

  with dcCam do begin
    Position.AsVector := PointMake(0, 0, 0);
    PitchAngle := 0;
    RollAngle  := 0;
    TurnAngle  := 0;
  end;

  GLCam.Position.AsVector := PointMake(0, 50, 70);
  GLLight.Position.AsVector := GLCam.Position.AsVector;
  // Face the camera.
  OrientateValues;
end;  // TfrmMain.ResetPositions

{-------------------------------------------------------------------------------
}
procedure TfrmMain.ResizeMines(ASize: Integer);
var
  i: Integer;
begin
  if ASize in [3..9] then begin
    FSize := ASize;
    GLScene.BeginUpdate;
    try
      for i := 0 to FMines.Count - 1 do
        with FMines[i] do begin
          Scale.SetVector(FSize, FSize, FSize);
          StructureChanged;
        end;
      for i := 0 to FValues.Count - 1 do
        with FValues[i] do begin
          Scale.SetVector(FSize / 2, FSize / 2, FSize / 2);
          StructureChanged;
        end;
    finally
      GLScene.EndUpdate;
    end;
  end;
end;  // TfrmMain.ResizeMines

{-------------------------------------------------------------------------------
}
procedure TfrmMain.Reveal(const AObject: TGLCustomSceneObject);
var
  lx, ly, lz: Integer;
  li, lj, lk: Integer;
  loMine: TGLCustomSceneObject;
  lValue: TSpaceText;
begin
  // Do not process objects tagged as mines
  if not IsRevealed(AObject) then
    // If empty space, remove and process surrounding objects
    if AObject.Tag = 0 then begin
      Inc(FRevealedCount);
      dcMain.Remove(AObject, False);  // Remove blank
      GetMineCoordinates(AObject, lx, ly, lz);
      if lz <> -1 then begin
        for li := lx - 1 to lx + 1 do
          for lj := ly - 1 to ly + 1 do
            for lk := lz - 1 to lz + 1 do begin
              loMine := Mines[li, lj, lk];
              if Assigned(loMine) and (dcMain.IndexOfChild(loMine) <> -1) then
                Reveal(loMine);
            end;
      end;
    end else
    // Check the object isn't transparent, if it is, it has already been "revealed", so skip
    if not IsHighlighted(AObject) then begin
      Inc(FRevealedCount);
      // Create TSpaceText to show number of surrounding mines.
      lValue := TSpaceText.Create(nil);
      with lValue do begin
        Position.AsVector := AObject.Position.AsVector;  // Same position as revealed "mine".
        Translate(0, -1, 0);        // Need to bring it down a little.
        Extrusion := 0.2;           // Give it some thickness
        Scale.Scale(FSize / 2);     // Scale it to fit within the "mine".

        Text := IntToStr(AObject.Tag);
        if AObject.Tag > 9 then
          Translate(-FSize/8, 0, 0);   // Try to center the number within the "mine"

        Material.FrontProperties.Diffuse.Color := clrBronze2;   // Dull colour
        Material.FrontProperties.Emission.Color := clrBronze2;  // Make it glow
        Adjust.Horz := haCenter;

        // Make it face the camera (see OrientateValues).
        Direction.AsVector := GLCam.Position.AsVector;
        Up.AsVector := YHmgVector;
      end;
      // Add to list for easier freeing later.
      FValues.Add(lValue);

      // Now remove texture and make it transparent.
      AObject.Material.MaterialLibrary := nil;
      AObject.Material.FrontProperties.Emission.Color := FHighlights[AObject.Tag];
      AObject.Material.FrontProperties.Diffuse.Alpha := ALPHA_FACTOR;
      dcMain.AddChild(lValue);
    end;
end;  // TfrmMain.Reveal

{-------------------------------------------------------------------------------
}
procedure TfrmMain.RevealMines;
var
  i: Integer;
begin
  // Show where the other mines were.
  ResizeMines(FSize);
  for i := 0 to FMines.Count - 1 do
    with FMines[i] do
      if Tag = -1 then
        Material.LibMaterialName := 'matYellow'
      else
      if isHighlighted(FMines[i]) then
        Material.LibMaterialName := 'matRed';

  tmrClock.Enabled := False;
  GLSceneViewer.Cursor := crDefault;
  // Game finished, won, lost or given up.
  FGameStarted := False;
  if not tmrSpin.Enabled and (FLostGame or FWonGame) and Assigned(FEndGameText)then begin
    FEndGameText.Scale.X := 0;
    FEndGameText.Scale.Y := 0;
    FEndGameText.Visible := True;
  end else
    SetVisiblility;

  tmrSpin.Enabled := True;
  ResetPositions;
end;  // TfrmMain.RevealMines

{-------------------------------------------------------------------------------
}
procedure TfrmMain.SetVisiblility;
begin
  dcGame.Visible := True;
  dcMain.Visible := not FOptions;
  dcInGameOptions.Visible := FGameStarted and not FOptions;
  dcMainMenu.Visible := not FGameStarted and not FOptions;
  dcOptions.Visible := FOptions;
  tmrClock.Enabled  := dcInGameOptions.Visible;
  if dcMainMenu.Visible then
    txtCopyright.Parent := dcMainMenu
  else
  if dcOptions.Visible then
    txtCopyright.Parent := dcOptions;
end;  // TfrmMain.SetVisiblility

{-------------------------------------------------------------------------------
}
procedure TfrmMain.StartGame;
begin
  // Get everything in place.
  ResetPositions;

  tmrSpin.Enabled       := False;
  FEndGameText          := nil;
  FPicked               := nil;
  txtLoser.Visible      := False;
  txtWinner.Visible     := False;
  fxExplosion.Reference := nil;
  cadExplosion.Enabled  := False;
  
  Randomize;
  Screen.Cursor := crHourglass;
  RemoveObjects(dcMain, nil);
  FMines.Clear;
  FValues.Clear;

  FRevealedCount := 0;
  FGameMineCount := FMineCount;
  FGameSizeX := FSizeX;
  FGameSizeY := FSizeY;
  FGameSizeZ := FSizeZ;
  FMinesToFind := FMineCount;
  txtRemaining.Text := Format('%.3d', [FMinesToFind]);
  CreateMines;

  FNewGame     := True;
  FGameStarted := True;
  FWonGame     := False;
  FLostGame    := False;

  FElapsedTime := 0;
  tmrClock.Enabled := True;
  
  Screen.Cursor := crDefault;
  
  SetVisiblility;
  OnMenuOption(-1, -1);
  OrientateValues;
end;  // TfrmMain.StartGame 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.tmrClockTimer(Sender: TObject);
begin
  Inc(FElapsedTime);
  txtTime.Text := Format('%.3d', [FElapsedTime]);
end;  // TfrmMain.tmrClockTimer 

{-------------------------------------------------------------------------------
}
procedure TfrmMain.tmrSpinTimer(Sender: TObject);
begin
  GLCam.MoveAroundTarget(0, 0.5);
  GLLight.Position.AsVector := GLCam.Position.AsVector;
  // Face the camera.
  OrientateValues;
  
  if dcInGameOptions.Visible and Assigned(FEndGameText) then
    if FEndGameText.Visible then begin
      if FEndGameText.Scale.Y < END_MSG_SCALE then
        FEndGameText.Scale.Y := FEndGameText.Scale.Y + 0.02;
      if FEndGameText.Scale.X < END_MSG_SCALE then
        FEndGameText.Scale.X := FEndGameText.Scale.X + 0.02;
    end;
end;  // TfrmMain.tmrSpinTimer 

{-------------------------------------------------------------------------------
}
function TfrmMain.UpdateValue(AHUDText: THUDText; AValue, AMin, AMax, ADirection: Integer; 
        ACharCount: Integer = 2): Integer;
  
  function FormatValue(AValue: Integer; ACharCount: Integer = 2): String;
  begin
    Result := IntToStr(AValue);
    while Length(Result) < ACharCount do Result := '0' + Result;
  end;
  
begin
  if AValue + ADirection < AMin then Result := AMin
  else
  if AValue + ADirection > AMax then Result := AMax
  else
    Result := AValue + ADirection;
  AHUDText.Text := FormatValue(Result, ACharCount);
end;  // TfrmMain.UpdateValue

{-==============================================================================
    THUDTextArea
===============================================================================}
{-------------------------------------------------------------------------------
}
constructor THUDTextArea.Create(AHUDText: THUDText);
begin
  FHUDText := AHUDText;
end;  // THUDTextArea.Create

{-------------------------------------------------------------------------------
}
function THUDTextArea.IsPointInside(X, Y: Integer): Boolean;
begin
  Result := FHUDText.Parent.Visible and
            (X >= FTopLeft.X) and (X <= FBottomRight.X) and
            (Y >= FTopLeft.Y) and (Y <= FBottomRight.Y);
end;  // THUDTextArea.IsPointInside

{-==============================================================================
    TMyDodecahedron
===============================================================================}
{-------------------------------------------------------------------------------
}
procedure TMyDodecahedron.BuildList(var rci : TRenderContextInfo);
  
  const
     A = 1.61803398875; // (Sqrt(5)+1)/2
     B = 0.61803398875; // (Sqrt(5)-1)/2
     C = 1;
  const
     Vertices: Array[0..19, 0..2] of TGLFloat =
        ((-A,  0,  B), (-A,  0, -B), ( A,  0, -B), (A,  0, B),
         ( B, -A,  0), (-B, -A,  0), (-B,  A,  0), (B,  A, 0),
         ( 0,  B, -A), ( 0, -B, -A), ( 0, -B,  A), (0,  B, A),
         (-C, -C,  C), (-C, -C, -C), ( C, -C, -C), (C, -C, C),
         (-C,  C,  C), (-C,  C, -C), ( C,  C, -C), (C,  C, C));
  
     Polygons: Array[0..11, 0..4] of TGLInt =
        (( 0, 12, 10, 11, 16),
         ( 1, 17,  8,  9, 13),
         ( 2, 14,  9,  8, 18),
         ( 3, 19, 11, 10, 15),
         ( 4, 14,  2,  3, 15),
         ( 5, 12,  0,  1, 13),
         ( 6, 17,  1,  0, 16),
         ( 7, 19,  3,  2, 18),
         ( 8, 17,  6,  7, 18),
         ( 9, 14,  4,  5, 13),
         (10, 12,  5,  4, 15),
         (11, 19,  7,  6, 16));
  
  var
    i     : Integer;
    U, V, N : TAffineVector;
  
begin
  inherited;
  glPushMatrix;
  glScalef(0.3, 0.3, 0.3);
  for i := 0 to 11 do begin
    U[0] := Vertices[Polygons[i, 2], 0] - Vertices[Polygons[i, 1], 0];
    U[1] := Vertices[Polygons[i, 2], 1] - Vertices[Polygons[i, 1], 1];
    U[2] := Vertices[Polygons[i, 2], 2] - Vertices[Polygons[i, 1], 2];
  
    V[0] := Vertices[Polygons[i, 0], 0] - Vertices[Polygons[i, 1], 0];
    V[1] := Vertices[Polygons[i, 0], 1] - Vertices[Polygons[i, 1], 1];
    V[2] := Vertices[Polygons[i, 0], 2] - Vertices[Polygons[i, 1], 2];
  
    VectorCrossProduct(U, V, N);
    NormalizeVector(N);
  
    glBegin(GL_TRIANGLE_FAN);
      glNormal3fv(@N);
        glTexCoord2f(0.5, 1);
      glVertex3fv(@Vertices[Polygons[I, 0], 0]);
        glTexCoord2f(0, 0);
      glVertex3fv(@Vertices[Polygons[I, 1], 0]);
        glTexCoord2f(1, 0);
      glVertex3fv(@Vertices[Polygons[I, 2], 0]);
        glTexCoord2f(0, 0);
      glVertex3fv(@Vertices[Polygons[I, 3], 0]);
        glTexCoord2f(1, 0);
      glVertex3fv(@Vertices[Polygons[I, 4], 0]);
    glEnd;
  end;
  glPopMatrix;
end;  // TMyDodecahedron.BuildList 

{-==============================================================================
    TGLObjectList
===============================================================================}
{-------------------------------------------------------------------------------
}
function TGLObjectList.GetGLItem(Index: Integer): TGLCustomSceneObject;
begin
  Result := TGLCustomSceneObject(Items[Index]);
end;  // TGLObjectList.GetGLItem

{-------------------------------------------------------------------------------
}
procedure TGLObjectList.SetGLItem(Index: Integer; AObject: TGLCustomSceneObject);
begin
  Items[Index] := AObject;
end;  // TGLObjectList.SetGLItem

{-------------------------------------------------------------------------------
}
end.


