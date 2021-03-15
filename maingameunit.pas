unit MainGameUnit;

{$mode objfpc}{$H+}
{$define showcam}
{$define useparrot}
//{$define usemonster}


interface

uses
  Classes, SysUtils, Math, CastleUIState,
  {$ifndef cgeapp}
  Forms, Controls, Graphics, Dialogs, CastleControl,
  {$else}
  CastleWindow,
  {$endif}
  CastleControls, CastleColors, CastleUIControls,
  CastleTriangles, CastleShapes, CastleVectors,
  CastleSceneCore, CastleScene, CastleTransform,
  CastleViewport, CastleCameras, CastleURIUtils,
  X3DNodes, X3DFields, X3DTIme, X3DLoad,
  CastleImages, CastleGLImages, CastleDownload,
  CastleTextureImages, CastleCompositeImage,
  CastleApplicationProperties, CastleLog, CastleTimeUtils, CastleKeysMouse;

type
  { TCastleApp }

  TCastleApp = class(TUIState)
    procedure BeforeRender; override; // TCastleUserInterface
    procedure Render; override; // TCastleUserInterface
    procedure Resize; override; // TCastleUserInterface
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override; // TUIState
    function  Motion(const Event: TInputMotion): Boolean; override; // TUIState
    function  Press(const Event: TInputPressRelease): Boolean; override; // TUIState
    function  Release(const Event: TInputPressRelease): Boolean; override; // TUIState
  private
    Viewport: TCastleViewport;
    Backport: TCastleViewport;
    Birdport: TCastleViewport;
    BackScene: TCastleScene;
    BirdScene: TCastleScene;
    LabelFPS: TCastleLabel;
    LabelRender: TCastleLabel;
    {$ifdef showcam}
    LabelCamPos: TCastleLabel;
    LabelCamDir: TCastleLabel;
    LabelCamUp: TCastleLabel;
    {$endif}
    CamXBtn: TCastleButton;
    CamYBtn: TCastleButton;
    CamZBtn: TCastleButton;
    CamXYBtn: TCastleButton;
    CamXZBtn: TCastleButton;
    CamYZBtn: TCastleButton;
    CamXYZBtn: TCastleButton;
  public
    procedure CreateLabel(var objLabel: TCastleLabel; const Line: Integer; const BottomUp: Boolean = True);
    procedure CreateButton(var objButton: TCastleButton; const ButtonText: String; const Line: Integer; const LeftPos: Integer; const ButtonCode: TNotifyEvent = nil);
    procedure Start; override; // TUIState
    procedure Stop; override; // TUIState
    procedure LoadViewport;
    procedure CamXBtnClick(Sender: TObject);
    procedure CamYBtnClick(Sender: TObject);
    procedure CamZBtnClick(Sender: TObject);
    procedure CamXYBtnClick(Sender: TObject);
    procedure CamXZBtnClick(Sender: TObject);
    procedure CamYZBtnClick(Sender: TObject);
    procedure CamXYZBtnClick(Sender: TObject);
  end;

var
  AppTime: Int64;
  PrepDone: Boolean;
  CastleApp: TCastleApp;
  RenderReady: Boolean;

function Vector32D(const AValue: Single): TVector3; inline;

implementation
{$ifdef cgeapp}
uses AppInitialization;
{$else}
uses GUIInitialization;
{$endif}

function Vector32D(const AValue: Single): TVector3; inline;
begin
  Result := Vector3(AValue, AValue, AValue);
end;

procedure TCastleApp.CreateLabel(var objLabel: TCastleLabel; const Line: Integer; const BottomUp: Boolean = True);
begin
  objLabel := TCastleLabel.Create(Application);
  objLabel.Padding := 5;
  objLabel.Color := White;
  objLabel.Frame := True;
  objLabel.FrameColor := Black;
  objLabel.Anchor(hpLeft, 10);
  if BottomUp then
    objLabel.Anchor(vpBottom, 10 + (Line * 35))
  else
    objLabel.Anchor(vpTop, -(10 + (Line * 35)));
  InsertFront(objLabel);
end;

procedure TCastleApp.CreateButton(var objButton: TCastleButton; const ButtonText: String; const Line: Integer; const LeftPos: Integer; const ButtonCode: TNotifyEvent = nil);
begin
  objButton := TCastleButton.Create(Application);
  objButton.Caption := ButtonText;
  objButton.Anchor(hpLeft, 10 + (LeftPos * 20));
  objButton.Anchor(vpBottom, 10 + (Line * 35));
  objButton.onClick := ButtonCode;
  InsertFront(objButton);
end;

procedure TCastleApp.LoadViewport;
var
  BirdScale: Single;
begin
  // Set up the background viewport
  Backport := TCastleViewport.Create(Application);
  // Use all the viewport
  Backport.FullSize := true;
  // Position the camera
  Backport.Camera.Orthographic.Origin := Vector2(0.5, 0.5);
  // Setup 2D
  Backport.Setup2D;
  // Add the viewport to the CGE control
  InsertFront(Backport);

  // Hacky load inline of background
  if not(BackScene = nil) then
    FreeAndNil(BackScene);
  BackScene := TCastleScene.Create(Application);
  BackScene.RenderOptions.MinificationFilter := minNearest;
  BackScene.RenderOptions.MagnificationFilter := magNearest;
  BackScene.Setup2D;
  BackScene.Load('castle-data:/tiles-iso.jpg');
  BackScene.Scale := Vector3(0.5, 0.5, 0.5);
  Backport.Items.Add(BackScene);
  Backport.Items.MainScene := BackScene;

  // Set up the background viewport
  Birdport := TCastleViewport.Create(Application);
  Birdport.FullSize := true;
  Birdport.AutoNavigation := True;
  Birdport.Transparent := True;
  Birdport.Camera.Orthographic.Origin := Vector2(0.5, 0.5);
  Birdport.Setup2D;
  InsertFront(Birdport);

  // Hacky load inline of parrot in Isometric view
  if not(BirdScene = nil) then
    FreeAndNil(BirdScene);
  BirdScene := TCastleScene.Create(Application);
  BirdScene.RenderOptions.MinificationFilter := minNearest;
  BirdScene.RenderOptions.MagnificationFilter := magNearest;
  BirdScene.Spatial := [ssDynamicCollisions, ssRendering];
  BirdScene.Setup2D;
  BirdScene.PrepareResources([prSpatial, prRenderSelf, prRenderClones, prScreenEffects],
      True,
      Birdport.PrepareParams);

  {$if defined(usemonster)}
  BirdScene.Load('castle-data:/monster.gltf');
  BirdScale := 50;
  {$elseif defined(useparrot)}
  BirdScene.Load('castle-data:/parrot.glb');
  BirdScale := 227.2;
  {$else}
  BirdScene.Load('castle-data:/cross3.x3dv');
  BirdScale := 2.2;
  {$endif}
  BirdScene.Scale := Vector32D(BirdScale * 2);
//  BirdScene.Rotation := Vector4(0, 1, 0, -pi / 2);
//  BirdScene.Translation := Vector3(500, 500, 0);
  Birdport.Items.Add(BirdScene);
  Birdport.Items.MainScene := BirdScene;

  // Some Labels
  {$ifdef showcam}
  CreateLabel(LabelCamPos, 0, False);
  CreateLabel(LabelCamDir, 1, False);
  CreateLabel(LabelCamUp, 2, False);
  {$endif}

  CreateButton(CamXBtn,   'CamDir(-1, 0, 0)', 10, 0, @CamXBtnClick);
  CreateButton(CamYBtn,   'CamDir( 0,-1, 0)',  9, 0, @CamYBtnClick);
  CreateButton(CamZBtn,   'CamDir( 0, 0,-1)',  8, 0, @CamZBtnClick);
  CreateButton(CamXYBtn,  'CamDir(-1,-1, 0)',  7, 0, @CamXYBtnClick);
  CreateButton(CamXZBtn,  'CamDir(-1, 0,-1)',  6, 0, @CamXZBtnClick);
  CreateButton(CamYZBtn,  'CamDir( 0,-1,-1)',  5, 0, @CamYZBtnClick);
  CreateButton(CamXYZBtn, 'CamDir(-1,-1,-1)',  4, 0, @CamXYZBtnClick);

  CreateLabel(LabelFPS, 1);
  CreateLabel(LabelRender, 0);
end;

procedure TCastleApp.Start;
begin
  inherited;
  LoadViewport;
  PrepDone := True;
end;

procedure TCastleApp.Stop;
begin
  inherited;
end;

procedure TCastleApp.BeforeRender;
{$ifdef showcam}
var
  Pos, Dir, Up: TVector3;
{$else}
var
{$endif}
  bpc: TVector2;
  bpp: TVector3;
begin
  inherited;
  LabelFPS.Caption := 'FPS = ' + FormatFloat('####0.00', Container.Fps.RealFps);
  LabelRender.Caption := 'Render = ' + FormatFloat('####0.00', Container.Fps.OnlyRenderFps);
{$ifdef showcam}
  if not(BirdScene = nil) then
    begin
    Birdport.Camera.GetView(Pos, Dir, Up);

    LabelCamPos.Caption := 'Cam Pos : ' +
      FormatFloat('####0.00', Pos.X) + ', ' +
      FormatFloat('####0.00', Pos.Y) + ', ' +
      FormatFloat('####0.00', Pos.Z);

    LabelCamDir.Caption := 'Cam Dir : ' +
      FormatFloat('####0.00', Dir.X) + ', ' +
      FormatFloat('####0.00', Dir.Y) + ', ' +
      FormatFloat('####0.00', Dir.Z);

    LabelCamUp.Caption := 'Cam Up : ' +
      FormatFloat('####0.00', Up.X) + ', ' +
      FormatFloat('####0.00', Up.Y) + ', ' +
      FormatFloat('####0.00', Up.Z);

    end;
{$endif}
end;

procedure TCastleApp.Render;
begin
  inherited;

  if PrepDone and GLInitialized and RenderReady then
    begin
      PrepDone := False;
    end;
  RenderReady := True;
end;

procedure TCastleApp.Resize;
begin
  inherited;
end;

procedure TCastleApp.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
end;

function TCastleApp.Motion(const Event: TInputMotion): Boolean;
begin
  Result := inherited;
end;

function TCastleApp.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
end;

function TCastleApp.Release(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
end;

procedure TCastleApp.CamXBtnClick(Sender: TObject);
begin
  Birdport.Camera.Direction := Vector3(-1, 0, 0); // Bad
end;

procedure TCastleApp.CamYBtnClick(Sender: TObject);
begin
  Birdport.Camera.Direction := Vector3( 0,-1, 0); // Bad
end;

procedure TCastleApp.CamZBtnClick(Sender: TObject);
begin
  Birdport.Camera.Direction := Vector3( 0, 0,-1); // OK
end;

procedure TCastleApp.CamXYBtnClick(Sender: TObject);
begin
  Birdport.Camera.Direction := Vector3(-1,-1, 0); // Bad
end;

procedure TCastleApp.CamXZBtnClick(Sender: TObject);
begin
  Birdport.Camera.Direction := Vector3(-1, 0,-1); // Bad
end;

procedure TCastleApp.CamYZBtnClick(Sender: TObject);
begin
  Birdport.Camera.Direction := Vector3( 0,-1,-1); // Bad
end;

procedure TCastleApp.CamXYZBtnClick(Sender: TObject);
begin
  Birdport.Camera.Direction := Vector3(-1,-1,-1); // Bad
end;


end.

