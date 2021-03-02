unit MainGameUnit;

{$mode objfpc}{$H+}

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
  CastleViewport, CastleCameras,
  X3DNodes, X3DFields, X3DTIme,
  CastleImages, CastleGLImages,
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
    CurrentMap: Integer;
    CurrentModel: Integer;
    CurrentAnimation: Integer;
    Viewport: TCastleViewport;
    Backport: TCastleViewport;
    Scene: TCastleScene;
    BackScene: TCastleScene;
    LabelModel: TCastleLabel;
    LabelMap: TCastleLabel;
    LabelAnimation: TCastleLabel;
    LabelFPS: TCastleLabel;
    LabelRender: TCastleLabel;
    {$ifdef showcam}
    LabelCamPos: TCastleLabel;
    LabelCamDir: TCastleLabel;
    LabelCamUp: TCastleLabel;
    {$endif}
    BtnNextAnim: TCastleButton;
    BtnPrevAnim: TCastleButton;
    BtnNextModel: TCastleButton;
    BtnPrevModel: TCastleButton;
  public
    procedure CreateLabel(var objLabel: TCastleLabel; const Line: Integer; const BottomUp: Boolean = True);
    procedure CreateButton(var objButton: TCastleButton; const ButtonText: String; const Line: Integer; const LeftPos: Integer; const ButtonCode: TNotifyEvent = nil);
    procedure Start; override; // TUIState
    procedure Stop; override; // TUIState
    procedure LoadViewport;
    procedure LoadScene(filename: String; const ModelIdx: Integer; const AnimationIdx: Integer; SetAnimation: Boolean = True);
    procedure PrevAnimButtonClick(Sender: TObject);
    procedure NextAnimButtonClick(Sender: TObject);
    procedure PrevModelButtonClick(Sender: TObject);
    procedure NextModelButtonClick(Sender: TObject);
    procedure UpdateAnimationLabels;
  end;

var
  AppTime: Int64;
  PrepDone: Boolean;
  CastleApp: TCastleApp;
  RenderReady: Boolean;

const
  Models: Array[0..2] of String = ('Apex_Hunter', 'Apex_Predator', 'Apex_Stalker');
  Maps: Array[0..7] of String = ('1', '2', '3', '4', '5', '6', '7', '8');

implementation
{$ifdef cgeapp}
uses AppInitialization;
{$else}
uses GUIInitialization;
{$endif}

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
begin
  // Set up the backgrund viewport
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

  // Set up the main viewport
  Viewport := TCastleViewport.Create(Application);
  // Use all the viewport
  Viewport.FullSize := true;
  // Position the camera
  Viewport.Camera.Orthographic.Origin := Vector2(0.5, 0.5);
  // Make the viewport Transparent (So we can see the background)
  Viewport.Transparent := True;
  // Setup 2D
  Viewport.Setup2D;
  // Add the viewport to the CGE control
  InsertFront(Viewport);

  // Some Buttons
  CreateButton(BtnPrevAnim, 'Prev  Anim', 5, 0, @PrevAnimButtonClick);
  CreateButton(BtnNextAnim, 'Next  Anim', 5, 8, @NextAnimButtonClick);
  CreateButton(BtnPrevModel, 'Prev Model', 6, 0, @PrevModelButtonClick);
  CreateButton(BtnNextModel, 'Next Model', 6, 8, @NextModelButtonClick);

  // Some Labels
  {$ifdef showcam}
  CreateLabel(LabelCamPos, 0, False);
  CreateLabel(LabelCamDir, 1, False);
  CreateLabel(LabelCamUp, 2, False);
  {$endif}

  CreateLabel(LabelModel, 4);
  CreateLabel(LabelMap, 3);
  CreateLabel(LabelAnimation, 2);
  CreateLabel(LabelFPS, 1);
  CreateLabel(LabelRender, 0);
end;

procedure TCastleApp.NextAnimButtonClick(Sender: TObject);
begin
  if CurrentAnimation < (Scene.AnimationsList.Count - 1) then
    begin
      Inc(CurrentAnimation);
      Scene.PlayAnimation(Scene.AnimationsList[CurrentAnimation], true);
    end
  else
    begin
      if CurrentMap < (Length(Maps) - 1) then
        begin
          Inc(CurrentMap);
          CurrentAnimation := 0;
          LoadScene('castle-data:/PVGames_ApexPredators/' + Models[CurrentModel] + '/apex_map_' + Maps[CurrentMap] + '.starling-xml', CurrentModel, CurrentAnimation);
        end
      else
        begin
          if CurrentModel < (Length(Models) - 1) then
            begin
              Inc(CurrentModel);
              CurrentMap := 0;
              CurrentAnimation := 0;
              LoadScene('castle-data:/PVGames_ApexPredators/' + Models[CurrentModel] + '/apex_map_' + Maps[CurrentMap] + '.starling-xml', CurrentModel, CurrentAnimation);
            end
          else
            begin
              CurrentModel := 0;
              CurrentMap := 0;
              CurrentAnimation := 0;
              LoadScene('castle-data:/PVGames_ApexPredators/' + Models[CurrentModel] + '/apex_map_' + Maps[CurrentMap] + '.starling-xml', CurrentModel, CurrentAnimation);
            end;
        end;
    end;
    UpdateAnimationLabels;
end;

procedure TCastleApp.PrevAnimButtonClick(Sender: TObject);
begin
  if CurrentAnimation > 0 then
    begin
      Dec(CurrentAnimation);
      Scene.PlayAnimation(Scene.AnimationsList[CurrentAnimation], true);
    end
  else
    begin
      if CurrentMap > 0 then
        begin
          Dec(CurrentMap);
          LoadScene('castle-data:/PVGames_ApexPredators/' + Models[CurrentModel] + '/apex_map_' + Maps[CurrentMap] + '.starling-xml', CurrentModel, 0, false);
          CurrentAnimation := Scene.AnimationsList.Count - 1;
          Scene.PlayAnimation(Scene.AnimationsList[CurrentAnimation], true);
        end
      else
        begin
          if CurrentModel > 0 then
            begin
              Dec(CurrentModel);
              CurrentMap := Length(Maps) - 1;
              LoadScene('castle-data:/PVGames_ApexPredators/' + Models[CurrentModel] + '/apex_map_' + Maps[CurrentMap] + '.starling-xml', CurrentModel, 0, false);
              CurrentAnimation := Scene.AnimationsList.Count - 1;
              Scene.PlayAnimation(Scene.AnimationsList[CurrentAnimation], true);
            end
          else
            begin
              CurrentModel := Length(Models) - 1;
              CurrentMap := Length(Maps) - 1;
              LoadScene('castle-data:/PVGames_ApexPredators/' + Models[CurrentModel] + '/apex_map_' + Maps[CurrentMap] + '.starling-xml', CurrentModel, 0, false);
              CurrentAnimation := Scene.AnimationsList.Count - 1;
              Scene.PlayAnimation(Scene.AnimationsList[CurrentAnimation], true);
            end;
        end;
    end;
  UpdateAnimationLabels;
end;

procedure TCastleApp.NextModelButtonClick(Sender: TObject);
begin
  if CurrentModel < (Length(Models) - 1) then
    begin
      Inc(CurrentModel);
      LoadScene('castle-data:/PVGames_ApexPredators/' + Models[CurrentModel] + '/apex_map_' + Maps[CurrentMap] + '.starling-xml', CurrentModel, CurrentAnimation);
    end
  else
    begin
      CurrentModel := 0;
      LoadScene('castle-data:/PVGames_ApexPredators/' + Models[CurrentModel] + '/apex_map_' + Maps[CurrentMap] + '.starling-xml', CurrentModel, CurrentAnimation);
    end;
  UpdateAnimationLabels;
end;

procedure TCastleApp.PrevModelButtonClick(Sender: TObject);
begin
  if CurrentModel > 0 then
    begin
      Dec(CurrentModel);
      LoadScene('castle-data:/PVGames_ApexPredators/' + Models[CurrentModel] + '/apex_map_' + Maps[CurrentMap] + '.starling-xml', CurrentModel, CurrentAnimation);
    end
  else
    begin
      CurrentModel := Length(Models) - 1;
      LoadScene('castle-data:/PVGames_ApexPredators/' + Models[CurrentModel] + '/apex_map_' + Maps[CurrentMap] + '.starling-xml', CurrentModel, CurrentAnimation);
    end;
end;

procedure TCastleApp.LoadScene(filename: String; const ModelIdx: Integer; const AnimationIdx: Integer; SetAnimation: Boolean = True);
begin
  try
    if not(Scene = nil) then
      FreeAndNil(Scene);
    Scene := TCastleScene.Create(Application);
    Scene.Spatial := [ssStaticCollisions, ssDynamicCollisions, ssRendering];
    Scene.RenderOptions.MinificationFilter := minNearest;
    Scene.RenderOptions.MagnificationFilter := magNearest;
    Scene.Setup2D;
    Scene.Load(filename);
    Scene.PrepareResources([prSpatial, prRenderSelf, prRenderClones, prScreenEffects],
        True,
        Viewport.PrepareParams);
    Viewport.Items.Add(Scene);
    Viewport.Items.MainScene := Scene;

    Scene.Scale := Vector3(2.0, 2.0, 2.0);

    if SetAnimation then
      begin
        Scene.PlayAnimation(Scene.AnimationsList[AnimationIdx], true);
        UpdateAnimationLabels;
      end;
  except
    on E : Exception do
      begin
        WriteLnLog('Oops #1' + LineEnding + E.ClassName + LineEnding + E.Message);
       end;
  end;
end;

procedure TCastleApp.UpdateAnimationLabels;
begin
  LabelModel.Caption := Models[CurrentModel] +
                        ' (' + IntToStr(CurrentModel + 1) +
                        ' of ' + IntToStr(Length(Models)) + ')';
  LabelMap.Caption := 'TextureAtlas' +
                        ' (' + IntToStr(CurrentMap + 1) +
                        ' of ' + IntToStr(Length(Maps)) + ')';
  LabelAnimation.Caption := 'Animation : ' + Scene.AnimationsList[CurrentAnimation] +
                        ' (' + IntToStr(CurrentAnimation + 1) +
                        ' of ' + IntToStr(Scene.AnimationsList.Count) + ')';
end;

procedure TCastleApp.Start;
begin
  inherited;
  CurrentMap := 0;
  CurrentModel := 0;
  CurrentAnimation := 0;
  Scene := nil;
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
{$endif}
begin
  inherited;
  LabelFPS.Caption := 'FPS = ' + FormatFloat('####0.00', Container.Fps.RealFps);
  LabelRender.Caption := 'Render = ' + FormatFloat('####0.00', Container.Fps.OnlyRenderFps);
{$ifdef showcam}
  if not(BackScene = nil) then
    begin
    Backport.Camera.GetView(Pos, Dir, Up);

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
      LoadScene('castle-data:/PVGames_ApexPredators/' + Models[CurrentModel] + '/apex_map_' + Maps[CurrentMap] + '.starling-xml', CurrentModel, CurrentAnimation);
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

end.

