unit u_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, Windows, fgl;

type

  TStringMap = specialize TFPGMap<string, string>;


  { TfMain }

  TfMain = class(TForm)
    bClose: TButton;
    bHideToTrya: TButton;
    bReloadConfig: TButton;
    Label1: TLabel;
    lbDescription: TLabel;
    miReloadConfig: TMenuItem;
    miClose: TMenuItem;
    pmPaste: TPopupMenu;
    pmTray: TPopupMenu;
    TrayIcon: TTrayIcon;
    procedure bCloseClick(Sender: TObject);
    procedure bHideToTryaClick(Sender: TObject);
    procedure bReloadConfigClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure miReloadConfigClick(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private
    FHotKeyReplaceCRLFID: integer;
    FHotKeyPasteMenuId: integer;

    FShortcuts: TStringMap;

    // GUI

    procedure HideToTray();

    // Shortcuts

    procedure LoadShortcutsConfig();
    procedure ConstructPasteMenu(const AShortcutMap: TStringMap);
    procedure ReloadShortcutsConfig();

    procedure HandlePasteMenu(Sender: TObject);




    // Events

    procedure WMHotKey(var Msg: TMessage); message WM_HOTKEY;

    procedure ReplaceCRLFInClipboard();
    procedure ShowPasteMenu();

  public


  end;

var
  fMain: TfMain;

implementation

{$R *.lfm}

uses
  Clipbrd, IniFiles,
  u_utils;

const
  ConfigFileName = 'prtools_config.ini';

{ TfMain }

procedure TfMain.FormCreate(Sender: TObject);
begin
  FShortcuts := TStringMap.Create;

  ReloadShortcutsConfig();

  // Hotkey description
  // https://www.askingbox.com/tutorial/delphi-system-wide-hotkey

  // HotKey CTRL + W
  FHotKeyReplaceCRLFID := GlobalAddAtom('FHotKeyReplaceCRLFID');
  RegisterHotKey(Handle, FHotKeyReplaceCRLFID, MOD_CONTROL, VK_W);

  // HotKey CTRL + E
  FHotKeyPasteMenuId := GlobalAddAtom('FHotKeyPasteMenuId');
  RegisterHotKey(Handle, FHotKeyPasteMenuId, MOD_CONTROL, VK_E);
end;

procedure TfMain.bCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfMain.bHideToTryaClick(Sender: TObject);
begin
  HideToTray;
end;

procedure TfMain.bReloadConfigClick(Sender: TObject);
begin
  ReloadShortcutsConfig();
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  UnRegisterHotKey(Handle, FHotKeyReplaceCRLFID);
  GlobalDeleteAtom(FHotKeyReplaceCRLFID);

  FreeAndNil(FShortcuts);
end;

procedure TfMain.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsminimized then
  begin
    HideToTray();
  end;
end;

procedure TfMain.miReloadConfigClick(Sender: TObject);
begin
  ReloadShortcutsConfig();
end;

procedure TfMain.TrayIconDblClick(Sender: TObject);
begin
  WindowState := wsNormal;
  Show;
  SetFocus;
  TrayIcon.Visible := False;
end;

procedure TfMain.HideToTray;
begin
  Hide;
  TrayIcon.Visible := True;
end;

procedure TfMain.LoadShortcutsConfig;
const
  SettingsSectionName = 'shortcuts';
var
  IniFile: TIniFile;
  Settings: TStrings;
  SettingName: string;
begin
  IniFile := nil;
  Settings := nil;

  IniFile := TIniFile.Create(ConfigFileName);
  Settings := TStringList.Create;

  try
    FShortcuts.Clear;

    IniFile.ReadSection(SettingsSectionName, Settings);
    for SettingName in Settings do
      FShortcuts.Add(SettingName, IniFile.ReadString(SettingsSectionName, SettingName, ''));
  finally
    FreeAndNil(IniFile);
    FreeAndNil(Settings);
  end;
end;

procedure TfMain.ConstructPasteMenu(const AShortcutMap: TStringMap);
var
  MenuItem: TMenuItem;
  i: integer;
begin
  pmPaste.Items.Clear;

  for i := 0 to AShortcutMap.Count - 1 do
  begin
    MenuItem := TMenuItem.Create(Self);
    MenuItem.Caption := AShortcutMap.Keys[i];
    MenuItem.OnClick := @HandlePasteMenu;
    pmPaste.Items.Add(MenuItem);
  end;
end;

procedure TfMain.ReloadShortcutsConfig;
begin
  LoadShortcutsConfig();
  ConstructPasteMenu(FShortcuts);
end;

procedure TfMain.HandlePasteMenu(Sender: TObject);
var
  Template, TemplateParam: string;
  ShiftState: TShiftState;
begin
  ShiftState := GetKeyShiftState;

  Template := FShortcuts[TMenuItem(Sender).Caption];

  if ssCtrl in ShiftState then
  begin
    TemplateParam := '';
  end else begin
    TemplateParam := Clipboard.AsText;
  end;

  Clipboard.AsText := Template.Replace('@', TemplateParam);
end;


procedure TfMain.WMHotKey(var Msg: TMessage);
begin
  if Msg.wParam = FHotKeyReplaceCRLFID then
    ReplaceCRLFInClipboard;

  if Msg.wParam = FHotKeyPasteMenuId then
    ShowPasteMenu;
end;

procedure TfMain.ReplaceCRLFInClipboard;
begin
  Clipboard.AsText := RemoveCRLF(Clipboard.AsText);
end;

procedure TfMain.ShowPasteMenu;
begin
  pmPaste.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

end.
