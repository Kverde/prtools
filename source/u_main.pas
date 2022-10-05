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
    Label1: TLabel;
    miReloadConfig: TMenuItem;
    miClose: TMenuItem;
    pmPaste: TPopupMenu;
    pmTray: TPopupMenu;
    TrayIcon: TTrayIcon;
    procedure bCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private
    FHotKeyReplaceCRLFID: integer;
    FHotKeyPasteMenuId: integer;

    // Shortcuts

    FShortcuts: TStringMap;

    procedure LoadShortcuts();
    procedure ConstructPasteMenu(const AShortcutMap: TStringMap);
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

{ TfMain }

procedure TfMain.FormCreate(Sender: TObject);
begin
  FShortcuts := TStringMap.Create;

  LoadShortcuts();
  ConstructPasteMenu(FShortcuts);

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
    Hide;
    TrayIcon.Visible := True;
  end;
end;

procedure TfMain.TrayIconDblClick(Sender: TObject);
begin
  WindowState := wsNormal;
  Show;
  SetFocus;
  TrayIcon.Visible := False;
end;

procedure TfMain.LoadShortcuts;
const
  IniFileName = 'pgtool_config.ini';
  SettingsSectionName = 'shortcuts';
var
  IniFile: TIniFile;
  Settings: TStrings;
  SettingName: string;
begin
  IniFile := nil;
  Settings := nil;

  IniFile := TIniFile.Create(IniFileName);
  Settings := TStringList.Create;

  try
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
  for i := 0 to AShortcutMap.Count - 1 do
  begin
    MenuItem := TMenuItem.Create(Self);
    MenuItem.Caption := AShortcutMap.Keys[i];
    MenuItem.OnClick := @HandlePasteMenu;
    pmPaste.Items.Add(MenuItem);
  end;
end;

procedure TfMain.HandlePasteMenu(Sender: TObject);
var
  Shortcut: string;
begin
  Shortcut := FShortcuts[TMenuItem(Sender).Caption];
  Clipboard.AsText := Shortcut;
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
