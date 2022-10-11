unit u_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, Windows,

  // custom
  u_utils;

type

  { TFormMain }

  TFormMain = class(TForm)
    bClose: TButton;
    bHideToTrya: TButton;
    bReloadConfig: TButton;
    Label1: TLabel;
    lbDescription: TLabel;
    miReloadConfig: TMenuItem;
    miClose: TMenuItem;
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

    procedure HandlePasteMenu(Sender: TObject);




    // Events

    procedure WMHotKey(var Msg: TMessage); message WM_HOTKEY;

    procedure ReplaceCRLFInClipboard();
    procedure ShowPasteMenu();

  public


  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses
  Clipbrd, IniFiles,
  // custom
  u_chice_shortcut;

const
  ConfigFileName = 'prtools_config.ini';

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FShortcuts := TStringMap.Create;

  LoadShortcutsConfig();

  // Hotkey description
  // https://www.askingbox.com/tutorial/delphi-system-wide-hotkey

  // HotKey CTRL + W
  FHotKeyReplaceCRLFID := GlobalAddAtom('FHotKeyReplaceCRLFID');
  RegisterHotKey(Handle, FHotKeyReplaceCRLFID, MOD_CONTROL, VK_W);

  // HotKey CTRL + E
  FHotKeyPasteMenuId := GlobalAddAtom('FHotKeyPasteMenuId');
  RegisterHotKey(Handle, FHotKeyPasteMenuId, MOD_CONTROL, VK_E);
end;

procedure TFormMain.bCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.bHideToTryaClick(Sender: TObject);
begin
  HideToTray;
end;

procedure TFormMain.bReloadConfigClick(Sender: TObject);
begin
  LoadShortcutsConfig();
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  UnRegisterHotKey(Handle, FHotKeyReplaceCRLFID);
  GlobalDeleteAtom(FHotKeyReplaceCRLFID);

  FreeAndNil(FShortcuts);
end;

procedure TFormMain.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsminimized then
  begin
    HideToTray();
  end;
end;

procedure TFormMain.miReloadConfigClick(Sender: TObject);
begin
  LoadShortcutsConfig();
end;

procedure TFormMain.TrayIconDblClick(Sender: TObject);
begin
  WindowState := wsNormal;
  Show;
  SetFocus;
  TrayIcon.Visible := False;
end;

procedure TFormMain.HideToTray;
begin
  Hide;
  TrayIcon.Visible := True;
end;

procedure TFormMain.LoadShortcutsConfig;
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

procedure TFormMain.HandlePasteMenu(Sender: TObject);
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


procedure TFormMain.WMHotKey(var Msg: TMessage);
begin
  if Msg.wParam = FHotKeyReplaceCRLFID then
    ReplaceCRLFInClipboard;

  if Msg.wParam = FHotKeyPasteMenuId then
    ShowPasteMenu;
end;

procedure TFormMain.ReplaceCRLFInClipboard;
begin
  Clipboard.AsText := RemoveCRLF(Clipboard.AsText);
end;

procedure TFormMain.ShowPasteMenu;
var
  Shortcut, Template, TemplateParam: string;
  ShiftState: TShiftState;

begin
  Shortcut := TFormPickFromList.ShowPickDialog(FShortcuts.GetKeys);

  if Shortcut.IsEmpty then
    Exit;

  Template := FShortcuts[Shortcut];

  ShiftState := GetKeyShiftState;

  if ssCtrl in ShiftState then
  begin
    TemplateParam := '';
  end else begin
    TemplateParam := Clipboard.AsText;
  end;

  Clipboard.AsText := Template.Replace('@', TemplateParam);
end;

end.
