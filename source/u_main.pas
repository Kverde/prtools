unit u_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Windows;

type

  { TfMain }

  TfMain = class(TForm)
    TrayIcon: TTrayIcon;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private
    FHotKeyReplaceCRLFID: Integer;

    procedure WMHotKey(var Msg:TMessage); message WM_HOTKEY;

    procedure ReplaceCRLFInClipboard();

  public


  end;

var
  fMain: TfMain;

implementation

{$R *.lfm}

uses
  Clipbrd,
  u_utils;

{ TfMain }

procedure TfMain.FormCreate(Sender: TObject);
const
  VK_A = $41;
begin
  // HotKey CTRL + W
  FHotKeyReplaceCRLFID := GlobalAddAtom('FHotKeyReplaceCRLFID');
  RegisterHotKey(Handle, FHotKeyReplaceCRLFID, MOD_CONTROL, VK_W);
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  UnRegisterHotKey(Handle, FHotKeyReplaceCRLFID);
  GlobalDeleteAtom(FHotKeyReplaceCRLFID);
end;

procedure TfMain.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsminimized then
  begin
     Hide;
     TrayIcon.Visible := true;
  end;
end;

procedure TfMain.TrayIconDblClick(Sender: TObject);
begin
  WindowState := wsNormal;
  Show;
  SetFocus;
  TrayIcon.Visible := false;
end;

procedure TfMain.WMHotKey(var Msg: TMessage);
begin
  if Msg.wParam = FHotKeyReplaceCRLFID  then
    ReplaceCRLFInClipboard;
end;

procedure TfMain.ReplaceCRLFInClipboard;
begin
  Clipboard.AsText := RemoveCRLF(Clipboard.AsText);
end;

end.

