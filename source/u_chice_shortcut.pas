unit u_chice_shortcut;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ListFilterEdit, ListViewFilterEdit;

type

  { TFormPickFromList }

  TFormPickFromList = class(TForm)
    bOK: TButton;
    bCancel: TButton;
    lbShortcut: TListBox;
    ListFilterEdit1: TListFilterEdit;
    Panel1: TPanel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lbShortcutDblClick(Sender: TObject);
  private

    procedure FillList(const APickList: TStringArray);
    function SelectedItem: string;
  public
    class function ShowPickDialog(const APickList: TStringArray): string;
  end;

implementation

{$R *.lfm}


{ TFormPickFromList }

procedure TFormPickFromList.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 13 then
  begin
    ModalResult := mrOK;
  end;

  if Key = 27 then
  begin
    ModalResult := mrCancel;
  end;
end;

procedure TFormPickFromList.lbShortcutDblClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TFormPickFromList.FillList(const APickList: TStringArray);
var
  Str: string;
begin
  lbShortcut.Items.Clear;

  for Str in APickList do
  begin
    lbShortcut.Items.Add(Str);
  end;

  // Если присвоить в дизайнере то фильтрация не работает
  ListFilterEdit1.FilteredListbox := lbShortcut;

  lbShortcut.ItemIndex := 0;
end;

function TFormPickFromList.SelectedItem: string;
var
  SelectedIndex: Integer;
begin
  if lbShortcut.ItemIndex = -1 then
    SelectedIndex := 0
  else
    SelectedIndex := lbShortcut.ItemIndex;

  Result := lbShortcut.Items[SelectedIndex];
end;

class function TFormPickFromList.ShowPickDialog(const APickList: TStringArray
  ): string;
var
  FormPickFromList: TFormPickFromList;
begin
  FormPickFromList := TFormPickFromList.Create(nil);
  try
     FormPickFromList.FillList(APickList);

    if FormPickFromList.ShowModal = mrOK then
    begin
      Result := FormPickFromList.SelectedItem;
    end else
    begin
      Result := '';
    end;

  finally
    FreeAndNil(FormPickFromList);
  end;
end;

end.

