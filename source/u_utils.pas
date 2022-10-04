unit u_utils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

  function RemoveCRLF(const AText: string): string;

implementation

function RemoveCRLF(const AText: string): string;
const
  BreakLineMarker = '@';
var
  Lines: TStringArray;
  i: Integer;
begin
  Lines := AText.Replace(#13#10#13#10, BreakLineMarker).Split(BreakLineMarker, TStringSplitOptions.ExcludeEmpty);

  for i := 0 to Length(Lines) - 1 do
  begin
    Lines[i] := Lines[i].Replace(#13#10, ' ');
  end;

  Result := string.Join(#13#10#13#10, Lines);
end;

end.

