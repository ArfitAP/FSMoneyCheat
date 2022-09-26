
'Zadnji update: 8.12.2018.
'Linija: 300
'Autor: Antonio Pavliš

Public Class Form1
    Dim valuta As String = " €"
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim file As String
        Dim args As String
        Dim savegame As String = ""

        If TextBox1.Text.Length = 0 Then
            MsgBox("Pogresan unos!" & vbCrLf & "Pokusajte ponovo.", MsgBoxStyle.Exclamation, "Greska")
            TextBox1.Focus()
            TextBox1.SelectionStart = TextBox1.TextLength
            TextBox1.ScrollToCaret()
            GoTo greska
        End If

        For k As Integer = 0 To TextBox1.Text.Length - 1
            If (TextBox1.Text(k) <> "0" And TextBox1.Text(k) <> "1" And TextBox1.Text(k) <> "2" And TextBox1.Text(k) <> "3" And TextBox1.Text(k) <> "4" And TextBox1.Text(k) <> "5" And TextBox1.Text(k) <> "6" And TextBox1.Text(k) <> "7" And TextBox1.Text(k) <> "8" And TextBox1.Text(k) <> "9") Then
                MsgBox("Pogresan unos!" & vbCrLf & "Pokusajte ponovo.", MsgBoxStyle.Exclamation, "Greska")
                TextBox1.Focus()
                TextBox1.SelectionStart = TextBox1.TextLength
                TextBox1.ScrollToCaret()
                GoTo greska
            End If
        Next

        file = String.Concat(My.Computer.FileSystem.SpecialDirectories.MyDocuments, "\My Games")

        If RadioButton1.Checked = True Then
            file = String.Concat(file, "\FarmingSimulator2011")
        ElseIf RadioButton3.Checked = True Then
            file = String.Concat(file, "\FarmingSimulator2013")
        ElseIf RadioButton2.Checked = True Then
            file = String.Concat(file, "\FarmingSimulator2015")
        ElseIf RadioButton10.Checked = True Then
            file = String.Concat(file, "\FarmingSimulator2017")
        ElseIf RadioButton9.Checked = True Then
            file = String.Concat(file, "\FarmingSimulator2019")
        ElseIf RadioButton11.Checked = True Then
            file = String.Concat(file, "\FarmingSimulator2022")
        End If

        If RadioButton4.Checked = True Then
            file = String.Concat(file, "\savegame1")
            savegame = "\savegame1"
        ElseIf RadioButton5.Checked = True Then
            file = String.Concat(file, "\savegame2")
            savegame = "\savegame2"
        ElseIf RadioButton6.Checked = True Then
            file = String.Concat(file, "\savegame3")
            savegame = "\savegame3"
        ElseIf RadioButton7.Checked = True Then
            file = String.Concat(file, "\savegame4")
            savegame = "\savegame4"
        ElseIf RadioButton8.Checked = True Then
            file = String.Concat(file, "\savegame5")
            savegame = "\savegame5"
        End If

        file = String.Concat(file, "\careerSavegame.xml")

        If My.Computer.FileSystem.FileExists(file) Then
            args = My.Computer.FileSystem.ReadAllText(file)

            Dim i As Integer
            For i = 5 To args.Length - 1
                If RadioButton10.Checked = True And args(i - 5) = "m" And args(i - 4) = "o" And args(i - 3) = "n" And args(i - 2) = "e" And args(i - 1) = "y" And args(i) = ">" Then
                    Dim j As Integer
                    Dim money As String = ""
                    For j = i + 1 To args.Length - 1
                        If IsNumeric(args(j)) Then
                            money = money & args(j)
                        Else
                            GoTo kraj1
                        End If
                    Next
kraj1:
                    If money.Length > 0 Then
                        money = "<money>" & money & "</money>"
                        Dim newmoney As String = "<money>" & TextBox1.Text & "</money>"
                        args = args.Replace(money, newmoney)
                        My.Computer.FileSystem.WriteAllText(file, args, False)
                        MsgBox("Iznos novca je uspjesno izmjenjen !" & vbCrLf & "Trenutna vrijednost je: " & TextBox1.Text & valuta, MsgBoxStyle.Information, "Obavijest")
                        TextBox1.Text = ""
                        GoTo kr
                    End If

                ElseIf (RadioButton9.Checked = True Or RadioButton11.Checked = True) And args(i - 5) = "m" And args(i - 4) = "o" And args(i - 3) = "n" And args(i - 2) = "e" And args(i - 1) = "y" And args(i) = ">" Then
                    Dim j, k As Integer
                    Dim money As String = ""
                    For j = i + 1 To args.Length - 1
                        If IsNumeric(args(j)) Then
                            money = money & args(j)
                        Else
                            GoTo kraj2
                        End If
                    Next
kraj2:
                    Dim argsfarm As String
                    Dim filefarm As String

                    If (RadioButton9.Checked = True) Then
                        filefarm = String.Concat(
                            My.Computer.FileSystem.SpecialDirectories.MyDocuments,
                              "\My Games\FarmingSimulator2019", savegame, "\farms.xml")
                    Else
                        filefarm = String.Concat(
                            My.Computer.FileSystem.SpecialDirectories.MyDocuments,
                              "\My Games\FarmingSimulator2022", savegame, "\farms.xml")
                    End If

                    argsfarm = My.Computer.FileSystem.ReadAllText(filefarm)

                    If money.Length > 0 Then
                        money = "<money>" & money & "</money>"
                        Dim newmoney As String = "<money>" & TextBox1.Text & "</money>"

                        args = args.Replace(money, newmoney)
                        My.Computer.FileSystem.WriteAllText(file, args, False)

                        money = "money=" & Chr(34)
                        k = argsfarm.IndexOf("money=")

                        For j = k + 7 To args.Length - 1
                            If IsNumeric(argsfarm(j)) Or argsfarm(j) = "." Then
                                money = money & argsfarm(j)
                            Else
                                GoTo pom
                            End If
                        Next
pom:
                        money = money & Chr(34)

                        newmoney = "money=" & Chr(34) & TextBox1.Text & Chr(34)
                        argsfarm = argsfarm.Replace(money, newmoney)
                        My.Computer.FileSystem.WriteAllText(filefarm, argsfarm, False)

                        MsgBox("Iznos novca je uspjesno izmjenjen !" & vbCrLf & "Trenutna vrijednost je: " & TextBox1.Text & valuta, MsgBoxStyle.Information, "Obavijest")
                        TextBox1.Text = ""
                        GoTo kr
                    End If

                ElseIf RadioButton10.Checked = False And RadioButton9.Checked = False And RadioButton11.Checked = False And args(i - 5) = "m" And args(i - 4) = "o" And args(i - 3) = "n" And args(i - 2) = "e" And args(i - 1) = "y" And args(i) = "=" Then
                    Dim j As Integer
                    Dim money As String = ""
                    For j = i + 2 To args.Length - 1
                        If IsNumeric(args(j)) Then
                            money = money & args(j)
                        Else
                            GoTo kraj
                        End If
                    Next
kraj:
                    If money.Length > 0 Then
                        money = "money=" & Chr(34) & money & Chr(34)
                        Dim newmoney As String = "money=" & Chr(34) & TextBox1.Text & Chr(34)
                        args = args.Replace(money, newmoney)
                        My.Computer.FileSystem.WriteAllText(file, args, False)
                        MsgBox("Iznos novca je uspjesno izmjenjen !" & vbCrLf & "Trenutna vrijednost je: " & TextBox1.Text & valuta, MsgBoxStyle.Information, "Obavijest")
                        TextBox1.Text = ""
                        GoTo kr
                    End If
                End If
            Next
kr:
        Else
            MsgBox("Odabrana datoteka ne postoji !", MsgBoxStyle.Exclamation, "Greska !")
        End If
greska:
        TextBox1.Focus()
        TextBox1.SelectionStart = TextBox1.TextLength
        TextBox1.ScrollToCaret()

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Dim file As String
        Dim args As String
        Dim savegame As String = ""

        file = String.Concat(My.Computer.FileSystem.SpecialDirectories.MyDocuments, "\My Games")

        If RadioButton1.Checked = True Then
            file = String.Concat(file, "\FarmingSimulator2011")
        ElseIf RadioButton3.Checked = True Then
            file = String.Concat(file, "\FarmingSimulator2013")
        ElseIf RadioButton2.Checked = True Then
            file = String.Concat(file, "\FarmingSimulator2015")
        ElseIf RadioButton10.Checked = True Then
            file = String.Concat(file, "\FarmingSimulator2017")
        ElseIf RadioButton9.Checked = True Then
            file = String.Concat(file, "\FarmingSimulator2019")
        ElseIf RadioButton11.Checked = True Then
            file = String.Concat(file, "\FarmingSimulator2022")
        End If

        If RadioButton4.Checked = True Then
            file = String.Concat(file, "\savegame1")
            savegame = "\savegame1"
        ElseIf RadioButton5.Checked = True Then
            file = String.Concat(file, "\savegame2")
            savegame = "\savegame2"
        ElseIf RadioButton6.Checked = True Then
            file = String.Concat(file, "\savegame3")
            savegame = "\savegame3"
        ElseIf RadioButton7.Checked = True Then
            file = String.Concat(file, "\savegame4")
            savegame = "\savegame4"
        ElseIf RadioButton8.Checked = True Then
            file = String.Concat(file, "\savegame5")
            savegame = "\savegame5"
        End If

        file = String.Concat(file, "\careerSavegame.xml")

        If My.Computer.FileSystem.FileExists(file) Then
            args = My.Computer.FileSystem.ReadAllText(file)

            Dim i As Integer
            For i = 5 To args.Length - 1
                If RadioButton10.Checked = True And args(i - 5) = "m" And args(i - 4) = "o" And args(i - 3) = "n" And args(i - 2) = "e" And args(i - 1) = "y" And args(i) = ">" Then
                    Dim j As Integer
                    Dim money As String = ""
                    For j = i + 1 To args.Length - 1
                        If IsNumeric(args(j)) Then
                            money = money & args(j)
                        Else
                            GoTo kraj1
                        End If
                    Next
kraj1:
                    If money.Length > 0 Then
                        MsgBox("Trenuta vrijednost je: " & money & valuta, MsgBoxStyle.Information, "Info")
                        GoTo kr
                    End If

                ElseIf (RadioButton9.Checked = True Or RadioButton11.Checked = True) And args(i - 5) = "m" And args(i - 4) = "o" And args(i - 3) = "n" And args(i - 2) = "e" And args(i - 1) = "y" And args(i) = ">" Then
                    Dim j, k As Integer
                    Dim money As String = ""
                    Dim filefarm As String

                    If (RadioButton9.Checked = True) Then
                        filefarm = String.Concat(
                            My.Computer.FileSystem.SpecialDirectories.MyDocuments,
                              "\My Games\FarmingSimulator2019", savegame, "\farms.xml")
                    Else
                        filefarm = String.Concat(
                            My.Computer.FileSystem.SpecialDirectories.MyDocuments,
                              "\My Games\FarmingSimulator2022", savegame, "\farms.xml")
                    End If

                    args = My.Computer.FileSystem.ReadAllText(filefarm)
                    k = args.IndexOf("money=")

                    For j = k + 7 To args.Length - 1
                        If IsNumeric(args(j)) Then
                            money = money & args(j)
                        Else
                            GoTo kraj2
                        End If
                    Next
kraj2:

                    If money.Length > 0 Then
                        MsgBox("Trenuta vrijednost je: " & money & valuta, MsgBoxStyle.Information, "Info")
                        GoTo kr
                    End If
                ElseIf RadioButton10.Checked = False And RadioButton9.Checked = False And RadioButton11.Checked = False And args(i - 5) = "m" And args(i - 4) = "o" And args(i - 3) = "n" And args(i - 2) = "e" And args(i - 1) = "y" And args(i) = "=" Then
                    Dim j As Integer
                    Dim money As String = ""
                    For j = i + 2 To args.Length - 1
                        If IsNumeric(args(j)) Then
                            money = money & args(j)
                        Else
                            GoTo kraj
                        End If
                    Next
kraj:
                    If money.Length > 0 Then
                        MsgBox("Trenuta vrijednost je: " & money & valuta, MsgBoxStyle.Information, "Info")
                        GoTo kr
                    End If
                End If
            Next
kr:
        Else
            MsgBox("Odabrana datoteka ne postoji !", MsgBoxStyle.Exclamation, "Greska !")
        End If
greska:
        TextBox1.Focus()
        TextBox1.SelectionStart = TextBox1.TextLength
        TextBox1.ScrollToCaret()

    End Sub

    Private Sub Form1_Shown(sender As Object, e As EventArgs) Handles MyBase.Shown
        TextBox1.Focus()
        TextBox1.SelectionStart = TextBox1.TextLength
        TextBox1.ScrollToCaret()
    End Sub

    Private Sub RadioButton1_CheckedChanged(sender As Object, e As EventArgs) Handles RadioButton1.Click
        valuta = " $"
        Label1.Text = "Iznos ($):"
    End Sub

    Private Sub RadioButton3_CheckedChanged(sender As Object, e As EventArgs) Handles RadioButton3.Click, RadioButton2.Click, RadioButton10.Click, RadioButton9.Click, RadioButton11.Click
        valuta = " €"
        Label1.Text = "Iznos (€):"
    End Sub

End Class
