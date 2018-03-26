﻿// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System.IO
open FSharp.Configuration

type AuditableDbConfig = YamlConfig<"AuditableDbConfig.yaml">
type TableConfig = AuditableDbConfig.Config_Type
type TableDef = AuditableDbConfig.Tables_Item_Type
type FieldDef = TableDef.Fields_Item_Type
type TypeMap = AuditableDbConfig.Config_Type.TypeMap_Item_Type
//type UniqueFields = TableDef.
type FieldTypeToSql = string -> string
type AuditFieldsSet = AuditableDbConfig.Config_Type.AuditFields_Item_Type

let newline = System.Environment.NewLine

let FieldTypeToStringBuilder (d:Map<string, string>) =
    fun (s:string) -> 
        if s.Contains("_") then
            let k = s.Substring(1)
            d.[k] +  " NULL"
        else
            d.[s] + " NOT NULL"

let FieldDefToString (m: FieldTypeToSql) (f:FieldDef) =
    sprintf "[%s] %s" f.Name (m f.Type)
    

let ComposeFieldsSql (fieldMapper:FieldTypeToSql) (c:TableConfig) (t:TableDef) =
    let indentation = String.replicate c.TableFieldIndentation " "
    let fieldComposer n t = sprintf "%s[%s] %s" indentation n (fieldMapper t)
    let auditFieldsSql = 
        c.AuditFields
        |> Seq.map (fun e -> fieldComposer e.Name e.Type)

    let dataFields =
        t.Fields
        |> Seq.map (fun e -> fieldComposer e.Name e.Type)

    let newline = sprintf ",%s" System.Environment.NewLine
    String.concat newline (Seq.append auditFieldsSql dataFields)

let ComposeTableDefSql (fm:FieldTypeToSql) (schema:string) (c:TableConfig) (t:TableDef) =
    let fieldsSql = ComposeFieldsSql fm c t
    let newline = System.Environment.NewLine
    sprintf "CREATE TABLE [%s].[%s]%s(%s%s%s)%sgo%s" schema t.Name newline newline fieldsSql newline newline newline

let ComposeUniqueConstriantSql (schema:string) (t:TableDef) =
    let newline = System.Environment.NewLine
    let fields = 
        t.Unique
        |> Seq.map (fun e -> sprintf "[%s]" e)
        |> String.concat ", "
    sprintf "CREATE UNIQUE INDEX UIDX_%s on [%s].[%s] (%s);%sgo" t.Name schema t.Name fields newline

let ComposeTableSql (fm:FieldTypeToSql) (c:TableConfig) (schema:string) (t:TableDef) =
    if (t.Unique.[0] <> "n" && c.AuditSchemaName <> schema) then
        (ComposeTableDefSql fm schema c t) + newline + (ComposeUniqueConstriantSql schema t)
    else
        ComposeTableDefSql fm schema c t

let CreateDir d =
    if not(Directory.Exists(d)) then
        Directory.CreateDirectory(d)
        |> ignore

let ComposeSequenceSql (schema:string) (t:TableDef) =
    sprintf """CREATE SEQUENCE [%s].[%sVer]
    AS BIGINT
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO CYCLE
    CACHE 10""" schema t.Name

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let configFile = argv.[0]
    let outputDir = argv.[1]
    let tableDir = Path.Combine(outputDir, argv.[2])
    let auditDir = Path.Combine(outputDir, argv.[3])
    let sequenceDir = Path.Combine(outputDir, argv.[4])

    CreateDir outputDir
    CreateDir tableDir
    CreateDir auditDir
    CreateDir sequenceDir

    let auditConfig = AuditableDbConfig()
    auditConfig.Load(configFile)
    let fieldConverter =
        auditConfig.Config.TypeMap
        |> Seq.map (fun e -> e.Type, e.Output)
        |> Map.ofSeq
        |> FieldTypeToStringBuilder
    

    for t in auditConfig.Tables do
        let tableSqlFile = Path.Combine(tableDir, (t.Name + ".sql"))
        let tableSql = ComposeTableSql fieldConverter auditConfig.Config "dbo" t
        File.WriteAllText(tableSqlFile, tableSql)

        let auditSqlFile = Path.Combine(auditDir, (t.Name + ".sql"))
        let auditSql = ComposeTableSql fieldConverter auditConfig.Config auditConfig.Config.AuditSchemaName t
        File.WriteAllText(auditSqlFile, auditSql)

        let sequenceSqlFile = Path.Combine(sequenceDir, (t.Name + "Ver.sql"))
        let sequenceSql = ComposeSequenceSql auditConfig.Config.AuditSchemaName t
        File.WriteAllText(sequenceSqlFile, sequenceSql)    

    0 // return an integer exit code
