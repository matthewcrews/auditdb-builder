// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System.IO
open FSharp.Configuration


type ExampleYamlConfig = YamlConfig<"ExampleConfig.yaml">
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
    fun s -> d.[s]

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
    let fields = String.concat ", " t.Unique
    sprintf "CREATE UNIQUE INDEX UIDX_%s on [%s].[%s] (%s);%sgo" t.Name schema t.Name fields newline

let ComposeTableSql (fm:FieldTypeToSql) (c:TableConfig) (schema:string) (t:TableDef) =
    match t.Unique.[0] <> "n" with
    | false -> ComposeTableDefSql fm schema c t
    | true -> (ComposeTableDefSql fm schema c t) + newline + (ComposeUniqueConstriantSql schema t)

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
    let tableDir = argv.[0]
    let auditDir = argv.[1]
    let sequenceDir = argv.[2]

    CreateDir tableDir
    CreateDir auditDir
    CreateDir sequenceDir

    let dbConfig = ExampleYamlConfig()
    let auditConfig = AuditableDbConfig()
    auditConfig.Load("AuditableDbConfig.yaml")
    let fieldConverter =
        auditConfig.Config.TypeMap
        |> Seq.map (fun e -> e.Type, e.Output)
        |> Map.ofSeq
        |> FieldTypeToStringBuilder

    //let auditFields =
    //    auditConfig.Config.AuditFields
    //    |> List.ofSeq
    

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

        //if t.Unique.[0] <> "n" then
        //    let uniqConstraintSql = (ComposeUniqueConstriantSql t)
        //    let outputSql = tableSql + newline + uniqConstraintSql
        //    File.WriteAllText(outputFile, tableSql)
        //else
        //    let tableSql = ComposeTableDefSql fieldConverter auditConfig.Config t
        //    File.WriteAllText(outputFile, tableSql)
    //auditConfig.Config.TypeMap.[0].
    //auditConfig.Tables.[0].fields.[0].
    //dbConfig.Companies.[0].
    

    0 // return an integer exit code
