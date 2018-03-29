// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System.IO
//open FSharp.Data
open FSharp.Configuration

type AuditDbConfig = YamlConfig<"AuditableDbConfig.yaml">
type TableConfig = AuditDbConfig.TableConfig_Type
type TableDef = AuditDbConfig.Tables_Item_Type

type FieldTypeToSql = string -> string

let newline = System.Environment.NewLine

let FieldTypeToStringBuilder (d:Map<string, string>) =
    fun (s:string) -> 
        if s.Contains("_") then
            let k = s.Substring(1)
            d.[k] +  " NULL"
        else
            d.[s] + " NOT NULL"

let ComposeFieldsSql (fieldMapper:FieldTypeToSql) (c:TableConfig) (t:TableDef) =
    let indentation = String.replicate c.TableFieldIndentation " "
    let fieldComposer n t = sprintf "%s[%s] %s" indentation n (fieldMapper t)
    let auditFieldsSql = 
        c.AuditFields
        |> Seq.map (fun e -> fieldComposer e.Name e.Type)
        |> (fun x -> Seq.append x [sprintf "%sPERIOD FOR SYSTEM_TIME(EffDateTime, ExpDateTime)" indentation])

    let dataFields =
        t.Fields
        |> Seq.map (fun e -> fieldComposer e.Name e.Type)
        
    Seq.append dataFields auditFieldsSql 

let ComposeConstraints (c:TableConfig) (t:TableDef) (tableName:string) =
    if t.ForeignKeys.Count > 0 then
        t.ForeignKeys
        |> Seq.map (fun e -> sprintf "%sCONSTRAINT FK_%s_%s FOREIGN KEY (%s) REFERENCES [app].[%s] (%s)" (String.replicate c.TableFieldIndentation " ") tableName e.Table e.Field e.Table e.Field)
    else
        Seq.empty<string>

let ComposeTableDefSql (fm:FieldTypeToSql) (schema:string) (c:TableConfig) (t:TableDef) (tableName:string) (includeForeignKeys:bool) =
    let fieldsSql = ComposeFieldsSql fm c t
    let constraintsSql = 
        match includeForeignKeys with
        | true -> ComposeConstraints c t tableName
        | false -> Seq.empty<string>
    let newlineDelimieter = sprintf ",%s" newline
    let tableSql = String.concat newlineDelimieter (Seq.append fieldsSql constraintsSql)
    sprintf """CREATE TABLE [%s].[%s](
%s
)
WITH 
    (
        SYSTEM_VERSIONING = ON (HISTORY_TABLE = [%s].[%sHistory])
    );
go""" schema tableName tableSql schema tableName

let ComposeUniqueConstriantSql (schema:string) (t:TableDef) =
    let newline = System.Environment.NewLine
    let fields = 
        t.Unique
        |> Seq.map (fun e -> sprintf "[%s]" e)
        |> String.concat ", "
    sprintf "CREATE UNIQUE INDEX UX_%s on [%s].[%s] (%s);%sgo" t.Name schema t.Name fields newline

let ComposeTableSql (fm:FieldTypeToSql) (c:TableConfig) (schema:string) (t:TableDef) (includeForeignKeys:bool) (includeUniqueConst:bool) (tableName:string) =
    if includeUniqueConst && t.ForeignKeys.Count > 0 then
        (ComposeTableDefSql fm schema c t tableName includeForeignKeys) + newline + (ComposeUniqueConstriantSql schema t)
    else
        ComposeTableDefSql fm schema c t tableName includeForeignKeys

let CreateDir d =
    if not(Directory.Exists(d)) then
        Directory.CreateDirectory(d)
        |> ignore

let ComposeSequenceSql (schema:string) (t:TableDef) =
    sprintf """CREATE SEQUENCE [%s].[%sVersion]
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

    let auditConfig = AuditDbConfig()
    auditConfig.Load(configFile)
    let tableFieldMapper =
        auditConfig.TableConfig.TypeMap
        |> Seq.map (fun e -> e.Type, e.Output)
        |> Map.ofSeq
        |> FieldTypeToStringBuilder

    //let historyTableFieldMapper =
    //    auditConfig.TableConfig.HistoryTableTypeMap
    //    |> Seq.map (fun e -> e.Type, e.Output)
    //    |> Map.ofSeq
    //    |> FieldTypeToStringBuilder
    

    for t in auditConfig.Tables do
        let tableSqlFile = Path.Combine(tableDir, (t.Name + ".sql"))
        let tableSql = ComposeTableSql tableFieldMapper auditConfig.TableConfig auditConfig.TableConfig.OutputSchema t true true t.Name
        File.WriteAllText(tableSqlFile, tableSql)

        //let auditSqlFile = Path.Combine(auditDir, (t.Name + auditConfig.TableConfig.AuditTableNameAppend + ".sql"))
        //let auditSql = ComposeTableSql historyTableFieldMapper auditConfig.TableConfig auditConfig.TableConfig.OutputSchema t false false (t.Name + auditConfig.TableConfig.AuditTableNameAppend)
        //File.WriteAllText(auditSqlFile, auditSql)

        let sequenceSqlFile = Path.Combine(sequenceDir, (t.Name + "Version.sql"))
        let sequenceSql = ComposeSequenceSql auditConfig.TableConfig.OutputSchema t
        File.WriteAllText(sequenceSqlFile, sequenceSql)    

    0 // return an integer exit code
