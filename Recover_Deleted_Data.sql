CREATE PROCEDURE Recover_Deleted_Data_Proc
@SchemaName_n_TableName NVARCHAR(Max),
@Date_From datetime='1900/01/01',
@Date_To datetime ='9999/12/31'
as
 
DECLARE @RowLogContents VARBINARY(8000)
DECLARE @TransactionID NVARCHAR(Max)
DECLARE @AllocUnitID BIGINT
Declare @AllocUnitName NVARCHAR(Max)
Declare @SQL NVARCHAR(Max)
 
DECLARE @bitTable TABLE
(
  [ID] INT,
  [Bitvalue] INT
)
--Create table to set the bit position of one byte.
 
INSERT INTO @bitTable
SELECT 0,2 UNION ALL
SELECT 1,2 UNION ALL
SELECT 2,4 UNION ALL
SELECT 3,8 UNION ALL
SELECT 4,16 UNION ALL
SELECT 5,32 UNION ALL
SELECT 6,64 UNION ALL
SELECT 7,128
 
--Create table to collect the row data.
DECLARE @DeletedRecords TABLE
(
    [RowLogContents]    VARBINARY(8000),
    [AllocUnitID]       BIGINT,
    [Transaction ID]    NVARCHAR(Max),
    [FixedLengthData]   SMALLINT,
    [TotalNoOfCols]     SMALLINT,
    [NullBitMapLength]  SMALLINT,
    [NullBytes]         VARBINARY(8000),
    [TotalNoofVarCols]  SMALLINT,
    [ColumnOffsetArray] VARBINARY(8000),
    [VarColumnStart]    SMALLINT,
    [NullBitMap]        VARCHAR(MAX)
)
--Create a common table expression to get all the row data plus how many bytes we have for each row.
;WITH RowData AS (
SELECT
 
[RowLog Contents 0] AS [RowLogContents]
 
,[AllocUnitID] AS [AllocUnitID]
 
,[Transaction ID] AS [Transaction ID]
 
--[Fixed Length Data] = Substring (RowLog content 0, Status Bit A+ Status Bit B + 1,2 bytes)
,CONVERT(SMALLINT, CONVERT(BINARY(2), REVERSE(SUBSTRING([RowLog Contents 0], 2 + 1, 2)))) AS [FixedLengthData]  --@FixedLengthData
 
-- [TotalnoOfCols] =  Substring (RowLog content 0, [Fixed Length Data] + 1,2 bytes)
,CONVERT(INT, CONVERT(BINARY(2), REVERSE(SUBSTRING([RowLog Contents 0], CONVERT(SMALLINT, CONVERT(BINARY(2)
,REVERSE(SUBSTRING([RowLog Contents 0], 2 + 1, 2)))) + 1, 2)))) as  [TotalNoOfCols]
 
--[NullBitMapLength]=ceiling([Total No of Columns] /8.0)
,CONVERT(INT, ceiling(CONVERT(INT, CONVERT(BINARY(2), REVERSE(SUBSTRING([RowLog Contents 0], CONVERT(SMALLINT, CONVERT(BINARY(2)
,REVERSE(SUBSTRING([RowLog Contents 0], 2 + 1, 2)))) + 1, 2))))/8.0)) as [NullBitMapLength]
 
--[Null Bytes] = Substring (RowLog content 0, Status Bit A+ Status Bit B + [Fixed Length Data] +1, [NullBitMapLength] )
,SUBSTRING([RowLog Contents 0], CONVERT(SMALLINT, CONVERT(BINARY(2), REVERSE(SUBSTRING([RowLog Contents 0], 2 + 1, 2)))) + 3,
CONVERT(INT, ceiling(CONVERT(INT, CONVERT(BINARY(2), REVERSE(SUBSTRING([RowLog Contents 0], CONVERT(SMALLINT, CONVERT(BINARY(2)
,REVERSE(SUBSTRING([RowLog Contents 0], 2 + 1, 2)))) + 1, 2))))/8.0))) as [NullBytes]
 
--[TotalNoofVarCols] = Substring (RowLog content 0, Status Bit A+ Status Bit B + [Fixed Length Data] +1, [Null Bitmap length] + 2 )
,(CASE WHEN SUBSTRING([RowLog Contents 0], 1, 1) In (0x30,0x70) THEN
CONVERT(INT, CONVERT(BINARY(2), REVERSE(SUBSTRING([RowLog Contents 0],
CONVERT(SMALLINT, CONVERT(BINARY(2), REVERSE(SUBSTRING([RowLog Contents 0], 2 + 1, 2)))) + 3
+ CONVERT(INT, ceiling(CONVERT(INT, CONVERT(BINARY(2), REVERSE(SUBSTRING([RowLog Contents 0], CONVERT(SMALLINT, CONVERT(BINARY(2)
,REVERSE(SUBSTRING([RowLog Contents 0], 2 + 1, 2)))) + 1, 2))))/8.0)), 2))))  ELSE null  END) AS [TotalNoofVarCols]
 
--[ColumnOffsetArray]= Substring (RowLog content 0, Status Bit A+ Status Bit B + [Fixed Length Data] +1, [Null Bitmap length] + 2 , [TotalNoofVarCols]*2 )
,(CASE WHEN SUBSTRING([RowLog Contents 0], 1, 1) In (0x30,0x70) THEN
SUBSTRING([RowLog Contents 0]
, CONVERT(SMALLINT, CONVERT(BINARY(2), REVERSE(SUBSTRING([RowLog Contents 0], 2 + 1, 2)))) + 3
+ CONVERT(INT, ceiling(CONVERT(INT, CONVERT(BINARY(2), REVERSE(SUBSTRING([RowLog Contents 0], CONVERT(SMALLINT, CONVERT(BINARY(2)
,REVERSE(SUBSTRING([RowLog Contents 0], 2 + 1, 2)))) + 1, 2))))/8.0)) + 2
, (CASE WHEN SUBSTRING([RowLog Contents 0], 1, 1) In (0x30,0x70) THEN
CONVERT(INT, CONVERT(BINARY(2), REVERSE(SUBSTRING([RowLog Contents 0],
CONVERT(SMALLINT, CONVERT(BINARY(2), REVERSE(SUBSTRING([RowLog Contents 0], 2 + 1, 2)))) + 3
+ CONVERT(INT, ceiling(CONVERT(INT, CONVERT(BINARY(2), REVERSE(SUBSTRING([RowLog Contents 0], CONVERT(SMALLINT, CONVERT(BINARY(2)
,REVERSE(SUBSTRING([RowLog Contents 0], 2 + 1, 2)))) + 1, 2))))/8.0)), 2))))  ELSE null  END)
* 2)  ELSE null  END) AS [ColumnOffsetArray]
 
--  Variable column Start = Status Bit A+ Status Bit B + [Fixed Length Data] + [Null Bitmap length] + 2+([TotalNoofVarCols]*2)
,CASE WHEN SUBSTRING([RowLog Contents 0], 1, 1)In (0x30,0x70)
THEN  (
CONVERT(SMALLINT, CONVERT(BINARY(2), REVERSE(SUBSTRING([RowLog Contents 0], 2 + 1, 2)))) + 4
 
+ CONVERT(INT, ceiling(CONVERT(INT, CONVERT(BINARY(2), REVERSE(SUBSTRING([RowLog Contents 0], CONVERT(SMALLINT, CONVERT(BINARY(2)
,REVERSE(SUBSTRING([RowLog Contents 0], 2 + 1, 2)))) + 1, 2))))/8.0))
 
+ ((CASE WHEN SUBSTRING([RowLog Contents 0], 1, 1) In (0x30,0x70) THEN
CONVERT(INT, CONVERT(BINARY(2), REVERSE(SUBSTRING([RowLog Contents 0],
CONVERT(SMALLINT, CONVERT(BINARY(2), REVERSE(SUBSTRING([RowLog Contents 0], 2 + 1, 2)))) + 3
+ CONVERT(INT, ceiling(CONVERT(INT, CONVERT(BINARY(2), REVERSE(SUBSTRING([RowLog Contents 0], CONVERT(SMALLINT, CONVERT(BINARY(2)
,REVERSE(SUBSTRING([RowLog Contents 0], 2 + 1, 2)))) + 1, 2))))/8.0)), 2))))  ELSE null  END) * 2))
 
ELSE null End AS [VarColumnStart]
 
FROM    sys.fn_dblog(NULL, NULL)
WHERE    AllocUnitName ='' + @SchemaName_n_TableName + '' --'dbo.Student'
AND Context IN ('LCX_MARK_AS_GHOST', 'LCX_HEAP') AND Operation in ('LOP_DELETE_ROWS')
And SUBSTRING([RowLog Contents 0], 1, 1)In (0x30,0x70)
 
/*Use this subquery to filter the date*/
AND [TRANSACTION ID] IN (Select DISTINCT [TRANSACTION ID] FROM    sys.fn_dblog(NULL, NULL)
Where Context IN ('LCX_NULL') AND Operation in ('LOP_BEGIN_XACT')
And [Transaction Name]='DELETE'
And  CONVERT(NVARCHAR(11),[Begin Time]) BETWEEN @Date_From AND @Date_To)),
 
--Use this technique to repeate the row till the no of bytes of the row.
N1 (n) AS (SELECT 1 UNION ALL SELECT 1),
N2 (n) AS (SELECT 1 FROM N1 AS X, N1 AS Y),
N3 (n) AS (SELECT 1 FROM N2 AS X, N2 AS Y),
N4 (n) AS (SELECT ROW_NUMBER() OVER(ORDER BY X.n)
           FROM N3 AS X, N3 AS Y)
 
insert into @DeletedRecords
Select  RowLogContents
        ,[AllocUnitID]
        ,[Transaction ID]
        ,[FixedLengthData]
        ,[TotalNoOfCols]
        ,[NullBitMapLength]
        ,[NullBytes]
        ,[TotalNoofVarCols]
        ,[ColumnOffsetArray]
        ,[VarColumnStart]
         ---Get the Null value against each column (1 means null zero means not null)
        ,[NullBitMap]=(REPLACE(STUFF((SELECT ',' +
        (CASE WHEN [ID]=0 THEN CONVERT(NVARCHAR(1),(SUBSTRING(NullBytes, n, 1) % 2))  ELSE CONVERT(NVARCHAR(1),((SUBSTRING(NullBytes, n, 1) / [Bitvalue]) % 2)) END) --as [nullBitMap]
FROM N4 AS Nums
Join RowData AS C ON n<=NullBitMapLength
Cross Join @bitTable WHERE C.[RowLogContents]=D.[RowLogContents] ORDER BY [RowLogContents],n ASC FOR XML PATH('')),1,1,''),',',''))
FROM RowData D
 
CREATE TABLE [#temp_Data]
(
    [FieldName]  VARCHAR(MAX),
    [FieldValue] VARCHAR(MAX),
    [Rowlogcontents] VARBINARY(8000)
 
)
--Create common table expression and join it with the rowdata table
-- to get each column details
;With CTE AS (
/*This part is for variable data columns*/
SELECT Rowlogcontents,
NAME ,
cols.leaf_null_bit AS nullbit,
leaf_offset,
ISNULL(syscolumns.length, cols.max_length) AS [length],
cols.system_type_id,
cols.leaf_bit_position AS bitpos,
ISNULL(syscolumns.xprec, cols.precision) AS xprec,
ISNULL(syscolumns.xscale, cols.scale) AS xscale,
SUBSTRING([nullBitMap], cols.leaf_null_bit, 1) AS is_null,
--Calculate the variable column size from the variable column offset array
(CASE WHEN leaf_offset<1 and SUBSTRING([nullBitMap], cols.leaf_null_bit, 1)=0 THEN
CONVERT(SMALLINT, CONVERT(BINARY(2), REVERSE (SUBSTRING ([ColumnOffsetArray], (2 * leaf_offset*-1) - 1, 2)))) ELSE 0 END) AS [Column value Size],
 
---Calculate the column length
(CASE WHEN leaf_offset<1 and SUBSTRING([nullBitMap], cols.leaf_null_bit, 1)=0 THEN  CONVERT(SMALLINT, CONVERT(BINARY(2), REVERSE (SUBSTRING ([ColumnOffsetArray], (2 * (leaf_offset*-1)) - 1, 2))))
- ISNULL(NULLIF(CONVERT(SMALLINT, CONVERT(BINARY(2), REVERSE (SUBSTRING ([ColumnOffsetArray], (2 * ((leaf_offset*-1) - 1)) - 1, 2)))), 0), [varColumnStart])
ELSE 0 END) AS [Column Length]
 
--Get the Hexa decimal value from the RowlogContent
--HexValue of the variable column=Substring([Column value Size] - [Column Length] + 1,[Column Length])
--This is the data of your column but in the Hexvalue
,CASE WHEN SUBSTRING([nullBitMap], cols.leaf_null_bit, 1)=1 THEN NULL ELSE
SUBSTRING(Rowlogcontents,((CASE WHEN leaf_offset<1 and SUBSTRING([nullBitMap], cols.leaf_null_bit, 1)=0 THEN CONVERT(SMALLINT, CONVERT(BINARY(2), REVERSE (SUBSTRING ([ColumnOffsetArray], (2 * leaf_offset*-1) - 1, 2)))) ELSE 0 END)
- ((CASE WHEN leaf_offset<1 and SUBSTRING([nullBitMap], cols.leaf_null_bit, 1)=0 THEN  CONVERT(SMALLINT, CONVERT(BINARY(2), REVERSE (SUBSTRING ([ColumnOffsetArray], (2 * (leaf_offset*-1)) - 1, 2))))
- ISNULL(NULLIF(CONVERT(SMALLINT, CONVERT(BINARY(2), REVERSE (SUBSTRING ([ColumnOffsetArray], (2 * ((leaf_offset*-1) - 1)) - 1, 2)))), 0), [varColumnStart])
ELSE 0 END))) + 1,((CASE WHEN leaf_offset<1 and SUBSTRING([nullBitMap], cols.leaf_null_bit, 1)=0 THEN  CONVERT(SMALLINT, CONVERT(BINARY(2), REVERSE (SUBSTRING ([ColumnOffsetArray], (2 * (leaf_offset*-1)) - 1, 2))))
- ISNULL(NULLIF(CONVERT(SMALLINT, CONVERT(BINARY(2), REVERSE (SUBSTRING ([ColumnOffsetArray], (2 * ((leaf_offset*-1) - 1)) - 1, 2)))), 0), [varColumnStart])
ELSE 0 END))) END AS hex_Value
 
FROM @DeletedRecords A
Inner Join sys.allocation_units allocunits On A.[AllocUnitId]=allocunits.[Allocation_Unit_Id]
INNER JOIN sys.partitions partitions ON (allocunits.type IN (1, 3)
AND partitions.hobt_id = allocunits.container_id) OR (allocunits.type = 2 AND partitions.partition_id = allocunits.container_id)
INNER JOIN sys.system_internals_partition_columns cols ON cols.partition_id = partitions.partition_id
LEFT OUTER JOIN syscolumns ON syscolumns.id = partitions.object_id AND syscolumns.colid = cols.partition_column_id
WHERE leaf_offset<0
 
UNION
/*This part is for fixed data columns*/
SELECT  Rowlogcontents,
NAME ,
cols.leaf_null_bit AS nullbit,
leaf_offset,
ISNULL(syscolumns.length, cols.max_length) AS [length],
cols.system_type_id,
cols.leaf_bit_position AS bitpos,
ISNULL(syscolumns.xprec, cols.precision) AS xprec,
ISNULL(syscolumns.xscale, cols.scale) AS xscale,
SUBSTRING([nullBitMap], cols.leaf_null_bit, 1) AS is_null,
(SELECT TOP 1 ISNULL(SUM(CASE WHEN C.leaf_offset >1 THEN max_length ELSE 0 END),0) FROM
sys.system_internals_partition_columns C WHERE cols.partition_id =C.partition_id And C.leaf_null_bit<cols.leaf_null_bit)+5 AS [Column value Size],
syscolumns.length AS [Column Length]
 
,CASE WHEN SUBSTRING([nullBitMap], cols.leaf_null_bit, 1)=1 THEN NULL ELSE
SUBSTRING
(
Rowlogcontents,(SELECT TOP 1 ISNULL(SUM(CASE WHEN C.leaf_offset >1 THEN max_length ELSE 0 END),0) FROM
sys.system_internals_partition_columns C where cols.partition_id =C.partition_id And C.leaf_null_bit<cols.leaf_null_bit)+5
,syscolumns.length) END AS hex_Value
 
FROM @DeletedRecords A
Inner Join sys.allocation_units allocunits ON A.[AllocUnitId]=allocunits.[Allocation_Unit_Id]
INNER JOIN sys.partitions partitions ON (allocunits.type IN (1, 3)
 AND partitions.hobt_id = allocunits.container_id) OR (allocunits.type = 2 AND partitions.partition_id = allocunits.container_id)
INNER JOIN sys.system_internals_partition_columns cols ON cols.partition_id = partitions.partition_id
LEFT OUTER JOIN syscolumns ON syscolumns.id = partitions.object_id AND syscolumns.colid = cols.partition_column_id
WHERE leaf_offset>0 )
 
--Converting data from Hexvalue to its orgional datatype.
--Implemented datatype conversion mechanism for each datatype
 
INSERT INTO #temp_Data
SELECT NAME,
CASE
 WHEN system_type_id IN (231, 239) THEN  LTRIM(RTRIM(CONVERT(NVARCHAR(max),hex_Value)))  --NVARCHAR ,NCHAR
 WHEN system_type_id IN (167,175) THEN  LTRIM(RTRIM(CONVERT(VARCHAR(max),REPLACE(hex_Value, 0x00, 0x20))))  --VARCHAR,CHAR
 WHEN system_type_id = 48 THEN CONVERT(VARCHAR(MAX), CONVERT(TINYINT, CONVERT(BINARY(1), REVERSE (hex_Value)))) --TINY INTEGER
 WHEN system_type_id = 52 THEN CONVERT(VARCHAR(MAX), CONVERT(SMALLINT, CONVERT(BINARY(2), REVERSE (hex_Value)))) --SMALL INTEGER
 WHEN system_type_id = 56 THEN CONVERT(VARCHAR(MAX), CONVERT(INT, CONVERT(BINARY(4), REVERSE(hex_Value)))) -- INTEGER
 WHEN system_type_id = 127 THEN CONVERT(VARCHAR(MAX), CONVERT(BIGINT, CONVERT(BINARY(8), REVERSE(hex_Value))))-- BIG INTEGER
 WHEN system_type_id = 61 Then CONVERT(VARCHAR(MAX),CONVERT(DATETIME,CONVERT(VARBINARY(8000),REVERSE (hex_Value))),100) --DATETIME
 WHEN system_type_id =58 Then CONVERT(VARCHAR(MAX),CONVERT(SMALLDATETIME,CONVERT(VARBINARY(8000),REVERSE(hex_Value))),100) --SMALL DATETIME
 WHEN system_type_id = 108 THEN CONVERT(VARCHAR(MAX), CAST(CONVERT(NUMERIC(38,30), CONVERT(VARBINARY,CONVERT(VARBINARY,xprec)+CONVERT(VARBINARY,xscale))+CONVERT(VARBINARY(1),0) + hex_Value) as FLOAT)) --- NUMERIC
 WHEN system_type_id In(60,122) THEN CONVERT(VARCHAR(MAX),Convert(MONEY,Convert(VARBINARY(8000),Reverse(hex_Value))),2) --MONEY,SMALLMONEY
 WHEN system_type_id =106 THEN CONVERT(VARCHAR(MAX), CAST(CONVERT(Decimal(38,34), CONVERT(VARBINARY,Convert(VARBINARY,xprec)+CONVERT(VARBINARY,xscale))+CONVERT(VARBINARY(1),0) + hex_Value) as FLOAT)) --- DECIMAL
 WHEN system_type_id = 104 THEN CONVERT(VARCHAR(MAX),CONVERT (BIT,CONVERT(BINARY(1), hex_Value)%2))  -- BIT
 WHEN system_type_id =62 THEN  RTRIM(LTRIM(STR(CONVERT(FLOAT,SIGN(CAST(CONVERT(VARBINARY(8000),Reverse(hex_Value)) AS BIGINT)) * (1.0 + (CAST(CONVERT(VARBINARY(8000),Reverse(hex_Value)) AS BIGINT) & 0x000FFFFFFFFFFFFF) * POWER(CAST(2 AS FLOAT), -52)) * POWER(CAST(2 AS FLOAT),((CAST(CONVERT(VARBINARY(8000),Reverse(hex_Value)) AS BIGINT) & 0x7ff0000000000000) / EXP(52 * LOG(2))-1023))),53,LEN(hex_Value)))) --- FLOAT
 When system_type_id =59 THEN  Left(LTRIM(STR(CAST(SIGN(CAST(Convert(VARBINARY(8000),REVERSE(hex_Value)) AS BIGINT))* (1.0 + (CAST(CONVERT(VARBINARY(8000),Reverse(hex_Value)) AS BIGINT) & 0x007FFFFF) * POWER(CAST(2 AS Real), -23)) * POWER(CAST(2 AS Real),(((CAST(CONVERT(VARBINARY(8000),Reverse(hex_Value)) AS INT) )& 0x7f800000)/ EXP(23 * LOG(2))-127))AS REAL),23,23)),8) --Real
 WHEN system_type_id In (165,173) THEN (CASE WHEN CHARINDEX(0x,cast('' AS XML).value('xs:hexBinary(sql:column("hex_Value"))', 'VARBINARY(8000)')) = 0 THEN '0x' ELSE '' END) +cast('' AS XML).value('xs:hexBinary(sql:column("hex_Value"))', 'varchar(max)') -- BINARY,VARBINARY
 WHEN system_type_id =36 THEN CONVERT(VARCHAR(MAX),CONVERT(UNIQUEIDENTIFIER,hex_Value)) --UNIQUEIDENTIFIER
 
 END AS FieldValue
,[Rowlogcontents]
FROM CTE ORDER BY nullbit
 
--Create the column name in the same order to do pivot table.
 
SET @SQL = STUFF(
(
SELECT ', (Case when [FieldName]=''' + CAST([Name] AS VARCHAR(MAX)) + ''' Then [FieldValue] else NULL End) AS ' + CAST(QUOTENAME([Name]) AS VARCHAR(MAX))  FROM syscolumns WHERE id=object_id('' + @SchemaName_n_TableName + '')
FOR XML PATH('')), 1, 1, '')
 
DECLARE @FieldNameMin VARCHAR(max)
SET @FieldNameMin = STUFF(
(
SELECT ', Min(' + CAST(QUOTENAME([Name]) AS VARCHAR(MAX)) + ') AS ' + CAST(QUOTENAME([Name]) AS VARCHAR(MAX)) FROM syscolumns WHERE id=object_id('' + @SchemaName_n_TableName + '')
FOR XML PATH('')), 1, 1, '')
 
--Finally did pivot table and get the data back in the same format.
 
Set @SQL=';WITH CTE AS (SELECT ' + @SQL + ',[Rowlogcontents] as [Rowlogcontents] FROM #temp_Data) SELECT ' + @FieldNameMin + ' FROM CTE GROUP BY [Rowlogcontents]'
 
EXEC  (@SQL)
 
GO
--Execute the procedure like
--Recover_Deleted_Data_Proc 'Schema.table name'
--EXAMPLE #1 : FOR ALL DELETED RECORDS
EXEC Recover_Deleted_Data_Proc 'dbo.Student'
 
--EXAMPLE #2 : FOR ANY SPECIFIC DATE RANGE
EXEC Recover_Deleted_Data_Proc 'dbo.Student' ,'2020/12/23','2020/12/25'
 
--It will give you the result of all deleted records.