SELECT 
	objectName,
	begintime,
	endtime,
	account,
 SUM(LOP_INSERT_ROWS) [Insert],
 SUM(LOP_MODIFY_ROW) [Update],
 SUM(LOP_DELETE_ROWS) [Delete]
FROM
(
	SELECT 
		object_name(p.object_id) objectName, 
		tlog.[Current LSN], 
		T.begintime begintime,
		c.endTime endtime, 
		tlog.Operation operation,
		T.account account
		
	FROM 
		sys.objects so
		inner join sys.partitions p on p.object_id=so.object_id
		inner join sys.system_internals_allocation_units AU on p.partition_id=AU.container_id
		inner join(
			select [Current LSN], [transaction ID] tranID,[end time] endTime, AllocUnitId, operation, Context
			from ::fn_dbLog(null, null)
			where (operation in ('LOP_INSERT_ROWS', 'LOP_MODIFY_ROW', 'LOP_DELETE_ROWS'))
			)tlog on tlog.AllocUnitId=AU.allocation_unit_id
		inner join 
		(
		   select [Transaction ID] tranID, [End Time] endTime
		   from ::fn_dbLog(null, null)
		   where Operation = 'LOP_COMMIT_XACT' and [End Time]>=DateADD(mi, -3000, Current_TimeStamp)
	   ) c on tlog.tranID = c.tranID
	   inner join
	   (
		   SELECT
			[Transaction ID] tranID,
			SUSER_SNAME ([Transaction SID]) AS account, [Begin Time] as begintime
			FROM fn_dblog (NULL, NULL)
			WHERE [Operation] = N'LOP_BEGIN_XACT') T
			on tlog.tranID = T.tranID
	WHERE
			so.type='U'
)
X
pivot
(
  COUNT(operation)
  FOR operation in (LOP_INSERT_ROWS, LOP_MODIFY_ROW,LOP_DELETE_ROWS,LOP_ABORT_XACT,LOP_COMMIT_XACT)
)p
GROUP BY objectName,begintime,endtime,account