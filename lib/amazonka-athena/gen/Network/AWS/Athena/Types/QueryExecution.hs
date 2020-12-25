{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.QueryExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.QueryExecution
  ( QueryExecution (..),

    -- * Smart constructor
    mkQueryExecution,

    -- * Lenses
    qeQuery,
    qeQueryExecutionContext,
    qeQueryExecutionId,
    qeResultConfiguration,
    qeStatementType,
    qeStatistics,
    qeStatus,
    qeWorkGroup,
  )
where

import qualified Network.AWS.Athena.Types.Query as Types
import qualified Network.AWS.Athena.Types.QueryExecutionContext as Types
import qualified Network.AWS.Athena.Types.QueryExecutionId as Types
import qualified Network.AWS.Athena.Types.QueryExecutionStatistics as Types
import qualified Network.AWS.Athena.Types.QueryExecutionStatus as Types
import qualified Network.AWS.Athena.Types.ResultConfiguration as Types
import qualified Network.AWS.Athena.Types.StatementType as Types
import qualified Network.AWS.Athena.Types.WorkGroupName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a single instance of a query execution.
--
-- /See:/ 'mkQueryExecution' smart constructor.
data QueryExecution = QueryExecution'
  { -- | The SQL query statements which the query execution ran.
    query :: Core.Maybe Types.Query,
    -- | The database in which the query execution occurred.
    queryExecutionContext :: Core.Maybe Types.QueryExecutionContext,
    -- | The unique identifier for each query execution.
    queryExecutionId :: Core.Maybe Types.QueryExecutionId,
    -- | The location in Amazon S3 where query results were stored and the encryption option, if any, used for query results. These are known as "client-side settings". If workgroup settings override client-side settings, then the query uses the location for the query results and the encryption configuration that are specified for the workgroup.
    resultConfiguration :: Core.Maybe Types.ResultConfiguration,
    -- | The type of query statement that was run. @DDL@ indicates DDL query statements. @DML@ indicates DML (Data Manipulation Language) query statements, such as @CREATE TABLE AS SELECT@ . @UTILITY@ indicates query statements other than DDL and DML, such as @SHOW CREATE TABLE@ , or @DESCRIBE <table>@ .
    statementType :: Core.Maybe Types.StatementType,
    -- | Query execution statistics, such as the amount of data scanned, the amount of time that the query took to process, and the type of statement that was run.
    statistics :: Core.Maybe Types.QueryExecutionStatistics,
    -- | The completion date, current state, submission time, and state change reason (if applicable) for the query execution.
    status :: Core.Maybe Types.QueryExecutionStatus,
    -- | The name of the workgroup in which the query ran.
    workGroup :: Core.Maybe Types.WorkGroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'QueryExecution' value with any optional fields omitted.
mkQueryExecution ::
  QueryExecution
mkQueryExecution =
  QueryExecution'
    { query = Core.Nothing,
      queryExecutionContext = Core.Nothing,
      queryExecutionId = Core.Nothing,
      resultConfiguration = Core.Nothing,
      statementType = Core.Nothing,
      statistics = Core.Nothing,
      status = Core.Nothing,
      workGroup = Core.Nothing
    }

-- | The SQL query statements which the query execution ran.
--
-- /Note:/ Consider using 'query' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qeQuery :: Lens.Lens' QueryExecution (Core.Maybe Types.Query)
qeQuery = Lens.field @"query"
{-# DEPRECATED qeQuery "Use generic-lens or generic-optics with 'query' instead." #-}

-- | The database in which the query execution occurred.
--
-- /Note:/ Consider using 'queryExecutionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qeQueryExecutionContext :: Lens.Lens' QueryExecution (Core.Maybe Types.QueryExecutionContext)
qeQueryExecutionContext = Lens.field @"queryExecutionContext"
{-# DEPRECATED qeQueryExecutionContext "Use generic-lens or generic-optics with 'queryExecutionContext' instead." #-}

-- | The unique identifier for each query execution.
--
-- /Note:/ Consider using 'queryExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qeQueryExecutionId :: Lens.Lens' QueryExecution (Core.Maybe Types.QueryExecutionId)
qeQueryExecutionId = Lens.field @"queryExecutionId"
{-# DEPRECATED qeQueryExecutionId "Use generic-lens or generic-optics with 'queryExecutionId' instead." #-}

-- | The location in Amazon S3 where query results were stored and the encryption option, if any, used for query results. These are known as "client-side settings". If workgroup settings override client-side settings, then the query uses the location for the query results and the encryption configuration that are specified for the workgroup.
--
-- /Note:/ Consider using 'resultConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qeResultConfiguration :: Lens.Lens' QueryExecution (Core.Maybe Types.ResultConfiguration)
qeResultConfiguration = Lens.field @"resultConfiguration"
{-# DEPRECATED qeResultConfiguration "Use generic-lens or generic-optics with 'resultConfiguration' instead." #-}

-- | The type of query statement that was run. @DDL@ indicates DDL query statements. @DML@ indicates DML (Data Manipulation Language) query statements, such as @CREATE TABLE AS SELECT@ . @UTILITY@ indicates query statements other than DDL and DML, such as @SHOW CREATE TABLE@ , or @DESCRIBE <table>@ .
--
-- /Note:/ Consider using 'statementType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qeStatementType :: Lens.Lens' QueryExecution (Core.Maybe Types.StatementType)
qeStatementType = Lens.field @"statementType"
{-# DEPRECATED qeStatementType "Use generic-lens or generic-optics with 'statementType' instead." #-}

-- | Query execution statistics, such as the amount of data scanned, the amount of time that the query took to process, and the type of statement that was run.
--
-- /Note:/ Consider using 'statistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qeStatistics :: Lens.Lens' QueryExecution (Core.Maybe Types.QueryExecutionStatistics)
qeStatistics = Lens.field @"statistics"
{-# DEPRECATED qeStatistics "Use generic-lens or generic-optics with 'statistics' instead." #-}

-- | The completion date, current state, submission time, and state change reason (if applicable) for the query execution.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qeStatus :: Lens.Lens' QueryExecution (Core.Maybe Types.QueryExecutionStatus)
qeStatus = Lens.field @"status"
{-# DEPRECATED qeStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the workgroup in which the query ran.
--
-- /Note:/ Consider using 'workGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qeWorkGroup :: Lens.Lens' QueryExecution (Core.Maybe Types.WorkGroupName)
qeWorkGroup = Lens.field @"workGroup"
{-# DEPRECATED qeWorkGroup "Use generic-lens or generic-optics with 'workGroup' instead." #-}

instance Core.FromJSON QueryExecution where
  parseJSON =
    Core.withObject "QueryExecution" Core.$
      \x ->
        QueryExecution'
          Core.<$> (x Core..:? "Query")
          Core.<*> (x Core..:? "QueryExecutionContext")
          Core.<*> (x Core..:? "QueryExecutionId")
          Core.<*> (x Core..:? "ResultConfiguration")
          Core.<*> (x Core..:? "StatementType")
          Core.<*> (x Core..:? "Statistics")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "WorkGroup")
