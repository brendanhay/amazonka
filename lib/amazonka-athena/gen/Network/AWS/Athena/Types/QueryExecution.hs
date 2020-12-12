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
    qeStatus,
    qeQueryExecutionContext,
    qeResultConfiguration,
    qeQuery,
    qeStatementType,
    qeStatistics,
    qeQueryExecutionId,
    qeWorkGroup,
  )
where

import Network.AWS.Athena.Types.QueryExecutionContext
import Network.AWS.Athena.Types.QueryExecutionStatistics
import Network.AWS.Athena.Types.QueryExecutionStatus
import Network.AWS.Athena.Types.ResultConfiguration
import Network.AWS.Athena.Types.StatementType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a single instance of a query execution.
--
-- /See:/ 'mkQueryExecution' smart constructor.
data QueryExecution = QueryExecution'
  { status ::
      Lude.Maybe QueryExecutionStatus,
    queryExecutionContext :: Lude.Maybe QueryExecutionContext,
    resultConfiguration :: Lude.Maybe ResultConfiguration,
    query :: Lude.Maybe Lude.Text,
    statementType :: Lude.Maybe StatementType,
    statistics :: Lude.Maybe QueryExecutionStatistics,
    queryExecutionId :: Lude.Maybe Lude.Text,
    workGroup :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QueryExecution' with the minimum fields required to make a request.
--
-- * 'query' - The SQL query statements which the query execution ran.
-- * 'queryExecutionContext' - The database in which the query execution occurred.
-- * 'queryExecutionId' - The unique identifier for each query execution.
-- * 'resultConfiguration' - The location in Amazon S3 where query results were stored and the encryption option, if any, used for query results. These are known as "client-side settings". If workgroup settings override client-side settings, then the query uses the location for the query results and the encryption configuration that are specified for the workgroup.
-- * 'statementType' - The type of query statement that was run. @DDL@ indicates DDL query statements. @DML@ indicates DML (Data Manipulation Language) query statements, such as @CREATE TABLE AS SELECT@ . @UTILITY@ indicates query statements other than DDL and DML, such as @SHOW CREATE TABLE@ , or @DESCRIBE <table>@ .
-- * 'statistics' - Query execution statistics, such as the amount of data scanned, the amount of time that the query took to process, and the type of statement that was run.
-- * 'status' - The completion date, current state, submission time, and state change reason (if applicable) for the query execution.
-- * 'workGroup' - The name of the workgroup in which the query ran.
mkQueryExecution ::
  QueryExecution
mkQueryExecution =
  QueryExecution'
    { status = Lude.Nothing,
      queryExecutionContext = Lude.Nothing,
      resultConfiguration = Lude.Nothing,
      query = Lude.Nothing,
      statementType = Lude.Nothing,
      statistics = Lude.Nothing,
      queryExecutionId = Lude.Nothing,
      workGroup = Lude.Nothing
    }

-- | The completion date, current state, submission time, and state change reason (if applicable) for the query execution.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qeStatus :: Lens.Lens' QueryExecution (Lude.Maybe QueryExecutionStatus)
qeStatus = Lens.lens (status :: QueryExecution -> Lude.Maybe QueryExecutionStatus) (\s a -> s {status = a} :: QueryExecution)
{-# DEPRECATED qeStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The database in which the query execution occurred.
--
-- /Note:/ Consider using 'queryExecutionContext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qeQueryExecutionContext :: Lens.Lens' QueryExecution (Lude.Maybe QueryExecutionContext)
qeQueryExecutionContext = Lens.lens (queryExecutionContext :: QueryExecution -> Lude.Maybe QueryExecutionContext) (\s a -> s {queryExecutionContext = a} :: QueryExecution)
{-# DEPRECATED qeQueryExecutionContext "Use generic-lens or generic-optics with 'queryExecutionContext' instead." #-}

-- | The location in Amazon S3 where query results were stored and the encryption option, if any, used for query results. These are known as "client-side settings". If workgroup settings override client-side settings, then the query uses the location for the query results and the encryption configuration that are specified for the workgroup.
--
-- /Note:/ Consider using 'resultConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qeResultConfiguration :: Lens.Lens' QueryExecution (Lude.Maybe ResultConfiguration)
qeResultConfiguration = Lens.lens (resultConfiguration :: QueryExecution -> Lude.Maybe ResultConfiguration) (\s a -> s {resultConfiguration = a} :: QueryExecution)
{-# DEPRECATED qeResultConfiguration "Use generic-lens or generic-optics with 'resultConfiguration' instead." #-}

-- | The SQL query statements which the query execution ran.
--
-- /Note:/ Consider using 'query' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qeQuery :: Lens.Lens' QueryExecution (Lude.Maybe Lude.Text)
qeQuery = Lens.lens (query :: QueryExecution -> Lude.Maybe Lude.Text) (\s a -> s {query = a} :: QueryExecution)
{-# DEPRECATED qeQuery "Use generic-lens or generic-optics with 'query' instead." #-}

-- | The type of query statement that was run. @DDL@ indicates DDL query statements. @DML@ indicates DML (Data Manipulation Language) query statements, such as @CREATE TABLE AS SELECT@ . @UTILITY@ indicates query statements other than DDL and DML, such as @SHOW CREATE TABLE@ , or @DESCRIBE <table>@ .
--
-- /Note:/ Consider using 'statementType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qeStatementType :: Lens.Lens' QueryExecution (Lude.Maybe StatementType)
qeStatementType = Lens.lens (statementType :: QueryExecution -> Lude.Maybe StatementType) (\s a -> s {statementType = a} :: QueryExecution)
{-# DEPRECATED qeStatementType "Use generic-lens or generic-optics with 'statementType' instead." #-}

-- | Query execution statistics, such as the amount of data scanned, the amount of time that the query took to process, and the type of statement that was run.
--
-- /Note:/ Consider using 'statistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qeStatistics :: Lens.Lens' QueryExecution (Lude.Maybe QueryExecutionStatistics)
qeStatistics = Lens.lens (statistics :: QueryExecution -> Lude.Maybe QueryExecutionStatistics) (\s a -> s {statistics = a} :: QueryExecution)
{-# DEPRECATED qeStatistics "Use generic-lens or generic-optics with 'statistics' instead." #-}

-- | The unique identifier for each query execution.
--
-- /Note:/ Consider using 'queryExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qeQueryExecutionId :: Lens.Lens' QueryExecution (Lude.Maybe Lude.Text)
qeQueryExecutionId = Lens.lens (queryExecutionId :: QueryExecution -> Lude.Maybe Lude.Text) (\s a -> s {queryExecutionId = a} :: QueryExecution)
{-# DEPRECATED qeQueryExecutionId "Use generic-lens or generic-optics with 'queryExecutionId' instead." #-}

-- | The name of the workgroup in which the query ran.
--
-- /Note:/ Consider using 'workGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qeWorkGroup :: Lens.Lens' QueryExecution (Lude.Maybe Lude.Text)
qeWorkGroup = Lens.lens (workGroup :: QueryExecution -> Lude.Maybe Lude.Text) (\s a -> s {workGroup = a} :: QueryExecution)
{-# DEPRECATED qeWorkGroup "Use generic-lens or generic-optics with 'workGroup' instead." #-}

instance Lude.FromJSON QueryExecution where
  parseJSON =
    Lude.withObject
      "QueryExecution"
      ( \x ->
          QueryExecution'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "QueryExecutionContext")
            Lude.<*> (x Lude..:? "ResultConfiguration")
            Lude.<*> (x Lude..:? "Query")
            Lude.<*> (x Lude..:? "StatementType")
            Lude.<*> (x Lude..:? "Statistics")
            Lude.<*> (x Lude..:? "QueryExecutionId")
            Lude.<*> (x Lude..:? "WorkGroup")
      )
