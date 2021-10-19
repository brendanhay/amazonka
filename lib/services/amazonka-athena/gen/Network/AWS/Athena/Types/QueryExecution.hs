{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.QueryExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.QueryExecution where

import Network.AWS.Athena.Types.EngineVersion
import Network.AWS.Athena.Types.QueryExecutionContext
import Network.AWS.Athena.Types.QueryExecutionStatistics
import Network.AWS.Athena.Types.QueryExecutionStatus
import Network.AWS.Athena.Types.ResultConfiguration
import Network.AWS.Athena.Types.StatementType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a single instance of a query execution.
--
-- /See:/ 'newQueryExecution' smart constructor.
data QueryExecution = QueryExecution'
  { -- | The engine version that executed the query.
    engineVersion :: Prelude.Maybe EngineVersion,
    -- | The completion date, current state, submission time, and state change
    -- reason (if applicable) for the query execution.
    status :: Prelude.Maybe QueryExecutionStatus,
    -- | The database in which the query execution occurred.
    queryExecutionContext :: Prelude.Maybe QueryExecutionContext,
    -- | The location in Amazon S3 where query results were stored and the
    -- encryption option, if any, used for query results. These are known as
    -- \"client-side settings\". If workgroup settings override client-side
    -- settings, then the query uses the location for the query results and the
    -- encryption configuration that are specified for the workgroup.
    resultConfiguration :: Prelude.Maybe ResultConfiguration,
    -- | The SQL query statements which the query execution ran.
    query :: Prelude.Maybe Prelude.Text,
    -- | The type of query statement that was run. @DDL@ indicates DDL query
    -- statements. @DML@ indicates DML (Data Manipulation Language) query
    -- statements, such as @CREATE TABLE AS SELECT@. @UTILITY@ indicates query
    -- statements other than DDL and DML, such as @SHOW CREATE TABLE@, or
    -- @DESCRIBE TABLE@.
    statementType :: Prelude.Maybe StatementType,
    -- | Query execution statistics, such as the amount of data scanned, the
    -- amount of time that the query took to process, and the type of statement
    -- that was run.
    statistics :: Prelude.Maybe QueryExecutionStatistics,
    -- | The unique identifier for each query execution.
    queryExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The name of the workgroup in which the query ran.
    workGroup :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'queryExecution_engineVersion' - The engine version that executed the query.
--
-- 'status', 'queryExecution_status' - The completion date, current state, submission time, and state change
-- reason (if applicable) for the query execution.
--
-- 'queryExecutionContext', 'queryExecution_queryExecutionContext' - The database in which the query execution occurred.
--
-- 'resultConfiguration', 'queryExecution_resultConfiguration' - The location in Amazon S3 where query results were stored and the
-- encryption option, if any, used for query results. These are known as
-- \"client-side settings\". If workgroup settings override client-side
-- settings, then the query uses the location for the query results and the
-- encryption configuration that are specified for the workgroup.
--
-- 'query', 'queryExecution_query' - The SQL query statements which the query execution ran.
--
-- 'statementType', 'queryExecution_statementType' - The type of query statement that was run. @DDL@ indicates DDL query
-- statements. @DML@ indicates DML (Data Manipulation Language) query
-- statements, such as @CREATE TABLE AS SELECT@. @UTILITY@ indicates query
-- statements other than DDL and DML, such as @SHOW CREATE TABLE@, or
-- @DESCRIBE TABLE@.
--
-- 'statistics', 'queryExecution_statistics' - Query execution statistics, such as the amount of data scanned, the
-- amount of time that the query took to process, and the type of statement
-- that was run.
--
-- 'queryExecutionId', 'queryExecution_queryExecutionId' - The unique identifier for each query execution.
--
-- 'workGroup', 'queryExecution_workGroup' - The name of the workgroup in which the query ran.
newQueryExecution ::
  QueryExecution
newQueryExecution =
  QueryExecution'
    { engineVersion = Prelude.Nothing,
      status = Prelude.Nothing,
      queryExecutionContext = Prelude.Nothing,
      resultConfiguration = Prelude.Nothing,
      query = Prelude.Nothing,
      statementType = Prelude.Nothing,
      statistics = Prelude.Nothing,
      queryExecutionId = Prelude.Nothing,
      workGroup = Prelude.Nothing
    }

-- | The engine version that executed the query.
queryExecution_engineVersion :: Lens.Lens' QueryExecution (Prelude.Maybe EngineVersion)
queryExecution_engineVersion = Lens.lens (\QueryExecution' {engineVersion} -> engineVersion) (\s@QueryExecution' {} a -> s {engineVersion = a} :: QueryExecution)

-- | The completion date, current state, submission time, and state change
-- reason (if applicable) for the query execution.
queryExecution_status :: Lens.Lens' QueryExecution (Prelude.Maybe QueryExecutionStatus)
queryExecution_status = Lens.lens (\QueryExecution' {status} -> status) (\s@QueryExecution' {} a -> s {status = a} :: QueryExecution)

-- | The database in which the query execution occurred.
queryExecution_queryExecutionContext :: Lens.Lens' QueryExecution (Prelude.Maybe QueryExecutionContext)
queryExecution_queryExecutionContext = Lens.lens (\QueryExecution' {queryExecutionContext} -> queryExecutionContext) (\s@QueryExecution' {} a -> s {queryExecutionContext = a} :: QueryExecution)

-- | The location in Amazon S3 where query results were stored and the
-- encryption option, if any, used for query results. These are known as
-- \"client-side settings\". If workgroup settings override client-side
-- settings, then the query uses the location for the query results and the
-- encryption configuration that are specified for the workgroup.
queryExecution_resultConfiguration :: Lens.Lens' QueryExecution (Prelude.Maybe ResultConfiguration)
queryExecution_resultConfiguration = Lens.lens (\QueryExecution' {resultConfiguration} -> resultConfiguration) (\s@QueryExecution' {} a -> s {resultConfiguration = a} :: QueryExecution)

-- | The SQL query statements which the query execution ran.
queryExecution_query :: Lens.Lens' QueryExecution (Prelude.Maybe Prelude.Text)
queryExecution_query = Lens.lens (\QueryExecution' {query} -> query) (\s@QueryExecution' {} a -> s {query = a} :: QueryExecution)

-- | The type of query statement that was run. @DDL@ indicates DDL query
-- statements. @DML@ indicates DML (Data Manipulation Language) query
-- statements, such as @CREATE TABLE AS SELECT@. @UTILITY@ indicates query
-- statements other than DDL and DML, such as @SHOW CREATE TABLE@, or
-- @DESCRIBE TABLE@.
queryExecution_statementType :: Lens.Lens' QueryExecution (Prelude.Maybe StatementType)
queryExecution_statementType = Lens.lens (\QueryExecution' {statementType} -> statementType) (\s@QueryExecution' {} a -> s {statementType = a} :: QueryExecution)

-- | Query execution statistics, such as the amount of data scanned, the
-- amount of time that the query took to process, and the type of statement
-- that was run.
queryExecution_statistics :: Lens.Lens' QueryExecution (Prelude.Maybe QueryExecutionStatistics)
queryExecution_statistics = Lens.lens (\QueryExecution' {statistics} -> statistics) (\s@QueryExecution' {} a -> s {statistics = a} :: QueryExecution)

-- | The unique identifier for each query execution.
queryExecution_queryExecutionId :: Lens.Lens' QueryExecution (Prelude.Maybe Prelude.Text)
queryExecution_queryExecutionId = Lens.lens (\QueryExecution' {queryExecutionId} -> queryExecutionId) (\s@QueryExecution' {} a -> s {queryExecutionId = a} :: QueryExecution)

-- | The name of the workgroup in which the query ran.
queryExecution_workGroup :: Lens.Lens' QueryExecution (Prelude.Maybe Prelude.Text)
queryExecution_workGroup = Lens.lens (\QueryExecution' {workGroup} -> workGroup) (\s@QueryExecution' {} a -> s {workGroup = a} :: QueryExecution)

instance Core.FromJSON QueryExecution where
  parseJSON =
    Core.withObject
      "QueryExecution"
      ( \x ->
          QueryExecution'
            Prelude.<$> (x Core..:? "EngineVersion")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "QueryExecutionContext")
            Prelude.<*> (x Core..:? "ResultConfiguration")
            Prelude.<*> (x Core..:? "Query")
            Prelude.<*> (x Core..:? "StatementType")
            Prelude.<*> (x Core..:? "Statistics")
            Prelude.<*> (x Core..:? "QueryExecutionId")
            Prelude.<*> (x Core..:? "WorkGroup")
      )

instance Prelude.Hashable QueryExecution

instance Prelude.NFData QueryExecution
