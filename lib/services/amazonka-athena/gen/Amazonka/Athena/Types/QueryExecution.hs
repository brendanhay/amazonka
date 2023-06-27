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
-- Module      : Amazonka.Athena.Types.QueryExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.QueryExecution where

import Amazonka.Athena.Types.EngineVersion
import Amazonka.Athena.Types.QueryExecutionContext
import Amazonka.Athena.Types.QueryExecutionStatistics
import Amazonka.Athena.Types.QueryExecutionStatus
import Amazonka.Athena.Types.ResultConfiguration
import Amazonka.Athena.Types.ResultReuseConfiguration
import Amazonka.Athena.Types.StatementType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a single instance of a query execution.
--
-- /See:/ 'newQueryExecution' smart constructor.
data QueryExecution = QueryExecution'
  { -- | The engine version that executed the query.
    engineVersion :: Prelude.Maybe EngineVersion,
    -- | A list of values for the parameters in a query. The values are applied
    -- sequentially to the parameters in the query in the order in which the
    -- parameters occur. The list of parameters is not returned in the
    -- response.
    executionParameters :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The SQL query statements which the query execution ran.
    query :: Prelude.Maybe Prelude.Text,
    -- | The database in which the query execution occurred.
    queryExecutionContext :: Prelude.Maybe QueryExecutionContext,
    -- | The unique identifier for each query execution.
    queryExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The location in Amazon S3 where query and calculation results are stored
    -- and the encryption option, if any, used for query results. These are
    -- known as \"client-side settings\". If workgroup settings override
    -- client-side settings, then the query uses the location for the query
    -- results and the encryption configuration that are specified for the
    -- workgroup.
    resultConfiguration :: Prelude.Maybe ResultConfiguration,
    -- | Specifies the query result reuse behavior that was used for the query.
    resultReuseConfiguration :: Prelude.Maybe ResultReuseConfiguration,
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
    -- | The completion date, current state, submission time, and state change
    -- reason (if applicable) for the query execution.
    status :: Prelude.Maybe QueryExecutionStatus,
    -- | The kind of query statement that was run.
    substatementType :: Prelude.Maybe Prelude.Text,
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
-- 'executionParameters', 'queryExecution_executionParameters' - A list of values for the parameters in a query. The values are applied
-- sequentially to the parameters in the query in the order in which the
-- parameters occur. The list of parameters is not returned in the
-- response.
--
-- 'query', 'queryExecution_query' - The SQL query statements which the query execution ran.
--
-- 'queryExecutionContext', 'queryExecution_queryExecutionContext' - The database in which the query execution occurred.
--
-- 'queryExecutionId', 'queryExecution_queryExecutionId' - The unique identifier for each query execution.
--
-- 'resultConfiguration', 'queryExecution_resultConfiguration' - The location in Amazon S3 where query and calculation results are stored
-- and the encryption option, if any, used for query results. These are
-- known as \"client-side settings\". If workgroup settings override
-- client-side settings, then the query uses the location for the query
-- results and the encryption configuration that are specified for the
-- workgroup.
--
-- 'resultReuseConfiguration', 'queryExecution_resultReuseConfiguration' - Specifies the query result reuse behavior that was used for the query.
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
-- 'status', 'queryExecution_status' - The completion date, current state, submission time, and state change
-- reason (if applicable) for the query execution.
--
-- 'substatementType', 'queryExecution_substatementType' - The kind of query statement that was run.
--
-- 'workGroup', 'queryExecution_workGroup' - The name of the workgroup in which the query ran.
newQueryExecution ::
  QueryExecution
newQueryExecution =
  QueryExecution'
    { engineVersion = Prelude.Nothing,
      executionParameters = Prelude.Nothing,
      query = Prelude.Nothing,
      queryExecutionContext = Prelude.Nothing,
      queryExecutionId = Prelude.Nothing,
      resultConfiguration = Prelude.Nothing,
      resultReuseConfiguration = Prelude.Nothing,
      statementType = Prelude.Nothing,
      statistics = Prelude.Nothing,
      status = Prelude.Nothing,
      substatementType = Prelude.Nothing,
      workGroup = Prelude.Nothing
    }

-- | The engine version that executed the query.
queryExecution_engineVersion :: Lens.Lens' QueryExecution (Prelude.Maybe EngineVersion)
queryExecution_engineVersion = Lens.lens (\QueryExecution' {engineVersion} -> engineVersion) (\s@QueryExecution' {} a -> s {engineVersion = a} :: QueryExecution)

-- | A list of values for the parameters in a query. The values are applied
-- sequentially to the parameters in the query in the order in which the
-- parameters occur. The list of parameters is not returned in the
-- response.
queryExecution_executionParameters :: Lens.Lens' QueryExecution (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
queryExecution_executionParameters = Lens.lens (\QueryExecution' {executionParameters} -> executionParameters) (\s@QueryExecution' {} a -> s {executionParameters = a} :: QueryExecution) Prelude.. Lens.mapping Lens.coerced

-- | The SQL query statements which the query execution ran.
queryExecution_query :: Lens.Lens' QueryExecution (Prelude.Maybe Prelude.Text)
queryExecution_query = Lens.lens (\QueryExecution' {query} -> query) (\s@QueryExecution' {} a -> s {query = a} :: QueryExecution)

-- | The database in which the query execution occurred.
queryExecution_queryExecutionContext :: Lens.Lens' QueryExecution (Prelude.Maybe QueryExecutionContext)
queryExecution_queryExecutionContext = Lens.lens (\QueryExecution' {queryExecutionContext} -> queryExecutionContext) (\s@QueryExecution' {} a -> s {queryExecutionContext = a} :: QueryExecution)

-- | The unique identifier for each query execution.
queryExecution_queryExecutionId :: Lens.Lens' QueryExecution (Prelude.Maybe Prelude.Text)
queryExecution_queryExecutionId = Lens.lens (\QueryExecution' {queryExecutionId} -> queryExecutionId) (\s@QueryExecution' {} a -> s {queryExecutionId = a} :: QueryExecution)

-- | The location in Amazon S3 where query and calculation results are stored
-- and the encryption option, if any, used for query results. These are
-- known as \"client-side settings\". If workgroup settings override
-- client-side settings, then the query uses the location for the query
-- results and the encryption configuration that are specified for the
-- workgroup.
queryExecution_resultConfiguration :: Lens.Lens' QueryExecution (Prelude.Maybe ResultConfiguration)
queryExecution_resultConfiguration = Lens.lens (\QueryExecution' {resultConfiguration} -> resultConfiguration) (\s@QueryExecution' {} a -> s {resultConfiguration = a} :: QueryExecution)

-- | Specifies the query result reuse behavior that was used for the query.
queryExecution_resultReuseConfiguration :: Lens.Lens' QueryExecution (Prelude.Maybe ResultReuseConfiguration)
queryExecution_resultReuseConfiguration = Lens.lens (\QueryExecution' {resultReuseConfiguration} -> resultReuseConfiguration) (\s@QueryExecution' {} a -> s {resultReuseConfiguration = a} :: QueryExecution)

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

-- | The completion date, current state, submission time, and state change
-- reason (if applicable) for the query execution.
queryExecution_status :: Lens.Lens' QueryExecution (Prelude.Maybe QueryExecutionStatus)
queryExecution_status = Lens.lens (\QueryExecution' {status} -> status) (\s@QueryExecution' {} a -> s {status = a} :: QueryExecution)

-- | The kind of query statement that was run.
queryExecution_substatementType :: Lens.Lens' QueryExecution (Prelude.Maybe Prelude.Text)
queryExecution_substatementType = Lens.lens (\QueryExecution' {substatementType} -> substatementType) (\s@QueryExecution' {} a -> s {substatementType = a} :: QueryExecution)

-- | The name of the workgroup in which the query ran.
queryExecution_workGroup :: Lens.Lens' QueryExecution (Prelude.Maybe Prelude.Text)
queryExecution_workGroup = Lens.lens (\QueryExecution' {workGroup} -> workGroup) (\s@QueryExecution' {} a -> s {workGroup = a} :: QueryExecution)

instance Data.FromJSON QueryExecution where
  parseJSON =
    Data.withObject
      "QueryExecution"
      ( \x ->
          QueryExecution'
            Prelude.<$> (x Data..:? "EngineVersion")
            Prelude.<*> (x Data..:? "ExecutionParameters")
            Prelude.<*> (x Data..:? "Query")
            Prelude.<*> (x Data..:? "QueryExecutionContext")
            Prelude.<*> (x Data..:? "QueryExecutionId")
            Prelude.<*> (x Data..:? "ResultConfiguration")
            Prelude.<*> (x Data..:? "ResultReuseConfiguration")
            Prelude.<*> (x Data..:? "StatementType")
            Prelude.<*> (x Data..:? "Statistics")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "SubstatementType")
            Prelude.<*> (x Data..:? "WorkGroup")
      )

instance Prelude.Hashable QueryExecution where
  hashWithSalt _salt QueryExecution' {..} =
    _salt
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` executionParameters
      `Prelude.hashWithSalt` query
      `Prelude.hashWithSalt` queryExecutionContext
      `Prelude.hashWithSalt` queryExecutionId
      `Prelude.hashWithSalt` resultConfiguration
      `Prelude.hashWithSalt` resultReuseConfiguration
      `Prelude.hashWithSalt` statementType
      `Prelude.hashWithSalt` statistics
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` substatementType
      `Prelude.hashWithSalt` workGroup

instance Prelude.NFData QueryExecution where
  rnf QueryExecution' {..} =
    Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf executionParameters
      `Prelude.seq` Prelude.rnf query
      `Prelude.seq` Prelude.rnf queryExecutionContext
      `Prelude.seq` Prelude.rnf queryExecutionId
      `Prelude.seq` Prelude.rnf resultConfiguration
      `Prelude.seq` Prelude.rnf resultReuseConfiguration
      `Prelude.seq` Prelude.rnf statementType
      `Prelude.seq` Prelude.rnf statistics
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf substatementType
      `Prelude.seq` Prelude.rnf workGroup
