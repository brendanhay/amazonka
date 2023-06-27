{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Glue.GetDataQualityRulesetEvaluationRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a specific run where a ruleset is evaluated against a data
-- source.
module Amazonka.Glue.GetDataQualityRulesetEvaluationRun
  ( -- * Creating a Request
    GetDataQualityRulesetEvaluationRun (..),
    newGetDataQualityRulesetEvaluationRun,

    -- * Request Lenses
    getDataQualityRulesetEvaluationRun_runId,

    -- * Destructuring the Response
    GetDataQualityRulesetEvaluationRunResponse (..),
    newGetDataQualityRulesetEvaluationRunResponse,

    -- * Response Lenses
    getDataQualityRulesetEvaluationRunResponse_additionalDataSources,
    getDataQualityRulesetEvaluationRunResponse_additionalRunOptions,
    getDataQualityRulesetEvaluationRunResponse_completedOn,
    getDataQualityRulesetEvaluationRunResponse_dataSource,
    getDataQualityRulesetEvaluationRunResponse_errorString,
    getDataQualityRulesetEvaluationRunResponse_executionTime,
    getDataQualityRulesetEvaluationRunResponse_lastModifiedOn,
    getDataQualityRulesetEvaluationRunResponse_numberOfWorkers,
    getDataQualityRulesetEvaluationRunResponse_resultIds,
    getDataQualityRulesetEvaluationRunResponse_role,
    getDataQualityRulesetEvaluationRunResponse_rulesetNames,
    getDataQualityRulesetEvaluationRunResponse_runId,
    getDataQualityRulesetEvaluationRunResponse_startedOn,
    getDataQualityRulesetEvaluationRunResponse_status,
    getDataQualityRulesetEvaluationRunResponse_timeout,
    getDataQualityRulesetEvaluationRunResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDataQualityRulesetEvaluationRun' smart constructor.
data GetDataQualityRulesetEvaluationRun = GetDataQualityRulesetEvaluationRun'
  { -- | The unique run identifier associated with this run.
    runId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataQualityRulesetEvaluationRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runId', 'getDataQualityRulesetEvaluationRun_runId' - The unique run identifier associated with this run.
newGetDataQualityRulesetEvaluationRun ::
  -- | 'runId'
  Prelude.Text ->
  GetDataQualityRulesetEvaluationRun
newGetDataQualityRulesetEvaluationRun pRunId_ =
  GetDataQualityRulesetEvaluationRun'
    { runId =
        pRunId_
    }

-- | The unique run identifier associated with this run.
getDataQualityRulesetEvaluationRun_runId :: Lens.Lens' GetDataQualityRulesetEvaluationRun Prelude.Text
getDataQualityRulesetEvaluationRun_runId = Lens.lens (\GetDataQualityRulesetEvaluationRun' {runId} -> runId) (\s@GetDataQualityRulesetEvaluationRun' {} a -> s {runId = a} :: GetDataQualityRulesetEvaluationRun)

instance
  Core.AWSRequest
    GetDataQualityRulesetEvaluationRun
  where
  type
    AWSResponse GetDataQualityRulesetEvaluationRun =
      GetDataQualityRulesetEvaluationRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataQualityRulesetEvaluationRunResponse'
            Prelude.<$> ( x
                            Data..?> "AdditionalDataSources"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "AdditionalRunOptions")
            Prelude.<*> (x Data..?> "CompletedOn")
            Prelude.<*> (x Data..?> "DataSource")
            Prelude.<*> (x Data..?> "ErrorString")
            Prelude.<*> (x Data..?> "ExecutionTime")
            Prelude.<*> (x Data..?> "LastModifiedOn")
            Prelude.<*> (x Data..?> "NumberOfWorkers")
            Prelude.<*> (x Data..?> "ResultIds")
            Prelude.<*> (x Data..?> "Role")
            Prelude.<*> (x Data..?> "RulesetNames")
            Prelude.<*> (x Data..?> "RunId")
            Prelude.<*> (x Data..?> "StartedOn")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "Timeout")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetDataQualityRulesetEvaluationRun
  where
  hashWithSalt
    _salt
    GetDataQualityRulesetEvaluationRun' {..} =
      _salt `Prelude.hashWithSalt` runId

instance
  Prelude.NFData
    GetDataQualityRulesetEvaluationRun
  where
  rnf GetDataQualityRulesetEvaluationRun' {..} =
    Prelude.rnf runId

instance
  Data.ToHeaders
    GetDataQualityRulesetEvaluationRun
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.GetDataQualityRulesetEvaluationRun" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    GetDataQualityRulesetEvaluationRun
  where
  toJSON GetDataQualityRulesetEvaluationRun' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("RunId" Data..= runId)]
      )

instance
  Data.ToPath
    GetDataQualityRulesetEvaluationRun
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetDataQualityRulesetEvaluationRun
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDataQualityRulesetEvaluationRunResponse' smart constructor.
data GetDataQualityRulesetEvaluationRunResponse = GetDataQualityRulesetEvaluationRunResponse'
  { -- | A map of reference strings to additional data sources you can specify
    -- for an evaluation run.
    additionalDataSources :: Prelude.Maybe (Prelude.HashMap Prelude.Text DataSource),
    -- | Additional run options you can specify for an evaluation run.
    additionalRunOptions :: Prelude.Maybe DataQualityEvaluationRunAdditionalRunOptions,
    -- | The date and time when this run was completed.
    completedOn :: Prelude.Maybe Data.POSIX,
    -- | The data source (an Glue table) associated with this evaluation run.
    dataSource :: Prelude.Maybe DataSource,
    -- | The error strings that are associated with the run.
    errorString :: Prelude.Maybe Prelude.Text,
    -- | The amount of time (in seconds) that the run consumed resources.
    executionTime :: Prelude.Maybe Prelude.Int,
    -- | A timestamp. The last point in time when this data quality rule
    -- recommendation run was modified.
    lastModifiedOn :: Prelude.Maybe Data.POSIX,
    -- | The number of @G.1X@ workers to be used in the run. The default is 5.
    numberOfWorkers :: Prelude.Maybe Prelude.Int,
    -- | A list of result IDs for the data quality results for the run.
    resultIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | An IAM role supplied to encrypt the results of the run.
    role' :: Prelude.Maybe Prelude.Text,
    -- | A list of ruleset names for the run.
    rulesetNames :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The unique run identifier associated with this run.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The date and time when this run started.
    startedOn :: Prelude.Maybe Data.POSIX,
    -- | The status for this run.
    status :: Prelude.Maybe TaskStatusType,
    -- | The timeout for a run in minutes. This is the maximum time that a run
    -- can consume resources before it is terminated and enters @TIMEOUT@
    -- status. The default is 2,880 minutes (48 hours).
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataQualityRulesetEvaluationRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalDataSources', 'getDataQualityRulesetEvaluationRunResponse_additionalDataSources' - A map of reference strings to additional data sources you can specify
-- for an evaluation run.
--
-- 'additionalRunOptions', 'getDataQualityRulesetEvaluationRunResponse_additionalRunOptions' - Additional run options you can specify for an evaluation run.
--
-- 'completedOn', 'getDataQualityRulesetEvaluationRunResponse_completedOn' - The date and time when this run was completed.
--
-- 'dataSource', 'getDataQualityRulesetEvaluationRunResponse_dataSource' - The data source (an Glue table) associated with this evaluation run.
--
-- 'errorString', 'getDataQualityRulesetEvaluationRunResponse_errorString' - The error strings that are associated with the run.
--
-- 'executionTime', 'getDataQualityRulesetEvaluationRunResponse_executionTime' - The amount of time (in seconds) that the run consumed resources.
--
-- 'lastModifiedOn', 'getDataQualityRulesetEvaluationRunResponse_lastModifiedOn' - A timestamp. The last point in time when this data quality rule
-- recommendation run was modified.
--
-- 'numberOfWorkers', 'getDataQualityRulesetEvaluationRunResponse_numberOfWorkers' - The number of @G.1X@ workers to be used in the run. The default is 5.
--
-- 'resultIds', 'getDataQualityRulesetEvaluationRunResponse_resultIds' - A list of result IDs for the data quality results for the run.
--
-- 'role'', 'getDataQualityRulesetEvaluationRunResponse_role' - An IAM role supplied to encrypt the results of the run.
--
-- 'rulesetNames', 'getDataQualityRulesetEvaluationRunResponse_rulesetNames' - A list of ruleset names for the run.
--
-- 'runId', 'getDataQualityRulesetEvaluationRunResponse_runId' - The unique run identifier associated with this run.
--
-- 'startedOn', 'getDataQualityRulesetEvaluationRunResponse_startedOn' - The date and time when this run started.
--
-- 'status', 'getDataQualityRulesetEvaluationRunResponse_status' - The status for this run.
--
-- 'timeout', 'getDataQualityRulesetEvaluationRunResponse_timeout' - The timeout for a run in minutes. This is the maximum time that a run
-- can consume resources before it is terminated and enters @TIMEOUT@
-- status. The default is 2,880 minutes (48 hours).
--
-- 'httpStatus', 'getDataQualityRulesetEvaluationRunResponse_httpStatus' - The response's http status code.
newGetDataQualityRulesetEvaluationRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDataQualityRulesetEvaluationRunResponse
newGetDataQualityRulesetEvaluationRunResponse
  pHttpStatus_ =
    GetDataQualityRulesetEvaluationRunResponse'
      { additionalDataSources =
          Prelude.Nothing,
        additionalRunOptions =
          Prelude.Nothing,
        completedOn = Prelude.Nothing,
        dataSource = Prelude.Nothing,
        errorString = Prelude.Nothing,
        executionTime = Prelude.Nothing,
        lastModifiedOn =
          Prelude.Nothing,
        numberOfWorkers =
          Prelude.Nothing,
        resultIds = Prelude.Nothing,
        role' = Prelude.Nothing,
        rulesetNames = Prelude.Nothing,
        runId = Prelude.Nothing,
        startedOn = Prelude.Nothing,
        status = Prelude.Nothing,
        timeout = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A map of reference strings to additional data sources you can specify
-- for an evaluation run.
getDataQualityRulesetEvaluationRunResponse_additionalDataSources :: Lens.Lens' GetDataQualityRulesetEvaluationRunResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text DataSource))
getDataQualityRulesetEvaluationRunResponse_additionalDataSources = Lens.lens (\GetDataQualityRulesetEvaluationRunResponse' {additionalDataSources} -> additionalDataSources) (\s@GetDataQualityRulesetEvaluationRunResponse' {} a -> s {additionalDataSources = a} :: GetDataQualityRulesetEvaluationRunResponse) Prelude.. Lens.mapping Lens.coerced

-- | Additional run options you can specify for an evaluation run.
getDataQualityRulesetEvaluationRunResponse_additionalRunOptions :: Lens.Lens' GetDataQualityRulesetEvaluationRunResponse (Prelude.Maybe DataQualityEvaluationRunAdditionalRunOptions)
getDataQualityRulesetEvaluationRunResponse_additionalRunOptions = Lens.lens (\GetDataQualityRulesetEvaluationRunResponse' {additionalRunOptions} -> additionalRunOptions) (\s@GetDataQualityRulesetEvaluationRunResponse' {} a -> s {additionalRunOptions = a} :: GetDataQualityRulesetEvaluationRunResponse)

-- | The date and time when this run was completed.
getDataQualityRulesetEvaluationRunResponse_completedOn :: Lens.Lens' GetDataQualityRulesetEvaluationRunResponse (Prelude.Maybe Prelude.UTCTime)
getDataQualityRulesetEvaluationRunResponse_completedOn = Lens.lens (\GetDataQualityRulesetEvaluationRunResponse' {completedOn} -> completedOn) (\s@GetDataQualityRulesetEvaluationRunResponse' {} a -> s {completedOn = a} :: GetDataQualityRulesetEvaluationRunResponse) Prelude.. Lens.mapping Data._Time

-- | The data source (an Glue table) associated with this evaluation run.
getDataQualityRulesetEvaluationRunResponse_dataSource :: Lens.Lens' GetDataQualityRulesetEvaluationRunResponse (Prelude.Maybe DataSource)
getDataQualityRulesetEvaluationRunResponse_dataSource = Lens.lens (\GetDataQualityRulesetEvaluationRunResponse' {dataSource} -> dataSource) (\s@GetDataQualityRulesetEvaluationRunResponse' {} a -> s {dataSource = a} :: GetDataQualityRulesetEvaluationRunResponse)

-- | The error strings that are associated with the run.
getDataQualityRulesetEvaluationRunResponse_errorString :: Lens.Lens' GetDataQualityRulesetEvaluationRunResponse (Prelude.Maybe Prelude.Text)
getDataQualityRulesetEvaluationRunResponse_errorString = Lens.lens (\GetDataQualityRulesetEvaluationRunResponse' {errorString} -> errorString) (\s@GetDataQualityRulesetEvaluationRunResponse' {} a -> s {errorString = a} :: GetDataQualityRulesetEvaluationRunResponse)

-- | The amount of time (in seconds) that the run consumed resources.
getDataQualityRulesetEvaluationRunResponse_executionTime :: Lens.Lens' GetDataQualityRulesetEvaluationRunResponse (Prelude.Maybe Prelude.Int)
getDataQualityRulesetEvaluationRunResponse_executionTime = Lens.lens (\GetDataQualityRulesetEvaluationRunResponse' {executionTime} -> executionTime) (\s@GetDataQualityRulesetEvaluationRunResponse' {} a -> s {executionTime = a} :: GetDataQualityRulesetEvaluationRunResponse)

-- | A timestamp. The last point in time when this data quality rule
-- recommendation run was modified.
getDataQualityRulesetEvaluationRunResponse_lastModifiedOn :: Lens.Lens' GetDataQualityRulesetEvaluationRunResponse (Prelude.Maybe Prelude.UTCTime)
getDataQualityRulesetEvaluationRunResponse_lastModifiedOn = Lens.lens (\GetDataQualityRulesetEvaluationRunResponse' {lastModifiedOn} -> lastModifiedOn) (\s@GetDataQualityRulesetEvaluationRunResponse' {} a -> s {lastModifiedOn = a} :: GetDataQualityRulesetEvaluationRunResponse) Prelude.. Lens.mapping Data._Time

-- | The number of @G.1X@ workers to be used in the run. The default is 5.
getDataQualityRulesetEvaluationRunResponse_numberOfWorkers :: Lens.Lens' GetDataQualityRulesetEvaluationRunResponse (Prelude.Maybe Prelude.Int)
getDataQualityRulesetEvaluationRunResponse_numberOfWorkers = Lens.lens (\GetDataQualityRulesetEvaluationRunResponse' {numberOfWorkers} -> numberOfWorkers) (\s@GetDataQualityRulesetEvaluationRunResponse' {} a -> s {numberOfWorkers = a} :: GetDataQualityRulesetEvaluationRunResponse)

-- | A list of result IDs for the data quality results for the run.
getDataQualityRulesetEvaluationRunResponse_resultIds :: Lens.Lens' GetDataQualityRulesetEvaluationRunResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
getDataQualityRulesetEvaluationRunResponse_resultIds = Lens.lens (\GetDataQualityRulesetEvaluationRunResponse' {resultIds} -> resultIds) (\s@GetDataQualityRulesetEvaluationRunResponse' {} a -> s {resultIds = a} :: GetDataQualityRulesetEvaluationRunResponse) Prelude.. Lens.mapping Lens.coerced

-- | An IAM role supplied to encrypt the results of the run.
getDataQualityRulesetEvaluationRunResponse_role :: Lens.Lens' GetDataQualityRulesetEvaluationRunResponse (Prelude.Maybe Prelude.Text)
getDataQualityRulesetEvaluationRunResponse_role = Lens.lens (\GetDataQualityRulesetEvaluationRunResponse' {role'} -> role') (\s@GetDataQualityRulesetEvaluationRunResponse' {} a -> s {role' = a} :: GetDataQualityRulesetEvaluationRunResponse)

-- | A list of ruleset names for the run.
getDataQualityRulesetEvaluationRunResponse_rulesetNames :: Lens.Lens' GetDataQualityRulesetEvaluationRunResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
getDataQualityRulesetEvaluationRunResponse_rulesetNames = Lens.lens (\GetDataQualityRulesetEvaluationRunResponse' {rulesetNames} -> rulesetNames) (\s@GetDataQualityRulesetEvaluationRunResponse' {} a -> s {rulesetNames = a} :: GetDataQualityRulesetEvaluationRunResponse) Prelude.. Lens.mapping Lens.coerced

-- | The unique run identifier associated with this run.
getDataQualityRulesetEvaluationRunResponse_runId :: Lens.Lens' GetDataQualityRulesetEvaluationRunResponse (Prelude.Maybe Prelude.Text)
getDataQualityRulesetEvaluationRunResponse_runId = Lens.lens (\GetDataQualityRulesetEvaluationRunResponse' {runId} -> runId) (\s@GetDataQualityRulesetEvaluationRunResponse' {} a -> s {runId = a} :: GetDataQualityRulesetEvaluationRunResponse)

-- | The date and time when this run started.
getDataQualityRulesetEvaluationRunResponse_startedOn :: Lens.Lens' GetDataQualityRulesetEvaluationRunResponse (Prelude.Maybe Prelude.UTCTime)
getDataQualityRulesetEvaluationRunResponse_startedOn = Lens.lens (\GetDataQualityRulesetEvaluationRunResponse' {startedOn} -> startedOn) (\s@GetDataQualityRulesetEvaluationRunResponse' {} a -> s {startedOn = a} :: GetDataQualityRulesetEvaluationRunResponse) Prelude.. Lens.mapping Data._Time

-- | The status for this run.
getDataQualityRulesetEvaluationRunResponse_status :: Lens.Lens' GetDataQualityRulesetEvaluationRunResponse (Prelude.Maybe TaskStatusType)
getDataQualityRulesetEvaluationRunResponse_status = Lens.lens (\GetDataQualityRulesetEvaluationRunResponse' {status} -> status) (\s@GetDataQualityRulesetEvaluationRunResponse' {} a -> s {status = a} :: GetDataQualityRulesetEvaluationRunResponse)

-- | The timeout for a run in minutes. This is the maximum time that a run
-- can consume resources before it is terminated and enters @TIMEOUT@
-- status. The default is 2,880 minutes (48 hours).
getDataQualityRulesetEvaluationRunResponse_timeout :: Lens.Lens' GetDataQualityRulesetEvaluationRunResponse (Prelude.Maybe Prelude.Natural)
getDataQualityRulesetEvaluationRunResponse_timeout = Lens.lens (\GetDataQualityRulesetEvaluationRunResponse' {timeout} -> timeout) (\s@GetDataQualityRulesetEvaluationRunResponse' {} a -> s {timeout = a} :: GetDataQualityRulesetEvaluationRunResponse)

-- | The response's http status code.
getDataQualityRulesetEvaluationRunResponse_httpStatus :: Lens.Lens' GetDataQualityRulesetEvaluationRunResponse Prelude.Int
getDataQualityRulesetEvaluationRunResponse_httpStatus = Lens.lens (\GetDataQualityRulesetEvaluationRunResponse' {httpStatus} -> httpStatus) (\s@GetDataQualityRulesetEvaluationRunResponse' {} a -> s {httpStatus = a} :: GetDataQualityRulesetEvaluationRunResponse)

instance
  Prelude.NFData
    GetDataQualityRulesetEvaluationRunResponse
  where
  rnf GetDataQualityRulesetEvaluationRunResponse' {..} =
    Prelude.rnf additionalDataSources
      `Prelude.seq` Prelude.rnf additionalRunOptions
      `Prelude.seq` Prelude.rnf completedOn
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf errorString
      `Prelude.seq` Prelude.rnf executionTime
      `Prelude.seq` Prelude.rnf lastModifiedOn
      `Prelude.seq` Prelude.rnf numberOfWorkers
      `Prelude.seq` Prelude.rnf resultIds
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf rulesetNames
      `Prelude.seq` Prelude.rnf runId
      `Prelude.seq` Prelude.rnf startedOn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf httpStatus
