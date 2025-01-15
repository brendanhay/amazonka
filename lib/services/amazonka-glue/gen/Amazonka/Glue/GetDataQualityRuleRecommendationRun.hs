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
-- Module      : Amazonka.Glue.GetDataQualityRuleRecommendationRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified recommendation run that was used to generate rules.
module Amazonka.Glue.GetDataQualityRuleRecommendationRun
  ( -- * Creating a Request
    GetDataQualityRuleRecommendationRun (..),
    newGetDataQualityRuleRecommendationRun,

    -- * Request Lenses
    getDataQualityRuleRecommendationRun_runId,

    -- * Destructuring the Response
    GetDataQualityRuleRecommendationRunResponse (..),
    newGetDataQualityRuleRecommendationRunResponse,

    -- * Response Lenses
    getDataQualityRuleRecommendationRunResponse_completedOn,
    getDataQualityRuleRecommendationRunResponse_createdRulesetName,
    getDataQualityRuleRecommendationRunResponse_dataSource,
    getDataQualityRuleRecommendationRunResponse_errorString,
    getDataQualityRuleRecommendationRunResponse_executionTime,
    getDataQualityRuleRecommendationRunResponse_lastModifiedOn,
    getDataQualityRuleRecommendationRunResponse_numberOfWorkers,
    getDataQualityRuleRecommendationRunResponse_recommendedRuleset,
    getDataQualityRuleRecommendationRunResponse_role,
    getDataQualityRuleRecommendationRunResponse_runId,
    getDataQualityRuleRecommendationRunResponse_startedOn,
    getDataQualityRuleRecommendationRunResponse_status,
    getDataQualityRuleRecommendationRunResponse_timeout,
    getDataQualityRuleRecommendationRunResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDataQualityRuleRecommendationRun' smart constructor.
data GetDataQualityRuleRecommendationRun = GetDataQualityRuleRecommendationRun'
  { -- | The unique run identifier associated with this run.
    runId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataQualityRuleRecommendationRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runId', 'getDataQualityRuleRecommendationRun_runId' - The unique run identifier associated with this run.
newGetDataQualityRuleRecommendationRun ::
  -- | 'runId'
  Prelude.Text ->
  GetDataQualityRuleRecommendationRun
newGetDataQualityRuleRecommendationRun pRunId_ =
  GetDataQualityRuleRecommendationRun'
    { runId =
        pRunId_
    }

-- | The unique run identifier associated with this run.
getDataQualityRuleRecommendationRun_runId :: Lens.Lens' GetDataQualityRuleRecommendationRun Prelude.Text
getDataQualityRuleRecommendationRun_runId = Lens.lens (\GetDataQualityRuleRecommendationRun' {runId} -> runId) (\s@GetDataQualityRuleRecommendationRun' {} a -> s {runId = a} :: GetDataQualityRuleRecommendationRun)

instance
  Core.AWSRequest
    GetDataQualityRuleRecommendationRun
  where
  type
    AWSResponse GetDataQualityRuleRecommendationRun =
      GetDataQualityRuleRecommendationRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataQualityRuleRecommendationRunResponse'
            Prelude.<$> (x Data..?> "CompletedOn")
            Prelude.<*> (x Data..?> "CreatedRulesetName")
            Prelude.<*> (x Data..?> "DataSource")
            Prelude.<*> (x Data..?> "ErrorString")
            Prelude.<*> (x Data..?> "ExecutionTime")
            Prelude.<*> (x Data..?> "LastModifiedOn")
            Prelude.<*> (x Data..?> "NumberOfWorkers")
            Prelude.<*> (x Data..?> "RecommendedRuleset")
            Prelude.<*> (x Data..?> "Role")
            Prelude.<*> (x Data..?> "RunId")
            Prelude.<*> (x Data..?> "StartedOn")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "Timeout")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetDataQualityRuleRecommendationRun
  where
  hashWithSalt
    _salt
    GetDataQualityRuleRecommendationRun' {..} =
      _salt `Prelude.hashWithSalt` runId

instance
  Prelude.NFData
    GetDataQualityRuleRecommendationRun
  where
  rnf GetDataQualityRuleRecommendationRun' {..} =
    Prelude.rnf runId

instance
  Data.ToHeaders
    GetDataQualityRuleRecommendationRun
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.GetDataQualityRuleRecommendationRun" ::
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
    GetDataQualityRuleRecommendationRun
  where
  toJSON GetDataQualityRuleRecommendationRun' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("RunId" Data..= runId)]
      )

instance
  Data.ToPath
    GetDataQualityRuleRecommendationRun
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetDataQualityRuleRecommendationRun
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDataQualityRuleRecommendationRunResponse' smart constructor.
data GetDataQualityRuleRecommendationRunResponse = GetDataQualityRuleRecommendationRunResponse'
  { -- | The date and time when this run was completed.
    completedOn :: Prelude.Maybe Data.POSIX,
    -- | The name of the ruleset that was created by the run.
    createdRulesetName :: Prelude.Maybe Prelude.Text,
    -- | The data source (an Glue table) associated with this run.
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
    -- | When a start rule recommendation run completes, it creates a recommended
    -- ruleset (a set of rules). This member has those rules in Data Quality
    -- Definition Language (DQDL) format.
    recommendedRuleset :: Prelude.Maybe Prelude.Text,
    -- | An IAM role supplied to encrypt the results of the run.
    role' :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'GetDataQualityRuleRecommendationRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completedOn', 'getDataQualityRuleRecommendationRunResponse_completedOn' - The date and time when this run was completed.
--
-- 'createdRulesetName', 'getDataQualityRuleRecommendationRunResponse_createdRulesetName' - The name of the ruleset that was created by the run.
--
-- 'dataSource', 'getDataQualityRuleRecommendationRunResponse_dataSource' - The data source (an Glue table) associated with this run.
--
-- 'errorString', 'getDataQualityRuleRecommendationRunResponse_errorString' - The error strings that are associated with the run.
--
-- 'executionTime', 'getDataQualityRuleRecommendationRunResponse_executionTime' - The amount of time (in seconds) that the run consumed resources.
--
-- 'lastModifiedOn', 'getDataQualityRuleRecommendationRunResponse_lastModifiedOn' - A timestamp. The last point in time when this data quality rule
-- recommendation run was modified.
--
-- 'numberOfWorkers', 'getDataQualityRuleRecommendationRunResponse_numberOfWorkers' - The number of @G.1X@ workers to be used in the run. The default is 5.
--
-- 'recommendedRuleset', 'getDataQualityRuleRecommendationRunResponse_recommendedRuleset' - When a start rule recommendation run completes, it creates a recommended
-- ruleset (a set of rules). This member has those rules in Data Quality
-- Definition Language (DQDL) format.
--
-- 'role'', 'getDataQualityRuleRecommendationRunResponse_role' - An IAM role supplied to encrypt the results of the run.
--
-- 'runId', 'getDataQualityRuleRecommendationRunResponse_runId' - The unique run identifier associated with this run.
--
-- 'startedOn', 'getDataQualityRuleRecommendationRunResponse_startedOn' - The date and time when this run started.
--
-- 'status', 'getDataQualityRuleRecommendationRunResponse_status' - The status for this run.
--
-- 'timeout', 'getDataQualityRuleRecommendationRunResponse_timeout' - The timeout for a run in minutes. This is the maximum time that a run
-- can consume resources before it is terminated and enters @TIMEOUT@
-- status. The default is 2,880 minutes (48 hours).
--
-- 'httpStatus', 'getDataQualityRuleRecommendationRunResponse_httpStatus' - The response's http status code.
newGetDataQualityRuleRecommendationRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDataQualityRuleRecommendationRunResponse
newGetDataQualityRuleRecommendationRunResponse
  pHttpStatus_ =
    GetDataQualityRuleRecommendationRunResponse'
      { completedOn =
          Prelude.Nothing,
        createdRulesetName =
          Prelude.Nothing,
        dataSource = Prelude.Nothing,
        errorString = Prelude.Nothing,
        executionTime =
          Prelude.Nothing,
        lastModifiedOn =
          Prelude.Nothing,
        numberOfWorkers =
          Prelude.Nothing,
        recommendedRuleset =
          Prelude.Nothing,
        role' = Prelude.Nothing,
        runId = Prelude.Nothing,
        startedOn = Prelude.Nothing,
        status = Prelude.Nothing,
        timeout = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The date and time when this run was completed.
getDataQualityRuleRecommendationRunResponse_completedOn :: Lens.Lens' GetDataQualityRuleRecommendationRunResponse (Prelude.Maybe Prelude.UTCTime)
getDataQualityRuleRecommendationRunResponse_completedOn = Lens.lens (\GetDataQualityRuleRecommendationRunResponse' {completedOn} -> completedOn) (\s@GetDataQualityRuleRecommendationRunResponse' {} a -> s {completedOn = a} :: GetDataQualityRuleRecommendationRunResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the ruleset that was created by the run.
getDataQualityRuleRecommendationRunResponse_createdRulesetName :: Lens.Lens' GetDataQualityRuleRecommendationRunResponse (Prelude.Maybe Prelude.Text)
getDataQualityRuleRecommendationRunResponse_createdRulesetName = Lens.lens (\GetDataQualityRuleRecommendationRunResponse' {createdRulesetName} -> createdRulesetName) (\s@GetDataQualityRuleRecommendationRunResponse' {} a -> s {createdRulesetName = a} :: GetDataQualityRuleRecommendationRunResponse)

-- | The data source (an Glue table) associated with this run.
getDataQualityRuleRecommendationRunResponse_dataSource :: Lens.Lens' GetDataQualityRuleRecommendationRunResponse (Prelude.Maybe DataSource)
getDataQualityRuleRecommendationRunResponse_dataSource = Lens.lens (\GetDataQualityRuleRecommendationRunResponse' {dataSource} -> dataSource) (\s@GetDataQualityRuleRecommendationRunResponse' {} a -> s {dataSource = a} :: GetDataQualityRuleRecommendationRunResponse)

-- | The error strings that are associated with the run.
getDataQualityRuleRecommendationRunResponse_errorString :: Lens.Lens' GetDataQualityRuleRecommendationRunResponse (Prelude.Maybe Prelude.Text)
getDataQualityRuleRecommendationRunResponse_errorString = Lens.lens (\GetDataQualityRuleRecommendationRunResponse' {errorString} -> errorString) (\s@GetDataQualityRuleRecommendationRunResponse' {} a -> s {errorString = a} :: GetDataQualityRuleRecommendationRunResponse)

-- | The amount of time (in seconds) that the run consumed resources.
getDataQualityRuleRecommendationRunResponse_executionTime :: Lens.Lens' GetDataQualityRuleRecommendationRunResponse (Prelude.Maybe Prelude.Int)
getDataQualityRuleRecommendationRunResponse_executionTime = Lens.lens (\GetDataQualityRuleRecommendationRunResponse' {executionTime} -> executionTime) (\s@GetDataQualityRuleRecommendationRunResponse' {} a -> s {executionTime = a} :: GetDataQualityRuleRecommendationRunResponse)

-- | A timestamp. The last point in time when this data quality rule
-- recommendation run was modified.
getDataQualityRuleRecommendationRunResponse_lastModifiedOn :: Lens.Lens' GetDataQualityRuleRecommendationRunResponse (Prelude.Maybe Prelude.UTCTime)
getDataQualityRuleRecommendationRunResponse_lastModifiedOn = Lens.lens (\GetDataQualityRuleRecommendationRunResponse' {lastModifiedOn} -> lastModifiedOn) (\s@GetDataQualityRuleRecommendationRunResponse' {} a -> s {lastModifiedOn = a} :: GetDataQualityRuleRecommendationRunResponse) Prelude.. Lens.mapping Data._Time

-- | The number of @G.1X@ workers to be used in the run. The default is 5.
getDataQualityRuleRecommendationRunResponse_numberOfWorkers :: Lens.Lens' GetDataQualityRuleRecommendationRunResponse (Prelude.Maybe Prelude.Int)
getDataQualityRuleRecommendationRunResponse_numberOfWorkers = Lens.lens (\GetDataQualityRuleRecommendationRunResponse' {numberOfWorkers} -> numberOfWorkers) (\s@GetDataQualityRuleRecommendationRunResponse' {} a -> s {numberOfWorkers = a} :: GetDataQualityRuleRecommendationRunResponse)

-- | When a start rule recommendation run completes, it creates a recommended
-- ruleset (a set of rules). This member has those rules in Data Quality
-- Definition Language (DQDL) format.
getDataQualityRuleRecommendationRunResponse_recommendedRuleset :: Lens.Lens' GetDataQualityRuleRecommendationRunResponse (Prelude.Maybe Prelude.Text)
getDataQualityRuleRecommendationRunResponse_recommendedRuleset = Lens.lens (\GetDataQualityRuleRecommendationRunResponse' {recommendedRuleset} -> recommendedRuleset) (\s@GetDataQualityRuleRecommendationRunResponse' {} a -> s {recommendedRuleset = a} :: GetDataQualityRuleRecommendationRunResponse)

-- | An IAM role supplied to encrypt the results of the run.
getDataQualityRuleRecommendationRunResponse_role :: Lens.Lens' GetDataQualityRuleRecommendationRunResponse (Prelude.Maybe Prelude.Text)
getDataQualityRuleRecommendationRunResponse_role = Lens.lens (\GetDataQualityRuleRecommendationRunResponse' {role'} -> role') (\s@GetDataQualityRuleRecommendationRunResponse' {} a -> s {role' = a} :: GetDataQualityRuleRecommendationRunResponse)

-- | The unique run identifier associated with this run.
getDataQualityRuleRecommendationRunResponse_runId :: Lens.Lens' GetDataQualityRuleRecommendationRunResponse (Prelude.Maybe Prelude.Text)
getDataQualityRuleRecommendationRunResponse_runId = Lens.lens (\GetDataQualityRuleRecommendationRunResponse' {runId} -> runId) (\s@GetDataQualityRuleRecommendationRunResponse' {} a -> s {runId = a} :: GetDataQualityRuleRecommendationRunResponse)

-- | The date and time when this run started.
getDataQualityRuleRecommendationRunResponse_startedOn :: Lens.Lens' GetDataQualityRuleRecommendationRunResponse (Prelude.Maybe Prelude.UTCTime)
getDataQualityRuleRecommendationRunResponse_startedOn = Lens.lens (\GetDataQualityRuleRecommendationRunResponse' {startedOn} -> startedOn) (\s@GetDataQualityRuleRecommendationRunResponse' {} a -> s {startedOn = a} :: GetDataQualityRuleRecommendationRunResponse) Prelude.. Lens.mapping Data._Time

-- | The status for this run.
getDataQualityRuleRecommendationRunResponse_status :: Lens.Lens' GetDataQualityRuleRecommendationRunResponse (Prelude.Maybe TaskStatusType)
getDataQualityRuleRecommendationRunResponse_status = Lens.lens (\GetDataQualityRuleRecommendationRunResponse' {status} -> status) (\s@GetDataQualityRuleRecommendationRunResponse' {} a -> s {status = a} :: GetDataQualityRuleRecommendationRunResponse)

-- | The timeout for a run in minutes. This is the maximum time that a run
-- can consume resources before it is terminated and enters @TIMEOUT@
-- status. The default is 2,880 minutes (48 hours).
getDataQualityRuleRecommendationRunResponse_timeout :: Lens.Lens' GetDataQualityRuleRecommendationRunResponse (Prelude.Maybe Prelude.Natural)
getDataQualityRuleRecommendationRunResponse_timeout = Lens.lens (\GetDataQualityRuleRecommendationRunResponse' {timeout} -> timeout) (\s@GetDataQualityRuleRecommendationRunResponse' {} a -> s {timeout = a} :: GetDataQualityRuleRecommendationRunResponse)

-- | The response's http status code.
getDataQualityRuleRecommendationRunResponse_httpStatus :: Lens.Lens' GetDataQualityRuleRecommendationRunResponse Prelude.Int
getDataQualityRuleRecommendationRunResponse_httpStatus = Lens.lens (\GetDataQualityRuleRecommendationRunResponse' {httpStatus} -> httpStatus) (\s@GetDataQualityRuleRecommendationRunResponse' {} a -> s {httpStatus = a} :: GetDataQualityRuleRecommendationRunResponse)

instance
  Prelude.NFData
    GetDataQualityRuleRecommendationRunResponse
  where
  rnf GetDataQualityRuleRecommendationRunResponse' {..} =
    Prelude.rnf completedOn `Prelude.seq`
      Prelude.rnf createdRulesetName `Prelude.seq`
        Prelude.rnf dataSource `Prelude.seq`
          Prelude.rnf errorString `Prelude.seq`
            Prelude.rnf executionTime `Prelude.seq`
              Prelude.rnf lastModifiedOn `Prelude.seq`
                Prelude.rnf numberOfWorkers `Prelude.seq`
                  Prelude.rnf recommendedRuleset `Prelude.seq`
                    Prelude.rnf role' `Prelude.seq`
                      Prelude.rnf runId `Prelude.seq`
                        Prelude.rnf startedOn `Prelude.seq`
                          Prelude.rnf status `Prelude.seq`
                            Prelude.rnf timeout `Prelude.seq`
                              Prelude.rnf httpStatus
