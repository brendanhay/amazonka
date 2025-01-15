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
-- Module      : Amazonka.Glue.StartDataQualityRulesetEvaluationRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Once you have a ruleset definition (either recommended or your own), you
-- call this operation to evaluate the ruleset against a data source (Glue
-- table). The evaluation computes results which you can retrieve with the
-- @GetDataQualityResult@ API.
module Amazonka.Glue.StartDataQualityRulesetEvaluationRun
  ( -- * Creating a Request
    StartDataQualityRulesetEvaluationRun (..),
    newStartDataQualityRulesetEvaluationRun,

    -- * Request Lenses
    startDataQualityRulesetEvaluationRun_additionalRunOptions,
    startDataQualityRulesetEvaluationRun_clientToken,
    startDataQualityRulesetEvaluationRun_numberOfWorkers,
    startDataQualityRulesetEvaluationRun_timeout,
    startDataQualityRulesetEvaluationRun_dataSource,
    startDataQualityRulesetEvaluationRun_role,
    startDataQualityRulesetEvaluationRun_rulesetNames,

    -- * Destructuring the Response
    StartDataQualityRulesetEvaluationRunResponse (..),
    newStartDataQualityRulesetEvaluationRunResponse,

    -- * Response Lenses
    startDataQualityRulesetEvaluationRunResponse_runId,
    startDataQualityRulesetEvaluationRunResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartDataQualityRulesetEvaluationRun' smart constructor.
data StartDataQualityRulesetEvaluationRun = StartDataQualityRulesetEvaluationRun'
  { -- | Additional run options you can specify for an evaluation run.
    additionalRunOptions :: Prelude.Maybe DataQualityEvaluationRunAdditionalRunOptions,
    -- | Used for idempotency and is recommended to be set to a random ID (such
    -- as a UUID) to avoid creating or starting multiple instances of the same
    -- resource.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The number of @G.1X@ workers to be used in the run. The default is 5.
    numberOfWorkers :: Prelude.Maybe Prelude.Int,
    -- | The timeout for a run in minutes. This is the maximum time that a run
    -- can consume resources before it is terminated and enters @TIMEOUT@
    -- status. The default is 2,880 minutes (48 hours).
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | The data source (Glue table) associated with this run.
    dataSource :: DataSource,
    -- | An IAM role supplied to encrypt the results of the run.
    role' :: Prelude.Text,
    -- | A list of ruleset names.
    rulesetNames :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDataQualityRulesetEvaluationRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalRunOptions', 'startDataQualityRulesetEvaluationRun_additionalRunOptions' - Additional run options you can specify for an evaluation run.
--
-- 'clientToken', 'startDataQualityRulesetEvaluationRun_clientToken' - Used for idempotency and is recommended to be set to a random ID (such
-- as a UUID) to avoid creating or starting multiple instances of the same
-- resource.
--
-- 'numberOfWorkers', 'startDataQualityRulesetEvaluationRun_numberOfWorkers' - The number of @G.1X@ workers to be used in the run. The default is 5.
--
-- 'timeout', 'startDataQualityRulesetEvaluationRun_timeout' - The timeout for a run in minutes. This is the maximum time that a run
-- can consume resources before it is terminated and enters @TIMEOUT@
-- status. The default is 2,880 minutes (48 hours).
--
-- 'dataSource', 'startDataQualityRulesetEvaluationRun_dataSource' - The data source (Glue table) associated with this run.
--
-- 'role'', 'startDataQualityRulesetEvaluationRun_role' - An IAM role supplied to encrypt the results of the run.
--
-- 'rulesetNames', 'startDataQualityRulesetEvaluationRun_rulesetNames' - A list of ruleset names.
newStartDataQualityRulesetEvaluationRun ::
  -- | 'dataSource'
  DataSource ->
  -- | 'role''
  Prelude.Text ->
  -- | 'rulesetNames'
  Prelude.NonEmpty Prelude.Text ->
  StartDataQualityRulesetEvaluationRun
newStartDataQualityRulesetEvaluationRun
  pDataSource_
  pRole_
  pRulesetNames_ =
    StartDataQualityRulesetEvaluationRun'
      { additionalRunOptions =
          Prelude.Nothing,
        clientToken = Prelude.Nothing,
        numberOfWorkers = Prelude.Nothing,
        timeout = Prelude.Nothing,
        dataSource = pDataSource_,
        role' = pRole_,
        rulesetNames =
          Lens.coerced Lens.# pRulesetNames_
      }

-- | Additional run options you can specify for an evaluation run.
startDataQualityRulesetEvaluationRun_additionalRunOptions :: Lens.Lens' StartDataQualityRulesetEvaluationRun (Prelude.Maybe DataQualityEvaluationRunAdditionalRunOptions)
startDataQualityRulesetEvaluationRun_additionalRunOptions = Lens.lens (\StartDataQualityRulesetEvaluationRun' {additionalRunOptions} -> additionalRunOptions) (\s@StartDataQualityRulesetEvaluationRun' {} a -> s {additionalRunOptions = a} :: StartDataQualityRulesetEvaluationRun)

-- | Used for idempotency and is recommended to be set to a random ID (such
-- as a UUID) to avoid creating or starting multiple instances of the same
-- resource.
startDataQualityRulesetEvaluationRun_clientToken :: Lens.Lens' StartDataQualityRulesetEvaluationRun (Prelude.Maybe Prelude.Text)
startDataQualityRulesetEvaluationRun_clientToken = Lens.lens (\StartDataQualityRulesetEvaluationRun' {clientToken} -> clientToken) (\s@StartDataQualityRulesetEvaluationRun' {} a -> s {clientToken = a} :: StartDataQualityRulesetEvaluationRun)

-- | The number of @G.1X@ workers to be used in the run. The default is 5.
startDataQualityRulesetEvaluationRun_numberOfWorkers :: Lens.Lens' StartDataQualityRulesetEvaluationRun (Prelude.Maybe Prelude.Int)
startDataQualityRulesetEvaluationRun_numberOfWorkers = Lens.lens (\StartDataQualityRulesetEvaluationRun' {numberOfWorkers} -> numberOfWorkers) (\s@StartDataQualityRulesetEvaluationRun' {} a -> s {numberOfWorkers = a} :: StartDataQualityRulesetEvaluationRun)

-- | The timeout for a run in minutes. This is the maximum time that a run
-- can consume resources before it is terminated and enters @TIMEOUT@
-- status. The default is 2,880 minutes (48 hours).
startDataQualityRulesetEvaluationRun_timeout :: Lens.Lens' StartDataQualityRulesetEvaluationRun (Prelude.Maybe Prelude.Natural)
startDataQualityRulesetEvaluationRun_timeout = Lens.lens (\StartDataQualityRulesetEvaluationRun' {timeout} -> timeout) (\s@StartDataQualityRulesetEvaluationRun' {} a -> s {timeout = a} :: StartDataQualityRulesetEvaluationRun)

-- | The data source (Glue table) associated with this run.
startDataQualityRulesetEvaluationRun_dataSource :: Lens.Lens' StartDataQualityRulesetEvaluationRun DataSource
startDataQualityRulesetEvaluationRun_dataSource = Lens.lens (\StartDataQualityRulesetEvaluationRun' {dataSource} -> dataSource) (\s@StartDataQualityRulesetEvaluationRun' {} a -> s {dataSource = a} :: StartDataQualityRulesetEvaluationRun)

-- | An IAM role supplied to encrypt the results of the run.
startDataQualityRulesetEvaluationRun_role :: Lens.Lens' StartDataQualityRulesetEvaluationRun Prelude.Text
startDataQualityRulesetEvaluationRun_role = Lens.lens (\StartDataQualityRulesetEvaluationRun' {role'} -> role') (\s@StartDataQualityRulesetEvaluationRun' {} a -> s {role' = a} :: StartDataQualityRulesetEvaluationRun)

-- | A list of ruleset names.
startDataQualityRulesetEvaluationRun_rulesetNames :: Lens.Lens' StartDataQualityRulesetEvaluationRun (Prelude.NonEmpty Prelude.Text)
startDataQualityRulesetEvaluationRun_rulesetNames = Lens.lens (\StartDataQualityRulesetEvaluationRun' {rulesetNames} -> rulesetNames) (\s@StartDataQualityRulesetEvaluationRun' {} a -> s {rulesetNames = a} :: StartDataQualityRulesetEvaluationRun) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    StartDataQualityRulesetEvaluationRun
  where
  type
    AWSResponse StartDataQualityRulesetEvaluationRun =
      StartDataQualityRulesetEvaluationRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartDataQualityRulesetEvaluationRunResponse'
            Prelude.<$> (x Data..?> "RunId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartDataQualityRulesetEvaluationRun
  where
  hashWithSalt
    _salt
    StartDataQualityRulesetEvaluationRun' {..} =
      _salt
        `Prelude.hashWithSalt` additionalRunOptions
        `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` numberOfWorkers
        `Prelude.hashWithSalt` timeout
        `Prelude.hashWithSalt` dataSource
        `Prelude.hashWithSalt` role'
        `Prelude.hashWithSalt` rulesetNames

instance
  Prelude.NFData
    StartDataQualityRulesetEvaluationRun
  where
  rnf StartDataQualityRulesetEvaluationRun' {..} =
    Prelude.rnf additionalRunOptions `Prelude.seq`
      Prelude.rnf clientToken `Prelude.seq`
        Prelude.rnf numberOfWorkers `Prelude.seq`
          Prelude.rnf timeout `Prelude.seq`
            Prelude.rnf dataSource `Prelude.seq`
              Prelude.rnf role' `Prelude.seq`
                Prelude.rnf rulesetNames

instance
  Data.ToHeaders
    StartDataQualityRulesetEvaluationRun
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.StartDataQualityRulesetEvaluationRun" ::
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
    StartDataQualityRulesetEvaluationRun
  where
  toJSON StartDataQualityRulesetEvaluationRun' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalRunOptions" Data..=)
              Prelude.<$> additionalRunOptions,
            ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("NumberOfWorkers" Data..=)
              Prelude.<$> numberOfWorkers,
            ("Timeout" Data..=) Prelude.<$> timeout,
            Prelude.Just ("DataSource" Data..= dataSource),
            Prelude.Just ("Role" Data..= role'),
            Prelude.Just ("RulesetNames" Data..= rulesetNames)
          ]
      )

instance
  Data.ToPath
    StartDataQualityRulesetEvaluationRun
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    StartDataQualityRulesetEvaluationRun
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartDataQualityRulesetEvaluationRunResponse' smart constructor.
data StartDataQualityRulesetEvaluationRunResponse = StartDataQualityRulesetEvaluationRunResponse'
  { -- | The unique run identifier associated with this run.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDataQualityRulesetEvaluationRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runId', 'startDataQualityRulesetEvaluationRunResponse_runId' - The unique run identifier associated with this run.
--
-- 'httpStatus', 'startDataQualityRulesetEvaluationRunResponse_httpStatus' - The response's http status code.
newStartDataQualityRulesetEvaluationRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartDataQualityRulesetEvaluationRunResponse
newStartDataQualityRulesetEvaluationRunResponse
  pHttpStatus_ =
    StartDataQualityRulesetEvaluationRunResponse'
      { runId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The unique run identifier associated with this run.
startDataQualityRulesetEvaluationRunResponse_runId :: Lens.Lens' StartDataQualityRulesetEvaluationRunResponse (Prelude.Maybe Prelude.Text)
startDataQualityRulesetEvaluationRunResponse_runId = Lens.lens (\StartDataQualityRulesetEvaluationRunResponse' {runId} -> runId) (\s@StartDataQualityRulesetEvaluationRunResponse' {} a -> s {runId = a} :: StartDataQualityRulesetEvaluationRunResponse)

-- | The response's http status code.
startDataQualityRulesetEvaluationRunResponse_httpStatus :: Lens.Lens' StartDataQualityRulesetEvaluationRunResponse Prelude.Int
startDataQualityRulesetEvaluationRunResponse_httpStatus = Lens.lens (\StartDataQualityRulesetEvaluationRunResponse' {httpStatus} -> httpStatus) (\s@StartDataQualityRulesetEvaluationRunResponse' {} a -> s {httpStatus = a} :: StartDataQualityRulesetEvaluationRunResponse)

instance
  Prelude.NFData
    StartDataQualityRulesetEvaluationRunResponse
  where
  rnf StartDataQualityRulesetEvaluationRunResponse' {..} =
    Prelude.rnf runId `Prelude.seq`
      Prelude.rnf httpStatus
