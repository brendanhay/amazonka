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
-- Module      : Amazonka.Glue.StartDataQualityRuleRecommendationRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a recommendation run that is used to generate rules when you
-- don\'t know what rules to write. Glue Data Quality analyzes the data and
-- comes up with recommendations for a potential ruleset. You can then
-- triage the ruleset and modify the generated ruleset to your liking.
module Amazonka.Glue.StartDataQualityRuleRecommendationRun
  ( -- * Creating a Request
    StartDataQualityRuleRecommendationRun (..),
    newStartDataQualityRuleRecommendationRun,

    -- * Request Lenses
    startDataQualityRuleRecommendationRun_clientToken,
    startDataQualityRuleRecommendationRun_createdRulesetName,
    startDataQualityRuleRecommendationRun_numberOfWorkers,
    startDataQualityRuleRecommendationRun_timeout,
    startDataQualityRuleRecommendationRun_dataSource,
    startDataQualityRuleRecommendationRun_role,

    -- * Destructuring the Response
    StartDataQualityRuleRecommendationRunResponse (..),
    newStartDataQualityRuleRecommendationRunResponse,

    -- * Response Lenses
    startDataQualityRuleRecommendationRunResponse_runId,
    startDataQualityRuleRecommendationRunResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartDataQualityRuleRecommendationRun' smart constructor.
data StartDataQualityRuleRecommendationRun = StartDataQualityRuleRecommendationRun'
  { -- | Used for idempotency and is recommended to be set to a random ID (such
    -- as a UUID) to avoid creating or starting multiple instances of the same
    -- resource.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A name for the ruleset.
    createdRulesetName :: Prelude.Maybe Prelude.Text,
    -- | The number of @G.1X@ workers to be used in the run. The default is 5.
    numberOfWorkers :: Prelude.Maybe Prelude.Int,
    -- | The timeout for a run in minutes. This is the maximum time that a run
    -- can consume resources before it is terminated and enters @TIMEOUT@
    -- status. The default is 2,880 minutes (48 hours).
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | The data source (Glue table) associated with this run.
    dataSource :: DataSource,
    -- | An IAM role supplied to encrypt the results of the run.
    role' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDataQualityRuleRecommendationRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'startDataQualityRuleRecommendationRun_clientToken' - Used for idempotency and is recommended to be set to a random ID (such
-- as a UUID) to avoid creating or starting multiple instances of the same
-- resource.
--
-- 'createdRulesetName', 'startDataQualityRuleRecommendationRun_createdRulesetName' - A name for the ruleset.
--
-- 'numberOfWorkers', 'startDataQualityRuleRecommendationRun_numberOfWorkers' - The number of @G.1X@ workers to be used in the run. The default is 5.
--
-- 'timeout', 'startDataQualityRuleRecommendationRun_timeout' - The timeout for a run in minutes. This is the maximum time that a run
-- can consume resources before it is terminated and enters @TIMEOUT@
-- status. The default is 2,880 minutes (48 hours).
--
-- 'dataSource', 'startDataQualityRuleRecommendationRun_dataSource' - The data source (Glue table) associated with this run.
--
-- 'role'', 'startDataQualityRuleRecommendationRun_role' - An IAM role supplied to encrypt the results of the run.
newStartDataQualityRuleRecommendationRun ::
  -- | 'dataSource'
  DataSource ->
  -- | 'role''
  Prelude.Text ->
  StartDataQualityRuleRecommendationRun
newStartDataQualityRuleRecommendationRun
  pDataSource_
  pRole_ =
    StartDataQualityRuleRecommendationRun'
      { clientToken =
          Prelude.Nothing,
        createdRulesetName = Prelude.Nothing,
        numberOfWorkers = Prelude.Nothing,
        timeout = Prelude.Nothing,
        dataSource = pDataSource_,
        role' = pRole_
      }

-- | Used for idempotency and is recommended to be set to a random ID (such
-- as a UUID) to avoid creating or starting multiple instances of the same
-- resource.
startDataQualityRuleRecommendationRun_clientToken :: Lens.Lens' StartDataQualityRuleRecommendationRun (Prelude.Maybe Prelude.Text)
startDataQualityRuleRecommendationRun_clientToken = Lens.lens (\StartDataQualityRuleRecommendationRun' {clientToken} -> clientToken) (\s@StartDataQualityRuleRecommendationRun' {} a -> s {clientToken = a} :: StartDataQualityRuleRecommendationRun)

-- | A name for the ruleset.
startDataQualityRuleRecommendationRun_createdRulesetName :: Lens.Lens' StartDataQualityRuleRecommendationRun (Prelude.Maybe Prelude.Text)
startDataQualityRuleRecommendationRun_createdRulesetName = Lens.lens (\StartDataQualityRuleRecommendationRun' {createdRulesetName} -> createdRulesetName) (\s@StartDataQualityRuleRecommendationRun' {} a -> s {createdRulesetName = a} :: StartDataQualityRuleRecommendationRun)

-- | The number of @G.1X@ workers to be used in the run. The default is 5.
startDataQualityRuleRecommendationRun_numberOfWorkers :: Lens.Lens' StartDataQualityRuleRecommendationRun (Prelude.Maybe Prelude.Int)
startDataQualityRuleRecommendationRun_numberOfWorkers = Lens.lens (\StartDataQualityRuleRecommendationRun' {numberOfWorkers} -> numberOfWorkers) (\s@StartDataQualityRuleRecommendationRun' {} a -> s {numberOfWorkers = a} :: StartDataQualityRuleRecommendationRun)

-- | The timeout for a run in minutes. This is the maximum time that a run
-- can consume resources before it is terminated and enters @TIMEOUT@
-- status. The default is 2,880 minutes (48 hours).
startDataQualityRuleRecommendationRun_timeout :: Lens.Lens' StartDataQualityRuleRecommendationRun (Prelude.Maybe Prelude.Natural)
startDataQualityRuleRecommendationRun_timeout = Lens.lens (\StartDataQualityRuleRecommendationRun' {timeout} -> timeout) (\s@StartDataQualityRuleRecommendationRun' {} a -> s {timeout = a} :: StartDataQualityRuleRecommendationRun)

-- | The data source (Glue table) associated with this run.
startDataQualityRuleRecommendationRun_dataSource :: Lens.Lens' StartDataQualityRuleRecommendationRun DataSource
startDataQualityRuleRecommendationRun_dataSource = Lens.lens (\StartDataQualityRuleRecommendationRun' {dataSource} -> dataSource) (\s@StartDataQualityRuleRecommendationRun' {} a -> s {dataSource = a} :: StartDataQualityRuleRecommendationRun)

-- | An IAM role supplied to encrypt the results of the run.
startDataQualityRuleRecommendationRun_role :: Lens.Lens' StartDataQualityRuleRecommendationRun Prelude.Text
startDataQualityRuleRecommendationRun_role = Lens.lens (\StartDataQualityRuleRecommendationRun' {role'} -> role') (\s@StartDataQualityRuleRecommendationRun' {} a -> s {role' = a} :: StartDataQualityRuleRecommendationRun)

instance
  Core.AWSRequest
    StartDataQualityRuleRecommendationRun
  where
  type
    AWSResponse
      StartDataQualityRuleRecommendationRun =
      StartDataQualityRuleRecommendationRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartDataQualityRuleRecommendationRunResponse'
            Prelude.<$> (x Data..?> "RunId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartDataQualityRuleRecommendationRun
  where
  hashWithSalt
    _salt
    StartDataQualityRuleRecommendationRun' {..} =
      _salt
        `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` createdRulesetName
        `Prelude.hashWithSalt` numberOfWorkers
        `Prelude.hashWithSalt` timeout
        `Prelude.hashWithSalt` dataSource
        `Prelude.hashWithSalt` role'

instance
  Prelude.NFData
    StartDataQualityRuleRecommendationRun
  where
  rnf StartDataQualityRuleRecommendationRun' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf createdRulesetName `Prelude.seq`
        Prelude.rnf numberOfWorkers `Prelude.seq`
          Prelude.rnf timeout `Prelude.seq`
            Prelude.rnf dataSource `Prelude.seq`
              Prelude.rnf role'

instance
  Data.ToHeaders
    StartDataQualityRuleRecommendationRun
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.StartDataQualityRuleRecommendationRun" ::
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
    StartDataQualityRuleRecommendationRun
  where
  toJSON StartDataQualityRuleRecommendationRun' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("CreatedRulesetName" Data..=)
              Prelude.<$> createdRulesetName,
            ("NumberOfWorkers" Data..=)
              Prelude.<$> numberOfWorkers,
            ("Timeout" Data..=) Prelude.<$> timeout,
            Prelude.Just ("DataSource" Data..= dataSource),
            Prelude.Just ("Role" Data..= role')
          ]
      )

instance
  Data.ToPath
    StartDataQualityRuleRecommendationRun
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    StartDataQualityRuleRecommendationRun
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartDataQualityRuleRecommendationRunResponse' smart constructor.
data StartDataQualityRuleRecommendationRunResponse = StartDataQualityRuleRecommendationRunResponse'
  { -- | The unique run identifier associated with this run.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDataQualityRuleRecommendationRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runId', 'startDataQualityRuleRecommendationRunResponse_runId' - The unique run identifier associated with this run.
--
-- 'httpStatus', 'startDataQualityRuleRecommendationRunResponse_httpStatus' - The response's http status code.
newStartDataQualityRuleRecommendationRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartDataQualityRuleRecommendationRunResponse
newStartDataQualityRuleRecommendationRunResponse
  pHttpStatus_ =
    StartDataQualityRuleRecommendationRunResponse'
      { runId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The unique run identifier associated with this run.
startDataQualityRuleRecommendationRunResponse_runId :: Lens.Lens' StartDataQualityRuleRecommendationRunResponse (Prelude.Maybe Prelude.Text)
startDataQualityRuleRecommendationRunResponse_runId = Lens.lens (\StartDataQualityRuleRecommendationRunResponse' {runId} -> runId) (\s@StartDataQualityRuleRecommendationRunResponse' {} a -> s {runId = a} :: StartDataQualityRuleRecommendationRunResponse)

-- | The response's http status code.
startDataQualityRuleRecommendationRunResponse_httpStatus :: Lens.Lens' StartDataQualityRuleRecommendationRunResponse Prelude.Int
startDataQualityRuleRecommendationRunResponse_httpStatus = Lens.lens (\StartDataQualityRuleRecommendationRunResponse' {httpStatus} -> httpStatus) (\s@StartDataQualityRuleRecommendationRunResponse' {} a -> s {httpStatus = a} :: StartDataQualityRuleRecommendationRunResponse)

instance
  Prelude.NFData
    StartDataQualityRuleRecommendationRunResponse
  where
  rnf
    StartDataQualityRuleRecommendationRunResponse' {..} =
      Prelude.rnf runId `Prelude.seq`
        Prelude.rnf httpStatus
