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
-- Module      : Amazonka.Glue.CancelDataQualityRulesetEvaluationRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a run where a ruleset is being evaluated against a data source.
module Amazonka.Glue.CancelDataQualityRulesetEvaluationRun
  ( -- * Creating a Request
    CancelDataQualityRulesetEvaluationRun (..),
    newCancelDataQualityRulesetEvaluationRun,

    -- * Request Lenses
    cancelDataQualityRulesetEvaluationRun_runId,

    -- * Destructuring the Response
    CancelDataQualityRulesetEvaluationRunResponse (..),
    newCancelDataQualityRulesetEvaluationRunResponse,

    -- * Response Lenses
    cancelDataQualityRulesetEvaluationRunResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelDataQualityRulesetEvaluationRun' smart constructor.
data CancelDataQualityRulesetEvaluationRun = CancelDataQualityRulesetEvaluationRun'
  { -- | The unique run identifier associated with this run.
    runId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelDataQualityRulesetEvaluationRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runId', 'cancelDataQualityRulesetEvaluationRun_runId' - The unique run identifier associated with this run.
newCancelDataQualityRulesetEvaluationRun ::
  -- | 'runId'
  Prelude.Text ->
  CancelDataQualityRulesetEvaluationRun
newCancelDataQualityRulesetEvaluationRun pRunId_ =
  CancelDataQualityRulesetEvaluationRun'
    { runId =
        pRunId_
    }

-- | The unique run identifier associated with this run.
cancelDataQualityRulesetEvaluationRun_runId :: Lens.Lens' CancelDataQualityRulesetEvaluationRun Prelude.Text
cancelDataQualityRulesetEvaluationRun_runId = Lens.lens (\CancelDataQualityRulesetEvaluationRun' {runId} -> runId) (\s@CancelDataQualityRulesetEvaluationRun' {} a -> s {runId = a} :: CancelDataQualityRulesetEvaluationRun)

instance
  Core.AWSRequest
    CancelDataQualityRulesetEvaluationRun
  where
  type
    AWSResponse
      CancelDataQualityRulesetEvaluationRun =
      CancelDataQualityRulesetEvaluationRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelDataQualityRulesetEvaluationRunResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CancelDataQualityRulesetEvaluationRun
  where
  hashWithSalt
    _salt
    CancelDataQualityRulesetEvaluationRun' {..} =
      _salt `Prelude.hashWithSalt` runId

instance
  Prelude.NFData
    CancelDataQualityRulesetEvaluationRun
  where
  rnf CancelDataQualityRulesetEvaluationRun' {..} =
    Prelude.rnf runId

instance
  Data.ToHeaders
    CancelDataQualityRulesetEvaluationRun
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.CancelDataQualityRulesetEvaluationRun" ::
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
    CancelDataQualityRulesetEvaluationRun
  where
  toJSON CancelDataQualityRulesetEvaluationRun' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("RunId" Data..= runId)]
      )

instance
  Data.ToPath
    CancelDataQualityRulesetEvaluationRun
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CancelDataQualityRulesetEvaluationRun
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelDataQualityRulesetEvaluationRunResponse' smart constructor.
data CancelDataQualityRulesetEvaluationRunResponse = CancelDataQualityRulesetEvaluationRunResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelDataQualityRulesetEvaluationRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelDataQualityRulesetEvaluationRunResponse_httpStatus' - The response's http status code.
newCancelDataQualityRulesetEvaluationRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelDataQualityRulesetEvaluationRunResponse
newCancelDataQualityRulesetEvaluationRunResponse
  pHttpStatus_ =
    CancelDataQualityRulesetEvaluationRunResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
cancelDataQualityRulesetEvaluationRunResponse_httpStatus :: Lens.Lens' CancelDataQualityRulesetEvaluationRunResponse Prelude.Int
cancelDataQualityRulesetEvaluationRunResponse_httpStatus = Lens.lens (\CancelDataQualityRulesetEvaluationRunResponse' {httpStatus} -> httpStatus) (\s@CancelDataQualityRulesetEvaluationRunResponse' {} a -> s {httpStatus = a} :: CancelDataQualityRulesetEvaluationRunResponse)

instance
  Prelude.NFData
    CancelDataQualityRulesetEvaluationRunResponse
  where
  rnf
    CancelDataQualityRulesetEvaluationRunResponse' {..} =
      Prelude.rnf httpStatus
