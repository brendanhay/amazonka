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
-- Module      : Amazonka.Glue.CancelDataQualityRuleRecommendationRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified recommendation run that was being used to generate
-- rules.
module Amazonka.Glue.CancelDataQualityRuleRecommendationRun
  ( -- * Creating a Request
    CancelDataQualityRuleRecommendationRun (..),
    newCancelDataQualityRuleRecommendationRun,

    -- * Request Lenses
    cancelDataQualityRuleRecommendationRun_runId,

    -- * Destructuring the Response
    CancelDataQualityRuleRecommendationRunResponse (..),
    newCancelDataQualityRuleRecommendationRunResponse,

    -- * Response Lenses
    cancelDataQualityRuleRecommendationRunResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelDataQualityRuleRecommendationRun' smart constructor.
data CancelDataQualityRuleRecommendationRun = CancelDataQualityRuleRecommendationRun'
  { -- | The unique run identifier associated with this run.
    runId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelDataQualityRuleRecommendationRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runId', 'cancelDataQualityRuleRecommendationRun_runId' - The unique run identifier associated with this run.
newCancelDataQualityRuleRecommendationRun ::
  -- | 'runId'
  Prelude.Text ->
  CancelDataQualityRuleRecommendationRun
newCancelDataQualityRuleRecommendationRun pRunId_ =
  CancelDataQualityRuleRecommendationRun'
    { runId =
        pRunId_
    }

-- | The unique run identifier associated with this run.
cancelDataQualityRuleRecommendationRun_runId :: Lens.Lens' CancelDataQualityRuleRecommendationRun Prelude.Text
cancelDataQualityRuleRecommendationRun_runId = Lens.lens (\CancelDataQualityRuleRecommendationRun' {runId} -> runId) (\s@CancelDataQualityRuleRecommendationRun' {} a -> s {runId = a} :: CancelDataQualityRuleRecommendationRun)

instance
  Core.AWSRequest
    CancelDataQualityRuleRecommendationRun
  where
  type
    AWSResponse
      CancelDataQualityRuleRecommendationRun =
      CancelDataQualityRuleRecommendationRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelDataQualityRuleRecommendationRunResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CancelDataQualityRuleRecommendationRun
  where
  hashWithSalt
    _salt
    CancelDataQualityRuleRecommendationRun' {..} =
      _salt `Prelude.hashWithSalt` runId

instance
  Prelude.NFData
    CancelDataQualityRuleRecommendationRun
  where
  rnf CancelDataQualityRuleRecommendationRun' {..} =
    Prelude.rnf runId

instance
  Data.ToHeaders
    CancelDataQualityRuleRecommendationRun
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.CancelDataQualityRuleRecommendationRun" ::
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
    CancelDataQualityRuleRecommendationRun
  where
  toJSON CancelDataQualityRuleRecommendationRun' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("RunId" Data..= runId)]
      )

instance
  Data.ToPath
    CancelDataQualityRuleRecommendationRun
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CancelDataQualityRuleRecommendationRun
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelDataQualityRuleRecommendationRunResponse' smart constructor.
data CancelDataQualityRuleRecommendationRunResponse = CancelDataQualityRuleRecommendationRunResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelDataQualityRuleRecommendationRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelDataQualityRuleRecommendationRunResponse_httpStatus' - The response's http status code.
newCancelDataQualityRuleRecommendationRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelDataQualityRuleRecommendationRunResponse
newCancelDataQualityRuleRecommendationRunResponse
  pHttpStatus_ =
    CancelDataQualityRuleRecommendationRunResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
cancelDataQualityRuleRecommendationRunResponse_httpStatus :: Lens.Lens' CancelDataQualityRuleRecommendationRunResponse Prelude.Int
cancelDataQualityRuleRecommendationRunResponse_httpStatus = Lens.lens (\CancelDataQualityRuleRecommendationRunResponse' {httpStatus} -> httpStatus) (\s@CancelDataQualityRuleRecommendationRunResponse' {} a -> s {httpStatus = a} :: CancelDataQualityRuleRecommendationRunResponse)

instance
  Prelude.NFData
    CancelDataQualityRuleRecommendationRunResponse
  where
  rnf
    CancelDataQualityRuleRecommendationRunResponse' {..} =
      Prelude.rnf httpStatus
