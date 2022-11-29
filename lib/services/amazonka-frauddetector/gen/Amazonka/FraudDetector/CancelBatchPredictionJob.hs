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
-- Module      : Amazonka.FraudDetector.CancelBatchPredictionJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified batch prediction job.
module Amazonka.FraudDetector.CancelBatchPredictionJob
  ( -- * Creating a Request
    CancelBatchPredictionJob (..),
    newCancelBatchPredictionJob,

    -- * Request Lenses
    cancelBatchPredictionJob_jobId,

    -- * Destructuring the Response
    CancelBatchPredictionJobResponse (..),
    newCancelBatchPredictionJobResponse,

    -- * Response Lenses
    cancelBatchPredictionJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelBatchPredictionJob' smart constructor.
data CancelBatchPredictionJob = CancelBatchPredictionJob'
  { -- | The ID of the batch prediction job to cancel.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelBatchPredictionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'cancelBatchPredictionJob_jobId' - The ID of the batch prediction job to cancel.
newCancelBatchPredictionJob ::
  -- | 'jobId'
  Prelude.Text ->
  CancelBatchPredictionJob
newCancelBatchPredictionJob pJobId_ =
  CancelBatchPredictionJob' {jobId = pJobId_}

-- | The ID of the batch prediction job to cancel.
cancelBatchPredictionJob_jobId :: Lens.Lens' CancelBatchPredictionJob Prelude.Text
cancelBatchPredictionJob_jobId = Lens.lens (\CancelBatchPredictionJob' {jobId} -> jobId) (\s@CancelBatchPredictionJob' {} a -> s {jobId = a} :: CancelBatchPredictionJob)

instance Core.AWSRequest CancelBatchPredictionJob where
  type
    AWSResponse CancelBatchPredictionJob =
      CancelBatchPredictionJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelBatchPredictionJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelBatchPredictionJob where
  hashWithSalt _salt CancelBatchPredictionJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData CancelBatchPredictionJob where
  rnf CancelBatchPredictionJob' {..} = Prelude.rnf jobId

instance Core.ToHeaders CancelBatchPredictionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHawksNestServiceFacade.CancelBatchPredictionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CancelBatchPredictionJob where
  toJSON CancelBatchPredictionJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("jobId" Core..= jobId)]
      )

instance Core.ToPath CancelBatchPredictionJob where
  toPath = Prelude.const "/"

instance Core.ToQuery CancelBatchPredictionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelBatchPredictionJobResponse' smart constructor.
data CancelBatchPredictionJobResponse = CancelBatchPredictionJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelBatchPredictionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelBatchPredictionJobResponse_httpStatus' - The response's http status code.
newCancelBatchPredictionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelBatchPredictionJobResponse
newCancelBatchPredictionJobResponse pHttpStatus_ =
  CancelBatchPredictionJobResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
cancelBatchPredictionJobResponse_httpStatus :: Lens.Lens' CancelBatchPredictionJobResponse Prelude.Int
cancelBatchPredictionJobResponse_httpStatus = Lens.lens (\CancelBatchPredictionJobResponse' {httpStatus} -> httpStatus) (\s@CancelBatchPredictionJobResponse' {} a -> s {httpStatus = a} :: CancelBatchPredictionJobResponse)

instance
  Prelude.NFData
    CancelBatchPredictionJobResponse
  where
  rnf CancelBatchPredictionJobResponse' {..} =
    Prelude.rnf httpStatus
