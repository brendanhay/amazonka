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
-- Module      : Amazonka.FraudDetector.CancelBatchImportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an in-progress batch import job.
module Amazonka.FraudDetector.CancelBatchImportJob
  ( -- * Creating a Request
    CancelBatchImportJob (..),
    newCancelBatchImportJob,

    -- * Request Lenses
    cancelBatchImportJob_jobId,

    -- * Destructuring the Response
    CancelBatchImportJobResponse (..),
    newCancelBatchImportJobResponse,

    -- * Response Lenses
    cancelBatchImportJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelBatchImportJob' smart constructor.
data CancelBatchImportJob = CancelBatchImportJob'
  { -- | The ID of an in-progress batch import job to cancel.
    --
    -- Amazon Fraud Detector will throw an error if the batch import job is in
    -- @FAILED@, @CANCELED@, or @COMPLETED@ state.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelBatchImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'cancelBatchImportJob_jobId' - The ID of an in-progress batch import job to cancel.
--
-- Amazon Fraud Detector will throw an error if the batch import job is in
-- @FAILED@, @CANCELED@, or @COMPLETED@ state.
newCancelBatchImportJob ::
  -- | 'jobId'
  Prelude.Text ->
  CancelBatchImportJob
newCancelBatchImportJob pJobId_ =
  CancelBatchImportJob' {jobId = pJobId_}

-- | The ID of an in-progress batch import job to cancel.
--
-- Amazon Fraud Detector will throw an error if the batch import job is in
-- @FAILED@, @CANCELED@, or @COMPLETED@ state.
cancelBatchImportJob_jobId :: Lens.Lens' CancelBatchImportJob Prelude.Text
cancelBatchImportJob_jobId = Lens.lens (\CancelBatchImportJob' {jobId} -> jobId) (\s@CancelBatchImportJob' {} a -> s {jobId = a} :: CancelBatchImportJob)

instance Core.AWSRequest CancelBatchImportJob where
  type
    AWSResponse CancelBatchImportJob =
      CancelBatchImportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelBatchImportJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelBatchImportJob where
  hashWithSalt _salt CancelBatchImportJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData CancelBatchImportJob where
  rnf CancelBatchImportJob' {..} = Prelude.rnf jobId

instance Data.ToHeaders CancelBatchImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.CancelBatchImportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelBatchImportJob where
  toJSON CancelBatchImportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("jobId" Data..= jobId)]
      )

instance Data.ToPath CancelBatchImportJob where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelBatchImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelBatchImportJobResponse' smart constructor.
data CancelBatchImportJobResponse = CancelBatchImportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelBatchImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelBatchImportJobResponse_httpStatus' - The response's http status code.
newCancelBatchImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelBatchImportJobResponse
newCancelBatchImportJobResponse pHttpStatus_ =
  CancelBatchImportJobResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
cancelBatchImportJobResponse_httpStatus :: Lens.Lens' CancelBatchImportJobResponse Prelude.Int
cancelBatchImportJobResponse_httpStatus = Lens.lens (\CancelBatchImportJobResponse' {httpStatus} -> httpStatus) (\s@CancelBatchImportJobResponse' {} a -> s {httpStatus = a} :: CancelBatchImportJobResponse)

instance Prelude.NFData CancelBatchImportJobResponse where
  rnf CancelBatchImportJobResponse' {..} =
    Prelude.rnf httpStatus
