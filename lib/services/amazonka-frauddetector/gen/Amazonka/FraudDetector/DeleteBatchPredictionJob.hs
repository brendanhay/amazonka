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
-- Module      : Amazonka.FraudDetector.DeleteBatchPredictionJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a batch prediction job.
module Amazonka.FraudDetector.DeleteBatchPredictionJob
  ( -- * Creating a Request
    DeleteBatchPredictionJob (..),
    newDeleteBatchPredictionJob,

    -- * Request Lenses
    deleteBatchPredictionJob_jobId,

    -- * Destructuring the Response
    DeleteBatchPredictionJobResponse (..),
    newDeleteBatchPredictionJobResponse,

    -- * Response Lenses
    deleteBatchPredictionJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteBatchPredictionJob' smart constructor.
data DeleteBatchPredictionJob = DeleteBatchPredictionJob'
  { -- | The ID of the batch prediction job to delete.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBatchPredictionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'deleteBatchPredictionJob_jobId' - The ID of the batch prediction job to delete.
newDeleteBatchPredictionJob ::
  -- | 'jobId'
  Prelude.Text ->
  DeleteBatchPredictionJob
newDeleteBatchPredictionJob pJobId_ =
  DeleteBatchPredictionJob' {jobId = pJobId_}

-- | The ID of the batch prediction job to delete.
deleteBatchPredictionJob_jobId :: Lens.Lens' DeleteBatchPredictionJob Prelude.Text
deleteBatchPredictionJob_jobId = Lens.lens (\DeleteBatchPredictionJob' {jobId} -> jobId) (\s@DeleteBatchPredictionJob' {} a -> s {jobId = a} :: DeleteBatchPredictionJob)

instance Core.AWSRequest DeleteBatchPredictionJob where
  type
    AWSResponse DeleteBatchPredictionJob =
      DeleteBatchPredictionJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteBatchPredictionJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBatchPredictionJob where
  hashWithSalt _salt DeleteBatchPredictionJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData DeleteBatchPredictionJob where
  rnf DeleteBatchPredictionJob' {..} = Prelude.rnf jobId

instance Data.ToHeaders DeleteBatchPredictionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.DeleteBatchPredictionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteBatchPredictionJob where
  toJSON DeleteBatchPredictionJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("jobId" Data..= jobId)]
      )

instance Data.ToPath DeleteBatchPredictionJob where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteBatchPredictionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBatchPredictionJobResponse' smart constructor.
data DeleteBatchPredictionJobResponse = DeleteBatchPredictionJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBatchPredictionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteBatchPredictionJobResponse_httpStatus' - The response's http status code.
newDeleteBatchPredictionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBatchPredictionJobResponse
newDeleteBatchPredictionJobResponse pHttpStatus_ =
  DeleteBatchPredictionJobResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteBatchPredictionJobResponse_httpStatus :: Lens.Lens' DeleteBatchPredictionJobResponse Prelude.Int
deleteBatchPredictionJobResponse_httpStatus = Lens.lens (\DeleteBatchPredictionJobResponse' {httpStatus} -> httpStatus) (\s@DeleteBatchPredictionJobResponse' {} a -> s {httpStatus = a} :: DeleteBatchPredictionJobResponse)

instance
  Prelude.NFData
    DeleteBatchPredictionJobResponse
  where
  rnf DeleteBatchPredictionJobResponse' {..} =
    Prelude.rnf httpStatus
