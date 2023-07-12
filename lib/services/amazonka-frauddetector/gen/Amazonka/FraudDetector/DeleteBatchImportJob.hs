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
-- Module      : Amazonka.FraudDetector.DeleteBatchImportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified batch import job ID record. This action does not
-- delete the data that was batch imported.
module Amazonka.FraudDetector.DeleteBatchImportJob
  ( -- * Creating a Request
    DeleteBatchImportJob (..),
    newDeleteBatchImportJob,

    -- * Request Lenses
    deleteBatchImportJob_jobId,

    -- * Destructuring the Response
    DeleteBatchImportJobResponse (..),
    newDeleteBatchImportJobResponse,

    -- * Response Lenses
    deleteBatchImportJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteBatchImportJob' smart constructor.
data DeleteBatchImportJob = DeleteBatchImportJob'
  { -- | The ID of the batch import job to delete.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBatchImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'deleteBatchImportJob_jobId' - The ID of the batch import job to delete.
newDeleteBatchImportJob ::
  -- | 'jobId'
  Prelude.Text ->
  DeleteBatchImportJob
newDeleteBatchImportJob pJobId_ =
  DeleteBatchImportJob' {jobId = pJobId_}

-- | The ID of the batch import job to delete.
deleteBatchImportJob_jobId :: Lens.Lens' DeleteBatchImportJob Prelude.Text
deleteBatchImportJob_jobId = Lens.lens (\DeleteBatchImportJob' {jobId} -> jobId) (\s@DeleteBatchImportJob' {} a -> s {jobId = a} :: DeleteBatchImportJob)

instance Core.AWSRequest DeleteBatchImportJob where
  type
    AWSResponse DeleteBatchImportJob =
      DeleteBatchImportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteBatchImportJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBatchImportJob where
  hashWithSalt _salt DeleteBatchImportJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData DeleteBatchImportJob where
  rnf DeleteBatchImportJob' {..} = Prelude.rnf jobId

instance Data.ToHeaders DeleteBatchImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.DeleteBatchImportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteBatchImportJob where
  toJSON DeleteBatchImportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("jobId" Data..= jobId)]
      )

instance Data.ToPath DeleteBatchImportJob where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteBatchImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBatchImportJobResponse' smart constructor.
data DeleteBatchImportJobResponse = DeleteBatchImportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBatchImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteBatchImportJobResponse_httpStatus' - The response's http status code.
newDeleteBatchImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBatchImportJobResponse
newDeleteBatchImportJobResponse pHttpStatus_ =
  DeleteBatchImportJobResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteBatchImportJobResponse_httpStatus :: Lens.Lens' DeleteBatchImportJobResponse Prelude.Int
deleteBatchImportJobResponse_httpStatus = Lens.lens (\DeleteBatchImportJobResponse' {httpStatus} -> httpStatus) (\s@DeleteBatchImportJobResponse' {} a -> s {httpStatus = a} :: DeleteBatchImportJobResponse)

instance Prelude.NFData DeleteBatchImportJobResponse where
  rnf DeleteBatchImportJobResponse' {..} =
    Prelude.rnf httpStatus
