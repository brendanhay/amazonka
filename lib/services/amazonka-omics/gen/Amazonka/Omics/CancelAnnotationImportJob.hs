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
-- Module      : Amazonka.Omics.CancelAnnotationImportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an annotation import job.
module Amazonka.Omics.CancelAnnotationImportJob
  ( -- * Creating a Request
    CancelAnnotationImportJob (..),
    newCancelAnnotationImportJob,

    -- * Request Lenses
    cancelAnnotationImportJob_jobId,

    -- * Destructuring the Response
    CancelAnnotationImportJobResponse (..),
    newCancelAnnotationImportJobResponse,

    -- * Response Lenses
    cancelAnnotationImportJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelAnnotationImportJob' smart constructor.
data CancelAnnotationImportJob = CancelAnnotationImportJob'
  { -- | The job\'s ID.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelAnnotationImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'cancelAnnotationImportJob_jobId' - The job\'s ID.
newCancelAnnotationImportJob ::
  -- | 'jobId'
  Prelude.Text ->
  CancelAnnotationImportJob
newCancelAnnotationImportJob pJobId_ =
  CancelAnnotationImportJob' {jobId = pJobId_}

-- | The job\'s ID.
cancelAnnotationImportJob_jobId :: Lens.Lens' CancelAnnotationImportJob Prelude.Text
cancelAnnotationImportJob_jobId = Lens.lens (\CancelAnnotationImportJob' {jobId} -> jobId) (\s@CancelAnnotationImportJob' {} a -> s {jobId = a} :: CancelAnnotationImportJob)

instance Core.AWSRequest CancelAnnotationImportJob where
  type
    AWSResponse CancelAnnotationImportJob =
      CancelAnnotationImportJobResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelAnnotationImportJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelAnnotationImportJob where
  hashWithSalt _salt CancelAnnotationImportJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData CancelAnnotationImportJob where
  rnf CancelAnnotationImportJob' {..} =
    Prelude.rnf jobId

instance Data.ToHeaders CancelAnnotationImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath CancelAnnotationImportJob where
  toPath CancelAnnotationImportJob' {..} =
    Prelude.mconcat
      ["/import/annotation/", Data.toBS jobId]

instance Data.ToQuery CancelAnnotationImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelAnnotationImportJobResponse' smart constructor.
data CancelAnnotationImportJobResponse = CancelAnnotationImportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelAnnotationImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelAnnotationImportJobResponse_httpStatus' - The response's http status code.
newCancelAnnotationImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelAnnotationImportJobResponse
newCancelAnnotationImportJobResponse pHttpStatus_ =
  CancelAnnotationImportJobResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
cancelAnnotationImportJobResponse_httpStatus :: Lens.Lens' CancelAnnotationImportJobResponse Prelude.Int
cancelAnnotationImportJobResponse_httpStatus = Lens.lens (\CancelAnnotationImportJobResponse' {httpStatus} -> httpStatus) (\s@CancelAnnotationImportJobResponse' {} a -> s {httpStatus = a} :: CancelAnnotationImportJobResponse)

instance
  Prelude.NFData
    CancelAnnotationImportJobResponse
  where
  rnf CancelAnnotationImportJobResponse' {..} =
    Prelude.rnf httpStatus
