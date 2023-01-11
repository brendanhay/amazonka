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
-- Module      : Amazonka.Omics.CancelVariantImportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a variant import job.
module Amazonka.Omics.CancelVariantImportJob
  ( -- * Creating a Request
    CancelVariantImportJob (..),
    newCancelVariantImportJob,

    -- * Request Lenses
    cancelVariantImportJob_jobId,

    -- * Destructuring the Response
    CancelVariantImportJobResponse (..),
    newCancelVariantImportJobResponse,

    -- * Response Lenses
    cancelVariantImportJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelVariantImportJob' smart constructor.
data CancelVariantImportJob = CancelVariantImportJob'
  { -- | The job\'s ID.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelVariantImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'cancelVariantImportJob_jobId' - The job\'s ID.
newCancelVariantImportJob ::
  -- | 'jobId'
  Prelude.Text ->
  CancelVariantImportJob
newCancelVariantImportJob pJobId_ =
  CancelVariantImportJob' {jobId = pJobId_}

-- | The job\'s ID.
cancelVariantImportJob_jobId :: Lens.Lens' CancelVariantImportJob Prelude.Text
cancelVariantImportJob_jobId = Lens.lens (\CancelVariantImportJob' {jobId} -> jobId) (\s@CancelVariantImportJob' {} a -> s {jobId = a} :: CancelVariantImportJob)

instance Core.AWSRequest CancelVariantImportJob where
  type
    AWSResponse CancelVariantImportJob =
      CancelVariantImportJobResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelVariantImportJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelVariantImportJob where
  hashWithSalt _salt CancelVariantImportJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData CancelVariantImportJob where
  rnf CancelVariantImportJob' {..} = Prelude.rnf jobId

instance Data.ToHeaders CancelVariantImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath CancelVariantImportJob where
  toPath CancelVariantImportJob' {..} =
    Prelude.mconcat
      ["/import/variant/", Data.toBS jobId]

instance Data.ToQuery CancelVariantImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelVariantImportJobResponse' smart constructor.
data CancelVariantImportJobResponse = CancelVariantImportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelVariantImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelVariantImportJobResponse_httpStatus' - The response's http status code.
newCancelVariantImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelVariantImportJobResponse
newCancelVariantImportJobResponse pHttpStatus_ =
  CancelVariantImportJobResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
cancelVariantImportJobResponse_httpStatus :: Lens.Lens' CancelVariantImportJobResponse Prelude.Int
cancelVariantImportJobResponse_httpStatus = Lens.lens (\CancelVariantImportJobResponse' {httpStatus} -> httpStatus) (\s@CancelVariantImportJobResponse' {} a -> s {httpStatus = a} :: CancelVariantImportJobResponse)

instance
  Prelude.NFData
    CancelVariantImportJobResponse
  where
  rnf CancelVariantImportJobResponse' {..} =
    Prelude.rnf httpStatus
