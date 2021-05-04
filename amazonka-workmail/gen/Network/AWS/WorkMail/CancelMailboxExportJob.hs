{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WorkMail.CancelMailboxExportJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a mailbox export job.
--
-- If the mailbox export job is near completion, it might not be possible
-- to cancel it.
module Network.AWS.WorkMail.CancelMailboxExportJob
  ( -- * Creating a Request
    CancelMailboxExportJob (..),
    newCancelMailboxExportJob,

    -- * Request Lenses
    cancelMailboxExportJob_clientToken,
    cancelMailboxExportJob_jobId,
    cancelMailboxExportJob_organizationId,

    -- * Destructuring the Response
    CancelMailboxExportJobResponse (..),
    newCancelMailboxExportJobResponse,

    -- * Response Lenses
    cancelMailboxExportJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newCancelMailboxExportJob' smart constructor.
data CancelMailboxExportJob = CancelMailboxExportJob'
  { -- | The idempotency token for the client request.
    clientToken :: Prelude.Text,
    -- | The job ID.
    jobId :: Prelude.Text,
    -- | The organization ID.
    organizationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelMailboxExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'cancelMailboxExportJob_clientToken' - The idempotency token for the client request.
--
-- 'jobId', 'cancelMailboxExportJob_jobId' - The job ID.
--
-- 'organizationId', 'cancelMailboxExportJob_organizationId' - The organization ID.
newCancelMailboxExportJob ::
  -- | 'clientToken'
  Prelude.Text ->
  -- | 'jobId'
  Prelude.Text ->
  -- | 'organizationId'
  Prelude.Text ->
  CancelMailboxExportJob
newCancelMailboxExportJob
  pClientToken_
  pJobId_
  pOrganizationId_ =
    CancelMailboxExportJob'
      { clientToken =
          pClientToken_,
        jobId = pJobId_,
        organizationId = pOrganizationId_
      }

-- | The idempotency token for the client request.
cancelMailboxExportJob_clientToken :: Lens.Lens' CancelMailboxExportJob Prelude.Text
cancelMailboxExportJob_clientToken = Lens.lens (\CancelMailboxExportJob' {clientToken} -> clientToken) (\s@CancelMailboxExportJob' {} a -> s {clientToken = a} :: CancelMailboxExportJob)

-- | The job ID.
cancelMailboxExportJob_jobId :: Lens.Lens' CancelMailboxExportJob Prelude.Text
cancelMailboxExportJob_jobId = Lens.lens (\CancelMailboxExportJob' {jobId} -> jobId) (\s@CancelMailboxExportJob' {} a -> s {jobId = a} :: CancelMailboxExportJob)

-- | The organization ID.
cancelMailboxExportJob_organizationId :: Lens.Lens' CancelMailboxExportJob Prelude.Text
cancelMailboxExportJob_organizationId = Lens.lens (\CancelMailboxExportJob' {organizationId} -> organizationId) (\s@CancelMailboxExportJob' {} a -> s {organizationId = a} :: CancelMailboxExportJob)

instance Prelude.AWSRequest CancelMailboxExportJob where
  type
    Rs CancelMailboxExportJob =
      CancelMailboxExportJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelMailboxExportJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelMailboxExportJob

instance Prelude.NFData CancelMailboxExportJob

instance Prelude.ToHeaders CancelMailboxExportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkMailService.CancelMailboxExportJob" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CancelMailboxExportJob where
  toJSON CancelMailboxExportJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ClientToken" Prelude..= clientToken),
            Prelude.Just ("JobId" Prelude..= jobId),
            Prelude.Just
              ("OrganizationId" Prelude..= organizationId)
          ]
      )

instance Prelude.ToPath CancelMailboxExportJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CancelMailboxExportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelMailboxExportJobResponse' smart constructor.
data CancelMailboxExportJobResponse = CancelMailboxExportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CancelMailboxExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelMailboxExportJobResponse_httpStatus' - The response's http status code.
newCancelMailboxExportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelMailboxExportJobResponse
newCancelMailboxExportJobResponse pHttpStatus_ =
  CancelMailboxExportJobResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
cancelMailboxExportJobResponse_httpStatus :: Lens.Lens' CancelMailboxExportJobResponse Prelude.Int
cancelMailboxExportJobResponse_httpStatus = Lens.lens (\CancelMailboxExportJobResponse' {httpStatus} -> httpStatus) (\s@CancelMailboxExportJobResponse' {} a -> s {httpStatus = a} :: CancelMailboxExportJobResponse)

instance
  Prelude.NFData
    CancelMailboxExportJobResponse
