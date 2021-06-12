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
-- Module      : Network.AWS.CodePipeline.AcknowledgeJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specified job and whether that job has been
-- received by the job worker. Used for custom actions only.
module Network.AWS.CodePipeline.AcknowledgeJob
  ( -- * Creating a Request
    AcknowledgeJob (..),
    newAcknowledgeJob,

    -- * Request Lenses
    acknowledgeJob_jobId,
    acknowledgeJob_nonce,

    -- * Destructuring the Response
    AcknowledgeJobResponse (..),
    newAcknowledgeJobResponse,

    -- * Response Lenses
    acknowledgeJobResponse_status,
    acknowledgeJobResponse_httpStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an AcknowledgeJob action.
--
-- /See:/ 'newAcknowledgeJob' smart constructor.
data AcknowledgeJob = AcknowledgeJob'
  { -- | The unique system-generated ID of the job for which you want to confirm
    -- receipt.
    jobId :: Core.Text,
    -- | A system-generated random number that AWS CodePipeline uses to ensure
    -- that the job is being worked on by only one job worker. Get this number
    -- from the response of the PollForJobs request that returned this job.
    nonce :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AcknowledgeJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'acknowledgeJob_jobId' - The unique system-generated ID of the job for which you want to confirm
-- receipt.
--
-- 'nonce', 'acknowledgeJob_nonce' - A system-generated random number that AWS CodePipeline uses to ensure
-- that the job is being worked on by only one job worker. Get this number
-- from the response of the PollForJobs request that returned this job.
newAcknowledgeJob ::
  -- | 'jobId'
  Core.Text ->
  -- | 'nonce'
  Core.Text ->
  AcknowledgeJob
newAcknowledgeJob pJobId_ pNonce_ =
  AcknowledgeJob' {jobId = pJobId_, nonce = pNonce_}

-- | The unique system-generated ID of the job for which you want to confirm
-- receipt.
acknowledgeJob_jobId :: Lens.Lens' AcknowledgeJob Core.Text
acknowledgeJob_jobId = Lens.lens (\AcknowledgeJob' {jobId} -> jobId) (\s@AcknowledgeJob' {} a -> s {jobId = a} :: AcknowledgeJob)

-- | A system-generated random number that AWS CodePipeline uses to ensure
-- that the job is being worked on by only one job worker. Get this number
-- from the response of the PollForJobs request that returned this job.
acknowledgeJob_nonce :: Lens.Lens' AcknowledgeJob Core.Text
acknowledgeJob_nonce = Lens.lens (\AcknowledgeJob' {nonce} -> nonce) (\s@AcknowledgeJob' {} a -> s {nonce = a} :: AcknowledgeJob)

instance Core.AWSRequest AcknowledgeJob where
  type
    AWSResponse AcknowledgeJob =
      AcknowledgeJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AcknowledgeJobResponse'
            Core.<$> (x Core..?> "status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AcknowledgeJob

instance Core.NFData AcknowledgeJob

instance Core.ToHeaders AcknowledgeJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.AcknowledgeJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AcknowledgeJob where
  toJSON AcknowledgeJob' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("jobId" Core..= jobId),
            Core.Just ("nonce" Core..= nonce)
          ]
      )

instance Core.ToPath AcknowledgeJob where
  toPath = Core.const "/"

instance Core.ToQuery AcknowledgeJob where
  toQuery = Core.const Core.mempty

-- | Represents the output of an AcknowledgeJob action.
--
-- /See:/ 'newAcknowledgeJobResponse' smart constructor.
data AcknowledgeJobResponse = AcknowledgeJobResponse'
  { -- | Whether the job worker has received the specified job.
    status :: Core.Maybe JobStatus,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AcknowledgeJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'acknowledgeJobResponse_status' - Whether the job worker has received the specified job.
--
-- 'httpStatus', 'acknowledgeJobResponse_httpStatus' - The response's http status code.
newAcknowledgeJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AcknowledgeJobResponse
newAcknowledgeJobResponse pHttpStatus_ =
  AcknowledgeJobResponse'
    { status = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Whether the job worker has received the specified job.
acknowledgeJobResponse_status :: Lens.Lens' AcknowledgeJobResponse (Core.Maybe JobStatus)
acknowledgeJobResponse_status = Lens.lens (\AcknowledgeJobResponse' {status} -> status) (\s@AcknowledgeJobResponse' {} a -> s {status = a} :: AcknowledgeJobResponse)

-- | The response's http status code.
acknowledgeJobResponse_httpStatus :: Lens.Lens' AcknowledgeJobResponse Core.Int
acknowledgeJobResponse_httpStatus = Lens.lens (\AcknowledgeJobResponse' {httpStatus} -> httpStatus) (\s@AcknowledgeJobResponse' {} a -> s {httpStatus = a} :: AcknowledgeJobResponse)

instance Core.NFData AcknowledgeJobResponse
