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
-- Module      : Network.AWS.IoT.CancelJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a job.
module Network.AWS.IoT.CancelJob
  ( -- * Creating a Request
    CancelJob (..),
    newCancelJob,

    -- * Request Lenses
    cancelJob_reasonCode,
    cancelJob_comment,
    cancelJob_force,
    cancelJob_jobId,

    -- * Destructuring the Response
    CancelJobResponse (..),
    newCancelJobResponse,

    -- * Response Lenses
    cancelJobResponse_jobArn,
    cancelJobResponse_description,
    cancelJobResponse_jobId,
    cancelJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCancelJob' smart constructor.
data CancelJob = CancelJob'
  { -- | (Optional)A reason code string that explains why the job was canceled.
    reasonCode :: Core.Maybe Core.Text,
    -- | An optional comment string describing why the job was canceled.
    comment :: Core.Maybe Core.Text,
    -- | (Optional) If @true@ job executions with status \"IN_PROGRESS\" and
    -- \"QUEUED\" are canceled, otherwise only job executions with status
    -- \"QUEUED\" are canceled. The default is @false@.
    --
    -- Canceling a job which is \"IN_PROGRESS\", will cause a device which is
    -- executing the job to be unable to update the job execution status. Use
    -- caution and ensure that each device executing a job which is canceled is
    -- able to recover to a valid state.
    force :: Core.Maybe Core.Bool,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CancelJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reasonCode', 'cancelJob_reasonCode' - (Optional)A reason code string that explains why the job was canceled.
--
-- 'comment', 'cancelJob_comment' - An optional comment string describing why the job was canceled.
--
-- 'force', 'cancelJob_force' - (Optional) If @true@ job executions with status \"IN_PROGRESS\" and
-- \"QUEUED\" are canceled, otherwise only job executions with status
-- \"QUEUED\" are canceled. The default is @false@.
--
-- Canceling a job which is \"IN_PROGRESS\", will cause a device which is
-- executing the job to be unable to update the job execution status. Use
-- caution and ensure that each device executing a job which is canceled is
-- able to recover to a valid state.
--
-- 'jobId', 'cancelJob_jobId' - The unique identifier you assigned to this job when it was created.
newCancelJob ::
  -- | 'jobId'
  Core.Text ->
  CancelJob
newCancelJob pJobId_ =
  CancelJob'
    { reasonCode = Core.Nothing,
      comment = Core.Nothing,
      force = Core.Nothing,
      jobId = pJobId_
    }

-- | (Optional)A reason code string that explains why the job was canceled.
cancelJob_reasonCode :: Lens.Lens' CancelJob (Core.Maybe Core.Text)
cancelJob_reasonCode = Lens.lens (\CancelJob' {reasonCode} -> reasonCode) (\s@CancelJob' {} a -> s {reasonCode = a} :: CancelJob)

-- | An optional comment string describing why the job was canceled.
cancelJob_comment :: Lens.Lens' CancelJob (Core.Maybe Core.Text)
cancelJob_comment = Lens.lens (\CancelJob' {comment} -> comment) (\s@CancelJob' {} a -> s {comment = a} :: CancelJob)

-- | (Optional) If @true@ job executions with status \"IN_PROGRESS\" and
-- \"QUEUED\" are canceled, otherwise only job executions with status
-- \"QUEUED\" are canceled. The default is @false@.
--
-- Canceling a job which is \"IN_PROGRESS\", will cause a device which is
-- executing the job to be unable to update the job execution status. Use
-- caution and ensure that each device executing a job which is canceled is
-- able to recover to a valid state.
cancelJob_force :: Lens.Lens' CancelJob (Core.Maybe Core.Bool)
cancelJob_force = Lens.lens (\CancelJob' {force} -> force) (\s@CancelJob' {} a -> s {force = a} :: CancelJob)

-- | The unique identifier you assigned to this job when it was created.
cancelJob_jobId :: Lens.Lens' CancelJob Core.Text
cancelJob_jobId = Lens.lens (\CancelJob' {jobId} -> jobId) (\s@CancelJob' {} a -> s {jobId = a} :: CancelJob)

instance Core.AWSRequest CancelJob where
  type AWSResponse CancelJob = CancelJobResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelJobResponse'
            Core.<$> (x Core..?> "jobArn")
            Core.<*> (x Core..?> "description")
            Core.<*> (x Core..?> "jobId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CancelJob

instance Core.NFData CancelJob

instance Core.ToHeaders CancelJob where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON CancelJob where
  toJSON CancelJob' {..} =
    Core.object
      ( Core.catMaybes
          [ ("reasonCode" Core..=) Core.<$> reasonCode,
            ("comment" Core..=) Core.<$> comment
          ]
      )

instance Core.ToPath CancelJob where
  toPath CancelJob' {..} =
    Core.mconcat ["/jobs/", Core.toBS jobId, "/cancel"]

instance Core.ToQuery CancelJob where
  toQuery CancelJob' {..} =
    Core.mconcat ["force" Core.=: force]

-- | /See:/ 'newCancelJobResponse' smart constructor.
data CancelJobResponse = CancelJobResponse'
  { -- | The job ARN.
    jobArn :: Core.Maybe Core.Text,
    -- | A short text description of the job.
    description :: Core.Maybe Core.Text,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CancelJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobArn', 'cancelJobResponse_jobArn' - The job ARN.
--
-- 'description', 'cancelJobResponse_description' - A short text description of the job.
--
-- 'jobId', 'cancelJobResponse_jobId' - The unique identifier you assigned to this job when it was created.
--
-- 'httpStatus', 'cancelJobResponse_httpStatus' - The response's http status code.
newCancelJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CancelJobResponse
newCancelJobResponse pHttpStatus_ =
  CancelJobResponse'
    { jobArn = Core.Nothing,
      description = Core.Nothing,
      jobId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The job ARN.
cancelJobResponse_jobArn :: Lens.Lens' CancelJobResponse (Core.Maybe Core.Text)
cancelJobResponse_jobArn = Lens.lens (\CancelJobResponse' {jobArn} -> jobArn) (\s@CancelJobResponse' {} a -> s {jobArn = a} :: CancelJobResponse)

-- | A short text description of the job.
cancelJobResponse_description :: Lens.Lens' CancelJobResponse (Core.Maybe Core.Text)
cancelJobResponse_description = Lens.lens (\CancelJobResponse' {description} -> description) (\s@CancelJobResponse' {} a -> s {description = a} :: CancelJobResponse)

-- | The unique identifier you assigned to this job when it was created.
cancelJobResponse_jobId :: Lens.Lens' CancelJobResponse (Core.Maybe Core.Text)
cancelJobResponse_jobId = Lens.lens (\CancelJobResponse' {jobId} -> jobId) (\s@CancelJobResponse' {} a -> s {jobId = a} :: CancelJobResponse)

-- | The response's http status code.
cancelJobResponse_httpStatus :: Lens.Lens' CancelJobResponse Core.Int
cancelJobResponse_httpStatus = Lens.lens (\CancelJobResponse' {httpStatus} -> httpStatus) (\s@CancelJobResponse' {} a -> s {httpStatus = a} :: CancelJobResponse)

instance Core.NFData CancelJobResponse
