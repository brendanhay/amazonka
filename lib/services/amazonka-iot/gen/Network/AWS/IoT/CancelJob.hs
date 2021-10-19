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
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CancelJob>
-- action.
module Network.AWS.IoT.CancelJob
  ( -- * Creating a Request
    CancelJob (..),
    newCancelJob,

    -- * Request Lenses
    cancelJob_force,
    cancelJob_reasonCode,
    cancelJob_comment,
    cancelJob_jobId,

    -- * Destructuring the Response
    CancelJobResponse (..),
    newCancelJobResponse,

    -- * Response Lenses
    cancelJobResponse_jobId,
    cancelJobResponse_jobArn,
    cancelJobResponse_description,
    cancelJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCancelJob' smart constructor.
data CancelJob = CancelJob'
  { -- | (Optional) If @true@ job executions with status \"IN_PROGRESS\" and
    -- \"QUEUED\" are canceled, otherwise only job executions with status
    -- \"QUEUED\" are canceled. The default is @false@.
    --
    -- Canceling a job which is \"IN_PROGRESS\", will cause a device which is
    -- executing the job to be unable to update the job execution status. Use
    -- caution and ensure that each device executing a job which is canceled is
    -- able to recover to a valid state.
    force :: Prelude.Maybe Prelude.Bool,
    -- | (Optional)A reason code string that explains why the job was canceled.
    reasonCode :: Prelude.Maybe Prelude.Text,
    -- | An optional comment string describing why the job was canceled.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'reasonCode', 'cancelJob_reasonCode' - (Optional)A reason code string that explains why the job was canceled.
--
-- 'comment', 'cancelJob_comment' - An optional comment string describing why the job was canceled.
--
-- 'jobId', 'cancelJob_jobId' - The unique identifier you assigned to this job when it was created.
newCancelJob ::
  -- | 'jobId'
  Prelude.Text ->
  CancelJob
newCancelJob pJobId_ =
  CancelJob'
    { force = Prelude.Nothing,
      reasonCode = Prelude.Nothing,
      comment = Prelude.Nothing,
      jobId = pJobId_
    }

-- | (Optional) If @true@ job executions with status \"IN_PROGRESS\" and
-- \"QUEUED\" are canceled, otherwise only job executions with status
-- \"QUEUED\" are canceled. The default is @false@.
--
-- Canceling a job which is \"IN_PROGRESS\", will cause a device which is
-- executing the job to be unable to update the job execution status. Use
-- caution and ensure that each device executing a job which is canceled is
-- able to recover to a valid state.
cancelJob_force :: Lens.Lens' CancelJob (Prelude.Maybe Prelude.Bool)
cancelJob_force = Lens.lens (\CancelJob' {force} -> force) (\s@CancelJob' {} a -> s {force = a} :: CancelJob)

-- | (Optional)A reason code string that explains why the job was canceled.
cancelJob_reasonCode :: Lens.Lens' CancelJob (Prelude.Maybe Prelude.Text)
cancelJob_reasonCode = Lens.lens (\CancelJob' {reasonCode} -> reasonCode) (\s@CancelJob' {} a -> s {reasonCode = a} :: CancelJob)

-- | An optional comment string describing why the job was canceled.
cancelJob_comment :: Lens.Lens' CancelJob (Prelude.Maybe Prelude.Text)
cancelJob_comment = Lens.lens (\CancelJob' {comment} -> comment) (\s@CancelJob' {} a -> s {comment = a} :: CancelJob)

-- | The unique identifier you assigned to this job when it was created.
cancelJob_jobId :: Lens.Lens' CancelJob Prelude.Text
cancelJob_jobId = Lens.lens (\CancelJob' {jobId} -> jobId) (\s@CancelJob' {} a -> s {jobId = a} :: CancelJob)

instance Core.AWSRequest CancelJob where
  type AWSResponse CancelJob = CancelJobResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelJobResponse'
            Prelude.<$> (x Core..?> "jobId")
            Prelude.<*> (x Core..?> "jobArn")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelJob

instance Prelude.NFData CancelJob

instance Core.ToHeaders CancelJob where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CancelJob where
  toJSON CancelJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("reasonCode" Core..=) Prelude.<$> reasonCode,
            ("comment" Core..=) Prelude.<$> comment
          ]
      )

instance Core.ToPath CancelJob where
  toPath CancelJob' {..} =
    Prelude.mconcat
      ["/jobs/", Core.toBS jobId, "/cancel"]

instance Core.ToQuery CancelJob where
  toQuery CancelJob' {..} =
    Prelude.mconcat ["force" Core.=: force]

-- | /See:/ 'newCancelJobResponse' smart constructor.
data CancelJobResponse = CancelJobResponse'
  { -- | The unique identifier you assigned to this job when it was created.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The job ARN.
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | A short text description of the job.
    description :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'cancelJobResponse_jobId' - The unique identifier you assigned to this job when it was created.
--
-- 'jobArn', 'cancelJobResponse_jobArn' - The job ARN.
--
-- 'description', 'cancelJobResponse_description' - A short text description of the job.
--
-- 'httpStatus', 'cancelJobResponse_httpStatus' - The response's http status code.
newCancelJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelJobResponse
newCancelJobResponse pHttpStatus_ =
  CancelJobResponse'
    { jobId = Prelude.Nothing,
      jobArn = Prelude.Nothing,
      description = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier you assigned to this job when it was created.
cancelJobResponse_jobId :: Lens.Lens' CancelJobResponse (Prelude.Maybe Prelude.Text)
cancelJobResponse_jobId = Lens.lens (\CancelJobResponse' {jobId} -> jobId) (\s@CancelJobResponse' {} a -> s {jobId = a} :: CancelJobResponse)

-- | The job ARN.
cancelJobResponse_jobArn :: Lens.Lens' CancelJobResponse (Prelude.Maybe Prelude.Text)
cancelJobResponse_jobArn = Lens.lens (\CancelJobResponse' {jobArn} -> jobArn) (\s@CancelJobResponse' {} a -> s {jobArn = a} :: CancelJobResponse)

-- | A short text description of the job.
cancelJobResponse_description :: Lens.Lens' CancelJobResponse (Prelude.Maybe Prelude.Text)
cancelJobResponse_description = Lens.lens (\CancelJobResponse' {description} -> description) (\s@CancelJobResponse' {} a -> s {description = a} :: CancelJobResponse)

-- | The response's http status code.
cancelJobResponse_httpStatus :: Lens.Lens' CancelJobResponse Prelude.Int
cancelJobResponse_httpStatus = Lens.lens (\CancelJobResponse' {httpStatus} -> httpStatus) (\s@CancelJobResponse' {} a -> s {httpStatus = a} :: CancelJobResponse)

instance Prelude.NFData CancelJobResponse
