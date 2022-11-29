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
-- Module      : Amazonka.MacieV2.UpdateClassificationJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the status of a classification job.
module Amazonka.MacieV2.UpdateClassificationJob
  ( -- * Creating a Request
    UpdateClassificationJob (..),
    newUpdateClassificationJob,

    -- * Request Lenses
    updateClassificationJob_jobId,
    updateClassificationJob_jobStatus,

    -- * Destructuring the Response
    UpdateClassificationJobResponse (..),
    newUpdateClassificationJobResponse,

    -- * Response Lenses
    updateClassificationJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateClassificationJob' smart constructor.
data UpdateClassificationJob = UpdateClassificationJob'
  { -- | The unique identifier for the classification job.
    jobId :: Prelude.Text,
    -- | The new status for the job. Valid values are:
    --
    -- -   CANCELLED - Stops the job permanently and cancels it. This value is
    --     valid only if the job\'s current status is IDLE, PAUSED, RUNNING, or
    --     USER_PAUSED.
    --
    --     If you specify this value and the job\'s current status is RUNNING,
    --     Amazon Macie immediately begins to stop all processing tasks for the
    --     job. You can\'t resume or restart a job after you cancel it.
    --
    -- -   RUNNING - Resumes the job. This value is valid only if the job\'s
    --     current status is USER_PAUSED.
    --
    --     If you paused the job while it was actively running and you specify
    --     this value less than 30 days after you paused the job, Macie
    --     immediately resumes processing from the point where you paused the
    --     job. Otherwise, Macie resumes the job according to the schedule and
    --     other settings for the job.
    --
    -- -   USER_PAUSED - Pauses the job temporarily. This value is valid only
    --     if the job\'s current status is IDLE, PAUSED, or RUNNING. If you
    --     specify this value and the job\'s current status is RUNNING, Macie
    --     immediately begins to pause all processing tasks for the job.
    --
    --     If you pause a one-time job and you don\'t resume it within 30 days,
    --     the job expires and Macie cancels the job. If you pause a recurring
    --     job when its status is RUNNING and you don\'t resume it within 30
    --     days, the job run expires and Macie cancels the run. To check the
    --     expiration date, refer to the UserPausedDetails.jobExpiresAt
    --     property.
    jobStatus :: JobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateClassificationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'updateClassificationJob_jobId' - The unique identifier for the classification job.
--
-- 'jobStatus', 'updateClassificationJob_jobStatus' - The new status for the job. Valid values are:
--
-- -   CANCELLED - Stops the job permanently and cancels it. This value is
--     valid only if the job\'s current status is IDLE, PAUSED, RUNNING, or
--     USER_PAUSED.
--
--     If you specify this value and the job\'s current status is RUNNING,
--     Amazon Macie immediately begins to stop all processing tasks for the
--     job. You can\'t resume or restart a job after you cancel it.
--
-- -   RUNNING - Resumes the job. This value is valid only if the job\'s
--     current status is USER_PAUSED.
--
--     If you paused the job while it was actively running and you specify
--     this value less than 30 days after you paused the job, Macie
--     immediately resumes processing from the point where you paused the
--     job. Otherwise, Macie resumes the job according to the schedule and
--     other settings for the job.
--
-- -   USER_PAUSED - Pauses the job temporarily. This value is valid only
--     if the job\'s current status is IDLE, PAUSED, or RUNNING. If you
--     specify this value and the job\'s current status is RUNNING, Macie
--     immediately begins to pause all processing tasks for the job.
--
--     If you pause a one-time job and you don\'t resume it within 30 days,
--     the job expires and Macie cancels the job. If you pause a recurring
--     job when its status is RUNNING and you don\'t resume it within 30
--     days, the job run expires and Macie cancels the run. To check the
--     expiration date, refer to the UserPausedDetails.jobExpiresAt
--     property.
newUpdateClassificationJob ::
  -- | 'jobId'
  Prelude.Text ->
  -- | 'jobStatus'
  JobStatus ->
  UpdateClassificationJob
newUpdateClassificationJob pJobId_ pJobStatus_ =
  UpdateClassificationJob'
    { jobId = pJobId_,
      jobStatus = pJobStatus_
    }

-- | The unique identifier for the classification job.
updateClassificationJob_jobId :: Lens.Lens' UpdateClassificationJob Prelude.Text
updateClassificationJob_jobId = Lens.lens (\UpdateClassificationJob' {jobId} -> jobId) (\s@UpdateClassificationJob' {} a -> s {jobId = a} :: UpdateClassificationJob)

-- | The new status for the job. Valid values are:
--
-- -   CANCELLED - Stops the job permanently and cancels it. This value is
--     valid only if the job\'s current status is IDLE, PAUSED, RUNNING, or
--     USER_PAUSED.
--
--     If you specify this value and the job\'s current status is RUNNING,
--     Amazon Macie immediately begins to stop all processing tasks for the
--     job. You can\'t resume or restart a job after you cancel it.
--
-- -   RUNNING - Resumes the job. This value is valid only if the job\'s
--     current status is USER_PAUSED.
--
--     If you paused the job while it was actively running and you specify
--     this value less than 30 days after you paused the job, Macie
--     immediately resumes processing from the point where you paused the
--     job. Otherwise, Macie resumes the job according to the schedule and
--     other settings for the job.
--
-- -   USER_PAUSED - Pauses the job temporarily. This value is valid only
--     if the job\'s current status is IDLE, PAUSED, or RUNNING. If you
--     specify this value and the job\'s current status is RUNNING, Macie
--     immediately begins to pause all processing tasks for the job.
--
--     If you pause a one-time job and you don\'t resume it within 30 days,
--     the job expires and Macie cancels the job. If you pause a recurring
--     job when its status is RUNNING and you don\'t resume it within 30
--     days, the job run expires and Macie cancels the run. To check the
--     expiration date, refer to the UserPausedDetails.jobExpiresAt
--     property.
updateClassificationJob_jobStatus :: Lens.Lens' UpdateClassificationJob JobStatus
updateClassificationJob_jobStatus = Lens.lens (\UpdateClassificationJob' {jobStatus} -> jobStatus) (\s@UpdateClassificationJob' {} a -> s {jobStatus = a} :: UpdateClassificationJob)

instance Core.AWSRequest UpdateClassificationJob where
  type
    AWSResponse UpdateClassificationJob =
      UpdateClassificationJobResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateClassificationJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateClassificationJob where
  hashWithSalt _salt UpdateClassificationJob' {..} =
    _salt `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` jobStatus

instance Prelude.NFData UpdateClassificationJob where
  rnf UpdateClassificationJob' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobStatus

instance Core.ToHeaders UpdateClassificationJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateClassificationJob where
  toJSON UpdateClassificationJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("jobStatus" Core..= jobStatus)]
      )

instance Core.ToPath UpdateClassificationJob where
  toPath UpdateClassificationJob' {..} =
    Prelude.mconcat ["/jobs/", Core.toBS jobId]

instance Core.ToQuery UpdateClassificationJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateClassificationJobResponse' smart constructor.
data UpdateClassificationJobResponse = UpdateClassificationJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateClassificationJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateClassificationJobResponse_httpStatus' - The response's http status code.
newUpdateClassificationJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateClassificationJobResponse
newUpdateClassificationJobResponse pHttpStatus_ =
  UpdateClassificationJobResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateClassificationJobResponse_httpStatus :: Lens.Lens' UpdateClassificationJobResponse Prelude.Int
updateClassificationJobResponse_httpStatus = Lens.lens (\UpdateClassificationJobResponse' {httpStatus} -> httpStatus) (\s@UpdateClassificationJobResponse' {} a -> s {httpStatus = a} :: UpdateClassificationJobResponse)

instance
  Prelude.NFData
    UpdateClassificationJobResponse
  where
  rnf UpdateClassificationJobResponse' {..} =
    Prelude.rnf httpStatus
