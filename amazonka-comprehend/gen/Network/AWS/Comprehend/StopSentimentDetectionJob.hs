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
-- Module      : Network.AWS.Comprehend.StopSentimentDetectionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a sentiment detection job in progress.
--
-- If the job state is @IN_PROGRESS@ the job is marked for termination and
-- put into the @STOP_REQUESTED@ state. If the job completes before it can
-- be stopped, it is put into the @COMPLETED@ state; otherwise the job is
-- be stopped and put into the @STOPPED@ state.
--
-- If the job is in the @COMPLETED@ or @FAILED@ state when you call the
-- @StopDominantLanguageDetectionJob@ operation, the operation returns a
-- 400 Internal Request Exception.
--
-- When a job is stopped, any documents already processed are written to
-- the output location.
module Network.AWS.Comprehend.StopSentimentDetectionJob
  ( -- * Creating a Request
    StopSentimentDetectionJob (..),
    newStopSentimentDetectionJob,

    -- * Request Lenses
    stopSentimentDetectionJob_jobId,

    -- * Destructuring the Response
    StopSentimentDetectionJobResponse (..),
    newStopSentimentDetectionJobResponse,

    -- * Response Lenses
    stopSentimentDetectionJobResponse_jobStatus,
    stopSentimentDetectionJobResponse_jobId,
    stopSentimentDetectionJobResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopSentimentDetectionJob' smart constructor.
data StopSentimentDetectionJob = StopSentimentDetectionJob'
  { -- | The identifier of the sentiment detection job to stop.
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopSentimentDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'stopSentimentDetectionJob_jobId' - The identifier of the sentiment detection job to stop.
newStopSentimentDetectionJob ::
  -- | 'jobId'
  Core.Text ->
  StopSentimentDetectionJob
newStopSentimentDetectionJob pJobId_ =
  StopSentimentDetectionJob' {jobId = pJobId_}

-- | The identifier of the sentiment detection job to stop.
stopSentimentDetectionJob_jobId :: Lens.Lens' StopSentimentDetectionJob Core.Text
stopSentimentDetectionJob_jobId = Lens.lens (\StopSentimentDetectionJob' {jobId} -> jobId) (\s@StopSentimentDetectionJob' {} a -> s {jobId = a} :: StopSentimentDetectionJob)

instance Core.AWSRequest StopSentimentDetectionJob where
  type
    AWSResponse StopSentimentDetectionJob =
      StopSentimentDetectionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopSentimentDetectionJobResponse'
            Core.<$> (x Core..?> "JobStatus")
            Core.<*> (x Core..?> "JobId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopSentimentDetectionJob

instance Core.NFData StopSentimentDetectionJob

instance Core.ToHeaders StopSentimentDetectionJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.StopSentimentDetectionJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopSentimentDetectionJob where
  toJSON StopSentimentDetectionJob' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.ToPath StopSentimentDetectionJob where
  toPath = Core.const "/"

instance Core.ToQuery StopSentimentDetectionJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopSentimentDetectionJobResponse' smart constructor.
data StopSentimentDetectionJobResponse = StopSentimentDetectionJobResponse'
  { -- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if
    -- the job was previously stopped with the @StopSentimentDetectionJob@
    -- operation.
    jobStatus :: Core.Maybe JobStatus,
    -- | The identifier of the sentiment detection job to stop.
    jobId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopSentimentDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'stopSentimentDetectionJobResponse_jobStatus' - Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if
-- the job was previously stopped with the @StopSentimentDetectionJob@
-- operation.
--
-- 'jobId', 'stopSentimentDetectionJobResponse_jobId' - The identifier of the sentiment detection job to stop.
--
-- 'httpStatus', 'stopSentimentDetectionJobResponse_httpStatus' - The response's http status code.
newStopSentimentDetectionJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopSentimentDetectionJobResponse
newStopSentimentDetectionJobResponse pHttpStatus_ =
  StopSentimentDetectionJobResponse'
    { jobStatus =
        Core.Nothing,
      jobId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if
-- the job was previously stopped with the @StopSentimentDetectionJob@
-- operation.
stopSentimentDetectionJobResponse_jobStatus :: Lens.Lens' StopSentimentDetectionJobResponse (Core.Maybe JobStatus)
stopSentimentDetectionJobResponse_jobStatus = Lens.lens (\StopSentimentDetectionJobResponse' {jobStatus} -> jobStatus) (\s@StopSentimentDetectionJobResponse' {} a -> s {jobStatus = a} :: StopSentimentDetectionJobResponse)

-- | The identifier of the sentiment detection job to stop.
stopSentimentDetectionJobResponse_jobId :: Lens.Lens' StopSentimentDetectionJobResponse (Core.Maybe Core.Text)
stopSentimentDetectionJobResponse_jobId = Lens.lens (\StopSentimentDetectionJobResponse' {jobId} -> jobId) (\s@StopSentimentDetectionJobResponse' {} a -> s {jobId = a} :: StopSentimentDetectionJobResponse)

-- | The response's http status code.
stopSentimentDetectionJobResponse_httpStatus :: Lens.Lens' StopSentimentDetectionJobResponse Core.Int
stopSentimentDetectionJobResponse_httpStatus = Lens.lens (\StopSentimentDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StopSentimentDetectionJobResponse' {} a -> s {httpStatus = a} :: StopSentimentDetectionJobResponse)

instance
  Core.NFData
    StopSentimentDetectionJobResponse
