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
-- Module      : Network.AWS.Comprehend.StopKeyPhrasesDetectionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a key phrases detection job in progress.
--
-- If the job state is @IN_PROGRESS@ the job is marked for termination and
-- put into the @STOP_REQUESTED@ state. If the job completes before it can
-- be stopped, it is put into the @COMPLETED@ state; otherwise the job is
-- stopped and put into the @STOPPED@ state.
--
-- If the job is in the @COMPLETED@ or @FAILED@ state when you call the
-- @StopDominantLanguageDetectionJob@ operation, the operation returns a
-- 400 Internal Request Exception.
--
-- When a job is stopped, any documents already processed are written to
-- the output location.
module Network.AWS.Comprehend.StopKeyPhrasesDetectionJob
  ( -- * Creating a Request
    StopKeyPhrasesDetectionJob (..),
    newStopKeyPhrasesDetectionJob,

    -- * Request Lenses
    stopKeyPhrasesDetectionJob_jobId,

    -- * Destructuring the Response
    StopKeyPhrasesDetectionJobResponse (..),
    newStopKeyPhrasesDetectionJobResponse,

    -- * Response Lenses
    stopKeyPhrasesDetectionJobResponse_jobStatus,
    stopKeyPhrasesDetectionJobResponse_jobId,
    stopKeyPhrasesDetectionJobResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopKeyPhrasesDetectionJob' smart constructor.
data StopKeyPhrasesDetectionJob = StopKeyPhrasesDetectionJob'
  { -- | The identifier of the key phrases detection job to stop.
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopKeyPhrasesDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'stopKeyPhrasesDetectionJob_jobId' - The identifier of the key phrases detection job to stop.
newStopKeyPhrasesDetectionJob ::
  -- | 'jobId'
  Core.Text ->
  StopKeyPhrasesDetectionJob
newStopKeyPhrasesDetectionJob pJobId_ =
  StopKeyPhrasesDetectionJob' {jobId = pJobId_}

-- | The identifier of the key phrases detection job to stop.
stopKeyPhrasesDetectionJob_jobId :: Lens.Lens' StopKeyPhrasesDetectionJob Core.Text
stopKeyPhrasesDetectionJob_jobId = Lens.lens (\StopKeyPhrasesDetectionJob' {jobId} -> jobId) (\s@StopKeyPhrasesDetectionJob' {} a -> s {jobId = a} :: StopKeyPhrasesDetectionJob)

instance Core.AWSRequest StopKeyPhrasesDetectionJob where
  type
    AWSResponse StopKeyPhrasesDetectionJob =
      StopKeyPhrasesDetectionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopKeyPhrasesDetectionJobResponse'
            Core.<$> (x Core..?> "JobStatus")
            Core.<*> (x Core..?> "JobId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopKeyPhrasesDetectionJob

instance Core.NFData StopKeyPhrasesDetectionJob

instance Core.ToHeaders StopKeyPhrasesDetectionJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.StopKeyPhrasesDetectionJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopKeyPhrasesDetectionJob where
  toJSON StopKeyPhrasesDetectionJob' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.ToPath StopKeyPhrasesDetectionJob where
  toPath = Core.const "/"

instance Core.ToQuery StopKeyPhrasesDetectionJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopKeyPhrasesDetectionJobResponse' smart constructor.
data StopKeyPhrasesDetectionJobResponse = StopKeyPhrasesDetectionJobResponse'
  { -- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if
    -- the job was previously stopped with the @StopKeyPhrasesDetectionJob@
    -- operation.
    jobStatus :: Core.Maybe JobStatus,
    -- | The identifier of the key phrases detection job to stop.
    jobId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopKeyPhrasesDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'stopKeyPhrasesDetectionJobResponse_jobStatus' - Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if
-- the job was previously stopped with the @StopKeyPhrasesDetectionJob@
-- operation.
--
-- 'jobId', 'stopKeyPhrasesDetectionJobResponse_jobId' - The identifier of the key phrases detection job to stop.
--
-- 'httpStatus', 'stopKeyPhrasesDetectionJobResponse_httpStatus' - The response's http status code.
newStopKeyPhrasesDetectionJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopKeyPhrasesDetectionJobResponse
newStopKeyPhrasesDetectionJobResponse pHttpStatus_ =
  StopKeyPhrasesDetectionJobResponse'
    { jobStatus =
        Core.Nothing,
      jobId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if
-- the job was previously stopped with the @StopKeyPhrasesDetectionJob@
-- operation.
stopKeyPhrasesDetectionJobResponse_jobStatus :: Lens.Lens' StopKeyPhrasesDetectionJobResponse (Core.Maybe JobStatus)
stopKeyPhrasesDetectionJobResponse_jobStatus = Lens.lens (\StopKeyPhrasesDetectionJobResponse' {jobStatus} -> jobStatus) (\s@StopKeyPhrasesDetectionJobResponse' {} a -> s {jobStatus = a} :: StopKeyPhrasesDetectionJobResponse)

-- | The identifier of the key phrases detection job to stop.
stopKeyPhrasesDetectionJobResponse_jobId :: Lens.Lens' StopKeyPhrasesDetectionJobResponse (Core.Maybe Core.Text)
stopKeyPhrasesDetectionJobResponse_jobId = Lens.lens (\StopKeyPhrasesDetectionJobResponse' {jobId} -> jobId) (\s@StopKeyPhrasesDetectionJobResponse' {} a -> s {jobId = a} :: StopKeyPhrasesDetectionJobResponse)

-- | The response's http status code.
stopKeyPhrasesDetectionJobResponse_httpStatus :: Lens.Lens' StopKeyPhrasesDetectionJobResponse Core.Int
stopKeyPhrasesDetectionJobResponse_httpStatus = Lens.lens (\StopKeyPhrasesDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StopKeyPhrasesDetectionJobResponse' {} a -> s {httpStatus = a} :: StopKeyPhrasesDetectionJobResponse)

instance
  Core.NFData
    StopKeyPhrasesDetectionJobResponse
