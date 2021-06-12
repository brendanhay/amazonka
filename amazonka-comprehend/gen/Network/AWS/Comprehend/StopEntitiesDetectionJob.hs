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
-- Module      : Network.AWS.Comprehend.StopEntitiesDetectionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an entities detection job in progress.
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
module Network.AWS.Comprehend.StopEntitiesDetectionJob
  ( -- * Creating a Request
    StopEntitiesDetectionJob (..),
    newStopEntitiesDetectionJob,

    -- * Request Lenses
    stopEntitiesDetectionJob_jobId,

    -- * Destructuring the Response
    StopEntitiesDetectionJobResponse (..),
    newStopEntitiesDetectionJobResponse,

    -- * Response Lenses
    stopEntitiesDetectionJobResponse_jobStatus,
    stopEntitiesDetectionJobResponse_jobId,
    stopEntitiesDetectionJobResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopEntitiesDetectionJob' smart constructor.
data StopEntitiesDetectionJob = StopEntitiesDetectionJob'
  { -- | The identifier of the entities detection job to stop.
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopEntitiesDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'stopEntitiesDetectionJob_jobId' - The identifier of the entities detection job to stop.
newStopEntitiesDetectionJob ::
  -- | 'jobId'
  Core.Text ->
  StopEntitiesDetectionJob
newStopEntitiesDetectionJob pJobId_ =
  StopEntitiesDetectionJob' {jobId = pJobId_}

-- | The identifier of the entities detection job to stop.
stopEntitiesDetectionJob_jobId :: Lens.Lens' StopEntitiesDetectionJob Core.Text
stopEntitiesDetectionJob_jobId = Lens.lens (\StopEntitiesDetectionJob' {jobId} -> jobId) (\s@StopEntitiesDetectionJob' {} a -> s {jobId = a} :: StopEntitiesDetectionJob)

instance Core.AWSRequest StopEntitiesDetectionJob where
  type
    AWSResponse StopEntitiesDetectionJob =
      StopEntitiesDetectionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopEntitiesDetectionJobResponse'
            Core.<$> (x Core..?> "JobStatus")
            Core.<*> (x Core..?> "JobId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopEntitiesDetectionJob

instance Core.NFData StopEntitiesDetectionJob

instance Core.ToHeaders StopEntitiesDetectionJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.StopEntitiesDetectionJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopEntitiesDetectionJob where
  toJSON StopEntitiesDetectionJob' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.ToPath StopEntitiesDetectionJob where
  toPath = Core.const "/"

instance Core.ToQuery StopEntitiesDetectionJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopEntitiesDetectionJobResponse' smart constructor.
data StopEntitiesDetectionJobResponse = StopEntitiesDetectionJobResponse'
  { -- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if
    -- the job was previously stopped with the @StopEntitiesDetectionJob@
    -- operation.
    jobStatus :: Core.Maybe JobStatus,
    -- | The identifier of the entities detection job to stop.
    jobId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopEntitiesDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'stopEntitiesDetectionJobResponse_jobStatus' - Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if
-- the job was previously stopped with the @StopEntitiesDetectionJob@
-- operation.
--
-- 'jobId', 'stopEntitiesDetectionJobResponse_jobId' - The identifier of the entities detection job to stop.
--
-- 'httpStatus', 'stopEntitiesDetectionJobResponse_httpStatus' - The response's http status code.
newStopEntitiesDetectionJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopEntitiesDetectionJobResponse
newStopEntitiesDetectionJobResponse pHttpStatus_ =
  StopEntitiesDetectionJobResponse'
    { jobStatus =
        Core.Nothing,
      jobId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if
-- the job was previously stopped with the @StopEntitiesDetectionJob@
-- operation.
stopEntitiesDetectionJobResponse_jobStatus :: Lens.Lens' StopEntitiesDetectionJobResponse (Core.Maybe JobStatus)
stopEntitiesDetectionJobResponse_jobStatus = Lens.lens (\StopEntitiesDetectionJobResponse' {jobStatus} -> jobStatus) (\s@StopEntitiesDetectionJobResponse' {} a -> s {jobStatus = a} :: StopEntitiesDetectionJobResponse)

-- | The identifier of the entities detection job to stop.
stopEntitiesDetectionJobResponse_jobId :: Lens.Lens' StopEntitiesDetectionJobResponse (Core.Maybe Core.Text)
stopEntitiesDetectionJobResponse_jobId = Lens.lens (\StopEntitiesDetectionJobResponse' {jobId} -> jobId) (\s@StopEntitiesDetectionJobResponse' {} a -> s {jobId = a} :: StopEntitiesDetectionJobResponse)

-- | The response's http status code.
stopEntitiesDetectionJobResponse_httpStatus :: Lens.Lens' StopEntitiesDetectionJobResponse Core.Int
stopEntitiesDetectionJobResponse_httpStatus = Lens.lens (\StopEntitiesDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StopEntitiesDetectionJobResponse' {} a -> s {httpStatus = a} :: StopEntitiesDetectionJobResponse)

instance Core.NFData StopEntitiesDetectionJobResponse
