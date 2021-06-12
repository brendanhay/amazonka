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
-- Module      : Network.AWS.Comprehend.StopEventsDetectionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an events detection job in progress.
module Network.AWS.Comprehend.StopEventsDetectionJob
  ( -- * Creating a Request
    StopEventsDetectionJob (..),
    newStopEventsDetectionJob,

    -- * Request Lenses
    stopEventsDetectionJob_jobId,

    -- * Destructuring the Response
    StopEventsDetectionJobResponse (..),
    newStopEventsDetectionJobResponse,

    -- * Response Lenses
    stopEventsDetectionJobResponse_jobStatus,
    stopEventsDetectionJobResponse_jobId,
    stopEventsDetectionJobResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopEventsDetectionJob' smart constructor.
data StopEventsDetectionJob = StopEventsDetectionJob'
  { -- | The identifier of the events detection job to stop.
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopEventsDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'stopEventsDetectionJob_jobId' - The identifier of the events detection job to stop.
newStopEventsDetectionJob ::
  -- | 'jobId'
  Core.Text ->
  StopEventsDetectionJob
newStopEventsDetectionJob pJobId_ =
  StopEventsDetectionJob' {jobId = pJobId_}

-- | The identifier of the events detection job to stop.
stopEventsDetectionJob_jobId :: Lens.Lens' StopEventsDetectionJob Core.Text
stopEventsDetectionJob_jobId = Lens.lens (\StopEventsDetectionJob' {jobId} -> jobId) (\s@StopEventsDetectionJob' {} a -> s {jobId = a} :: StopEventsDetectionJob)

instance Core.AWSRequest StopEventsDetectionJob where
  type
    AWSResponse StopEventsDetectionJob =
      StopEventsDetectionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopEventsDetectionJobResponse'
            Core.<$> (x Core..?> "JobStatus")
            Core.<*> (x Core..?> "JobId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopEventsDetectionJob

instance Core.NFData StopEventsDetectionJob

instance Core.ToHeaders StopEventsDetectionJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.StopEventsDetectionJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopEventsDetectionJob where
  toJSON StopEventsDetectionJob' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.ToPath StopEventsDetectionJob where
  toPath = Core.const "/"

instance Core.ToQuery StopEventsDetectionJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopEventsDetectionJobResponse' smart constructor.
data StopEventsDetectionJobResponse = StopEventsDetectionJobResponse'
  { -- | The status of the events detection job.
    jobStatus :: Core.Maybe JobStatus,
    -- | The identifier of the events detection job to stop.
    jobId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopEventsDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'stopEventsDetectionJobResponse_jobStatus' - The status of the events detection job.
--
-- 'jobId', 'stopEventsDetectionJobResponse_jobId' - The identifier of the events detection job to stop.
--
-- 'httpStatus', 'stopEventsDetectionJobResponse_httpStatus' - The response's http status code.
newStopEventsDetectionJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopEventsDetectionJobResponse
newStopEventsDetectionJobResponse pHttpStatus_ =
  StopEventsDetectionJobResponse'
    { jobStatus =
        Core.Nothing,
      jobId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the events detection job.
stopEventsDetectionJobResponse_jobStatus :: Lens.Lens' StopEventsDetectionJobResponse (Core.Maybe JobStatus)
stopEventsDetectionJobResponse_jobStatus = Lens.lens (\StopEventsDetectionJobResponse' {jobStatus} -> jobStatus) (\s@StopEventsDetectionJobResponse' {} a -> s {jobStatus = a} :: StopEventsDetectionJobResponse)

-- | The identifier of the events detection job to stop.
stopEventsDetectionJobResponse_jobId :: Lens.Lens' StopEventsDetectionJobResponse (Core.Maybe Core.Text)
stopEventsDetectionJobResponse_jobId = Lens.lens (\StopEventsDetectionJobResponse' {jobId} -> jobId) (\s@StopEventsDetectionJobResponse' {} a -> s {jobId = a} :: StopEventsDetectionJobResponse)

-- | The response's http status code.
stopEventsDetectionJobResponse_httpStatus :: Lens.Lens' StopEventsDetectionJobResponse Core.Int
stopEventsDetectionJobResponse_httpStatus = Lens.lens (\StopEventsDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StopEventsDetectionJobResponse' {} a -> s {httpStatus = a} :: StopEventsDetectionJobResponse)

instance Core.NFData StopEventsDetectionJobResponse
