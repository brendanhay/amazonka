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
-- Module      : Network.AWS.Comprehend.StopPiiEntitiesDetectionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a PII entities detection job in progress.
module Network.AWS.Comprehend.StopPiiEntitiesDetectionJob
  ( -- * Creating a Request
    StopPiiEntitiesDetectionJob (..),
    newStopPiiEntitiesDetectionJob,

    -- * Request Lenses
    stopPiiEntitiesDetectionJob_jobId,

    -- * Destructuring the Response
    StopPiiEntitiesDetectionJobResponse (..),
    newStopPiiEntitiesDetectionJobResponse,

    -- * Response Lenses
    stopPiiEntitiesDetectionJobResponse_jobStatus,
    stopPiiEntitiesDetectionJobResponse_jobId,
    stopPiiEntitiesDetectionJobResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopPiiEntitiesDetectionJob' smart constructor.
data StopPiiEntitiesDetectionJob = StopPiiEntitiesDetectionJob'
  { -- | The identifier of the PII entities detection job to stop.
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopPiiEntitiesDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'stopPiiEntitiesDetectionJob_jobId' - The identifier of the PII entities detection job to stop.
newStopPiiEntitiesDetectionJob ::
  -- | 'jobId'
  Core.Text ->
  StopPiiEntitiesDetectionJob
newStopPiiEntitiesDetectionJob pJobId_ =
  StopPiiEntitiesDetectionJob' {jobId = pJobId_}

-- | The identifier of the PII entities detection job to stop.
stopPiiEntitiesDetectionJob_jobId :: Lens.Lens' StopPiiEntitiesDetectionJob Core.Text
stopPiiEntitiesDetectionJob_jobId = Lens.lens (\StopPiiEntitiesDetectionJob' {jobId} -> jobId) (\s@StopPiiEntitiesDetectionJob' {} a -> s {jobId = a} :: StopPiiEntitiesDetectionJob)

instance Core.AWSRequest StopPiiEntitiesDetectionJob where
  type
    AWSResponse StopPiiEntitiesDetectionJob =
      StopPiiEntitiesDetectionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopPiiEntitiesDetectionJobResponse'
            Core.<$> (x Core..?> "JobStatus")
            Core.<*> (x Core..?> "JobId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopPiiEntitiesDetectionJob

instance Core.NFData StopPiiEntitiesDetectionJob

instance Core.ToHeaders StopPiiEntitiesDetectionJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.StopPiiEntitiesDetectionJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopPiiEntitiesDetectionJob where
  toJSON StopPiiEntitiesDetectionJob' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.ToPath StopPiiEntitiesDetectionJob where
  toPath = Core.const "/"

instance Core.ToQuery StopPiiEntitiesDetectionJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopPiiEntitiesDetectionJobResponse' smart constructor.
data StopPiiEntitiesDetectionJobResponse = StopPiiEntitiesDetectionJobResponse'
  { -- | The status of the PII entities detection job.
    jobStatus :: Core.Maybe JobStatus,
    -- | The identifier of the PII entities detection job to stop.
    jobId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopPiiEntitiesDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'stopPiiEntitiesDetectionJobResponse_jobStatus' - The status of the PII entities detection job.
--
-- 'jobId', 'stopPiiEntitiesDetectionJobResponse_jobId' - The identifier of the PII entities detection job to stop.
--
-- 'httpStatus', 'stopPiiEntitiesDetectionJobResponse_httpStatus' - The response's http status code.
newStopPiiEntitiesDetectionJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopPiiEntitiesDetectionJobResponse
newStopPiiEntitiesDetectionJobResponse pHttpStatus_ =
  StopPiiEntitiesDetectionJobResponse'
    { jobStatus =
        Core.Nothing,
      jobId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the PII entities detection job.
stopPiiEntitiesDetectionJobResponse_jobStatus :: Lens.Lens' StopPiiEntitiesDetectionJobResponse (Core.Maybe JobStatus)
stopPiiEntitiesDetectionJobResponse_jobStatus = Lens.lens (\StopPiiEntitiesDetectionJobResponse' {jobStatus} -> jobStatus) (\s@StopPiiEntitiesDetectionJobResponse' {} a -> s {jobStatus = a} :: StopPiiEntitiesDetectionJobResponse)

-- | The identifier of the PII entities detection job to stop.
stopPiiEntitiesDetectionJobResponse_jobId :: Lens.Lens' StopPiiEntitiesDetectionJobResponse (Core.Maybe Core.Text)
stopPiiEntitiesDetectionJobResponse_jobId = Lens.lens (\StopPiiEntitiesDetectionJobResponse' {jobId} -> jobId) (\s@StopPiiEntitiesDetectionJobResponse' {} a -> s {jobId = a} :: StopPiiEntitiesDetectionJobResponse)

-- | The response's http status code.
stopPiiEntitiesDetectionJobResponse_httpStatus :: Lens.Lens' StopPiiEntitiesDetectionJobResponse Core.Int
stopPiiEntitiesDetectionJobResponse_httpStatus = Lens.lens (\StopPiiEntitiesDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StopPiiEntitiesDetectionJobResponse' {} a -> s {httpStatus = a} :: StopPiiEntitiesDetectionJobResponse)

instance
  Core.NFData
    StopPiiEntitiesDetectionJobResponse
