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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopEventsDetectionJob' smart constructor.
data StopEventsDetectionJob = StopEventsDetectionJob'
  { -- | The identifier of the events detection job to stop.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  StopEventsDetectionJob
newStopEventsDetectionJob pJobId_ =
  StopEventsDetectionJob' {jobId = pJobId_}

-- | The identifier of the events detection job to stop.
stopEventsDetectionJob_jobId :: Lens.Lens' StopEventsDetectionJob Prelude.Text
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
            Prelude.<$> (x Core..?> "JobStatus")
            Prelude.<*> (x Core..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopEventsDetectionJob

instance Prelude.NFData StopEventsDetectionJob

instance Core.ToHeaders StopEventsDetectionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.StopEventsDetectionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopEventsDetectionJob where
  toJSON StopEventsDetectionJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Core..= jobId)]
      )

instance Core.ToPath StopEventsDetectionJob where
  toPath = Prelude.const "/"

instance Core.ToQuery StopEventsDetectionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopEventsDetectionJobResponse' smart constructor.
data StopEventsDetectionJobResponse = StopEventsDetectionJobResponse'
  { -- | The status of the events detection job.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The identifier of the events detection job to stop.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  StopEventsDetectionJobResponse
newStopEventsDetectionJobResponse pHttpStatus_ =
  StopEventsDetectionJobResponse'
    { jobStatus =
        Prelude.Nothing,
      jobId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the events detection job.
stopEventsDetectionJobResponse_jobStatus :: Lens.Lens' StopEventsDetectionJobResponse (Prelude.Maybe JobStatus)
stopEventsDetectionJobResponse_jobStatus = Lens.lens (\StopEventsDetectionJobResponse' {jobStatus} -> jobStatus) (\s@StopEventsDetectionJobResponse' {} a -> s {jobStatus = a} :: StopEventsDetectionJobResponse)

-- | The identifier of the events detection job to stop.
stopEventsDetectionJobResponse_jobId :: Lens.Lens' StopEventsDetectionJobResponse (Prelude.Maybe Prelude.Text)
stopEventsDetectionJobResponse_jobId = Lens.lens (\StopEventsDetectionJobResponse' {jobId} -> jobId) (\s@StopEventsDetectionJobResponse' {} a -> s {jobId = a} :: StopEventsDetectionJobResponse)

-- | The response's http status code.
stopEventsDetectionJobResponse_httpStatus :: Lens.Lens' StopEventsDetectionJobResponse Prelude.Int
stopEventsDetectionJobResponse_httpStatus = Lens.lens (\StopEventsDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StopEventsDetectionJobResponse' {} a -> s {httpStatus = a} :: StopEventsDetectionJobResponse)

instance
  Prelude.NFData
    StopEventsDetectionJobResponse
