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
-- Module      : Amazonka.Comprehend.StopEntitiesDetectionJob
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
module Amazonka.Comprehend.StopEntitiesDetectionJob
  ( -- * Creating a Request
    StopEntitiesDetectionJob (..),
    newStopEntitiesDetectionJob,

    -- * Request Lenses
    stopEntitiesDetectionJob_jobId,

    -- * Destructuring the Response
    StopEntitiesDetectionJobResponse (..),
    newStopEntitiesDetectionJobResponse,

    -- * Response Lenses
    stopEntitiesDetectionJobResponse_jobId,
    stopEntitiesDetectionJobResponse_jobStatus,
    stopEntitiesDetectionJobResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopEntitiesDetectionJob' smart constructor.
data StopEntitiesDetectionJob = StopEntitiesDetectionJob'
  { -- | The identifier of the entities detection job to stop.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  StopEntitiesDetectionJob
newStopEntitiesDetectionJob pJobId_ =
  StopEntitiesDetectionJob' {jobId = pJobId_}

-- | The identifier of the entities detection job to stop.
stopEntitiesDetectionJob_jobId :: Lens.Lens' StopEntitiesDetectionJob Prelude.Text
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
            Prelude.<$> (x Core..?> "JobId")
            Prelude.<*> (x Core..?> "JobStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopEntitiesDetectionJob

instance Prelude.NFData StopEntitiesDetectionJob

instance Core.ToHeaders StopEntitiesDetectionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.StopEntitiesDetectionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopEntitiesDetectionJob where
  toJSON StopEntitiesDetectionJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Core..= jobId)]
      )

instance Core.ToPath StopEntitiesDetectionJob where
  toPath = Prelude.const "/"

instance Core.ToQuery StopEntitiesDetectionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopEntitiesDetectionJobResponse' smart constructor.
data StopEntitiesDetectionJobResponse = StopEntitiesDetectionJobResponse'
  { -- | The identifier of the entities detection job to stop.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if
    -- the job was previously stopped with the @StopEntitiesDetectionJob@
    -- operation.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopEntitiesDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'stopEntitiesDetectionJobResponse_jobId' - The identifier of the entities detection job to stop.
--
-- 'jobStatus', 'stopEntitiesDetectionJobResponse_jobStatus' - Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if
-- the job was previously stopped with the @StopEntitiesDetectionJob@
-- operation.
--
-- 'httpStatus', 'stopEntitiesDetectionJobResponse_httpStatus' - The response's http status code.
newStopEntitiesDetectionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopEntitiesDetectionJobResponse
newStopEntitiesDetectionJobResponse pHttpStatus_ =
  StopEntitiesDetectionJobResponse'
    { jobId =
        Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the entities detection job to stop.
stopEntitiesDetectionJobResponse_jobId :: Lens.Lens' StopEntitiesDetectionJobResponse (Prelude.Maybe Prelude.Text)
stopEntitiesDetectionJobResponse_jobId = Lens.lens (\StopEntitiesDetectionJobResponse' {jobId} -> jobId) (\s@StopEntitiesDetectionJobResponse' {} a -> s {jobId = a} :: StopEntitiesDetectionJobResponse)

-- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if
-- the job was previously stopped with the @StopEntitiesDetectionJob@
-- operation.
stopEntitiesDetectionJobResponse_jobStatus :: Lens.Lens' StopEntitiesDetectionJobResponse (Prelude.Maybe JobStatus)
stopEntitiesDetectionJobResponse_jobStatus = Lens.lens (\StopEntitiesDetectionJobResponse' {jobStatus} -> jobStatus) (\s@StopEntitiesDetectionJobResponse' {} a -> s {jobStatus = a} :: StopEntitiesDetectionJobResponse)

-- | The response's http status code.
stopEntitiesDetectionJobResponse_httpStatus :: Lens.Lens' StopEntitiesDetectionJobResponse Prelude.Int
stopEntitiesDetectionJobResponse_httpStatus = Lens.lens (\StopEntitiesDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StopEntitiesDetectionJobResponse' {} a -> s {httpStatus = a} :: StopEntitiesDetectionJobResponse)

instance
  Prelude.NFData
    StopEntitiesDetectionJobResponse
