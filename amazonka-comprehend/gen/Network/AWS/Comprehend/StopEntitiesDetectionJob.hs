{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopEntitiesDetectionJob' smart constructor.
data StopEntitiesDetectionJob = StopEntitiesDetectionJob'
  { -- | The identifier of the entities detection job to stop.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest StopEntitiesDetectionJob where
  type
    Rs StopEntitiesDetectionJob =
      StopEntitiesDetectionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopEntitiesDetectionJobResponse'
            Prelude.<$> (x Prelude..?> "JobStatus")
            Prelude.<*> (x Prelude..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopEntitiesDetectionJob

instance Prelude.NFData StopEntitiesDetectionJob

instance Prelude.ToHeaders StopEntitiesDetectionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Comprehend_20171127.StopEntitiesDetectionJob" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StopEntitiesDetectionJob where
  toJSON StopEntitiesDetectionJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Prelude..= jobId)]
      )

instance Prelude.ToPath StopEntitiesDetectionJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StopEntitiesDetectionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopEntitiesDetectionJobResponse' smart constructor.
data StopEntitiesDetectionJobResponse = StopEntitiesDetectionJobResponse'
  { -- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if
    -- the job was previously stopped with the @StopEntitiesDetectionJob@
    -- operation.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The identifier of the entities detection job to stop.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  StopEntitiesDetectionJobResponse
newStopEntitiesDetectionJobResponse pHttpStatus_ =
  StopEntitiesDetectionJobResponse'
    { jobStatus =
        Prelude.Nothing,
      jobId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if
-- the job was previously stopped with the @StopEntitiesDetectionJob@
-- operation.
stopEntitiesDetectionJobResponse_jobStatus :: Lens.Lens' StopEntitiesDetectionJobResponse (Prelude.Maybe JobStatus)
stopEntitiesDetectionJobResponse_jobStatus = Lens.lens (\StopEntitiesDetectionJobResponse' {jobStatus} -> jobStatus) (\s@StopEntitiesDetectionJobResponse' {} a -> s {jobStatus = a} :: StopEntitiesDetectionJobResponse)

-- | The identifier of the entities detection job to stop.
stopEntitiesDetectionJobResponse_jobId :: Lens.Lens' StopEntitiesDetectionJobResponse (Prelude.Maybe Prelude.Text)
stopEntitiesDetectionJobResponse_jobId = Lens.lens (\StopEntitiesDetectionJobResponse' {jobId} -> jobId) (\s@StopEntitiesDetectionJobResponse' {} a -> s {jobId = a} :: StopEntitiesDetectionJobResponse)

-- | The response's http status code.
stopEntitiesDetectionJobResponse_httpStatus :: Lens.Lens' StopEntitiesDetectionJobResponse Prelude.Int
stopEntitiesDetectionJobResponse_httpStatus = Lens.lens (\StopEntitiesDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StopEntitiesDetectionJobResponse' {} a -> s {httpStatus = a} :: StopEntitiesDetectionJobResponse)

instance
  Prelude.NFData
    StopEntitiesDetectionJobResponse
