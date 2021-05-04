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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopKeyPhrasesDetectionJob' smart constructor.
data StopKeyPhrasesDetectionJob = StopKeyPhrasesDetectionJob'
  { -- | The identifier of the key phrases detection job to stop.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  StopKeyPhrasesDetectionJob
newStopKeyPhrasesDetectionJob pJobId_ =
  StopKeyPhrasesDetectionJob' {jobId = pJobId_}

-- | The identifier of the key phrases detection job to stop.
stopKeyPhrasesDetectionJob_jobId :: Lens.Lens' StopKeyPhrasesDetectionJob Prelude.Text
stopKeyPhrasesDetectionJob_jobId = Lens.lens (\StopKeyPhrasesDetectionJob' {jobId} -> jobId) (\s@StopKeyPhrasesDetectionJob' {} a -> s {jobId = a} :: StopKeyPhrasesDetectionJob)

instance
  Prelude.AWSRequest
    StopKeyPhrasesDetectionJob
  where
  type
    Rs StopKeyPhrasesDetectionJob =
      StopKeyPhrasesDetectionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopKeyPhrasesDetectionJobResponse'
            Prelude.<$> (x Prelude..?> "JobStatus")
            Prelude.<*> (x Prelude..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopKeyPhrasesDetectionJob

instance Prelude.NFData StopKeyPhrasesDetectionJob

instance Prelude.ToHeaders StopKeyPhrasesDetectionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Comprehend_20171127.StopKeyPhrasesDetectionJob" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StopKeyPhrasesDetectionJob where
  toJSON StopKeyPhrasesDetectionJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Prelude..= jobId)]
      )

instance Prelude.ToPath StopKeyPhrasesDetectionJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StopKeyPhrasesDetectionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopKeyPhrasesDetectionJobResponse' smart constructor.
data StopKeyPhrasesDetectionJobResponse = StopKeyPhrasesDetectionJobResponse'
  { -- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if
    -- the job was previously stopped with the @StopKeyPhrasesDetectionJob@
    -- operation.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The identifier of the key phrases detection job to stop.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  StopKeyPhrasesDetectionJobResponse
newStopKeyPhrasesDetectionJobResponse pHttpStatus_ =
  StopKeyPhrasesDetectionJobResponse'
    { jobStatus =
        Prelude.Nothing,
      jobId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if
-- the job was previously stopped with the @StopKeyPhrasesDetectionJob@
-- operation.
stopKeyPhrasesDetectionJobResponse_jobStatus :: Lens.Lens' StopKeyPhrasesDetectionJobResponse (Prelude.Maybe JobStatus)
stopKeyPhrasesDetectionJobResponse_jobStatus = Lens.lens (\StopKeyPhrasesDetectionJobResponse' {jobStatus} -> jobStatus) (\s@StopKeyPhrasesDetectionJobResponse' {} a -> s {jobStatus = a} :: StopKeyPhrasesDetectionJobResponse)

-- | The identifier of the key phrases detection job to stop.
stopKeyPhrasesDetectionJobResponse_jobId :: Lens.Lens' StopKeyPhrasesDetectionJobResponse (Prelude.Maybe Prelude.Text)
stopKeyPhrasesDetectionJobResponse_jobId = Lens.lens (\StopKeyPhrasesDetectionJobResponse' {jobId} -> jobId) (\s@StopKeyPhrasesDetectionJobResponse' {} a -> s {jobId = a} :: StopKeyPhrasesDetectionJobResponse)

-- | The response's http status code.
stopKeyPhrasesDetectionJobResponse_httpStatus :: Lens.Lens' StopKeyPhrasesDetectionJobResponse Prelude.Int
stopKeyPhrasesDetectionJobResponse_httpStatus = Lens.lens (\StopKeyPhrasesDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StopKeyPhrasesDetectionJobResponse' {} a -> s {httpStatus = a} :: StopKeyPhrasesDetectionJobResponse)

instance
  Prelude.NFData
    StopKeyPhrasesDetectionJobResponse
