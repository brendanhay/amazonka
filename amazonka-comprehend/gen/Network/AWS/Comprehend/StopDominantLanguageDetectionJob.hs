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
-- Module      : Network.AWS.Comprehend.StopDominantLanguageDetectionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a dominant language detection job in progress.
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
module Network.AWS.Comprehend.StopDominantLanguageDetectionJob
  ( -- * Creating a Request
    StopDominantLanguageDetectionJob (..),
    newStopDominantLanguageDetectionJob,

    -- * Request Lenses
    stopDominantLanguageDetectionJob_jobId,

    -- * Destructuring the Response
    StopDominantLanguageDetectionJobResponse (..),
    newStopDominantLanguageDetectionJobResponse,

    -- * Response Lenses
    stopDominantLanguageDetectionJobResponse_jobStatus,
    stopDominantLanguageDetectionJobResponse_jobId,
    stopDominantLanguageDetectionJobResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopDominantLanguageDetectionJob' smart constructor.
data StopDominantLanguageDetectionJob = StopDominantLanguageDetectionJob'
  { -- | The identifier of the dominant language detection job to stop.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopDominantLanguageDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'stopDominantLanguageDetectionJob_jobId' - The identifier of the dominant language detection job to stop.
newStopDominantLanguageDetectionJob ::
  -- | 'jobId'
  Prelude.Text ->
  StopDominantLanguageDetectionJob
newStopDominantLanguageDetectionJob pJobId_ =
  StopDominantLanguageDetectionJob' {jobId = pJobId_}

-- | The identifier of the dominant language detection job to stop.
stopDominantLanguageDetectionJob_jobId :: Lens.Lens' StopDominantLanguageDetectionJob Prelude.Text
stopDominantLanguageDetectionJob_jobId = Lens.lens (\StopDominantLanguageDetectionJob' {jobId} -> jobId) (\s@StopDominantLanguageDetectionJob' {} a -> s {jobId = a} :: StopDominantLanguageDetectionJob)

instance
  Prelude.AWSRequest
    StopDominantLanguageDetectionJob
  where
  type
    Rs StopDominantLanguageDetectionJob =
      StopDominantLanguageDetectionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopDominantLanguageDetectionJobResponse'
            Prelude.<$> (x Prelude..?> "JobStatus")
            Prelude.<*> (x Prelude..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StopDominantLanguageDetectionJob

instance
  Prelude.NFData
    StopDominantLanguageDetectionJob

instance
  Prelude.ToHeaders
    StopDominantLanguageDetectionJob
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Comprehend_20171127.StopDominantLanguageDetectionJob" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    StopDominantLanguageDetectionJob
  where
  toJSON StopDominantLanguageDetectionJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Prelude..= jobId)]
      )

instance
  Prelude.ToPath
    StopDominantLanguageDetectionJob
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    StopDominantLanguageDetectionJob
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopDominantLanguageDetectionJobResponse' smart constructor.
data StopDominantLanguageDetectionJobResponse = StopDominantLanguageDetectionJobResponse'
  { -- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if
    -- the job was previously stopped with the
    -- @StopDominantLanguageDetectionJob@ operation.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The identifier of the dominant language detection job to stop.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopDominantLanguageDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'stopDominantLanguageDetectionJobResponse_jobStatus' - Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if
-- the job was previously stopped with the
-- @StopDominantLanguageDetectionJob@ operation.
--
-- 'jobId', 'stopDominantLanguageDetectionJobResponse_jobId' - The identifier of the dominant language detection job to stop.
--
-- 'httpStatus', 'stopDominantLanguageDetectionJobResponse_httpStatus' - The response's http status code.
newStopDominantLanguageDetectionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopDominantLanguageDetectionJobResponse
newStopDominantLanguageDetectionJobResponse
  pHttpStatus_ =
    StopDominantLanguageDetectionJobResponse'
      { jobStatus =
          Prelude.Nothing,
        jobId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if
-- the job was previously stopped with the
-- @StopDominantLanguageDetectionJob@ operation.
stopDominantLanguageDetectionJobResponse_jobStatus :: Lens.Lens' StopDominantLanguageDetectionJobResponse (Prelude.Maybe JobStatus)
stopDominantLanguageDetectionJobResponse_jobStatus = Lens.lens (\StopDominantLanguageDetectionJobResponse' {jobStatus} -> jobStatus) (\s@StopDominantLanguageDetectionJobResponse' {} a -> s {jobStatus = a} :: StopDominantLanguageDetectionJobResponse)

-- | The identifier of the dominant language detection job to stop.
stopDominantLanguageDetectionJobResponse_jobId :: Lens.Lens' StopDominantLanguageDetectionJobResponse (Prelude.Maybe Prelude.Text)
stopDominantLanguageDetectionJobResponse_jobId = Lens.lens (\StopDominantLanguageDetectionJobResponse' {jobId} -> jobId) (\s@StopDominantLanguageDetectionJobResponse' {} a -> s {jobId = a} :: StopDominantLanguageDetectionJobResponse)

-- | The response's http status code.
stopDominantLanguageDetectionJobResponse_httpStatus :: Lens.Lens' StopDominantLanguageDetectionJobResponse Prelude.Int
stopDominantLanguageDetectionJobResponse_httpStatus = Lens.lens (\StopDominantLanguageDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StopDominantLanguageDetectionJobResponse' {} a -> s {httpStatus = a} :: StopDominantLanguageDetectionJobResponse)

instance
  Prelude.NFData
    StopDominantLanguageDetectionJobResponse
