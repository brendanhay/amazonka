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
-- Module      : Amazonka.Comprehend.StopSentimentDetectionJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a sentiment detection job in progress.
--
-- If the job state is @IN_PROGRESS@, the job is marked for termination and
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
module Amazonka.Comprehend.StopSentimentDetectionJob
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

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopSentimentDetectionJob' smart constructor.
data StopSentimentDetectionJob = StopSentimentDetectionJob'
  { -- | The identifier of the sentiment detection job to stop.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  StopSentimentDetectionJob
newStopSentimentDetectionJob pJobId_ =
  StopSentimentDetectionJob' {jobId = pJobId_}

-- | The identifier of the sentiment detection job to stop.
stopSentimentDetectionJob_jobId :: Lens.Lens' StopSentimentDetectionJob Prelude.Text
stopSentimentDetectionJob_jobId = Lens.lens (\StopSentimentDetectionJob' {jobId} -> jobId) (\s@StopSentimentDetectionJob' {} a -> s {jobId = a} :: StopSentimentDetectionJob)

instance Core.AWSRequest StopSentimentDetectionJob where
  type
    AWSResponse StopSentimentDetectionJob =
      StopSentimentDetectionJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopSentimentDetectionJobResponse'
            Prelude.<$> (x Core..?> "JobStatus")
            Prelude.<*> (x Core..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopSentimentDetectionJob where
  hashWithSalt _salt StopSentimentDetectionJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData StopSentimentDetectionJob where
  rnf StopSentimentDetectionJob' {..} =
    Prelude.rnf jobId

instance Core.ToHeaders StopSentimentDetectionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.StopSentimentDetectionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopSentimentDetectionJob where
  toJSON StopSentimentDetectionJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Core..= jobId)]
      )

instance Core.ToPath StopSentimentDetectionJob where
  toPath = Prelude.const "/"

instance Core.ToQuery StopSentimentDetectionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopSentimentDetectionJobResponse' smart constructor.
data StopSentimentDetectionJobResponse = StopSentimentDetectionJobResponse'
  { -- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if
    -- the job was previously stopped with the @StopSentimentDetectionJob@
    -- operation.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The identifier of the sentiment detection job to stop.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  StopSentimentDetectionJobResponse
newStopSentimentDetectionJobResponse pHttpStatus_ =
  StopSentimentDetectionJobResponse'
    { jobStatus =
        Prelude.Nothing,
      jobId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if
-- the job was previously stopped with the @StopSentimentDetectionJob@
-- operation.
stopSentimentDetectionJobResponse_jobStatus :: Lens.Lens' StopSentimentDetectionJobResponse (Prelude.Maybe JobStatus)
stopSentimentDetectionJobResponse_jobStatus = Lens.lens (\StopSentimentDetectionJobResponse' {jobStatus} -> jobStatus) (\s@StopSentimentDetectionJobResponse' {} a -> s {jobStatus = a} :: StopSentimentDetectionJobResponse)

-- | The identifier of the sentiment detection job to stop.
stopSentimentDetectionJobResponse_jobId :: Lens.Lens' StopSentimentDetectionJobResponse (Prelude.Maybe Prelude.Text)
stopSentimentDetectionJobResponse_jobId = Lens.lens (\StopSentimentDetectionJobResponse' {jobId} -> jobId) (\s@StopSentimentDetectionJobResponse' {} a -> s {jobId = a} :: StopSentimentDetectionJobResponse)

-- | The response's http status code.
stopSentimentDetectionJobResponse_httpStatus :: Lens.Lens' StopSentimentDetectionJobResponse Prelude.Int
stopSentimentDetectionJobResponse_httpStatus = Lens.lens (\StopSentimentDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StopSentimentDetectionJobResponse' {} a -> s {httpStatus = a} :: StopSentimentDetectionJobResponse)

instance
  Prelude.NFData
    StopSentimentDetectionJobResponse
  where
  rnf StopSentimentDetectionJobResponse' {..} =
    Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf httpStatus
