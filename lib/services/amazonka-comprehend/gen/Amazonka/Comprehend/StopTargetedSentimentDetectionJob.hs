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
-- Module      : Amazonka.Comprehend.StopTargetedSentimentDetectionJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a targeted sentiment detection job in progress.
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
module Amazonka.Comprehend.StopTargetedSentimentDetectionJob
  ( -- * Creating a Request
    StopTargetedSentimentDetectionJob (..),
    newStopTargetedSentimentDetectionJob,

    -- * Request Lenses
    stopTargetedSentimentDetectionJob_jobId,

    -- * Destructuring the Response
    StopTargetedSentimentDetectionJobResponse (..),
    newStopTargetedSentimentDetectionJobResponse,

    -- * Response Lenses
    stopTargetedSentimentDetectionJobResponse_jobId,
    stopTargetedSentimentDetectionJobResponse_jobStatus,
    stopTargetedSentimentDetectionJobResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopTargetedSentimentDetectionJob' smart constructor.
data StopTargetedSentimentDetectionJob = StopTargetedSentimentDetectionJob'
  { -- | The identifier of the targeted sentiment detection job to stop.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopTargetedSentimentDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'stopTargetedSentimentDetectionJob_jobId' - The identifier of the targeted sentiment detection job to stop.
newStopTargetedSentimentDetectionJob ::
  -- | 'jobId'
  Prelude.Text ->
  StopTargetedSentimentDetectionJob
newStopTargetedSentimentDetectionJob pJobId_ =
  StopTargetedSentimentDetectionJob' {jobId = pJobId_}

-- | The identifier of the targeted sentiment detection job to stop.
stopTargetedSentimentDetectionJob_jobId :: Lens.Lens' StopTargetedSentimentDetectionJob Prelude.Text
stopTargetedSentimentDetectionJob_jobId = Lens.lens (\StopTargetedSentimentDetectionJob' {jobId} -> jobId) (\s@StopTargetedSentimentDetectionJob' {} a -> s {jobId = a} :: StopTargetedSentimentDetectionJob)

instance
  Core.AWSRequest
    StopTargetedSentimentDetectionJob
  where
  type
    AWSResponse StopTargetedSentimentDetectionJob =
      StopTargetedSentimentDetectionJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopTargetedSentimentDetectionJobResponse'
            Prelude.<$> (x Data..?> "JobId")
            Prelude.<*> (x Data..?> "JobStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StopTargetedSentimentDetectionJob
  where
  hashWithSalt
    _salt
    StopTargetedSentimentDetectionJob' {..} =
      _salt `Prelude.hashWithSalt` jobId

instance
  Prelude.NFData
    StopTargetedSentimentDetectionJob
  where
  rnf StopTargetedSentimentDetectionJob' {..} =
    Prelude.rnf jobId

instance
  Data.ToHeaders
    StopTargetedSentimentDetectionJob
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.StopTargetedSentimentDetectionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    StopTargetedSentimentDetectionJob
  where
  toJSON StopTargetedSentimentDetectionJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Data..= jobId)]
      )

instance
  Data.ToPath
    StopTargetedSentimentDetectionJob
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    StopTargetedSentimentDetectionJob
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopTargetedSentimentDetectionJobResponse' smart constructor.
data StopTargetedSentimentDetectionJobResponse = StopTargetedSentimentDetectionJobResponse'
  { -- | The identifier of the targeted sentiment detection job to stop.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if
    -- the job was previously stopped with the @StopSentimentDetectionJob@
    -- operation.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopTargetedSentimentDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'stopTargetedSentimentDetectionJobResponse_jobId' - The identifier of the targeted sentiment detection job to stop.
--
-- 'jobStatus', 'stopTargetedSentimentDetectionJobResponse_jobStatus' - Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if
-- the job was previously stopped with the @StopSentimentDetectionJob@
-- operation.
--
-- 'httpStatus', 'stopTargetedSentimentDetectionJobResponse_httpStatus' - The response's http status code.
newStopTargetedSentimentDetectionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopTargetedSentimentDetectionJobResponse
newStopTargetedSentimentDetectionJobResponse
  pHttpStatus_ =
    StopTargetedSentimentDetectionJobResponse'
      { jobId =
          Prelude.Nothing,
        jobStatus = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The identifier of the targeted sentiment detection job to stop.
stopTargetedSentimentDetectionJobResponse_jobId :: Lens.Lens' StopTargetedSentimentDetectionJobResponse (Prelude.Maybe Prelude.Text)
stopTargetedSentimentDetectionJobResponse_jobId = Lens.lens (\StopTargetedSentimentDetectionJobResponse' {jobId} -> jobId) (\s@StopTargetedSentimentDetectionJobResponse' {} a -> s {jobId = a} :: StopTargetedSentimentDetectionJobResponse)

-- | Either @STOP_REQUESTED@ if the job is currently running, or @STOPPED@ if
-- the job was previously stopped with the @StopSentimentDetectionJob@
-- operation.
stopTargetedSentimentDetectionJobResponse_jobStatus :: Lens.Lens' StopTargetedSentimentDetectionJobResponse (Prelude.Maybe JobStatus)
stopTargetedSentimentDetectionJobResponse_jobStatus = Lens.lens (\StopTargetedSentimentDetectionJobResponse' {jobStatus} -> jobStatus) (\s@StopTargetedSentimentDetectionJobResponse' {} a -> s {jobStatus = a} :: StopTargetedSentimentDetectionJobResponse)

-- | The response's http status code.
stopTargetedSentimentDetectionJobResponse_httpStatus :: Lens.Lens' StopTargetedSentimentDetectionJobResponse Prelude.Int
stopTargetedSentimentDetectionJobResponse_httpStatus = Lens.lens (\StopTargetedSentimentDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StopTargetedSentimentDetectionJobResponse' {} a -> s {httpStatus = a} :: StopTargetedSentimentDetectionJobResponse)

instance
  Prelude.NFData
    StopTargetedSentimentDetectionJobResponse
  where
  rnf StopTargetedSentimentDetectionJobResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf httpStatus
