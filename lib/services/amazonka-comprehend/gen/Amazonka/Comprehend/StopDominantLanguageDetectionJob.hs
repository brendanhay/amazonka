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
-- Module      : Amazonka.Comprehend.StopDominantLanguageDetectionJob
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Comprehend.StopDominantLanguageDetectionJob
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

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopDominantLanguageDetectionJob' smart constructor.
data StopDominantLanguageDetectionJob = StopDominantLanguageDetectionJob'
  { -- | The identifier of the dominant language detection job to stop.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.AWSRequest
    StopDominantLanguageDetectionJob
  where
  type
    AWSResponse StopDominantLanguageDetectionJob =
      StopDominantLanguageDetectionJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopDominantLanguageDetectionJobResponse'
            Prelude.<$> (x Data..?> "JobStatus")
            Prelude.<*> (x Data..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StopDominantLanguageDetectionJob
  where
  hashWithSalt
    _salt
    StopDominantLanguageDetectionJob' {..} =
      _salt `Prelude.hashWithSalt` jobId

instance
  Prelude.NFData
    StopDominantLanguageDetectionJob
  where
  rnf StopDominantLanguageDetectionJob' {..} =
    Prelude.rnf jobId

instance
  Data.ToHeaders
    StopDominantLanguageDetectionJob
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.StopDominantLanguageDetectionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopDominantLanguageDetectionJob where
  toJSON StopDominantLanguageDetectionJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Data..= jobId)]
      )

instance Data.ToPath StopDominantLanguageDetectionJob where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf StopDominantLanguageDetectionJobResponse' {..} =
    Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf httpStatus
