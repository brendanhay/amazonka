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
-- Module      : Amazonka.Comprehend.StopEventsDetectionJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an events detection job in progress.
module Amazonka.Comprehend.StopEventsDetectionJob
  ( -- * Creating a Request
    StopEventsDetectionJob (..),
    newStopEventsDetectionJob,

    -- * Request Lenses
    stopEventsDetectionJob_jobId,

    -- * Destructuring the Response
    StopEventsDetectionJobResponse (..),
    newStopEventsDetectionJobResponse,

    -- * Response Lenses
    stopEventsDetectionJobResponse_jobId,
    stopEventsDetectionJobResponse_jobStatus,
    stopEventsDetectionJobResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopEventsDetectionJobResponse'
            Prelude.<$> (x Data..?> "JobId")
            Prelude.<*> (x Data..?> "JobStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopEventsDetectionJob where
  hashWithSalt _salt StopEventsDetectionJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData StopEventsDetectionJob where
  rnf StopEventsDetectionJob' {..} = Prelude.rnf jobId

instance Data.ToHeaders StopEventsDetectionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.StopEventsDetectionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopEventsDetectionJob where
  toJSON StopEventsDetectionJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Data..= jobId)]
      )

instance Data.ToPath StopEventsDetectionJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StopEventsDetectionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopEventsDetectionJobResponse' smart constructor.
data StopEventsDetectionJobResponse = StopEventsDetectionJobResponse'
  { -- | The identifier of the events detection job to stop.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The status of the events detection job.
    jobStatus :: Prelude.Maybe JobStatus,
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
-- 'jobId', 'stopEventsDetectionJobResponse_jobId' - The identifier of the events detection job to stop.
--
-- 'jobStatus', 'stopEventsDetectionJobResponse_jobStatus' - The status of the events detection job.
--
-- 'httpStatus', 'stopEventsDetectionJobResponse_httpStatus' - The response's http status code.
newStopEventsDetectionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopEventsDetectionJobResponse
newStopEventsDetectionJobResponse pHttpStatus_ =
  StopEventsDetectionJobResponse'
    { jobId =
        Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the events detection job to stop.
stopEventsDetectionJobResponse_jobId :: Lens.Lens' StopEventsDetectionJobResponse (Prelude.Maybe Prelude.Text)
stopEventsDetectionJobResponse_jobId = Lens.lens (\StopEventsDetectionJobResponse' {jobId} -> jobId) (\s@StopEventsDetectionJobResponse' {} a -> s {jobId = a} :: StopEventsDetectionJobResponse)

-- | The status of the events detection job.
stopEventsDetectionJobResponse_jobStatus :: Lens.Lens' StopEventsDetectionJobResponse (Prelude.Maybe JobStatus)
stopEventsDetectionJobResponse_jobStatus = Lens.lens (\StopEventsDetectionJobResponse' {jobStatus} -> jobStatus) (\s@StopEventsDetectionJobResponse' {} a -> s {jobStatus = a} :: StopEventsDetectionJobResponse)

-- | The response's http status code.
stopEventsDetectionJobResponse_httpStatus :: Lens.Lens' StopEventsDetectionJobResponse Prelude.Int
stopEventsDetectionJobResponse_httpStatus = Lens.lens (\StopEventsDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StopEventsDetectionJobResponse' {} a -> s {httpStatus = a} :: StopEventsDetectionJobResponse)

instance
  Prelude.NFData
    StopEventsDetectionJobResponse
  where
  rnf StopEventsDetectionJobResponse' {..} =
    Prelude.rnf jobId `Prelude.seq`
      Prelude.rnf jobStatus `Prelude.seq`
        Prelude.rnf httpStatus
