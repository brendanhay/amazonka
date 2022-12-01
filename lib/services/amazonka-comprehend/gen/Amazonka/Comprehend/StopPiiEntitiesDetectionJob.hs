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
-- Module      : Amazonka.Comprehend.StopPiiEntitiesDetectionJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a PII entities detection job in progress.
module Amazonka.Comprehend.StopPiiEntitiesDetectionJob
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

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopPiiEntitiesDetectionJob' smart constructor.
data StopPiiEntitiesDetectionJob = StopPiiEntitiesDetectionJob'
  { -- | The identifier of the PII entities detection job to stop.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  StopPiiEntitiesDetectionJob
newStopPiiEntitiesDetectionJob pJobId_ =
  StopPiiEntitiesDetectionJob' {jobId = pJobId_}

-- | The identifier of the PII entities detection job to stop.
stopPiiEntitiesDetectionJob_jobId :: Lens.Lens' StopPiiEntitiesDetectionJob Prelude.Text
stopPiiEntitiesDetectionJob_jobId = Lens.lens (\StopPiiEntitiesDetectionJob' {jobId} -> jobId) (\s@StopPiiEntitiesDetectionJob' {} a -> s {jobId = a} :: StopPiiEntitiesDetectionJob)

instance Core.AWSRequest StopPiiEntitiesDetectionJob where
  type
    AWSResponse StopPiiEntitiesDetectionJob =
      StopPiiEntitiesDetectionJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopPiiEntitiesDetectionJobResponse'
            Prelude.<$> (x Core..?> "JobStatus")
            Prelude.<*> (x Core..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopPiiEntitiesDetectionJob where
  hashWithSalt _salt StopPiiEntitiesDetectionJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData StopPiiEntitiesDetectionJob where
  rnf StopPiiEntitiesDetectionJob' {..} =
    Prelude.rnf jobId

instance Core.ToHeaders StopPiiEntitiesDetectionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.StopPiiEntitiesDetectionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopPiiEntitiesDetectionJob where
  toJSON StopPiiEntitiesDetectionJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Core..= jobId)]
      )

instance Core.ToPath StopPiiEntitiesDetectionJob where
  toPath = Prelude.const "/"

instance Core.ToQuery StopPiiEntitiesDetectionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopPiiEntitiesDetectionJobResponse' smart constructor.
data StopPiiEntitiesDetectionJobResponse = StopPiiEntitiesDetectionJobResponse'
  { -- | The status of the PII entities detection job.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | The identifier of the PII entities detection job to stop.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  StopPiiEntitiesDetectionJobResponse
newStopPiiEntitiesDetectionJobResponse pHttpStatus_ =
  StopPiiEntitiesDetectionJobResponse'
    { jobStatus =
        Prelude.Nothing,
      jobId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the PII entities detection job.
stopPiiEntitiesDetectionJobResponse_jobStatus :: Lens.Lens' StopPiiEntitiesDetectionJobResponse (Prelude.Maybe JobStatus)
stopPiiEntitiesDetectionJobResponse_jobStatus = Lens.lens (\StopPiiEntitiesDetectionJobResponse' {jobStatus} -> jobStatus) (\s@StopPiiEntitiesDetectionJobResponse' {} a -> s {jobStatus = a} :: StopPiiEntitiesDetectionJobResponse)

-- | The identifier of the PII entities detection job to stop.
stopPiiEntitiesDetectionJobResponse_jobId :: Lens.Lens' StopPiiEntitiesDetectionJobResponse (Prelude.Maybe Prelude.Text)
stopPiiEntitiesDetectionJobResponse_jobId = Lens.lens (\StopPiiEntitiesDetectionJobResponse' {jobId} -> jobId) (\s@StopPiiEntitiesDetectionJobResponse' {} a -> s {jobId = a} :: StopPiiEntitiesDetectionJobResponse)

-- | The response's http status code.
stopPiiEntitiesDetectionJobResponse_httpStatus :: Lens.Lens' StopPiiEntitiesDetectionJobResponse Prelude.Int
stopPiiEntitiesDetectionJobResponse_httpStatus = Lens.lens (\StopPiiEntitiesDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StopPiiEntitiesDetectionJobResponse' {} a -> s {httpStatus = a} :: StopPiiEntitiesDetectionJobResponse)

instance
  Prelude.NFData
    StopPiiEntitiesDetectionJobResponse
  where
  rnf StopPiiEntitiesDetectionJobResponse' {..} =
    Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf httpStatus
