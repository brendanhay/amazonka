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
-- Module      : Amazonka.ComprehendMedical.StopPHIDetectionJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a protected health information (PHI) detection job in progress.
module Amazonka.ComprehendMedical.StopPHIDetectionJob
  ( -- * Creating a Request
    StopPHIDetectionJob (..),
    newStopPHIDetectionJob,

    -- * Request Lenses
    stopPHIDetectionJob_jobId,

    -- * Destructuring the Response
    StopPHIDetectionJobResponse (..),
    newStopPHIDetectionJobResponse,

    -- * Response Lenses
    stopPHIDetectionJobResponse_jobId,
    stopPHIDetectionJobResponse_httpStatus,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopPHIDetectionJob' smart constructor.
data StopPHIDetectionJob = StopPHIDetectionJob'
  { -- | The identifier of the PHI detection job to stop.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopPHIDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'stopPHIDetectionJob_jobId' - The identifier of the PHI detection job to stop.
newStopPHIDetectionJob ::
  -- | 'jobId'
  Prelude.Text ->
  StopPHIDetectionJob
newStopPHIDetectionJob pJobId_ =
  StopPHIDetectionJob' {jobId = pJobId_}

-- | The identifier of the PHI detection job to stop.
stopPHIDetectionJob_jobId :: Lens.Lens' StopPHIDetectionJob Prelude.Text
stopPHIDetectionJob_jobId = Lens.lens (\StopPHIDetectionJob' {jobId} -> jobId) (\s@StopPHIDetectionJob' {} a -> s {jobId = a} :: StopPHIDetectionJob)

instance Core.AWSRequest StopPHIDetectionJob where
  type
    AWSResponse StopPHIDetectionJob =
      StopPHIDetectionJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopPHIDetectionJobResponse'
            Prelude.<$> (x Data..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopPHIDetectionJob where
  hashWithSalt _salt StopPHIDetectionJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData StopPHIDetectionJob where
  rnf StopPHIDetectionJob' {..} = Prelude.rnf jobId

instance Data.ToHeaders StopPHIDetectionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComprehendMedical_20181030.StopPHIDetectionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopPHIDetectionJob where
  toJSON StopPHIDetectionJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Data..= jobId)]
      )

instance Data.ToPath StopPHIDetectionJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StopPHIDetectionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopPHIDetectionJobResponse' smart constructor.
data StopPHIDetectionJobResponse = StopPHIDetectionJobResponse'
  { -- | The identifier of the PHI detection job that was stopped.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopPHIDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'stopPHIDetectionJobResponse_jobId' - The identifier of the PHI detection job that was stopped.
--
-- 'httpStatus', 'stopPHIDetectionJobResponse_httpStatus' - The response's http status code.
newStopPHIDetectionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopPHIDetectionJobResponse
newStopPHIDetectionJobResponse pHttpStatus_ =
  StopPHIDetectionJobResponse'
    { jobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the PHI detection job that was stopped.
stopPHIDetectionJobResponse_jobId :: Lens.Lens' StopPHIDetectionJobResponse (Prelude.Maybe Prelude.Text)
stopPHIDetectionJobResponse_jobId = Lens.lens (\StopPHIDetectionJobResponse' {jobId} -> jobId) (\s@StopPHIDetectionJobResponse' {} a -> s {jobId = a} :: StopPHIDetectionJobResponse)

-- | The response's http status code.
stopPHIDetectionJobResponse_httpStatus :: Lens.Lens' StopPHIDetectionJobResponse Prelude.Int
stopPHIDetectionJobResponse_httpStatus = Lens.lens (\StopPHIDetectionJobResponse' {httpStatus} -> httpStatus) (\s@StopPHIDetectionJobResponse' {} a -> s {httpStatus = a} :: StopPHIDetectionJobResponse)

instance Prelude.NFData StopPHIDetectionJobResponse where
  rnf StopPHIDetectionJobResponse' {..} =
    Prelude.rnf jobId `Prelude.seq`
      Prelude.rnf httpStatus
