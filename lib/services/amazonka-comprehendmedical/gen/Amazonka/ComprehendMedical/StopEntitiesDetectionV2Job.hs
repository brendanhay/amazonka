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
-- Module      : Amazonka.ComprehendMedical.StopEntitiesDetectionV2Job
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a medical entities detection job in progress.
module Amazonka.ComprehendMedical.StopEntitiesDetectionV2Job
  ( -- * Creating a Request
    StopEntitiesDetectionV2Job (..),
    newStopEntitiesDetectionV2Job,

    -- * Request Lenses
    stopEntitiesDetectionV2Job_jobId,

    -- * Destructuring the Response
    StopEntitiesDetectionV2JobResponse (..),
    newStopEntitiesDetectionV2JobResponse,

    -- * Response Lenses
    stopEntitiesDetectionV2JobResponse_jobId,
    stopEntitiesDetectionV2JobResponse_httpStatus,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopEntitiesDetectionV2Job' smart constructor.
data StopEntitiesDetectionV2Job = StopEntitiesDetectionV2Job'
  { -- | The identifier of the medical entities job to stop.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopEntitiesDetectionV2Job' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'stopEntitiesDetectionV2Job_jobId' - The identifier of the medical entities job to stop.
newStopEntitiesDetectionV2Job ::
  -- | 'jobId'
  Prelude.Text ->
  StopEntitiesDetectionV2Job
newStopEntitiesDetectionV2Job pJobId_ =
  StopEntitiesDetectionV2Job' {jobId = pJobId_}

-- | The identifier of the medical entities job to stop.
stopEntitiesDetectionV2Job_jobId :: Lens.Lens' StopEntitiesDetectionV2Job Prelude.Text
stopEntitiesDetectionV2Job_jobId = Lens.lens (\StopEntitiesDetectionV2Job' {jobId} -> jobId) (\s@StopEntitiesDetectionV2Job' {} a -> s {jobId = a} :: StopEntitiesDetectionV2Job)

instance Core.AWSRequest StopEntitiesDetectionV2Job where
  type
    AWSResponse StopEntitiesDetectionV2Job =
      StopEntitiesDetectionV2JobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopEntitiesDetectionV2JobResponse'
            Prelude.<$> (x Data..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopEntitiesDetectionV2Job where
  hashWithSalt _salt StopEntitiesDetectionV2Job' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData StopEntitiesDetectionV2Job where
  rnf StopEntitiesDetectionV2Job' {..} =
    Prelude.rnf jobId

instance Data.ToHeaders StopEntitiesDetectionV2Job where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComprehendMedical_20181030.StopEntitiesDetectionV2Job" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopEntitiesDetectionV2Job where
  toJSON StopEntitiesDetectionV2Job' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Data..= jobId)]
      )

instance Data.ToPath StopEntitiesDetectionV2Job where
  toPath = Prelude.const "/"

instance Data.ToQuery StopEntitiesDetectionV2Job where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopEntitiesDetectionV2JobResponse' smart constructor.
data StopEntitiesDetectionV2JobResponse = StopEntitiesDetectionV2JobResponse'
  { -- | The identifier of the medical entities detection job that was stopped.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopEntitiesDetectionV2JobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'stopEntitiesDetectionV2JobResponse_jobId' - The identifier of the medical entities detection job that was stopped.
--
-- 'httpStatus', 'stopEntitiesDetectionV2JobResponse_httpStatus' - The response's http status code.
newStopEntitiesDetectionV2JobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopEntitiesDetectionV2JobResponse
newStopEntitiesDetectionV2JobResponse pHttpStatus_ =
  StopEntitiesDetectionV2JobResponse'
    { jobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the medical entities detection job that was stopped.
stopEntitiesDetectionV2JobResponse_jobId :: Lens.Lens' StopEntitiesDetectionV2JobResponse (Prelude.Maybe Prelude.Text)
stopEntitiesDetectionV2JobResponse_jobId = Lens.lens (\StopEntitiesDetectionV2JobResponse' {jobId} -> jobId) (\s@StopEntitiesDetectionV2JobResponse' {} a -> s {jobId = a} :: StopEntitiesDetectionV2JobResponse)

-- | The response's http status code.
stopEntitiesDetectionV2JobResponse_httpStatus :: Lens.Lens' StopEntitiesDetectionV2JobResponse Prelude.Int
stopEntitiesDetectionV2JobResponse_httpStatus = Lens.lens (\StopEntitiesDetectionV2JobResponse' {httpStatus} -> httpStatus) (\s@StopEntitiesDetectionV2JobResponse' {} a -> s {httpStatus = a} :: StopEntitiesDetectionV2JobResponse)

instance
  Prelude.NFData
    StopEntitiesDetectionV2JobResponse
  where
  rnf StopEntitiesDetectionV2JobResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf httpStatus
