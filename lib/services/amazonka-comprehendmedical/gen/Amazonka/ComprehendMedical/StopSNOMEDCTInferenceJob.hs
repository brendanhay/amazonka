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
-- Module      : Amazonka.ComprehendMedical.StopSNOMEDCTInferenceJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an InferSNOMEDCT inference job in progress.
module Amazonka.ComprehendMedical.StopSNOMEDCTInferenceJob
  ( -- * Creating a Request
    StopSNOMEDCTInferenceJob (..),
    newStopSNOMEDCTInferenceJob,

    -- * Request Lenses
    stopSNOMEDCTInferenceJob_jobId,

    -- * Destructuring the Response
    StopSNOMEDCTInferenceJobResponse (..),
    newStopSNOMEDCTInferenceJobResponse,

    -- * Response Lenses
    stopSNOMEDCTInferenceJobResponse_jobId,
    stopSNOMEDCTInferenceJobResponse_httpStatus,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopSNOMEDCTInferenceJob' smart constructor.
data StopSNOMEDCTInferenceJob = StopSNOMEDCTInferenceJob'
  { -- | The job id of the asynchronous InferSNOMEDCT job to be stopped.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopSNOMEDCTInferenceJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'stopSNOMEDCTInferenceJob_jobId' - The job id of the asynchronous InferSNOMEDCT job to be stopped.
newStopSNOMEDCTInferenceJob ::
  -- | 'jobId'
  Prelude.Text ->
  StopSNOMEDCTInferenceJob
newStopSNOMEDCTInferenceJob pJobId_ =
  StopSNOMEDCTInferenceJob' {jobId = pJobId_}

-- | The job id of the asynchronous InferSNOMEDCT job to be stopped.
stopSNOMEDCTInferenceJob_jobId :: Lens.Lens' StopSNOMEDCTInferenceJob Prelude.Text
stopSNOMEDCTInferenceJob_jobId = Lens.lens (\StopSNOMEDCTInferenceJob' {jobId} -> jobId) (\s@StopSNOMEDCTInferenceJob' {} a -> s {jobId = a} :: StopSNOMEDCTInferenceJob)

instance Core.AWSRequest StopSNOMEDCTInferenceJob where
  type
    AWSResponse StopSNOMEDCTInferenceJob =
      StopSNOMEDCTInferenceJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopSNOMEDCTInferenceJobResponse'
            Prelude.<$> (x Data..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopSNOMEDCTInferenceJob where
  hashWithSalt _salt StopSNOMEDCTInferenceJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData StopSNOMEDCTInferenceJob where
  rnf StopSNOMEDCTInferenceJob' {..} = Prelude.rnf jobId

instance Data.ToHeaders StopSNOMEDCTInferenceJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComprehendMedical_20181030.StopSNOMEDCTInferenceJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopSNOMEDCTInferenceJob where
  toJSON StopSNOMEDCTInferenceJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Data..= jobId)]
      )

instance Data.ToPath StopSNOMEDCTInferenceJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StopSNOMEDCTInferenceJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopSNOMEDCTInferenceJobResponse' smart constructor.
data StopSNOMEDCTInferenceJobResponse = StopSNOMEDCTInferenceJobResponse'
  { -- | The identifier generated for the job. To get the status of job, use this
    -- identifier with the DescribeSNOMEDCTInferenceJob operation.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopSNOMEDCTInferenceJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'stopSNOMEDCTInferenceJobResponse_jobId' - The identifier generated for the job. To get the status of job, use this
-- identifier with the DescribeSNOMEDCTInferenceJob operation.
--
-- 'httpStatus', 'stopSNOMEDCTInferenceJobResponse_httpStatus' - The response's http status code.
newStopSNOMEDCTInferenceJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopSNOMEDCTInferenceJobResponse
newStopSNOMEDCTInferenceJobResponse pHttpStatus_ =
  StopSNOMEDCTInferenceJobResponse'
    { jobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier generated for the job. To get the status of job, use this
-- identifier with the DescribeSNOMEDCTInferenceJob operation.
stopSNOMEDCTInferenceJobResponse_jobId :: Lens.Lens' StopSNOMEDCTInferenceJobResponse (Prelude.Maybe Prelude.Text)
stopSNOMEDCTInferenceJobResponse_jobId = Lens.lens (\StopSNOMEDCTInferenceJobResponse' {jobId} -> jobId) (\s@StopSNOMEDCTInferenceJobResponse' {} a -> s {jobId = a} :: StopSNOMEDCTInferenceJobResponse)

-- | The response's http status code.
stopSNOMEDCTInferenceJobResponse_httpStatus :: Lens.Lens' StopSNOMEDCTInferenceJobResponse Prelude.Int
stopSNOMEDCTInferenceJobResponse_httpStatus = Lens.lens (\StopSNOMEDCTInferenceJobResponse' {httpStatus} -> httpStatus) (\s@StopSNOMEDCTInferenceJobResponse' {} a -> s {httpStatus = a} :: StopSNOMEDCTInferenceJobResponse)

instance
  Prelude.NFData
    StopSNOMEDCTInferenceJobResponse
  where
  rnf StopSNOMEDCTInferenceJobResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf httpStatus
