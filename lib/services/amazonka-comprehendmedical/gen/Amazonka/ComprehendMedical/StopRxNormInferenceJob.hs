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
-- Module      : Amazonka.ComprehendMedical.StopRxNormInferenceJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an InferRxNorm inference job in progress.
module Amazonka.ComprehendMedical.StopRxNormInferenceJob
  ( -- * Creating a Request
    StopRxNormInferenceJob (..),
    newStopRxNormInferenceJob,

    -- * Request Lenses
    stopRxNormInferenceJob_jobId,

    -- * Destructuring the Response
    StopRxNormInferenceJobResponse (..),
    newStopRxNormInferenceJobResponse,

    -- * Response Lenses
    stopRxNormInferenceJobResponse_jobId,
    stopRxNormInferenceJobResponse_httpStatus,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopRxNormInferenceJob' smart constructor.
data StopRxNormInferenceJob = StopRxNormInferenceJob'
  { -- | The identifier of the job.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopRxNormInferenceJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'stopRxNormInferenceJob_jobId' - The identifier of the job.
newStopRxNormInferenceJob ::
  -- | 'jobId'
  Prelude.Text ->
  StopRxNormInferenceJob
newStopRxNormInferenceJob pJobId_ =
  StopRxNormInferenceJob' {jobId = pJobId_}

-- | The identifier of the job.
stopRxNormInferenceJob_jobId :: Lens.Lens' StopRxNormInferenceJob Prelude.Text
stopRxNormInferenceJob_jobId = Lens.lens (\StopRxNormInferenceJob' {jobId} -> jobId) (\s@StopRxNormInferenceJob' {} a -> s {jobId = a} :: StopRxNormInferenceJob)

instance Core.AWSRequest StopRxNormInferenceJob where
  type
    AWSResponse StopRxNormInferenceJob =
      StopRxNormInferenceJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopRxNormInferenceJobResponse'
            Prelude.<$> (x Data..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopRxNormInferenceJob where
  hashWithSalt _salt StopRxNormInferenceJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData StopRxNormInferenceJob where
  rnf StopRxNormInferenceJob' {..} = Prelude.rnf jobId

instance Data.ToHeaders StopRxNormInferenceJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComprehendMedical_20181030.StopRxNormInferenceJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopRxNormInferenceJob where
  toJSON StopRxNormInferenceJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Data..= jobId)]
      )

instance Data.ToPath StopRxNormInferenceJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StopRxNormInferenceJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopRxNormInferenceJobResponse' smart constructor.
data StopRxNormInferenceJobResponse = StopRxNormInferenceJobResponse'
  { -- | The identifier generated for the job. To get the status of job, use this
    -- identifier with the @DescribeRxNormInferenceJob@ operation.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopRxNormInferenceJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'stopRxNormInferenceJobResponse_jobId' - The identifier generated for the job. To get the status of job, use this
-- identifier with the @DescribeRxNormInferenceJob@ operation.
--
-- 'httpStatus', 'stopRxNormInferenceJobResponse_httpStatus' - The response's http status code.
newStopRxNormInferenceJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopRxNormInferenceJobResponse
newStopRxNormInferenceJobResponse pHttpStatus_ =
  StopRxNormInferenceJobResponse'
    { jobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier generated for the job. To get the status of job, use this
-- identifier with the @DescribeRxNormInferenceJob@ operation.
stopRxNormInferenceJobResponse_jobId :: Lens.Lens' StopRxNormInferenceJobResponse (Prelude.Maybe Prelude.Text)
stopRxNormInferenceJobResponse_jobId = Lens.lens (\StopRxNormInferenceJobResponse' {jobId} -> jobId) (\s@StopRxNormInferenceJobResponse' {} a -> s {jobId = a} :: StopRxNormInferenceJobResponse)

-- | The response's http status code.
stopRxNormInferenceJobResponse_httpStatus :: Lens.Lens' StopRxNormInferenceJobResponse Prelude.Int
stopRxNormInferenceJobResponse_httpStatus = Lens.lens (\StopRxNormInferenceJobResponse' {httpStatus} -> httpStatus) (\s@StopRxNormInferenceJobResponse' {} a -> s {httpStatus = a} :: StopRxNormInferenceJobResponse)

instance
  Prelude.NFData
    StopRxNormInferenceJobResponse
  where
  rnf StopRxNormInferenceJobResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf httpStatus
