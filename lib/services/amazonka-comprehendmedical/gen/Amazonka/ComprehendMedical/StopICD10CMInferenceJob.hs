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
-- Module      : Amazonka.ComprehendMedical.StopICD10CMInferenceJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an InferICD10CM inference job in progress.
module Amazonka.ComprehendMedical.StopICD10CMInferenceJob
  ( -- * Creating a Request
    StopICD10CMInferenceJob (..),
    newStopICD10CMInferenceJob,

    -- * Request Lenses
    stopICD10CMInferenceJob_jobId,

    -- * Destructuring the Response
    StopICD10CMInferenceJobResponse (..),
    newStopICD10CMInferenceJobResponse,

    -- * Response Lenses
    stopICD10CMInferenceJobResponse_jobId,
    stopICD10CMInferenceJobResponse_httpStatus,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopICD10CMInferenceJob' smart constructor.
data StopICD10CMInferenceJob = StopICD10CMInferenceJob'
  { -- | The identifier of the job.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopICD10CMInferenceJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'stopICD10CMInferenceJob_jobId' - The identifier of the job.
newStopICD10CMInferenceJob ::
  -- | 'jobId'
  Prelude.Text ->
  StopICD10CMInferenceJob
newStopICD10CMInferenceJob pJobId_ =
  StopICD10CMInferenceJob' {jobId = pJobId_}

-- | The identifier of the job.
stopICD10CMInferenceJob_jobId :: Lens.Lens' StopICD10CMInferenceJob Prelude.Text
stopICD10CMInferenceJob_jobId = Lens.lens (\StopICD10CMInferenceJob' {jobId} -> jobId) (\s@StopICD10CMInferenceJob' {} a -> s {jobId = a} :: StopICD10CMInferenceJob)

instance Core.AWSRequest StopICD10CMInferenceJob where
  type
    AWSResponse StopICD10CMInferenceJob =
      StopICD10CMInferenceJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopICD10CMInferenceJobResponse'
            Prelude.<$> (x Data..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopICD10CMInferenceJob where
  hashWithSalt _salt StopICD10CMInferenceJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData StopICD10CMInferenceJob where
  rnf StopICD10CMInferenceJob' {..} = Prelude.rnf jobId

instance Data.ToHeaders StopICD10CMInferenceJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComprehendMedical_20181030.StopICD10CMInferenceJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopICD10CMInferenceJob where
  toJSON StopICD10CMInferenceJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Data..= jobId)]
      )

instance Data.ToPath StopICD10CMInferenceJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StopICD10CMInferenceJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopICD10CMInferenceJobResponse' smart constructor.
data StopICD10CMInferenceJobResponse = StopICD10CMInferenceJobResponse'
  { -- | The identifier generated for the job. To get the status of job, use this
    -- identifier with the @DescribeICD10CMInferenceJob@ operation.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopICD10CMInferenceJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'stopICD10CMInferenceJobResponse_jobId' - The identifier generated for the job. To get the status of job, use this
-- identifier with the @DescribeICD10CMInferenceJob@ operation.
--
-- 'httpStatus', 'stopICD10CMInferenceJobResponse_httpStatus' - The response's http status code.
newStopICD10CMInferenceJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopICD10CMInferenceJobResponse
newStopICD10CMInferenceJobResponse pHttpStatus_ =
  StopICD10CMInferenceJobResponse'
    { jobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier generated for the job. To get the status of job, use this
-- identifier with the @DescribeICD10CMInferenceJob@ operation.
stopICD10CMInferenceJobResponse_jobId :: Lens.Lens' StopICD10CMInferenceJobResponse (Prelude.Maybe Prelude.Text)
stopICD10CMInferenceJobResponse_jobId = Lens.lens (\StopICD10CMInferenceJobResponse' {jobId} -> jobId) (\s@StopICD10CMInferenceJobResponse' {} a -> s {jobId = a} :: StopICD10CMInferenceJobResponse)

-- | The response's http status code.
stopICD10CMInferenceJobResponse_httpStatus :: Lens.Lens' StopICD10CMInferenceJobResponse Prelude.Int
stopICD10CMInferenceJobResponse_httpStatus = Lens.lens (\StopICD10CMInferenceJobResponse' {httpStatus} -> httpStatus) (\s@StopICD10CMInferenceJobResponse' {} a -> s {httpStatus = a} :: StopICD10CMInferenceJobResponse)

instance
  Prelude.NFData
    StopICD10CMInferenceJobResponse
  where
  rnf StopICD10CMInferenceJobResponse' {..} =
    Prelude.rnf jobId `Prelude.seq`
      Prelude.rnf httpStatus
