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
-- Module      : Amazonka.ComprehendMedical.DescribeRxNormInferenceJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with an InferRxNorm job. Use this
-- operation to get the status of an inference job.
module Amazonka.ComprehendMedical.DescribeRxNormInferenceJob
  ( -- * Creating a Request
    DescribeRxNormInferenceJob (..),
    newDescribeRxNormInferenceJob,

    -- * Request Lenses
    describeRxNormInferenceJob_jobId,

    -- * Destructuring the Response
    DescribeRxNormInferenceJobResponse (..),
    newDescribeRxNormInferenceJobResponse,

    -- * Response Lenses
    describeRxNormInferenceJobResponse_comprehendMedicalAsyncJobProperties,
    describeRxNormInferenceJobResponse_httpStatus,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRxNormInferenceJob' smart constructor.
data DescribeRxNormInferenceJob = DescribeRxNormInferenceJob'
  { -- | The identifier that Amazon Comprehend Medical generated for the job. The
    -- StartRxNormInferenceJob operation returns this identifier in its
    -- response.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRxNormInferenceJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describeRxNormInferenceJob_jobId' - The identifier that Amazon Comprehend Medical generated for the job. The
-- StartRxNormInferenceJob operation returns this identifier in its
-- response.
newDescribeRxNormInferenceJob ::
  -- | 'jobId'
  Prelude.Text ->
  DescribeRxNormInferenceJob
newDescribeRxNormInferenceJob pJobId_ =
  DescribeRxNormInferenceJob' {jobId = pJobId_}

-- | The identifier that Amazon Comprehend Medical generated for the job. The
-- StartRxNormInferenceJob operation returns this identifier in its
-- response.
describeRxNormInferenceJob_jobId :: Lens.Lens' DescribeRxNormInferenceJob Prelude.Text
describeRxNormInferenceJob_jobId = Lens.lens (\DescribeRxNormInferenceJob' {jobId} -> jobId) (\s@DescribeRxNormInferenceJob' {} a -> s {jobId = a} :: DescribeRxNormInferenceJob)

instance Core.AWSRequest DescribeRxNormInferenceJob where
  type
    AWSResponse DescribeRxNormInferenceJob =
      DescribeRxNormInferenceJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRxNormInferenceJobResponse'
            Prelude.<$> (x Data..?> "ComprehendMedicalAsyncJobProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRxNormInferenceJob where
  hashWithSalt _salt DescribeRxNormInferenceJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData DescribeRxNormInferenceJob where
  rnf DescribeRxNormInferenceJob' {..} =
    Prelude.rnf jobId

instance Data.ToHeaders DescribeRxNormInferenceJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComprehendMedical_20181030.DescribeRxNormInferenceJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeRxNormInferenceJob where
  toJSON DescribeRxNormInferenceJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Data..= jobId)]
      )

instance Data.ToPath DescribeRxNormInferenceJob where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeRxNormInferenceJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRxNormInferenceJobResponse' smart constructor.
data DescribeRxNormInferenceJobResponse = DescribeRxNormInferenceJobResponse'
  { -- | An object that contains the properties associated with a detection job.
    comprehendMedicalAsyncJobProperties :: Prelude.Maybe ComprehendMedicalAsyncJobProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRxNormInferenceJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comprehendMedicalAsyncJobProperties', 'describeRxNormInferenceJobResponse_comprehendMedicalAsyncJobProperties' - An object that contains the properties associated with a detection job.
--
-- 'httpStatus', 'describeRxNormInferenceJobResponse_httpStatus' - The response's http status code.
newDescribeRxNormInferenceJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRxNormInferenceJobResponse
newDescribeRxNormInferenceJobResponse pHttpStatus_ =
  DescribeRxNormInferenceJobResponse'
    { comprehendMedicalAsyncJobProperties =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains the properties associated with a detection job.
describeRxNormInferenceJobResponse_comprehendMedicalAsyncJobProperties :: Lens.Lens' DescribeRxNormInferenceJobResponse (Prelude.Maybe ComprehendMedicalAsyncJobProperties)
describeRxNormInferenceJobResponse_comprehendMedicalAsyncJobProperties = Lens.lens (\DescribeRxNormInferenceJobResponse' {comprehendMedicalAsyncJobProperties} -> comprehendMedicalAsyncJobProperties) (\s@DescribeRxNormInferenceJobResponse' {} a -> s {comprehendMedicalAsyncJobProperties = a} :: DescribeRxNormInferenceJobResponse)

-- | The response's http status code.
describeRxNormInferenceJobResponse_httpStatus :: Lens.Lens' DescribeRxNormInferenceJobResponse Prelude.Int
describeRxNormInferenceJobResponse_httpStatus = Lens.lens (\DescribeRxNormInferenceJobResponse' {httpStatus} -> httpStatus) (\s@DescribeRxNormInferenceJobResponse' {} a -> s {httpStatus = a} :: DescribeRxNormInferenceJobResponse)

instance
  Prelude.NFData
    DescribeRxNormInferenceJobResponse
  where
  rnf DescribeRxNormInferenceJobResponse' {..} =
    Prelude.rnf comprehendMedicalAsyncJobProperties
      `Prelude.seq` Prelude.rnf httpStatus
