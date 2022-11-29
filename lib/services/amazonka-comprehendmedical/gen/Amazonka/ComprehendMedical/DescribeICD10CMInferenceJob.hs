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
-- Module      : Amazonka.ComprehendMedical.DescribeICD10CMInferenceJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with an InferICD10CM job. Use this
-- operation to get the status of an inference job.
module Amazonka.ComprehendMedical.DescribeICD10CMInferenceJob
  ( -- * Creating a Request
    DescribeICD10CMInferenceJob (..),
    newDescribeICD10CMInferenceJob,

    -- * Request Lenses
    describeICD10CMInferenceJob_jobId,

    -- * Destructuring the Response
    DescribeICD10CMInferenceJobResponse (..),
    newDescribeICD10CMInferenceJobResponse,

    -- * Response Lenses
    describeICD10CMInferenceJobResponse_comprehendMedicalAsyncJobProperties,
    describeICD10CMInferenceJobResponse_httpStatus,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeICD10CMInferenceJob' smart constructor.
data DescribeICD10CMInferenceJob = DescribeICD10CMInferenceJob'
  { -- | The identifier that Amazon Comprehend Medical generated for the job.
    -- @The StartICD10CMInferenceJob@ operation returns this identifier in its
    -- response.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeICD10CMInferenceJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describeICD10CMInferenceJob_jobId' - The identifier that Amazon Comprehend Medical generated for the job.
-- @The StartICD10CMInferenceJob@ operation returns this identifier in its
-- response.
newDescribeICD10CMInferenceJob ::
  -- | 'jobId'
  Prelude.Text ->
  DescribeICD10CMInferenceJob
newDescribeICD10CMInferenceJob pJobId_ =
  DescribeICD10CMInferenceJob' {jobId = pJobId_}

-- | The identifier that Amazon Comprehend Medical generated for the job.
-- @The StartICD10CMInferenceJob@ operation returns this identifier in its
-- response.
describeICD10CMInferenceJob_jobId :: Lens.Lens' DescribeICD10CMInferenceJob Prelude.Text
describeICD10CMInferenceJob_jobId = Lens.lens (\DescribeICD10CMInferenceJob' {jobId} -> jobId) (\s@DescribeICD10CMInferenceJob' {} a -> s {jobId = a} :: DescribeICD10CMInferenceJob)

instance Core.AWSRequest DescribeICD10CMInferenceJob where
  type
    AWSResponse DescribeICD10CMInferenceJob =
      DescribeICD10CMInferenceJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeICD10CMInferenceJobResponse'
            Prelude.<$> (x Core..?> "ComprehendMedicalAsyncJobProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeICD10CMInferenceJob where
  hashWithSalt _salt DescribeICD10CMInferenceJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData DescribeICD10CMInferenceJob where
  rnf DescribeICD10CMInferenceJob' {..} =
    Prelude.rnf jobId

instance Core.ToHeaders DescribeICD10CMInferenceJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ComprehendMedical_20181030.DescribeICD10CMInferenceJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeICD10CMInferenceJob where
  toJSON DescribeICD10CMInferenceJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Core..= jobId)]
      )

instance Core.ToPath DescribeICD10CMInferenceJob where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeICD10CMInferenceJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeICD10CMInferenceJobResponse' smart constructor.
data DescribeICD10CMInferenceJobResponse = DescribeICD10CMInferenceJobResponse'
  { -- | An object that contains the properties associated with a detection job.
    comprehendMedicalAsyncJobProperties :: Prelude.Maybe ComprehendMedicalAsyncJobProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeICD10CMInferenceJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comprehendMedicalAsyncJobProperties', 'describeICD10CMInferenceJobResponse_comprehendMedicalAsyncJobProperties' - An object that contains the properties associated with a detection job.
--
-- 'httpStatus', 'describeICD10CMInferenceJobResponse_httpStatus' - The response's http status code.
newDescribeICD10CMInferenceJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeICD10CMInferenceJobResponse
newDescribeICD10CMInferenceJobResponse pHttpStatus_ =
  DescribeICD10CMInferenceJobResponse'
    { comprehendMedicalAsyncJobProperties =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains the properties associated with a detection job.
describeICD10CMInferenceJobResponse_comprehendMedicalAsyncJobProperties :: Lens.Lens' DescribeICD10CMInferenceJobResponse (Prelude.Maybe ComprehendMedicalAsyncJobProperties)
describeICD10CMInferenceJobResponse_comprehendMedicalAsyncJobProperties = Lens.lens (\DescribeICD10CMInferenceJobResponse' {comprehendMedicalAsyncJobProperties} -> comprehendMedicalAsyncJobProperties) (\s@DescribeICD10CMInferenceJobResponse' {} a -> s {comprehendMedicalAsyncJobProperties = a} :: DescribeICD10CMInferenceJobResponse)

-- | The response's http status code.
describeICD10CMInferenceJobResponse_httpStatus :: Lens.Lens' DescribeICD10CMInferenceJobResponse Prelude.Int
describeICD10CMInferenceJobResponse_httpStatus = Lens.lens (\DescribeICD10CMInferenceJobResponse' {httpStatus} -> httpStatus) (\s@DescribeICD10CMInferenceJobResponse' {} a -> s {httpStatus = a} :: DescribeICD10CMInferenceJobResponse)

instance
  Prelude.NFData
    DescribeICD10CMInferenceJobResponse
  where
  rnf DescribeICD10CMInferenceJobResponse' {..} =
    Prelude.rnf comprehendMedicalAsyncJobProperties
      `Prelude.seq` Prelude.rnf httpStatus
