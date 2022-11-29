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
-- Module      : Amazonka.ComprehendMedical.DescribeSNOMEDCTInferenceJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with an InferSNOMEDCT job. Use this
-- operation to get the status of an inference job.
module Amazonka.ComprehendMedical.DescribeSNOMEDCTInferenceJob
  ( -- * Creating a Request
    DescribeSNOMEDCTInferenceJob (..),
    newDescribeSNOMEDCTInferenceJob,

    -- * Request Lenses
    describeSNOMEDCTInferenceJob_jobId,

    -- * Destructuring the Response
    DescribeSNOMEDCTInferenceJobResponse (..),
    newDescribeSNOMEDCTInferenceJobResponse,

    -- * Response Lenses
    describeSNOMEDCTInferenceJobResponse_comprehendMedicalAsyncJobProperties,
    describeSNOMEDCTInferenceJobResponse_httpStatus,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSNOMEDCTInferenceJob' smart constructor.
data DescribeSNOMEDCTInferenceJob = DescribeSNOMEDCTInferenceJob'
  { -- | The identifier that Amazon Comprehend Medical generated for the job. The
    -- StartSNOMEDCTInferenceJob operation returns this identifier in its
    -- response.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSNOMEDCTInferenceJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describeSNOMEDCTInferenceJob_jobId' - The identifier that Amazon Comprehend Medical generated for the job. The
-- StartSNOMEDCTInferenceJob operation returns this identifier in its
-- response.
newDescribeSNOMEDCTInferenceJob ::
  -- | 'jobId'
  Prelude.Text ->
  DescribeSNOMEDCTInferenceJob
newDescribeSNOMEDCTInferenceJob pJobId_ =
  DescribeSNOMEDCTInferenceJob' {jobId = pJobId_}

-- | The identifier that Amazon Comprehend Medical generated for the job. The
-- StartSNOMEDCTInferenceJob operation returns this identifier in its
-- response.
describeSNOMEDCTInferenceJob_jobId :: Lens.Lens' DescribeSNOMEDCTInferenceJob Prelude.Text
describeSNOMEDCTInferenceJob_jobId = Lens.lens (\DescribeSNOMEDCTInferenceJob' {jobId} -> jobId) (\s@DescribeSNOMEDCTInferenceJob' {} a -> s {jobId = a} :: DescribeSNOMEDCTInferenceJob)

instance Core.AWSRequest DescribeSNOMEDCTInferenceJob where
  type
    AWSResponse DescribeSNOMEDCTInferenceJob =
      DescribeSNOMEDCTInferenceJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSNOMEDCTInferenceJobResponse'
            Prelude.<$> (x Core..?> "ComprehendMedicalAsyncJobProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeSNOMEDCTInferenceJob
  where
  hashWithSalt _salt DescribeSNOMEDCTInferenceJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData DescribeSNOMEDCTInferenceJob where
  rnf DescribeSNOMEDCTInferenceJob' {..} =
    Prelude.rnf jobId

instance Core.ToHeaders DescribeSNOMEDCTInferenceJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ComprehendMedical_20181030.DescribeSNOMEDCTInferenceJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeSNOMEDCTInferenceJob where
  toJSON DescribeSNOMEDCTInferenceJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Core..= jobId)]
      )

instance Core.ToPath DescribeSNOMEDCTInferenceJob where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeSNOMEDCTInferenceJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSNOMEDCTInferenceJobResponse' smart constructor.
data DescribeSNOMEDCTInferenceJobResponse = DescribeSNOMEDCTInferenceJobResponse'
  { comprehendMedicalAsyncJobProperties :: Prelude.Maybe ComprehendMedicalAsyncJobProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSNOMEDCTInferenceJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comprehendMedicalAsyncJobProperties', 'describeSNOMEDCTInferenceJobResponse_comprehendMedicalAsyncJobProperties' - Undocumented member.
--
-- 'httpStatus', 'describeSNOMEDCTInferenceJobResponse_httpStatus' - The response's http status code.
newDescribeSNOMEDCTInferenceJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSNOMEDCTInferenceJobResponse
newDescribeSNOMEDCTInferenceJobResponse pHttpStatus_ =
  DescribeSNOMEDCTInferenceJobResponse'
    { comprehendMedicalAsyncJobProperties =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeSNOMEDCTInferenceJobResponse_comprehendMedicalAsyncJobProperties :: Lens.Lens' DescribeSNOMEDCTInferenceJobResponse (Prelude.Maybe ComprehendMedicalAsyncJobProperties)
describeSNOMEDCTInferenceJobResponse_comprehendMedicalAsyncJobProperties = Lens.lens (\DescribeSNOMEDCTInferenceJobResponse' {comprehendMedicalAsyncJobProperties} -> comprehendMedicalAsyncJobProperties) (\s@DescribeSNOMEDCTInferenceJobResponse' {} a -> s {comprehendMedicalAsyncJobProperties = a} :: DescribeSNOMEDCTInferenceJobResponse)

-- | The response's http status code.
describeSNOMEDCTInferenceJobResponse_httpStatus :: Lens.Lens' DescribeSNOMEDCTInferenceJobResponse Prelude.Int
describeSNOMEDCTInferenceJobResponse_httpStatus = Lens.lens (\DescribeSNOMEDCTInferenceJobResponse' {httpStatus} -> httpStatus) (\s@DescribeSNOMEDCTInferenceJobResponse' {} a -> s {httpStatus = a} :: DescribeSNOMEDCTInferenceJobResponse)

instance
  Prelude.NFData
    DescribeSNOMEDCTInferenceJobResponse
  where
  rnf DescribeSNOMEDCTInferenceJobResponse' {..} =
    Prelude.rnf comprehendMedicalAsyncJobProperties
      `Prelude.seq` Prelude.rnf httpStatus
