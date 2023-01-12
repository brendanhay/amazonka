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
-- Module      : Amazonka.ComprehendMedical.DescribePHIDetectionJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a protected health information (PHI)
-- detection job. Use this operation to get the status of a detection job.
module Amazonka.ComprehendMedical.DescribePHIDetectionJob
  ( -- * Creating a Request
    DescribePHIDetectionJob (..),
    newDescribePHIDetectionJob,

    -- * Request Lenses
    describePHIDetectionJob_jobId,

    -- * Destructuring the Response
    DescribePHIDetectionJobResponse (..),
    newDescribePHIDetectionJobResponse,

    -- * Response Lenses
    describePHIDetectionJobResponse_comprehendMedicalAsyncJobProperties,
    describePHIDetectionJobResponse_httpStatus,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePHIDetectionJob' smart constructor.
data DescribePHIDetectionJob = DescribePHIDetectionJob'
  { -- | The identifier that Comprehend Medical; generated for the job. The
    -- @StartPHIDetectionJob@ operation returns this identifier in its
    -- response.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePHIDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describePHIDetectionJob_jobId' - The identifier that Comprehend Medical; generated for the job. The
-- @StartPHIDetectionJob@ operation returns this identifier in its
-- response.
newDescribePHIDetectionJob ::
  -- | 'jobId'
  Prelude.Text ->
  DescribePHIDetectionJob
newDescribePHIDetectionJob pJobId_ =
  DescribePHIDetectionJob' {jobId = pJobId_}

-- | The identifier that Comprehend Medical; generated for the job. The
-- @StartPHIDetectionJob@ operation returns this identifier in its
-- response.
describePHIDetectionJob_jobId :: Lens.Lens' DescribePHIDetectionJob Prelude.Text
describePHIDetectionJob_jobId = Lens.lens (\DescribePHIDetectionJob' {jobId} -> jobId) (\s@DescribePHIDetectionJob' {} a -> s {jobId = a} :: DescribePHIDetectionJob)

instance Core.AWSRequest DescribePHIDetectionJob where
  type
    AWSResponse DescribePHIDetectionJob =
      DescribePHIDetectionJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePHIDetectionJobResponse'
            Prelude.<$> (x Data..?> "ComprehendMedicalAsyncJobProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePHIDetectionJob where
  hashWithSalt _salt DescribePHIDetectionJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData DescribePHIDetectionJob where
  rnf DescribePHIDetectionJob' {..} = Prelude.rnf jobId

instance Data.ToHeaders DescribePHIDetectionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComprehendMedical_20181030.DescribePHIDetectionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribePHIDetectionJob where
  toJSON DescribePHIDetectionJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Data..= jobId)]
      )

instance Data.ToPath DescribePHIDetectionJob where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribePHIDetectionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePHIDetectionJobResponse' smart constructor.
data DescribePHIDetectionJobResponse = DescribePHIDetectionJobResponse'
  { -- | An object that contains the properties associated with a detection job.
    comprehendMedicalAsyncJobProperties :: Prelude.Maybe ComprehendMedicalAsyncJobProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePHIDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comprehendMedicalAsyncJobProperties', 'describePHIDetectionJobResponse_comprehendMedicalAsyncJobProperties' - An object that contains the properties associated with a detection job.
--
-- 'httpStatus', 'describePHIDetectionJobResponse_httpStatus' - The response's http status code.
newDescribePHIDetectionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePHIDetectionJobResponse
newDescribePHIDetectionJobResponse pHttpStatus_ =
  DescribePHIDetectionJobResponse'
    { comprehendMedicalAsyncJobProperties =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains the properties associated with a detection job.
describePHIDetectionJobResponse_comprehendMedicalAsyncJobProperties :: Lens.Lens' DescribePHIDetectionJobResponse (Prelude.Maybe ComprehendMedicalAsyncJobProperties)
describePHIDetectionJobResponse_comprehendMedicalAsyncJobProperties = Lens.lens (\DescribePHIDetectionJobResponse' {comprehendMedicalAsyncJobProperties} -> comprehendMedicalAsyncJobProperties) (\s@DescribePHIDetectionJobResponse' {} a -> s {comprehendMedicalAsyncJobProperties = a} :: DescribePHIDetectionJobResponse)

-- | The response's http status code.
describePHIDetectionJobResponse_httpStatus :: Lens.Lens' DescribePHIDetectionJobResponse Prelude.Int
describePHIDetectionJobResponse_httpStatus = Lens.lens (\DescribePHIDetectionJobResponse' {httpStatus} -> httpStatus) (\s@DescribePHIDetectionJobResponse' {} a -> s {httpStatus = a} :: DescribePHIDetectionJobResponse)

instance
  Prelude.NFData
    DescribePHIDetectionJobResponse
  where
  rnf DescribePHIDetectionJobResponse' {..} =
    Prelude.rnf comprehendMedicalAsyncJobProperties
      `Prelude.seq` Prelude.rnf httpStatus
