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
-- Module      : Amazonka.ComprehendMedical.DescribeEntitiesDetectionV2Job
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a medical entities detection job.
-- Use this operation to get the status of a detection job.
module Amazonka.ComprehendMedical.DescribeEntitiesDetectionV2Job
  ( -- * Creating a Request
    DescribeEntitiesDetectionV2Job (..),
    newDescribeEntitiesDetectionV2Job,

    -- * Request Lenses
    describeEntitiesDetectionV2Job_jobId,

    -- * Destructuring the Response
    DescribeEntitiesDetectionV2JobResponse (..),
    newDescribeEntitiesDetectionV2JobResponse,

    -- * Response Lenses
    describeEntitiesDetectionV2JobResponse_comprehendMedicalAsyncJobProperties,
    describeEntitiesDetectionV2JobResponse_httpStatus,
  )
where

import Amazonka.ComprehendMedical.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEntitiesDetectionV2Job' smart constructor.
data DescribeEntitiesDetectionV2Job = DescribeEntitiesDetectionV2Job'
  { -- | The identifier that Comprehend Medical; generated for the job. The
    -- @StartEntitiesDetectionV2Job@ operation returns this identifier in its
    -- response.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEntitiesDetectionV2Job' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describeEntitiesDetectionV2Job_jobId' - The identifier that Comprehend Medical; generated for the job. The
-- @StartEntitiesDetectionV2Job@ operation returns this identifier in its
-- response.
newDescribeEntitiesDetectionV2Job ::
  -- | 'jobId'
  Prelude.Text ->
  DescribeEntitiesDetectionV2Job
newDescribeEntitiesDetectionV2Job pJobId_ =
  DescribeEntitiesDetectionV2Job' {jobId = pJobId_}

-- | The identifier that Comprehend Medical; generated for the job. The
-- @StartEntitiesDetectionV2Job@ operation returns this identifier in its
-- response.
describeEntitiesDetectionV2Job_jobId :: Lens.Lens' DescribeEntitiesDetectionV2Job Prelude.Text
describeEntitiesDetectionV2Job_jobId = Lens.lens (\DescribeEntitiesDetectionV2Job' {jobId} -> jobId) (\s@DescribeEntitiesDetectionV2Job' {} a -> s {jobId = a} :: DescribeEntitiesDetectionV2Job)

instance
  Core.AWSRequest
    DescribeEntitiesDetectionV2Job
  where
  type
    AWSResponse DescribeEntitiesDetectionV2Job =
      DescribeEntitiesDetectionV2JobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEntitiesDetectionV2JobResponse'
            Prelude.<$> (x Data..?> "ComprehendMedicalAsyncJobProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeEntitiesDetectionV2Job
  where
  hashWithSalt
    _salt
    DescribeEntitiesDetectionV2Job' {..} =
      _salt `Prelude.hashWithSalt` jobId

instance
  Prelude.NFData
    DescribeEntitiesDetectionV2Job
  where
  rnf DescribeEntitiesDetectionV2Job' {..} =
    Prelude.rnf jobId

instance
  Data.ToHeaders
    DescribeEntitiesDetectionV2Job
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComprehendMedical_20181030.DescribeEntitiesDetectionV2Job" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEntitiesDetectionV2Job where
  toJSON DescribeEntitiesDetectionV2Job' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Data..= jobId)]
      )

instance Data.ToPath DescribeEntitiesDetectionV2Job where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEntitiesDetectionV2Job where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEntitiesDetectionV2JobResponse' smart constructor.
data DescribeEntitiesDetectionV2JobResponse = DescribeEntitiesDetectionV2JobResponse'
  { -- | An object that contains the properties associated with a detection job.
    comprehendMedicalAsyncJobProperties :: Prelude.Maybe ComprehendMedicalAsyncJobProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEntitiesDetectionV2JobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comprehendMedicalAsyncJobProperties', 'describeEntitiesDetectionV2JobResponse_comprehendMedicalAsyncJobProperties' - An object that contains the properties associated with a detection job.
--
-- 'httpStatus', 'describeEntitiesDetectionV2JobResponse_httpStatus' - The response's http status code.
newDescribeEntitiesDetectionV2JobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEntitiesDetectionV2JobResponse
newDescribeEntitiesDetectionV2JobResponse
  pHttpStatus_ =
    DescribeEntitiesDetectionV2JobResponse'
      { comprehendMedicalAsyncJobProperties =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An object that contains the properties associated with a detection job.
describeEntitiesDetectionV2JobResponse_comprehendMedicalAsyncJobProperties :: Lens.Lens' DescribeEntitiesDetectionV2JobResponse (Prelude.Maybe ComprehendMedicalAsyncJobProperties)
describeEntitiesDetectionV2JobResponse_comprehendMedicalAsyncJobProperties = Lens.lens (\DescribeEntitiesDetectionV2JobResponse' {comprehendMedicalAsyncJobProperties} -> comprehendMedicalAsyncJobProperties) (\s@DescribeEntitiesDetectionV2JobResponse' {} a -> s {comprehendMedicalAsyncJobProperties = a} :: DescribeEntitiesDetectionV2JobResponse)

-- | The response's http status code.
describeEntitiesDetectionV2JobResponse_httpStatus :: Lens.Lens' DescribeEntitiesDetectionV2JobResponse Prelude.Int
describeEntitiesDetectionV2JobResponse_httpStatus = Lens.lens (\DescribeEntitiesDetectionV2JobResponse' {httpStatus} -> httpStatus) (\s@DescribeEntitiesDetectionV2JobResponse' {} a -> s {httpStatus = a} :: DescribeEntitiesDetectionV2JobResponse)

instance
  Prelude.NFData
    DescribeEntitiesDetectionV2JobResponse
  where
  rnf DescribeEntitiesDetectionV2JobResponse' {..} =
    Prelude.rnf comprehendMedicalAsyncJobProperties `Prelude.seq`
      Prelude.rnf httpStatus
