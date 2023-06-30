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
-- Module      : Amazonka.Comprehend.DescribeTargetedSentimentDetectionJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a targeted sentiment detection job.
-- Use this operation to get the status of the job.
module Amazonka.Comprehend.DescribeTargetedSentimentDetectionJob
  ( -- * Creating a Request
    DescribeTargetedSentimentDetectionJob (..),
    newDescribeTargetedSentimentDetectionJob,

    -- * Request Lenses
    describeTargetedSentimentDetectionJob_jobId,

    -- * Destructuring the Response
    DescribeTargetedSentimentDetectionJobResponse (..),
    newDescribeTargetedSentimentDetectionJobResponse,

    -- * Response Lenses
    describeTargetedSentimentDetectionJobResponse_targetedSentimentDetectionJobProperties,
    describeTargetedSentimentDetectionJobResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTargetedSentimentDetectionJob' smart constructor.
data DescribeTargetedSentimentDetectionJob = DescribeTargetedSentimentDetectionJob'
  { -- | The identifier that Amazon Comprehend generated for the job. The
    -- operation returns this identifier in its response.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTargetedSentimentDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describeTargetedSentimentDetectionJob_jobId' - The identifier that Amazon Comprehend generated for the job. The
-- operation returns this identifier in its response.
newDescribeTargetedSentimentDetectionJob ::
  -- | 'jobId'
  Prelude.Text ->
  DescribeTargetedSentimentDetectionJob
newDescribeTargetedSentimentDetectionJob pJobId_ =
  DescribeTargetedSentimentDetectionJob'
    { jobId =
        pJobId_
    }

-- | The identifier that Amazon Comprehend generated for the job. The
-- operation returns this identifier in its response.
describeTargetedSentimentDetectionJob_jobId :: Lens.Lens' DescribeTargetedSentimentDetectionJob Prelude.Text
describeTargetedSentimentDetectionJob_jobId = Lens.lens (\DescribeTargetedSentimentDetectionJob' {jobId} -> jobId) (\s@DescribeTargetedSentimentDetectionJob' {} a -> s {jobId = a} :: DescribeTargetedSentimentDetectionJob)

instance
  Core.AWSRequest
    DescribeTargetedSentimentDetectionJob
  where
  type
    AWSResponse
      DescribeTargetedSentimentDetectionJob =
      DescribeTargetedSentimentDetectionJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTargetedSentimentDetectionJobResponse'
            Prelude.<$> ( x
                            Data..?> "TargetedSentimentDetectionJobProperties"
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTargetedSentimentDetectionJob
  where
  hashWithSalt
    _salt
    DescribeTargetedSentimentDetectionJob' {..} =
      _salt `Prelude.hashWithSalt` jobId

instance
  Prelude.NFData
    DescribeTargetedSentimentDetectionJob
  where
  rnf DescribeTargetedSentimentDetectionJob' {..} =
    Prelude.rnf jobId

instance
  Data.ToHeaders
    DescribeTargetedSentimentDetectionJob
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.DescribeTargetedSentimentDetectionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeTargetedSentimentDetectionJob
  where
  toJSON DescribeTargetedSentimentDetectionJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Data..= jobId)]
      )

instance
  Data.ToPath
    DescribeTargetedSentimentDetectionJob
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeTargetedSentimentDetectionJob
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTargetedSentimentDetectionJobResponse' smart constructor.
data DescribeTargetedSentimentDetectionJobResponse = DescribeTargetedSentimentDetectionJobResponse'
  { -- | An object that contains the properties associated with a targeted
    -- sentiment detection job.
    targetedSentimentDetectionJobProperties :: Prelude.Maybe TargetedSentimentDetectionJobProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTargetedSentimentDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetedSentimentDetectionJobProperties', 'describeTargetedSentimentDetectionJobResponse_targetedSentimentDetectionJobProperties' - An object that contains the properties associated with a targeted
-- sentiment detection job.
--
-- 'httpStatus', 'describeTargetedSentimentDetectionJobResponse_httpStatus' - The response's http status code.
newDescribeTargetedSentimentDetectionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTargetedSentimentDetectionJobResponse
newDescribeTargetedSentimentDetectionJobResponse
  pHttpStatus_ =
    DescribeTargetedSentimentDetectionJobResponse'
      { targetedSentimentDetectionJobProperties =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An object that contains the properties associated with a targeted
-- sentiment detection job.
describeTargetedSentimentDetectionJobResponse_targetedSentimentDetectionJobProperties :: Lens.Lens' DescribeTargetedSentimentDetectionJobResponse (Prelude.Maybe TargetedSentimentDetectionJobProperties)
describeTargetedSentimentDetectionJobResponse_targetedSentimentDetectionJobProperties = Lens.lens (\DescribeTargetedSentimentDetectionJobResponse' {targetedSentimentDetectionJobProperties} -> targetedSentimentDetectionJobProperties) (\s@DescribeTargetedSentimentDetectionJobResponse' {} a -> s {targetedSentimentDetectionJobProperties = a} :: DescribeTargetedSentimentDetectionJobResponse)

-- | The response's http status code.
describeTargetedSentimentDetectionJobResponse_httpStatus :: Lens.Lens' DescribeTargetedSentimentDetectionJobResponse Prelude.Int
describeTargetedSentimentDetectionJobResponse_httpStatus = Lens.lens (\DescribeTargetedSentimentDetectionJobResponse' {httpStatus} -> httpStatus) (\s@DescribeTargetedSentimentDetectionJobResponse' {} a -> s {httpStatus = a} :: DescribeTargetedSentimentDetectionJobResponse)

instance
  Prelude.NFData
    DescribeTargetedSentimentDetectionJobResponse
  where
  rnf
    DescribeTargetedSentimentDetectionJobResponse' {..} =
      Prelude.rnf targetedSentimentDetectionJobProperties
        `Prelude.seq` Prelude.rnf httpStatus
