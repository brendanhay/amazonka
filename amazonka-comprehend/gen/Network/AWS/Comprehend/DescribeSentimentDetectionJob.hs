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
-- Module      : Network.AWS.Comprehend.DescribeSentimentDetectionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a sentiment detection job. Use this
-- operation to get the status of a detection job.
module Network.AWS.Comprehend.DescribeSentimentDetectionJob
  ( -- * Creating a Request
    DescribeSentimentDetectionJob (..),
    newDescribeSentimentDetectionJob,

    -- * Request Lenses
    describeSentimentDetectionJob_jobId,

    -- * Destructuring the Response
    DescribeSentimentDetectionJobResponse (..),
    newDescribeSentimentDetectionJobResponse,

    -- * Response Lenses
    describeSentimentDetectionJobResponse_sentimentDetectionJobProperties,
    describeSentimentDetectionJobResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeSentimentDetectionJob' smart constructor.
data DescribeSentimentDetectionJob = DescribeSentimentDetectionJob'
  { -- | The identifier that Amazon Comprehend generated for the job. The
    -- operation returns this identifier in its response.
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSentimentDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describeSentimentDetectionJob_jobId' - The identifier that Amazon Comprehend generated for the job. The
-- operation returns this identifier in its response.
newDescribeSentimentDetectionJob ::
  -- | 'jobId'
  Core.Text ->
  DescribeSentimentDetectionJob
newDescribeSentimentDetectionJob pJobId_ =
  DescribeSentimentDetectionJob' {jobId = pJobId_}

-- | The identifier that Amazon Comprehend generated for the job. The
-- operation returns this identifier in its response.
describeSentimentDetectionJob_jobId :: Lens.Lens' DescribeSentimentDetectionJob Core.Text
describeSentimentDetectionJob_jobId = Lens.lens (\DescribeSentimentDetectionJob' {jobId} -> jobId) (\s@DescribeSentimentDetectionJob' {} a -> s {jobId = a} :: DescribeSentimentDetectionJob)

instance
  Core.AWSRequest
    DescribeSentimentDetectionJob
  where
  type
    AWSResponse DescribeSentimentDetectionJob =
      DescribeSentimentDetectionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSentimentDetectionJobResponse'
            Core.<$> (x Core..?> "SentimentDetectionJobProperties")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeSentimentDetectionJob

instance Core.NFData DescribeSentimentDetectionJob

instance Core.ToHeaders DescribeSentimentDetectionJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.DescribeSentimentDetectionJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeSentimentDetectionJob where
  toJSON DescribeSentimentDetectionJob' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.ToPath DescribeSentimentDetectionJob where
  toPath = Core.const "/"

instance Core.ToQuery DescribeSentimentDetectionJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeSentimentDetectionJobResponse' smart constructor.
data DescribeSentimentDetectionJobResponse = DescribeSentimentDetectionJobResponse'
  { -- | An object that contains the properties associated with a sentiment
    -- detection job.
    sentimentDetectionJobProperties :: Core.Maybe SentimentDetectionJobProperties,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSentimentDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sentimentDetectionJobProperties', 'describeSentimentDetectionJobResponse_sentimentDetectionJobProperties' - An object that contains the properties associated with a sentiment
-- detection job.
--
-- 'httpStatus', 'describeSentimentDetectionJobResponse_httpStatus' - The response's http status code.
newDescribeSentimentDetectionJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeSentimentDetectionJobResponse
newDescribeSentimentDetectionJobResponse pHttpStatus_ =
  DescribeSentimentDetectionJobResponse'
    { sentimentDetectionJobProperties =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains the properties associated with a sentiment
-- detection job.
describeSentimentDetectionJobResponse_sentimentDetectionJobProperties :: Lens.Lens' DescribeSentimentDetectionJobResponse (Core.Maybe SentimentDetectionJobProperties)
describeSentimentDetectionJobResponse_sentimentDetectionJobProperties = Lens.lens (\DescribeSentimentDetectionJobResponse' {sentimentDetectionJobProperties} -> sentimentDetectionJobProperties) (\s@DescribeSentimentDetectionJobResponse' {} a -> s {sentimentDetectionJobProperties = a} :: DescribeSentimentDetectionJobResponse)

-- | The response's http status code.
describeSentimentDetectionJobResponse_httpStatus :: Lens.Lens' DescribeSentimentDetectionJobResponse Core.Int
describeSentimentDetectionJobResponse_httpStatus = Lens.lens (\DescribeSentimentDetectionJobResponse' {httpStatus} -> httpStatus) (\s@DescribeSentimentDetectionJobResponse' {} a -> s {httpStatus = a} :: DescribeSentimentDetectionJobResponse)

instance
  Core.NFData
    DescribeSentimentDetectionJobResponse
