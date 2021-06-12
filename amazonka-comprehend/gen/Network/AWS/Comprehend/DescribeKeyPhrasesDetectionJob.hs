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
-- Module      : Network.AWS.Comprehend.DescribeKeyPhrasesDetectionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a key phrases detection job. Use
-- this operation to get the status of a detection job.
module Network.AWS.Comprehend.DescribeKeyPhrasesDetectionJob
  ( -- * Creating a Request
    DescribeKeyPhrasesDetectionJob (..),
    newDescribeKeyPhrasesDetectionJob,

    -- * Request Lenses
    describeKeyPhrasesDetectionJob_jobId,

    -- * Destructuring the Response
    DescribeKeyPhrasesDetectionJobResponse (..),
    newDescribeKeyPhrasesDetectionJobResponse,

    -- * Response Lenses
    describeKeyPhrasesDetectionJobResponse_keyPhrasesDetectionJobProperties,
    describeKeyPhrasesDetectionJobResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeKeyPhrasesDetectionJob' smart constructor.
data DescribeKeyPhrasesDetectionJob = DescribeKeyPhrasesDetectionJob'
  { -- | The identifier that Amazon Comprehend generated for the job. The
    -- operation returns this identifier in its response.
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeKeyPhrasesDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describeKeyPhrasesDetectionJob_jobId' - The identifier that Amazon Comprehend generated for the job. The
-- operation returns this identifier in its response.
newDescribeKeyPhrasesDetectionJob ::
  -- | 'jobId'
  Core.Text ->
  DescribeKeyPhrasesDetectionJob
newDescribeKeyPhrasesDetectionJob pJobId_ =
  DescribeKeyPhrasesDetectionJob' {jobId = pJobId_}

-- | The identifier that Amazon Comprehend generated for the job. The
-- operation returns this identifier in its response.
describeKeyPhrasesDetectionJob_jobId :: Lens.Lens' DescribeKeyPhrasesDetectionJob Core.Text
describeKeyPhrasesDetectionJob_jobId = Lens.lens (\DescribeKeyPhrasesDetectionJob' {jobId} -> jobId) (\s@DescribeKeyPhrasesDetectionJob' {} a -> s {jobId = a} :: DescribeKeyPhrasesDetectionJob)

instance
  Core.AWSRequest
    DescribeKeyPhrasesDetectionJob
  where
  type
    AWSResponse DescribeKeyPhrasesDetectionJob =
      DescribeKeyPhrasesDetectionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeKeyPhrasesDetectionJobResponse'
            Core.<$> (x Core..?> "KeyPhrasesDetectionJobProperties")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeKeyPhrasesDetectionJob

instance Core.NFData DescribeKeyPhrasesDetectionJob

instance
  Core.ToHeaders
    DescribeKeyPhrasesDetectionJob
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.DescribeKeyPhrasesDetectionJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeKeyPhrasesDetectionJob where
  toJSON DescribeKeyPhrasesDetectionJob' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.ToPath DescribeKeyPhrasesDetectionJob where
  toPath = Core.const "/"

instance Core.ToQuery DescribeKeyPhrasesDetectionJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeKeyPhrasesDetectionJobResponse' smart constructor.
data DescribeKeyPhrasesDetectionJobResponse = DescribeKeyPhrasesDetectionJobResponse'
  { -- | An object that contains the properties associated with a key phrases
    -- detection job.
    keyPhrasesDetectionJobProperties :: Core.Maybe KeyPhrasesDetectionJobProperties,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeKeyPhrasesDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPhrasesDetectionJobProperties', 'describeKeyPhrasesDetectionJobResponse_keyPhrasesDetectionJobProperties' - An object that contains the properties associated with a key phrases
-- detection job.
--
-- 'httpStatus', 'describeKeyPhrasesDetectionJobResponse_httpStatus' - The response's http status code.
newDescribeKeyPhrasesDetectionJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeKeyPhrasesDetectionJobResponse
newDescribeKeyPhrasesDetectionJobResponse
  pHttpStatus_ =
    DescribeKeyPhrasesDetectionJobResponse'
      { keyPhrasesDetectionJobProperties =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An object that contains the properties associated with a key phrases
-- detection job.
describeKeyPhrasesDetectionJobResponse_keyPhrasesDetectionJobProperties :: Lens.Lens' DescribeKeyPhrasesDetectionJobResponse (Core.Maybe KeyPhrasesDetectionJobProperties)
describeKeyPhrasesDetectionJobResponse_keyPhrasesDetectionJobProperties = Lens.lens (\DescribeKeyPhrasesDetectionJobResponse' {keyPhrasesDetectionJobProperties} -> keyPhrasesDetectionJobProperties) (\s@DescribeKeyPhrasesDetectionJobResponse' {} a -> s {keyPhrasesDetectionJobProperties = a} :: DescribeKeyPhrasesDetectionJobResponse)

-- | The response's http status code.
describeKeyPhrasesDetectionJobResponse_httpStatus :: Lens.Lens' DescribeKeyPhrasesDetectionJobResponse Core.Int
describeKeyPhrasesDetectionJobResponse_httpStatus = Lens.lens (\DescribeKeyPhrasesDetectionJobResponse' {httpStatus} -> httpStatus) (\s@DescribeKeyPhrasesDetectionJobResponse' {} a -> s {httpStatus = a} :: DescribeKeyPhrasesDetectionJobResponse)

instance
  Core.NFData
    DescribeKeyPhrasesDetectionJobResponse
