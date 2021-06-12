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
-- Module      : Network.AWS.Comprehend.DescribeTopicsDetectionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a topic detection job. Use this
-- operation to get the status of a detection job.
module Network.AWS.Comprehend.DescribeTopicsDetectionJob
  ( -- * Creating a Request
    DescribeTopicsDetectionJob (..),
    newDescribeTopicsDetectionJob,

    -- * Request Lenses
    describeTopicsDetectionJob_jobId,

    -- * Destructuring the Response
    DescribeTopicsDetectionJobResponse (..),
    newDescribeTopicsDetectionJobResponse,

    -- * Response Lenses
    describeTopicsDetectionJobResponse_topicsDetectionJobProperties,
    describeTopicsDetectionJobResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTopicsDetectionJob' smart constructor.
data DescribeTopicsDetectionJob = DescribeTopicsDetectionJob'
  { -- | The identifier assigned by the user to the detection job.
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTopicsDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describeTopicsDetectionJob_jobId' - The identifier assigned by the user to the detection job.
newDescribeTopicsDetectionJob ::
  -- | 'jobId'
  Core.Text ->
  DescribeTopicsDetectionJob
newDescribeTopicsDetectionJob pJobId_ =
  DescribeTopicsDetectionJob' {jobId = pJobId_}

-- | The identifier assigned by the user to the detection job.
describeTopicsDetectionJob_jobId :: Lens.Lens' DescribeTopicsDetectionJob Core.Text
describeTopicsDetectionJob_jobId = Lens.lens (\DescribeTopicsDetectionJob' {jobId} -> jobId) (\s@DescribeTopicsDetectionJob' {} a -> s {jobId = a} :: DescribeTopicsDetectionJob)

instance Core.AWSRequest DescribeTopicsDetectionJob where
  type
    AWSResponse DescribeTopicsDetectionJob =
      DescribeTopicsDetectionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTopicsDetectionJobResponse'
            Core.<$> (x Core..?> "TopicsDetectionJobProperties")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeTopicsDetectionJob

instance Core.NFData DescribeTopicsDetectionJob

instance Core.ToHeaders DescribeTopicsDetectionJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.DescribeTopicsDetectionJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeTopicsDetectionJob where
  toJSON DescribeTopicsDetectionJob' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.ToPath DescribeTopicsDetectionJob where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTopicsDetectionJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeTopicsDetectionJobResponse' smart constructor.
data DescribeTopicsDetectionJobResponse = DescribeTopicsDetectionJobResponse'
  { -- | The list of properties for the requested job.
    topicsDetectionJobProperties :: Core.Maybe TopicsDetectionJobProperties,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTopicsDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topicsDetectionJobProperties', 'describeTopicsDetectionJobResponse_topicsDetectionJobProperties' - The list of properties for the requested job.
--
-- 'httpStatus', 'describeTopicsDetectionJobResponse_httpStatus' - The response's http status code.
newDescribeTopicsDetectionJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeTopicsDetectionJobResponse
newDescribeTopicsDetectionJobResponse pHttpStatus_ =
  DescribeTopicsDetectionJobResponse'
    { topicsDetectionJobProperties =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of properties for the requested job.
describeTopicsDetectionJobResponse_topicsDetectionJobProperties :: Lens.Lens' DescribeTopicsDetectionJobResponse (Core.Maybe TopicsDetectionJobProperties)
describeTopicsDetectionJobResponse_topicsDetectionJobProperties = Lens.lens (\DescribeTopicsDetectionJobResponse' {topicsDetectionJobProperties} -> topicsDetectionJobProperties) (\s@DescribeTopicsDetectionJobResponse' {} a -> s {topicsDetectionJobProperties = a} :: DescribeTopicsDetectionJobResponse)

-- | The response's http status code.
describeTopicsDetectionJobResponse_httpStatus :: Lens.Lens' DescribeTopicsDetectionJobResponse Core.Int
describeTopicsDetectionJobResponse_httpStatus = Lens.lens (\DescribeTopicsDetectionJobResponse' {httpStatus} -> httpStatus) (\s@DescribeTopicsDetectionJobResponse' {} a -> s {httpStatus = a} :: DescribeTopicsDetectionJobResponse)

instance
  Core.NFData
    DescribeTopicsDetectionJobResponse
