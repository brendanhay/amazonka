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
-- Module      : Network.AWS.Comprehend.DescribeEventsDetectionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status and details of an events detection job.
module Network.AWS.Comprehend.DescribeEventsDetectionJob
  ( -- * Creating a Request
    DescribeEventsDetectionJob (..),
    newDescribeEventsDetectionJob,

    -- * Request Lenses
    describeEventsDetectionJob_jobId,

    -- * Destructuring the Response
    DescribeEventsDetectionJobResponse (..),
    newDescribeEventsDetectionJobResponse,

    -- * Response Lenses
    describeEventsDetectionJobResponse_eventsDetectionJobProperties,
    describeEventsDetectionJobResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEventsDetectionJob' smart constructor.
data DescribeEventsDetectionJob = DescribeEventsDetectionJob'
  { -- | The identifier of the events detection job.
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEventsDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describeEventsDetectionJob_jobId' - The identifier of the events detection job.
newDescribeEventsDetectionJob ::
  -- | 'jobId'
  Core.Text ->
  DescribeEventsDetectionJob
newDescribeEventsDetectionJob pJobId_ =
  DescribeEventsDetectionJob' {jobId = pJobId_}

-- | The identifier of the events detection job.
describeEventsDetectionJob_jobId :: Lens.Lens' DescribeEventsDetectionJob Core.Text
describeEventsDetectionJob_jobId = Lens.lens (\DescribeEventsDetectionJob' {jobId} -> jobId) (\s@DescribeEventsDetectionJob' {} a -> s {jobId = a} :: DescribeEventsDetectionJob)

instance Core.AWSRequest DescribeEventsDetectionJob where
  type
    AWSResponse DescribeEventsDetectionJob =
      DescribeEventsDetectionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventsDetectionJobResponse'
            Core.<$> (x Core..?> "EventsDetectionJobProperties")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEventsDetectionJob

instance Core.NFData DescribeEventsDetectionJob

instance Core.ToHeaders DescribeEventsDetectionJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.DescribeEventsDetectionJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeEventsDetectionJob where
  toJSON DescribeEventsDetectionJob' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.ToPath DescribeEventsDetectionJob where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEventsDetectionJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeEventsDetectionJobResponse' smart constructor.
data DescribeEventsDetectionJobResponse = DescribeEventsDetectionJobResponse'
  { -- | An object that contains the properties associated with an event
    -- detection job.
    eventsDetectionJobProperties :: Core.Maybe EventsDetectionJobProperties,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEventsDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventsDetectionJobProperties', 'describeEventsDetectionJobResponse_eventsDetectionJobProperties' - An object that contains the properties associated with an event
-- detection job.
--
-- 'httpStatus', 'describeEventsDetectionJobResponse_httpStatus' - The response's http status code.
newDescribeEventsDetectionJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeEventsDetectionJobResponse
newDescribeEventsDetectionJobResponse pHttpStatus_ =
  DescribeEventsDetectionJobResponse'
    { eventsDetectionJobProperties =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains the properties associated with an event
-- detection job.
describeEventsDetectionJobResponse_eventsDetectionJobProperties :: Lens.Lens' DescribeEventsDetectionJobResponse (Core.Maybe EventsDetectionJobProperties)
describeEventsDetectionJobResponse_eventsDetectionJobProperties = Lens.lens (\DescribeEventsDetectionJobResponse' {eventsDetectionJobProperties} -> eventsDetectionJobProperties) (\s@DescribeEventsDetectionJobResponse' {} a -> s {eventsDetectionJobProperties = a} :: DescribeEventsDetectionJobResponse)

-- | The response's http status code.
describeEventsDetectionJobResponse_httpStatus :: Lens.Lens' DescribeEventsDetectionJobResponse Core.Int
describeEventsDetectionJobResponse_httpStatus = Lens.lens (\DescribeEventsDetectionJobResponse' {httpStatus} -> httpStatus) (\s@DescribeEventsDetectionJobResponse' {} a -> s {httpStatus = a} :: DescribeEventsDetectionJobResponse)

instance
  Core.NFData
    DescribeEventsDetectionJobResponse
