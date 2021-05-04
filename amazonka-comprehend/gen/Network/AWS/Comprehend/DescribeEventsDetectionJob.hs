{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEventsDetectionJob' smart constructor.
data DescribeEventsDetectionJob = DescribeEventsDetectionJob'
  { -- | The identifier of the events detection job.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DescribeEventsDetectionJob
newDescribeEventsDetectionJob pJobId_ =
  DescribeEventsDetectionJob' {jobId = pJobId_}

-- | The identifier of the events detection job.
describeEventsDetectionJob_jobId :: Lens.Lens' DescribeEventsDetectionJob Prelude.Text
describeEventsDetectionJob_jobId = Lens.lens (\DescribeEventsDetectionJob' {jobId} -> jobId) (\s@DescribeEventsDetectionJob' {} a -> s {jobId = a} :: DescribeEventsDetectionJob)

instance
  Prelude.AWSRequest
    DescribeEventsDetectionJob
  where
  type
    Rs DescribeEventsDetectionJob =
      DescribeEventsDetectionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventsDetectionJobResponse'
            Prelude.<$> (x Prelude..?> "EventsDetectionJobProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEventsDetectionJob

instance Prelude.NFData DescribeEventsDetectionJob

instance Prelude.ToHeaders DescribeEventsDetectionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Comprehend_20171127.DescribeEventsDetectionJob" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeEventsDetectionJob where
  toJSON DescribeEventsDetectionJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Prelude..= jobId)]
      )

instance Prelude.ToPath DescribeEventsDetectionJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeEventsDetectionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEventsDetectionJobResponse' smart constructor.
data DescribeEventsDetectionJobResponse = DescribeEventsDetectionJobResponse'
  { -- | An object that contains the properties associated with an event
    -- detection job.
    eventsDetectionJobProperties :: Prelude.Maybe EventsDetectionJobProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeEventsDetectionJobResponse
newDescribeEventsDetectionJobResponse pHttpStatus_ =
  DescribeEventsDetectionJobResponse'
    { eventsDetectionJobProperties =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains the properties associated with an event
-- detection job.
describeEventsDetectionJobResponse_eventsDetectionJobProperties :: Lens.Lens' DescribeEventsDetectionJobResponse (Prelude.Maybe EventsDetectionJobProperties)
describeEventsDetectionJobResponse_eventsDetectionJobProperties = Lens.lens (\DescribeEventsDetectionJobResponse' {eventsDetectionJobProperties} -> eventsDetectionJobProperties) (\s@DescribeEventsDetectionJobResponse' {} a -> s {eventsDetectionJobProperties = a} :: DescribeEventsDetectionJobResponse)

-- | The response's http status code.
describeEventsDetectionJobResponse_httpStatus :: Lens.Lens' DescribeEventsDetectionJobResponse Prelude.Int
describeEventsDetectionJobResponse_httpStatus = Lens.lens (\DescribeEventsDetectionJobResponse' {httpStatus} -> httpStatus) (\s@DescribeEventsDetectionJobResponse' {} a -> s {httpStatus = a} :: DescribeEventsDetectionJobResponse)

instance
  Prelude.NFData
    DescribeEventsDetectionJobResponse
