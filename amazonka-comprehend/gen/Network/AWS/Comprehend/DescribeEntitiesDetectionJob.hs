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
-- Module      : Network.AWS.Comprehend.DescribeEntitiesDetectionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with an entities detection job. Use this
-- operation to get the status of a detection job.
module Network.AWS.Comprehend.DescribeEntitiesDetectionJob
  ( -- * Creating a Request
    DescribeEntitiesDetectionJob (..),
    newDescribeEntitiesDetectionJob,

    -- * Request Lenses
    describeEntitiesDetectionJob_jobId,

    -- * Destructuring the Response
    DescribeEntitiesDetectionJobResponse (..),
    newDescribeEntitiesDetectionJobResponse,

    -- * Response Lenses
    describeEntitiesDetectionJobResponse_entitiesDetectionJobProperties,
    describeEntitiesDetectionJobResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEntitiesDetectionJob' smart constructor.
data DescribeEntitiesDetectionJob = DescribeEntitiesDetectionJob'
  { -- | The identifier that Amazon Comprehend generated for the job. The
    -- operation returns this identifier in its response.
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEntitiesDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describeEntitiesDetectionJob_jobId' - The identifier that Amazon Comprehend generated for the job. The
-- operation returns this identifier in its response.
newDescribeEntitiesDetectionJob ::
  -- | 'jobId'
  Core.Text ->
  DescribeEntitiesDetectionJob
newDescribeEntitiesDetectionJob pJobId_ =
  DescribeEntitiesDetectionJob' {jobId = pJobId_}

-- | The identifier that Amazon Comprehend generated for the job. The
-- operation returns this identifier in its response.
describeEntitiesDetectionJob_jobId :: Lens.Lens' DescribeEntitiesDetectionJob Core.Text
describeEntitiesDetectionJob_jobId = Lens.lens (\DescribeEntitiesDetectionJob' {jobId} -> jobId) (\s@DescribeEntitiesDetectionJob' {} a -> s {jobId = a} :: DescribeEntitiesDetectionJob)

instance Core.AWSRequest DescribeEntitiesDetectionJob where
  type
    AWSResponse DescribeEntitiesDetectionJob =
      DescribeEntitiesDetectionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEntitiesDetectionJobResponse'
            Core.<$> (x Core..?> "EntitiesDetectionJobProperties")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEntitiesDetectionJob

instance Core.NFData DescribeEntitiesDetectionJob

instance Core.ToHeaders DescribeEntitiesDetectionJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.DescribeEntitiesDetectionJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeEntitiesDetectionJob where
  toJSON DescribeEntitiesDetectionJob' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.ToPath DescribeEntitiesDetectionJob where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEntitiesDetectionJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeEntitiesDetectionJobResponse' smart constructor.
data DescribeEntitiesDetectionJobResponse = DescribeEntitiesDetectionJobResponse'
  { -- | An object that contains the properties associated with an entities
    -- detection job.
    entitiesDetectionJobProperties :: Core.Maybe EntitiesDetectionJobProperties,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEntitiesDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entitiesDetectionJobProperties', 'describeEntitiesDetectionJobResponse_entitiesDetectionJobProperties' - An object that contains the properties associated with an entities
-- detection job.
--
-- 'httpStatus', 'describeEntitiesDetectionJobResponse_httpStatus' - The response's http status code.
newDescribeEntitiesDetectionJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeEntitiesDetectionJobResponse
newDescribeEntitiesDetectionJobResponse pHttpStatus_ =
  DescribeEntitiesDetectionJobResponse'
    { entitiesDetectionJobProperties =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains the properties associated with an entities
-- detection job.
describeEntitiesDetectionJobResponse_entitiesDetectionJobProperties :: Lens.Lens' DescribeEntitiesDetectionJobResponse (Core.Maybe EntitiesDetectionJobProperties)
describeEntitiesDetectionJobResponse_entitiesDetectionJobProperties = Lens.lens (\DescribeEntitiesDetectionJobResponse' {entitiesDetectionJobProperties} -> entitiesDetectionJobProperties) (\s@DescribeEntitiesDetectionJobResponse' {} a -> s {entitiesDetectionJobProperties = a} :: DescribeEntitiesDetectionJobResponse)

-- | The response's http status code.
describeEntitiesDetectionJobResponse_httpStatus :: Lens.Lens' DescribeEntitiesDetectionJobResponse Core.Int
describeEntitiesDetectionJobResponse_httpStatus = Lens.lens (\DescribeEntitiesDetectionJobResponse' {httpStatus} -> httpStatus) (\s@DescribeEntitiesDetectionJobResponse' {} a -> s {httpStatus = a} :: DescribeEntitiesDetectionJobResponse)

instance
  Core.NFData
    DescribeEntitiesDetectionJobResponse
