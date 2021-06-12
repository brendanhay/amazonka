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
-- Module      : Network.AWS.Comprehend.DescribePiiEntitiesDetectionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a PII entities detection job. For
-- example, you can use this operation to get the job status.
module Network.AWS.Comprehend.DescribePiiEntitiesDetectionJob
  ( -- * Creating a Request
    DescribePiiEntitiesDetectionJob (..),
    newDescribePiiEntitiesDetectionJob,

    -- * Request Lenses
    describePiiEntitiesDetectionJob_jobId,

    -- * Destructuring the Response
    DescribePiiEntitiesDetectionJobResponse (..),
    newDescribePiiEntitiesDetectionJobResponse,

    -- * Response Lenses
    describePiiEntitiesDetectionJobResponse_piiEntitiesDetectionJobProperties,
    describePiiEntitiesDetectionJobResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribePiiEntitiesDetectionJob' smart constructor.
data DescribePiiEntitiesDetectionJob = DescribePiiEntitiesDetectionJob'
  { -- | The identifier that Amazon Comprehend generated for the job. The
    -- operation returns this identifier in its response.
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePiiEntitiesDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describePiiEntitiesDetectionJob_jobId' - The identifier that Amazon Comprehend generated for the job. The
-- operation returns this identifier in its response.
newDescribePiiEntitiesDetectionJob ::
  -- | 'jobId'
  Core.Text ->
  DescribePiiEntitiesDetectionJob
newDescribePiiEntitiesDetectionJob pJobId_ =
  DescribePiiEntitiesDetectionJob' {jobId = pJobId_}

-- | The identifier that Amazon Comprehend generated for the job. The
-- operation returns this identifier in its response.
describePiiEntitiesDetectionJob_jobId :: Lens.Lens' DescribePiiEntitiesDetectionJob Core.Text
describePiiEntitiesDetectionJob_jobId = Lens.lens (\DescribePiiEntitiesDetectionJob' {jobId} -> jobId) (\s@DescribePiiEntitiesDetectionJob' {} a -> s {jobId = a} :: DescribePiiEntitiesDetectionJob)

instance
  Core.AWSRequest
    DescribePiiEntitiesDetectionJob
  where
  type
    AWSResponse DescribePiiEntitiesDetectionJob =
      DescribePiiEntitiesDetectionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePiiEntitiesDetectionJobResponse'
            Core.<$> (x Core..?> "PiiEntitiesDetectionJobProperties")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribePiiEntitiesDetectionJob

instance Core.NFData DescribePiiEntitiesDetectionJob

instance
  Core.ToHeaders
    DescribePiiEntitiesDetectionJob
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.DescribePiiEntitiesDetectionJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribePiiEntitiesDetectionJob where
  toJSON DescribePiiEntitiesDetectionJob' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.ToPath DescribePiiEntitiesDetectionJob where
  toPath = Core.const "/"

instance Core.ToQuery DescribePiiEntitiesDetectionJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribePiiEntitiesDetectionJobResponse' smart constructor.
data DescribePiiEntitiesDetectionJobResponse = DescribePiiEntitiesDetectionJobResponse'
  { piiEntitiesDetectionJobProperties :: Core.Maybe PiiEntitiesDetectionJobProperties,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePiiEntitiesDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'piiEntitiesDetectionJobProperties', 'describePiiEntitiesDetectionJobResponse_piiEntitiesDetectionJobProperties' - Undocumented member.
--
-- 'httpStatus', 'describePiiEntitiesDetectionJobResponse_httpStatus' - The response's http status code.
newDescribePiiEntitiesDetectionJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribePiiEntitiesDetectionJobResponse
newDescribePiiEntitiesDetectionJobResponse
  pHttpStatus_ =
    DescribePiiEntitiesDetectionJobResponse'
      { piiEntitiesDetectionJobProperties =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
describePiiEntitiesDetectionJobResponse_piiEntitiesDetectionJobProperties :: Lens.Lens' DescribePiiEntitiesDetectionJobResponse (Core.Maybe PiiEntitiesDetectionJobProperties)
describePiiEntitiesDetectionJobResponse_piiEntitiesDetectionJobProperties = Lens.lens (\DescribePiiEntitiesDetectionJobResponse' {piiEntitiesDetectionJobProperties} -> piiEntitiesDetectionJobProperties) (\s@DescribePiiEntitiesDetectionJobResponse' {} a -> s {piiEntitiesDetectionJobProperties = a} :: DescribePiiEntitiesDetectionJobResponse)

-- | The response's http status code.
describePiiEntitiesDetectionJobResponse_httpStatus :: Lens.Lens' DescribePiiEntitiesDetectionJobResponse Core.Int
describePiiEntitiesDetectionJobResponse_httpStatus = Lens.lens (\DescribePiiEntitiesDetectionJobResponse' {httpStatus} -> httpStatus) (\s@DescribePiiEntitiesDetectionJobResponse' {} a -> s {httpStatus = a} :: DescribePiiEntitiesDetectionJobResponse)

instance
  Core.NFData
    DescribePiiEntitiesDetectionJobResponse
