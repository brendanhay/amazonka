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
-- Module      : Network.AWS.Snowball.DescribeJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific job including shipping information,
-- job status, and other important metadata.
module Network.AWS.Snowball.DescribeJob
  ( -- * Creating a Request
    DescribeJob (..),
    newDescribeJob,

    -- * Request Lenses
    describeJob_jobId,

    -- * Destructuring the Response
    DescribeJobResponse (..),
    newDescribeJobResponse,

    -- * Response Lenses
    describeJobResponse_subJobMetadata,
    describeJobResponse_jobMetadata,
    describeJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Snowball.Types

-- | /See:/ 'newDescribeJob' smart constructor.
data DescribeJob = DescribeJob'
  { -- | The automatically generated ID for a job, for example
    -- @JID123e4567-e89b-12d3-a456-426655440000@.
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describeJob_jobId' - The automatically generated ID for a job, for example
-- @JID123e4567-e89b-12d3-a456-426655440000@.
newDescribeJob ::
  -- | 'jobId'
  Core.Text ->
  DescribeJob
newDescribeJob pJobId_ =
  DescribeJob' {jobId = pJobId_}

-- | The automatically generated ID for a job, for example
-- @JID123e4567-e89b-12d3-a456-426655440000@.
describeJob_jobId :: Lens.Lens' DescribeJob Core.Text
describeJob_jobId = Lens.lens (\DescribeJob' {jobId} -> jobId) (\s@DescribeJob' {} a -> s {jobId = a} :: DescribeJob)

instance Core.AWSRequest DescribeJob where
  type AWSResponse DescribeJob = DescribeJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeJobResponse'
            Core.<$> (x Core..?> "SubJobMetadata" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "JobMetadata")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeJob

instance Core.NFData DescribeJob

instance Core.ToHeaders DescribeJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSIESnowballJobManagementService.DescribeJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeJob where
  toJSON DescribeJob' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.ToPath DescribeJob where
  toPath = Core.const "/"

instance Core.ToQuery DescribeJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeJobResponse' smart constructor.
data DescribeJobResponse = DescribeJobResponse'
  { -- | Information about a specific job part (in the case of an export job),
    -- including shipping information, job status, and other important
    -- metadata.
    subJobMetadata :: Core.Maybe [JobMetadata],
    -- | Information about a specific job, including shipping information, job
    -- status, and other important metadata.
    jobMetadata :: Core.Maybe JobMetadata,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subJobMetadata', 'describeJobResponse_subJobMetadata' - Information about a specific job part (in the case of an export job),
-- including shipping information, job status, and other important
-- metadata.
--
-- 'jobMetadata', 'describeJobResponse_jobMetadata' - Information about a specific job, including shipping information, job
-- status, and other important metadata.
--
-- 'httpStatus', 'describeJobResponse_httpStatus' - The response's http status code.
newDescribeJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeJobResponse
newDescribeJobResponse pHttpStatus_ =
  DescribeJobResponse'
    { subJobMetadata = Core.Nothing,
      jobMetadata = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a specific job part (in the case of an export job),
-- including shipping information, job status, and other important
-- metadata.
describeJobResponse_subJobMetadata :: Lens.Lens' DescribeJobResponse (Core.Maybe [JobMetadata])
describeJobResponse_subJobMetadata = Lens.lens (\DescribeJobResponse' {subJobMetadata} -> subJobMetadata) (\s@DescribeJobResponse' {} a -> s {subJobMetadata = a} :: DescribeJobResponse) Core.. Lens.mapping Lens._Coerce

-- | Information about a specific job, including shipping information, job
-- status, and other important metadata.
describeJobResponse_jobMetadata :: Lens.Lens' DescribeJobResponse (Core.Maybe JobMetadata)
describeJobResponse_jobMetadata = Lens.lens (\DescribeJobResponse' {jobMetadata} -> jobMetadata) (\s@DescribeJobResponse' {} a -> s {jobMetadata = a} :: DescribeJobResponse)

-- | The response's http status code.
describeJobResponse_httpStatus :: Lens.Lens' DescribeJobResponse Core.Int
describeJobResponse_httpStatus = Lens.lens (\DescribeJobResponse' {httpStatus} -> httpStatus) (\s@DescribeJobResponse' {} a -> s {httpStatus = a} :: DescribeJobResponse)

instance Core.NFData DescribeJobResponse
