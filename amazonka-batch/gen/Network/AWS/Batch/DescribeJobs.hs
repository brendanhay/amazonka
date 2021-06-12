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
-- Module      : Network.AWS.Batch.DescribeJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a list of AWS Batch jobs.
module Network.AWS.Batch.DescribeJobs
  ( -- * Creating a Request
    DescribeJobs (..),
    newDescribeJobs,

    -- * Request Lenses
    describeJobs_jobs,

    -- * Destructuring the Response
    DescribeJobsResponse (..),
    newDescribeJobsResponse,

    -- * Response Lenses
    describeJobsResponse_jobs,
    describeJobsResponse_httpStatus,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for @DescribeJobs@.
--
-- /See:/ 'newDescribeJobs' smart constructor.
data DescribeJobs = DescribeJobs'
  { -- | A list of up to 100 job IDs.
    jobs :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobs', 'describeJobs_jobs' - A list of up to 100 job IDs.
newDescribeJobs ::
  DescribeJobs
newDescribeJobs = DescribeJobs' {jobs = Core.mempty}

-- | A list of up to 100 job IDs.
describeJobs_jobs :: Lens.Lens' DescribeJobs [Core.Text]
describeJobs_jobs = Lens.lens (\DescribeJobs' {jobs} -> jobs) (\s@DescribeJobs' {} a -> s {jobs = a} :: DescribeJobs) Core.. Lens._Coerce

instance Core.AWSRequest DescribeJobs where
  type AWSResponse DescribeJobs = DescribeJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeJobsResponse'
            Core.<$> (x Core..?> "jobs" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeJobs

instance Core.NFData DescribeJobs

instance Core.ToHeaders DescribeJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeJobs where
  toJSON DescribeJobs' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("jobs" Core..= jobs)])

instance Core.ToPath DescribeJobs where
  toPath = Core.const "/v1/describejobs"

instance Core.ToQuery DescribeJobs where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeJobsResponse' smart constructor.
data DescribeJobsResponse = DescribeJobsResponse'
  { -- | The list of jobs.
    jobs :: Core.Maybe [JobDetail],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobs', 'describeJobsResponse_jobs' - The list of jobs.
--
-- 'httpStatus', 'describeJobsResponse_httpStatus' - The response's http status code.
newDescribeJobsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeJobsResponse
newDescribeJobsResponse pHttpStatus_ =
  DescribeJobsResponse'
    { jobs = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of jobs.
describeJobsResponse_jobs :: Lens.Lens' DescribeJobsResponse (Core.Maybe [JobDetail])
describeJobsResponse_jobs = Lens.lens (\DescribeJobsResponse' {jobs} -> jobs) (\s@DescribeJobsResponse' {} a -> s {jobs = a} :: DescribeJobsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeJobsResponse_httpStatus :: Lens.Lens' DescribeJobsResponse Core.Int
describeJobsResponse_httpStatus = Lens.lens (\DescribeJobsResponse' {httpStatus} -> httpStatus) (\s@DescribeJobsResponse' {} a -> s {httpStatus = a} :: DescribeJobsResponse)

instance Core.NFData DescribeJobsResponse
