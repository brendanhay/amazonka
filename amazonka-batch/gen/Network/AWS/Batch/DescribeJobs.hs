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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for @DescribeJobs@.
--
-- /See:/ 'newDescribeJobs' smart constructor.
data DescribeJobs = DescribeJobs'
  { -- | A list of up to 100 job IDs.
    jobs :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
newDescribeJobs =
  DescribeJobs' {jobs = Prelude.mempty}

-- | A list of up to 100 job IDs.
describeJobs_jobs :: Lens.Lens' DescribeJobs [Prelude.Text]
describeJobs_jobs = Lens.lens (\DescribeJobs' {jobs} -> jobs) (\s@DescribeJobs' {} a -> s {jobs = a} :: DescribeJobs) Prelude.. Lens._Coerce

instance Core.AWSRequest DescribeJobs where
  type AWSResponse DescribeJobs = DescribeJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeJobsResponse'
            Prelude.<$> (x Core..?> "jobs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeJobs

instance Prelude.NFData DescribeJobs

instance Core.ToHeaders DescribeJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeJobs where
  toJSON DescribeJobs' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("jobs" Core..= jobs)]
      )

instance Core.ToPath DescribeJobs where
  toPath = Prelude.const "/v1/describejobs"

instance Core.ToQuery DescribeJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeJobsResponse' smart constructor.
data DescribeJobsResponse = DescribeJobsResponse'
  { -- | The list of jobs.
    jobs :: Prelude.Maybe [JobDetail],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeJobsResponse
newDescribeJobsResponse pHttpStatus_ =
  DescribeJobsResponse'
    { jobs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of jobs.
describeJobsResponse_jobs :: Lens.Lens' DescribeJobsResponse (Prelude.Maybe [JobDetail])
describeJobsResponse_jobs = Lens.lens (\DescribeJobsResponse' {jobs} -> jobs) (\s@DescribeJobsResponse' {} a -> s {jobs = a} :: DescribeJobsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeJobsResponse_httpStatus :: Lens.Lens' DescribeJobsResponse Prelude.Int
describeJobsResponse_httpStatus = Lens.lens (\DescribeJobsResponse' {httpStatus} -> httpStatus) (\s@DescribeJobsResponse' {} a -> s {httpStatus = a} :: DescribeJobsResponse)

instance Prelude.NFData DescribeJobsResponse
