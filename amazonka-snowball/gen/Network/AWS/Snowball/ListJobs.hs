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
-- Module      : Network.AWS.Snowball.ListJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @JobListEntry@ objects of the specified length. Each
-- @JobListEntry@ object contains a job\'s state, a job\'s ID, and a value
-- that indicates whether the job is a job part, in the case of export
-- jobs. Calling this API action in one of the US regions will return jobs
-- from the list of all jobs associated with this account in all US
-- regions.
--
-- This operation returns paginated results.
module Network.AWS.Snowball.ListJobs
  ( -- * Creating a Request
    ListJobs (..),
    newListJobs,

    -- * Request Lenses
    listJobs_nextToken,
    listJobs_maxResults,

    -- * Destructuring the Response
    ListJobsResponse (..),
    newListJobsResponse,

    -- * Response Lenses
    listJobsResponse_jobListEntries,
    listJobsResponse_nextToken,
    listJobsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Snowball.Types

-- | /See:/ 'newListJobs' smart constructor.
data ListJobs = ListJobs'
  { -- | HTTP requests are stateless. To identify what object comes \"next\" in
    -- the list of @JobListEntry@ objects, you have the option of specifying
    -- @NextToken@ as the starting point for your returned list.
    nextToken :: Core.Maybe Core.Text,
    -- | The number of @JobListEntry@ objects to return.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listJobs_nextToken' - HTTP requests are stateless. To identify what object comes \"next\" in
-- the list of @JobListEntry@ objects, you have the option of specifying
-- @NextToken@ as the starting point for your returned list.
--
-- 'maxResults', 'listJobs_maxResults' - The number of @JobListEntry@ objects to return.
newListJobs ::
  ListJobs
newListJobs =
  ListJobs'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | HTTP requests are stateless. To identify what object comes \"next\" in
-- the list of @JobListEntry@ objects, you have the option of specifying
-- @NextToken@ as the starting point for your returned list.
listJobs_nextToken :: Lens.Lens' ListJobs (Core.Maybe Core.Text)
listJobs_nextToken = Lens.lens (\ListJobs' {nextToken} -> nextToken) (\s@ListJobs' {} a -> s {nextToken = a} :: ListJobs)

-- | The number of @JobListEntry@ objects to return.
listJobs_maxResults :: Lens.Lens' ListJobs (Core.Maybe Core.Natural)
listJobs_maxResults = Lens.lens (\ListJobs' {maxResults} -> maxResults) (\s@ListJobs' {} a -> s {maxResults = a} :: ListJobs)

instance Core.AWSPager ListJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listJobsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listJobsResponse_jobListEntries Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listJobs_nextToken
          Lens..~ rs
          Lens.^? listJobsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListJobs where
  type AWSResponse ListJobs = ListJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobsResponse'
            Core.<$> (x Core..?> "JobListEntries" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListJobs

instance Core.NFData ListJobs

instance Core.ToHeaders ListJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSIESnowballJobManagementService.ListJobs" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListJobs where
  toJSON ListJobs' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListJobs where
  toPath = Core.const "/"

instance Core.ToQuery ListJobs where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { -- | Each @JobListEntry@ object contains a job\'s state, a job\'s ID, and a
    -- value that indicates whether the job is a job part, in the case of
    -- export jobs.
    jobListEntries :: Core.Maybe [JobListEntry],
    -- | HTTP requests are stateless. If you use this automatically generated
    -- @NextToken@ value in your next @ListJobs@ call, your returned
    -- @JobListEntry@ objects will start from this point in the array.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobListEntries', 'listJobsResponse_jobListEntries' - Each @JobListEntry@ object contains a job\'s state, a job\'s ID, and a
-- value that indicates whether the job is a job part, in the case of
-- export jobs.
--
-- 'nextToken', 'listJobsResponse_nextToken' - HTTP requests are stateless. If you use this automatically generated
-- @NextToken@ value in your next @ListJobs@ call, your returned
-- @JobListEntry@ objects will start from this point in the array.
--
-- 'httpStatus', 'listJobsResponse_httpStatus' - The response's http status code.
newListJobsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListJobsResponse
newListJobsResponse pHttpStatus_ =
  ListJobsResponse'
    { jobListEntries = Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Each @JobListEntry@ object contains a job\'s state, a job\'s ID, and a
-- value that indicates whether the job is a job part, in the case of
-- export jobs.
listJobsResponse_jobListEntries :: Lens.Lens' ListJobsResponse (Core.Maybe [JobListEntry])
listJobsResponse_jobListEntries = Lens.lens (\ListJobsResponse' {jobListEntries} -> jobListEntries) (\s@ListJobsResponse' {} a -> s {jobListEntries = a} :: ListJobsResponse) Core.. Lens.mapping Lens._Coerce

-- | HTTP requests are stateless. If you use this automatically generated
-- @NextToken@ value in your next @ListJobs@ call, your returned
-- @JobListEntry@ objects will start from this point in the array.
listJobsResponse_nextToken :: Lens.Lens' ListJobsResponse (Core.Maybe Core.Text)
listJobsResponse_nextToken = Lens.lens (\ListJobsResponse' {nextToken} -> nextToken) (\s@ListJobsResponse' {} a -> s {nextToken = a} :: ListJobsResponse)

-- | The response's http status code.
listJobsResponse_httpStatus :: Lens.Lens' ListJobsResponse Core.Int
listJobsResponse_httpStatus = Lens.lens (\ListJobsResponse' {httpStatus} -> httpStatus) (\s@ListJobsResponse' {} a -> s {httpStatus = a} :: ListJobsResponse)

instance Core.NFData ListJobsResponse
