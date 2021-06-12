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
-- Module      : Network.AWS.MediaConvert.ListJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a JSON array of up to twenty of your most recently created
-- jobs. This array includes in-process, completed, and errored jobs. This
-- will return the jobs themselves, not just a list of the jobs. To
-- retrieve the twenty next most recent jobs, use the nextToken string
-- returned with the array.
--
-- This operation returns paginated results.
module Network.AWS.MediaConvert.ListJobs
  ( -- * Creating a Request
    ListJobs (..),
    newListJobs,

    -- * Request Lenses
    listJobs_status,
    listJobs_nextToken,
    listJobs_maxResults,
    listJobs_queue,
    listJobs_order,

    -- * Destructuring the Response
    ListJobsResponse (..),
    newListJobsResponse,

    -- * Response Lenses
    listJobsResponse_nextToken,
    listJobsResponse_jobs,
    listJobsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListJobs' smart constructor.
data ListJobs = ListJobs'
  { -- | Optional. A job\'s status can be SUBMITTED, PROGRESSING, COMPLETE,
    -- CANCELED, or ERROR.
    status :: Core.Maybe JobStatus,
    -- | Optional. Use this string, provided with the response to a previous
    -- request, to request the next batch of jobs.
    nextToken :: Core.Maybe Core.Text,
    -- | Optional. Number of jobs, up to twenty, that will be returned at one
    -- time.
    maxResults :: Core.Maybe Core.Natural,
    -- | Optional. Provide a queue name to get back only jobs from that queue.
    queue :: Core.Maybe Core.Text,
    -- | Optional. When you request lists of resources, you can specify whether
    -- they are sorted in ASCENDING or DESCENDING order. Default varies by
    -- resource.
    order :: Core.Maybe Order
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
-- 'status', 'listJobs_status' - Optional. A job\'s status can be SUBMITTED, PROGRESSING, COMPLETE,
-- CANCELED, or ERROR.
--
-- 'nextToken', 'listJobs_nextToken' - Optional. Use this string, provided with the response to a previous
-- request, to request the next batch of jobs.
--
-- 'maxResults', 'listJobs_maxResults' - Optional. Number of jobs, up to twenty, that will be returned at one
-- time.
--
-- 'queue', 'listJobs_queue' - Optional. Provide a queue name to get back only jobs from that queue.
--
-- 'order', 'listJobs_order' - Optional. When you request lists of resources, you can specify whether
-- they are sorted in ASCENDING or DESCENDING order. Default varies by
-- resource.
newListJobs ::
  ListJobs
newListJobs =
  ListJobs'
    { status = Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      queue = Core.Nothing,
      order = Core.Nothing
    }

-- | Optional. A job\'s status can be SUBMITTED, PROGRESSING, COMPLETE,
-- CANCELED, or ERROR.
listJobs_status :: Lens.Lens' ListJobs (Core.Maybe JobStatus)
listJobs_status = Lens.lens (\ListJobs' {status} -> status) (\s@ListJobs' {} a -> s {status = a} :: ListJobs)

-- | Optional. Use this string, provided with the response to a previous
-- request, to request the next batch of jobs.
listJobs_nextToken :: Lens.Lens' ListJobs (Core.Maybe Core.Text)
listJobs_nextToken = Lens.lens (\ListJobs' {nextToken} -> nextToken) (\s@ListJobs' {} a -> s {nextToken = a} :: ListJobs)

-- | Optional. Number of jobs, up to twenty, that will be returned at one
-- time.
listJobs_maxResults :: Lens.Lens' ListJobs (Core.Maybe Core.Natural)
listJobs_maxResults = Lens.lens (\ListJobs' {maxResults} -> maxResults) (\s@ListJobs' {} a -> s {maxResults = a} :: ListJobs)

-- | Optional. Provide a queue name to get back only jobs from that queue.
listJobs_queue :: Lens.Lens' ListJobs (Core.Maybe Core.Text)
listJobs_queue = Lens.lens (\ListJobs' {queue} -> queue) (\s@ListJobs' {} a -> s {queue = a} :: ListJobs)

-- | Optional. When you request lists of resources, you can specify whether
-- they are sorted in ASCENDING or DESCENDING order. Default varies by
-- resource.
listJobs_order :: Lens.Lens' ListJobs (Core.Maybe Order)
listJobs_order = Lens.lens (\ListJobs' {order} -> order) (\s@ListJobs' {} a -> s {order = a} :: ListJobs)

instance Core.AWSPager ListJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listJobsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^? listJobsResponse_jobs Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listJobs_nextToken
          Lens..~ rs
          Lens.^? listJobsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListJobs where
  type AWSResponse ListJobs = ListJobsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "jobs" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListJobs

instance Core.NFData ListJobs

instance Core.ToHeaders ListJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListJobs where
  toPath = Core.const "/2017-08-29/jobs"

instance Core.ToQuery ListJobs where
  toQuery ListJobs' {..} =
    Core.mconcat
      [ "status" Core.=: status,
        "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "queue" Core.=: queue,
        "order" Core.=: order
      ]

-- | /See:/ 'newListJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { -- | Use this string to request the next batch of jobs.
    nextToken :: Core.Maybe Core.Text,
    -- | List of jobs
    jobs :: Core.Maybe [Job],
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
-- 'nextToken', 'listJobsResponse_nextToken' - Use this string to request the next batch of jobs.
--
-- 'jobs', 'listJobsResponse_jobs' - List of jobs
--
-- 'httpStatus', 'listJobsResponse_httpStatus' - The response's http status code.
newListJobsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListJobsResponse
newListJobsResponse pHttpStatus_ =
  ListJobsResponse'
    { nextToken = Core.Nothing,
      jobs = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Use this string to request the next batch of jobs.
listJobsResponse_nextToken :: Lens.Lens' ListJobsResponse (Core.Maybe Core.Text)
listJobsResponse_nextToken = Lens.lens (\ListJobsResponse' {nextToken} -> nextToken) (\s@ListJobsResponse' {} a -> s {nextToken = a} :: ListJobsResponse)

-- | List of jobs
listJobsResponse_jobs :: Lens.Lens' ListJobsResponse (Core.Maybe [Job])
listJobsResponse_jobs = Lens.lens (\ListJobsResponse' {jobs} -> jobs) (\s@ListJobsResponse' {} a -> s {jobs = a} :: ListJobsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listJobsResponse_httpStatus :: Lens.Lens' ListJobsResponse Core.Int
listJobsResponse_httpStatus = Lens.lens (\ListJobsResponse' {httpStatus} -> httpStatus) (\s@ListJobsResponse' {} a -> s {httpStatus = a} :: ListJobsResponse)

instance Core.NFData ListJobsResponse
