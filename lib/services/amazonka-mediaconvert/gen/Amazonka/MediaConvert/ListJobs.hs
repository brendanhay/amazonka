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
-- Module      : Amazonka.MediaConvert.ListJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.MediaConvert.ListJobs
  ( -- * Creating a Request
    ListJobs (..),
    newListJobs,

    -- * Request Lenses
    listJobs_maxResults,
    listJobs_nextToken,
    listJobs_order,
    listJobs_queue,
    listJobs_status,

    -- * Destructuring the Response
    ListJobsResponse (..),
    newListJobsResponse,

    -- * Response Lenses
    listJobsResponse_jobs,
    listJobsResponse_nextToken,
    listJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListJobs' smart constructor.
data ListJobs = ListJobs'
  { -- | Optional. Number of jobs, up to twenty, that will be returned at one
    -- time.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Optional. Use this string, provided with the response to a previous
    -- request, to request the next batch of jobs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Optional. When you request lists of resources, you can specify whether
    -- they are sorted in ASCENDING or DESCENDING order. Default varies by
    -- resource.
    order :: Prelude.Maybe Order,
    -- | Optional. Provide a queue name to get back only jobs from that queue.
    queue :: Prelude.Maybe Prelude.Text,
    -- | Optional. A job\'s status can be SUBMITTED, PROGRESSING, COMPLETE,
    -- CANCELED, or ERROR.
    status :: Prelude.Maybe JobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listJobs_maxResults' - Optional. Number of jobs, up to twenty, that will be returned at one
-- time.
--
-- 'nextToken', 'listJobs_nextToken' - Optional. Use this string, provided with the response to a previous
-- request, to request the next batch of jobs.
--
-- 'order', 'listJobs_order' - Optional. When you request lists of resources, you can specify whether
-- they are sorted in ASCENDING or DESCENDING order. Default varies by
-- resource.
--
-- 'queue', 'listJobs_queue' - Optional. Provide a queue name to get back only jobs from that queue.
--
-- 'status', 'listJobs_status' - Optional. A job\'s status can be SUBMITTED, PROGRESSING, COMPLETE,
-- CANCELED, or ERROR.
newListJobs ::
  ListJobs
newListJobs =
  ListJobs'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      order = Prelude.Nothing,
      queue = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Optional. Number of jobs, up to twenty, that will be returned at one
-- time.
listJobs_maxResults :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Natural)
listJobs_maxResults = Lens.lens (\ListJobs' {maxResults} -> maxResults) (\s@ListJobs' {} a -> s {maxResults = a} :: ListJobs)

-- | Optional. Use this string, provided with the response to a previous
-- request, to request the next batch of jobs.
listJobs_nextToken :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_nextToken = Lens.lens (\ListJobs' {nextToken} -> nextToken) (\s@ListJobs' {} a -> s {nextToken = a} :: ListJobs)

-- | Optional. When you request lists of resources, you can specify whether
-- they are sorted in ASCENDING or DESCENDING order. Default varies by
-- resource.
listJobs_order :: Lens.Lens' ListJobs (Prelude.Maybe Order)
listJobs_order = Lens.lens (\ListJobs' {order} -> order) (\s@ListJobs' {} a -> s {order = a} :: ListJobs)

-- | Optional. Provide a queue name to get back only jobs from that queue.
listJobs_queue :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_queue = Lens.lens (\ListJobs' {queue} -> queue) (\s@ListJobs' {} a -> s {queue = a} :: ListJobs)

-- | Optional. A job\'s status can be SUBMITTED, PROGRESSING, COMPLETE,
-- CANCELED, or ERROR.
listJobs_status :: Lens.Lens' ListJobs (Prelude.Maybe JobStatus)
listJobs_status = Lens.lens (\ListJobs' {status} -> status) (\s@ListJobs' {} a -> s {status = a} :: ListJobs)

instance Core.AWSPager ListJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listJobsResponse_jobs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listJobs_nextToken
          Lens..~ rs
          Lens.^? listJobsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListJobs where
  type AWSResponse ListJobs = ListJobsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobsResponse'
            Prelude.<$> (x Data..?> "jobs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListJobs where
  hashWithSalt _salt ListJobs' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` order
      `Prelude.hashWithSalt` queue
      `Prelude.hashWithSalt` status

instance Prelude.NFData ListJobs where
  rnf ListJobs' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf order
      `Prelude.seq` Prelude.rnf queue
      `Prelude.seq` Prelude.rnf status

instance Data.ToHeaders ListJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListJobs where
  toPath = Prelude.const "/2017-08-29/jobs"

instance Data.ToQuery ListJobs where
  toQuery ListJobs' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "order" Data.=: order,
        "queue" Data.=: queue,
        "status" Data.=: status
      ]

-- | /See:/ 'newListJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { -- | List of jobs
    jobs :: Prelude.Maybe [Job],
    -- | Use this string to request the next batch of jobs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobs', 'listJobsResponse_jobs' - List of jobs
--
-- 'nextToken', 'listJobsResponse_nextToken' - Use this string to request the next batch of jobs.
--
-- 'httpStatus', 'listJobsResponse_httpStatus' - The response's http status code.
newListJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListJobsResponse
newListJobsResponse pHttpStatus_ =
  ListJobsResponse'
    { jobs = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of jobs
listJobsResponse_jobs :: Lens.Lens' ListJobsResponse (Prelude.Maybe [Job])
listJobsResponse_jobs = Lens.lens (\ListJobsResponse' {jobs} -> jobs) (\s@ListJobsResponse' {} a -> s {jobs = a} :: ListJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Use this string to request the next batch of jobs.
listJobsResponse_nextToken :: Lens.Lens' ListJobsResponse (Prelude.Maybe Prelude.Text)
listJobsResponse_nextToken = Lens.lens (\ListJobsResponse' {nextToken} -> nextToken) (\s@ListJobsResponse' {} a -> s {nextToken = a} :: ListJobsResponse)

-- | The response's http status code.
listJobsResponse_httpStatus :: Lens.Lens' ListJobsResponse Prelude.Int
listJobsResponse_httpStatus = Lens.lens (\ListJobsResponse' {httpStatus} -> httpStatus) (\s@ListJobsResponse' {} a -> s {httpStatus = a} :: ListJobsResponse)

instance Prelude.NFData ListJobsResponse where
  rnf ListJobsResponse' {..} =
    Prelude.rnf jobs
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
