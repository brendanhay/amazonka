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
-- Module      : Network.AWS.Comprehend.ListSentimentDetectionJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of sentiment detection jobs that you have submitted.
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListSentimentDetectionJobs
  ( -- * Creating a Request
    ListSentimentDetectionJobs (..),
    newListSentimentDetectionJobs,

    -- * Request Lenses
    listSentimentDetectionJobs_nextToken,
    listSentimentDetectionJobs_maxResults,
    listSentimentDetectionJobs_filter,

    -- * Destructuring the Response
    ListSentimentDetectionJobsResponse (..),
    newListSentimentDetectionJobsResponse,

    -- * Response Lenses
    listSentimentDetectionJobsResponse_sentimentDetectionJobPropertiesList,
    listSentimentDetectionJobsResponse_nextToken,
    listSentimentDetectionJobsResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSentimentDetectionJobs' smart constructor.
data ListSentimentDetectionJobs = ListSentimentDetectionJobs'
  { -- | Identifies the next page of results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | Filters the jobs that are returned. You can filter jobs on their name,
    -- status, or the date and time that they were submitted. You can only set
    -- one filter at a time.
    filter' :: Core.Maybe SentimentDetectionJobFilter
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSentimentDetectionJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSentimentDetectionJobs_nextToken' - Identifies the next page of results to return.
--
-- 'maxResults', 'listSentimentDetectionJobs_maxResults' - The maximum number of results to return in each page. The default is
-- 100.
--
-- 'filter'', 'listSentimentDetectionJobs_filter' - Filters the jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
newListSentimentDetectionJobs ::
  ListSentimentDetectionJobs
newListSentimentDetectionJobs =
  ListSentimentDetectionJobs'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      filter' = Core.Nothing
    }

-- | Identifies the next page of results to return.
listSentimentDetectionJobs_nextToken :: Lens.Lens' ListSentimentDetectionJobs (Core.Maybe Core.Text)
listSentimentDetectionJobs_nextToken = Lens.lens (\ListSentimentDetectionJobs' {nextToken} -> nextToken) (\s@ListSentimentDetectionJobs' {} a -> s {nextToken = a} :: ListSentimentDetectionJobs)

-- | The maximum number of results to return in each page. The default is
-- 100.
listSentimentDetectionJobs_maxResults :: Lens.Lens' ListSentimentDetectionJobs (Core.Maybe Core.Natural)
listSentimentDetectionJobs_maxResults = Lens.lens (\ListSentimentDetectionJobs' {maxResults} -> maxResults) (\s@ListSentimentDetectionJobs' {} a -> s {maxResults = a} :: ListSentimentDetectionJobs)

-- | Filters the jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
listSentimentDetectionJobs_filter :: Lens.Lens' ListSentimentDetectionJobs (Core.Maybe SentimentDetectionJobFilter)
listSentimentDetectionJobs_filter = Lens.lens (\ListSentimentDetectionJobs' {filter'} -> filter') (\s@ListSentimentDetectionJobs' {} a -> s {filter' = a} :: ListSentimentDetectionJobs)

instance Core.AWSPager ListSentimentDetectionJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSentimentDetectionJobsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listSentimentDetectionJobsResponse_sentimentDetectionJobPropertiesList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listSentimentDetectionJobs_nextToken
          Lens..~ rs
          Lens.^? listSentimentDetectionJobsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListSentimentDetectionJobs where
  type
    AWSResponse ListSentimentDetectionJobs =
      ListSentimentDetectionJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSentimentDetectionJobsResponse'
            Core.<$> ( x Core..?> "SentimentDetectionJobPropertiesList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListSentimentDetectionJobs

instance Core.NFData ListSentimentDetectionJobs

instance Core.ToHeaders ListSentimentDetectionJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.ListSentimentDetectionJobs" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListSentimentDetectionJobs where
  toJSON ListSentimentDetectionJobs' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filter" Core..=) Core.<$> filter'
          ]
      )

instance Core.ToPath ListSentimentDetectionJobs where
  toPath = Core.const "/"

instance Core.ToQuery ListSentimentDetectionJobs where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListSentimentDetectionJobsResponse' smart constructor.
data ListSentimentDetectionJobsResponse = ListSentimentDetectionJobsResponse'
  { -- | A list containing the properties of each job that is returned.
    sentimentDetectionJobPropertiesList :: Core.Maybe [SentimentDetectionJobProperties],
    -- | Identifies the next page of results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSentimentDetectionJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sentimentDetectionJobPropertiesList', 'listSentimentDetectionJobsResponse_sentimentDetectionJobPropertiesList' - A list containing the properties of each job that is returned.
--
-- 'nextToken', 'listSentimentDetectionJobsResponse_nextToken' - Identifies the next page of results to return.
--
-- 'httpStatus', 'listSentimentDetectionJobsResponse_httpStatus' - The response's http status code.
newListSentimentDetectionJobsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListSentimentDetectionJobsResponse
newListSentimentDetectionJobsResponse pHttpStatus_ =
  ListSentimentDetectionJobsResponse'
    { sentimentDetectionJobPropertiesList =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list containing the properties of each job that is returned.
listSentimentDetectionJobsResponse_sentimentDetectionJobPropertiesList :: Lens.Lens' ListSentimentDetectionJobsResponse (Core.Maybe [SentimentDetectionJobProperties])
listSentimentDetectionJobsResponse_sentimentDetectionJobPropertiesList = Lens.lens (\ListSentimentDetectionJobsResponse' {sentimentDetectionJobPropertiesList} -> sentimentDetectionJobPropertiesList) (\s@ListSentimentDetectionJobsResponse' {} a -> s {sentimentDetectionJobPropertiesList = a} :: ListSentimentDetectionJobsResponse) Core.. Lens.mapping Lens._Coerce

-- | Identifies the next page of results to return.
listSentimentDetectionJobsResponse_nextToken :: Lens.Lens' ListSentimentDetectionJobsResponse (Core.Maybe Core.Text)
listSentimentDetectionJobsResponse_nextToken = Lens.lens (\ListSentimentDetectionJobsResponse' {nextToken} -> nextToken) (\s@ListSentimentDetectionJobsResponse' {} a -> s {nextToken = a} :: ListSentimentDetectionJobsResponse)

-- | The response's http status code.
listSentimentDetectionJobsResponse_httpStatus :: Lens.Lens' ListSentimentDetectionJobsResponse Core.Int
listSentimentDetectionJobsResponse_httpStatus = Lens.lens (\ListSentimentDetectionJobsResponse' {httpStatus} -> httpStatus) (\s@ListSentimentDetectionJobsResponse' {} a -> s {httpStatus = a} :: ListSentimentDetectionJobsResponse)

instance
  Core.NFData
    ListSentimentDetectionJobsResponse
