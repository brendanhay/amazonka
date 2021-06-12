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
-- Module      : Network.AWS.Comprehend.ListTopicsDetectionJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the topic detection jobs that you have submitted.
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListTopicsDetectionJobs
  ( -- * Creating a Request
    ListTopicsDetectionJobs (..),
    newListTopicsDetectionJobs,

    -- * Request Lenses
    listTopicsDetectionJobs_nextToken,
    listTopicsDetectionJobs_maxResults,
    listTopicsDetectionJobs_filter,

    -- * Destructuring the Response
    ListTopicsDetectionJobsResponse (..),
    newListTopicsDetectionJobsResponse,

    -- * Response Lenses
    listTopicsDetectionJobsResponse_nextToken,
    listTopicsDetectionJobsResponse_topicsDetectionJobPropertiesList,
    listTopicsDetectionJobsResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTopicsDetectionJobs' smart constructor.
data ListTopicsDetectionJobs = ListTopicsDetectionJobs'
  { -- | Identifies the next page of results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | Filters the jobs that are returned. Jobs can be filtered on their name,
    -- status, or the date and time that they were submitted. You can set only
    -- one filter at a time.
    filter' :: Core.Maybe TopicsDetectionJobFilter
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTopicsDetectionJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTopicsDetectionJobs_nextToken' - Identifies the next page of results to return.
--
-- 'maxResults', 'listTopicsDetectionJobs_maxResults' - The maximum number of results to return in each page. The default is
-- 100.
--
-- 'filter'', 'listTopicsDetectionJobs_filter' - Filters the jobs that are returned. Jobs can be filtered on their name,
-- status, or the date and time that they were submitted. You can set only
-- one filter at a time.
newListTopicsDetectionJobs ::
  ListTopicsDetectionJobs
newListTopicsDetectionJobs =
  ListTopicsDetectionJobs'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filter' = Core.Nothing
    }

-- | Identifies the next page of results to return.
listTopicsDetectionJobs_nextToken :: Lens.Lens' ListTopicsDetectionJobs (Core.Maybe Core.Text)
listTopicsDetectionJobs_nextToken = Lens.lens (\ListTopicsDetectionJobs' {nextToken} -> nextToken) (\s@ListTopicsDetectionJobs' {} a -> s {nextToken = a} :: ListTopicsDetectionJobs)

-- | The maximum number of results to return in each page. The default is
-- 100.
listTopicsDetectionJobs_maxResults :: Lens.Lens' ListTopicsDetectionJobs (Core.Maybe Core.Natural)
listTopicsDetectionJobs_maxResults = Lens.lens (\ListTopicsDetectionJobs' {maxResults} -> maxResults) (\s@ListTopicsDetectionJobs' {} a -> s {maxResults = a} :: ListTopicsDetectionJobs)

-- | Filters the jobs that are returned. Jobs can be filtered on their name,
-- status, or the date and time that they were submitted. You can set only
-- one filter at a time.
listTopicsDetectionJobs_filter :: Lens.Lens' ListTopicsDetectionJobs (Core.Maybe TopicsDetectionJobFilter)
listTopicsDetectionJobs_filter = Lens.lens (\ListTopicsDetectionJobs' {filter'} -> filter') (\s@ListTopicsDetectionJobs' {} a -> s {filter' = a} :: ListTopicsDetectionJobs)

instance Core.AWSPager ListTopicsDetectionJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTopicsDetectionJobsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listTopicsDetectionJobsResponse_topicsDetectionJobPropertiesList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listTopicsDetectionJobs_nextToken
          Lens..~ rs
          Lens.^? listTopicsDetectionJobsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListTopicsDetectionJobs where
  type
    AWSResponse ListTopicsDetectionJobs =
      ListTopicsDetectionJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTopicsDetectionJobsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "TopicsDetectionJobPropertiesList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTopicsDetectionJobs

instance Core.NFData ListTopicsDetectionJobs

instance Core.ToHeaders ListTopicsDetectionJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.ListTopicsDetectionJobs" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListTopicsDetectionJobs where
  toJSON ListTopicsDetectionJobs' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filter" Core..=) Core.<$> filter'
          ]
      )

instance Core.ToPath ListTopicsDetectionJobs where
  toPath = Core.const "/"

instance Core.ToQuery ListTopicsDetectionJobs where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListTopicsDetectionJobsResponse' smart constructor.
data ListTopicsDetectionJobsResponse = ListTopicsDetectionJobsResponse'
  { -- | Identifies the next page of results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | A list containing the properties of each job that is returned.
    topicsDetectionJobPropertiesList :: Core.Maybe [TopicsDetectionJobProperties],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTopicsDetectionJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTopicsDetectionJobsResponse_nextToken' - Identifies the next page of results to return.
--
-- 'topicsDetectionJobPropertiesList', 'listTopicsDetectionJobsResponse_topicsDetectionJobPropertiesList' - A list containing the properties of each job that is returned.
--
-- 'httpStatus', 'listTopicsDetectionJobsResponse_httpStatus' - The response's http status code.
newListTopicsDetectionJobsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTopicsDetectionJobsResponse
newListTopicsDetectionJobsResponse pHttpStatus_ =
  ListTopicsDetectionJobsResponse'
    { nextToken =
        Core.Nothing,
      topicsDetectionJobPropertiesList =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifies the next page of results to return.
listTopicsDetectionJobsResponse_nextToken :: Lens.Lens' ListTopicsDetectionJobsResponse (Core.Maybe Core.Text)
listTopicsDetectionJobsResponse_nextToken = Lens.lens (\ListTopicsDetectionJobsResponse' {nextToken} -> nextToken) (\s@ListTopicsDetectionJobsResponse' {} a -> s {nextToken = a} :: ListTopicsDetectionJobsResponse)

-- | A list containing the properties of each job that is returned.
listTopicsDetectionJobsResponse_topicsDetectionJobPropertiesList :: Lens.Lens' ListTopicsDetectionJobsResponse (Core.Maybe [TopicsDetectionJobProperties])
listTopicsDetectionJobsResponse_topicsDetectionJobPropertiesList = Lens.lens (\ListTopicsDetectionJobsResponse' {topicsDetectionJobPropertiesList} -> topicsDetectionJobPropertiesList) (\s@ListTopicsDetectionJobsResponse' {} a -> s {topicsDetectionJobPropertiesList = a} :: ListTopicsDetectionJobsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTopicsDetectionJobsResponse_httpStatus :: Lens.Lens' ListTopicsDetectionJobsResponse Core.Int
listTopicsDetectionJobsResponse_httpStatus = Lens.lens (\ListTopicsDetectionJobsResponse' {httpStatus} -> httpStatus) (\s@ListTopicsDetectionJobsResponse' {} a -> s {httpStatus = a} :: ListTopicsDetectionJobsResponse)

instance Core.NFData ListTopicsDetectionJobsResponse
