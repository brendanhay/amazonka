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
-- Module      : Network.AWS.Comprehend.ListKeyPhrasesDetectionJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a list of key phrase detection jobs that you have submitted.
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListKeyPhrasesDetectionJobs
  ( -- * Creating a Request
    ListKeyPhrasesDetectionJobs (..),
    newListKeyPhrasesDetectionJobs,

    -- * Request Lenses
    listKeyPhrasesDetectionJobs_nextToken,
    listKeyPhrasesDetectionJobs_maxResults,
    listKeyPhrasesDetectionJobs_filter,

    -- * Destructuring the Response
    ListKeyPhrasesDetectionJobsResponse (..),
    newListKeyPhrasesDetectionJobsResponse,

    -- * Response Lenses
    listKeyPhrasesDetectionJobsResponse_nextToken,
    listKeyPhrasesDetectionJobsResponse_keyPhrasesDetectionJobPropertiesList,
    listKeyPhrasesDetectionJobsResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListKeyPhrasesDetectionJobs' smart constructor.
data ListKeyPhrasesDetectionJobs = ListKeyPhrasesDetectionJobs'
  { -- | Identifies the next page of results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | Filters the jobs that are returned. You can filter jobs on their name,
    -- status, or the date and time that they were submitted. You can only set
    -- one filter at a time.
    filter' :: Core.Maybe KeyPhrasesDetectionJobFilter
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListKeyPhrasesDetectionJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listKeyPhrasesDetectionJobs_nextToken' - Identifies the next page of results to return.
--
-- 'maxResults', 'listKeyPhrasesDetectionJobs_maxResults' - The maximum number of results to return in each page. The default is
-- 100.
--
-- 'filter'', 'listKeyPhrasesDetectionJobs_filter' - Filters the jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
newListKeyPhrasesDetectionJobs ::
  ListKeyPhrasesDetectionJobs
newListKeyPhrasesDetectionJobs =
  ListKeyPhrasesDetectionJobs'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      filter' = Core.Nothing
    }

-- | Identifies the next page of results to return.
listKeyPhrasesDetectionJobs_nextToken :: Lens.Lens' ListKeyPhrasesDetectionJobs (Core.Maybe Core.Text)
listKeyPhrasesDetectionJobs_nextToken = Lens.lens (\ListKeyPhrasesDetectionJobs' {nextToken} -> nextToken) (\s@ListKeyPhrasesDetectionJobs' {} a -> s {nextToken = a} :: ListKeyPhrasesDetectionJobs)

-- | The maximum number of results to return in each page. The default is
-- 100.
listKeyPhrasesDetectionJobs_maxResults :: Lens.Lens' ListKeyPhrasesDetectionJobs (Core.Maybe Core.Natural)
listKeyPhrasesDetectionJobs_maxResults = Lens.lens (\ListKeyPhrasesDetectionJobs' {maxResults} -> maxResults) (\s@ListKeyPhrasesDetectionJobs' {} a -> s {maxResults = a} :: ListKeyPhrasesDetectionJobs)

-- | Filters the jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
listKeyPhrasesDetectionJobs_filter :: Lens.Lens' ListKeyPhrasesDetectionJobs (Core.Maybe KeyPhrasesDetectionJobFilter)
listKeyPhrasesDetectionJobs_filter = Lens.lens (\ListKeyPhrasesDetectionJobs' {filter'} -> filter') (\s@ListKeyPhrasesDetectionJobs' {} a -> s {filter' = a} :: ListKeyPhrasesDetectionJobs)

instance Core.AWSPager ListKeyPhrasesDetectionJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listKeyPhrasesDetectionJobsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listKeyPhrasesDetectionJobsResponse_keyPhrasesDetectionJobPropertiesList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listKeyPhrasesDetectionJobs_nextToken
          Lens..~ rs
          Lens.^? listKeyPhrasesDetectionJobsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListKeyPhrasesDetectionJobs where
  type
    AWSResponse ListKeyPhrasesDetectionJobs =
      ListKeyPhrasesDetectionJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListKeyPhrasesDetectionJobsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "KeyPhrasesDetectionJobPropertiesList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListKeyPhrasesDetectionJobs

instance Core.NFData ListKeyPhrasesDetectionJobs

instance Core.ToHeaders ListKeyPhrasesDetectionJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.ListKeyPhrasesDetectionJobs" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListKeyPhrasesDetectionJobs where
  toJSON ListKeyPhrasesDetectionJobs' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filter" Core..=) Core.<$> filter'
          ]
      )

instance Core.ToPath ListKeyPhrasesDetectionJobs where
  toPath = Core.const "/"

instance Core.ToQuery ListKeyPhrasesDetectionJobs where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListKeyPhrasesDetectionJobsResponse' smart constructor.
data ListKeyPhrasesDetectionJobsResponse = ListKeyPhrasesDetectionJobsResponse'
  { -- | Identifies the next page of results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | A list containing the properties of each job that is returned.
    keyPhrasesDetectionJobPropertiesList :: Core.Maybe [KeyPhrasesDetectionJobProperties],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListKeyPhrasesDetectionJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listKeyPhrasesDetectionJobsResponse_nextToken' - Identifies the next page of results to return.
--
-- 'keyPhrasesDetectionJobPropertiesList', 'listKeyPhrasesDetectionJobsResponse_keyPhrasesDetectionJobPropertiesList' - A list containing the properties of each job that is returned.
--
-- 'httpStatus', 'listKeyPhrasesDetectionJobsResponse_httpStatus' - The response's http status code.
newListKeyPhrasesDetectionJobsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListKeyPhrasesDetectionJobsResponse
newListKeyPhrasesDetectionJobsResponse pHttpStatus_ =
  ListKeyPhrasesDetectionJobsResponse'
    { nextToken =
        Core.Nothing,
      keyPhrasesDetectionJobPropertiesList =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifies the next page of results to return.
listKeyPhrasesDetectionJobsResponse_nextToken :: Lens.Lens' ListKeyPhrasesDetectionJobsResponse (Core.Maybe Core.Text)
listKeyPhrasesDetectionJobsResponse_nextToken = Lens.lens (\ListKeyPhrasesDetectionJobsResponse' {nextToken} -> nextToken) (\s@ListKeyPhrasesDetectionJobsResponse' {} a -> s {nextToken = a} :: ListKeyPhrasesDetectionJobsResponse)

-- | A list containing the properties of each job that is returned.
listKeyPhrasesDetectionJobsResponse_keyPhrasesDetectionJobPropertiesList :: Lens.Lens' ListKeyPhrasesDetectionJobsResponse (Core.Maybe [KeyPhrasesDetectionJobProperties])
listKeyPhrasesDetectionJobsResponse_keyPhrasesDetectionJobPropertiesList = Lens.lens (\ListKeyPhrasesDetectionJobsResponse' {keyPhrasesDetectionJobPropertiesList} -> keyPhrasesDetectionJobPropertiesList) (\s@ListKeyPhrasesDetectionJobsResponse' {} a -> s {keyPhrasesDetectionJobPropertiesList = a} :: ListKeyPhrasesDetectionJobsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listKeyPhrasesDetectionJobsResponse_httpStatus :: Lens.Lens' ListKeyPhrasesDetectionJobsResponse Core.Int
listKeyPhrasesDetectionJobsResponse_httpStatus = Lens.lens (\ListKeyPhrasesDetectionJobsResponse' {httpStatus} -> httpStatus) (\s@ListKeyPhrasesDetectionJobsResponse' {} a -> s {httpStatus = a} :: ListKeyPhrasesDetectionJobsResponse)

instance
  Core.NFData
    ListKeyPhrasesDetectionJobsResponse
