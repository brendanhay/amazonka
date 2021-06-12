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
-- Module      : Network.AWS.Comprehend.ListDominantLanguageDetectionJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the dominant language detection jobs that you have
-- submitted.
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListDominantLanguageDetectionJobs
  ( -- * Creating a Request
    ListDominantLanguageDetectionJobs (..),
    newListDominantLanguageDetectionJobs,

    -- * Request Lenses
    listDominantLanguageDetectionJobs_nextToken,
    listDominantLanguageDetectionJobs_maxResults,
    listDominantLanguageDetectionJobs_filter,

    -- * Destructuring the Response
    ListDominantLanguageDetectionJobsResponse (..),
    newListDominantLanguageDetectionJobsResponse,

    -- * Response Lenses
    listDominantLanguageDetectionJobsResponse_nextToken,
    listDominantLanguageDetectionJobsResponse_dominantLanguageDetectionJobPropertiesList,
    listDominantLanguageDetectionJobsResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDominantLanguageDetectionJobs' smart constructor.
data ListDominantLanguageDetectionJobs = ListDominantLanguageDetectionJobs'
  { -- | Identifies the next page of results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | Filters that jobs that are returned. You can filter jobs on their name,
    -- status, or the date and time that they were submitted. You can only set
    -- one filter at a time.
    filter' :: Core.Maybe DominantLanguageDetectionJobFilter
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDominantLanguageDetectionJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDominantLanguageDetectionJobs_nextToken' - Identifies the next page of results to return.
--
-- 'maxResults', 'listDominantLanguageDetectionJobs_maxResults' - The maximum number of results to return in each page. The default is
-- 100.
--
-- 'filter'', 'listDominantLanguageDetectionJobs_filter' - Filters that jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
newListDominantLanguageDetectionJobs ::
  ListDominantLanguageDetectionJobs
newListDominantLanguageDetectionJobs =
  ListDominantLanguageDetectionJobs'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      filter' = Core.Nothing
    }

-- | Identifies the next page of results to return.
listDominantLanguageDetectionJobs_nextToken :: Lens.Lens' ListDominantLanguageDetectionJobs (Core.Maybe Core.Text)
listDominantLanguageDetectionJobs_nextToken = Lens.lens (\ListDominantLanguageDetectionJobs' {nextToken} -> nextToken) (\s@ListDominantLanguageDetectionJobs' {} a -> s {nextToken = a} :: ListDominantLanguageDetectionJobs)

-- | The maximum number of results to return in each page. The default is
-- 100.
listDominantLanguageDetectionJobs_maxResults :: Lens.Lens' ListDominantLanguageDetectionJobs (Core.Maybe Core.Natural)
listDominantLanguageDetectionJobs_maxResults = Lens.lens (\ListDominantLanguageDetectionJobs' {maxResults} -> maxResults) (\s@ListDominantLanguageDetectionJobs' {} a -> s {maxResults = a} :: ListDominantLanguageDetectionJobs)

-- | Filters that jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
listDominantLanguageDetectionJobs_filter :: Lens.Lens' ListDominantLanguageDetectionJobs (Core.Maybe DominantLanguageDetectionJobFilter)
listDominantLanguageDetectionJobs_filter = Lens.lens (\ListDominantLanguageDetectionJobs' {filter'} -> filter') (\s@ListDominantLanguageDetectionJobs' {} a -> s {filter' = a} :: ListDominantLanguageDetectionJobs)

instance
  Core.AWSPager
    ListDominantLanguageDetectionJobs
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDominantLanguageDetectionJobsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listDominantLanguageDetectionJobsResponse_dominantLanguageDetectionJobPropertiesList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDominantLanguageDetectionJobs_nextToken
          Lens..~ rs
          Lens.^? listDominantLanguageDetectionJobsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    ListDominantLanguageDetectionJobs
  where
  type
    AWSResponse ListDominantLanguageDetectionJobs =
      ListDominantLanguageDetectionJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDominantLanguageDetectionJobsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x
                         Core..?> "DominantLanguageDetectionJobPropertiesList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListDominantLanguageDetectionJobs

instance
  Core.NFData
    ListDominantLanguageDetectionJobs

instance
  Core.ToHeaders
    ListDominantLanguageDetectionJobs
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.ListDominantLanguageDetectionJobs" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    ListDominantLanguageDetectionJobs
  where
  toJSON ListDominantLanguageDetectionJobs' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filter" Core..=) Core.<$> filter'
          ]
      )

instance
  Core.ToPath
    ListDominantLanguageDetectionJobs
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    ListDominantLanguageDetectionJobs
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListDominantLanguageDetectionJobsResponse' smart constructor.
data ListDominantLanguageDetectionJobsResponse = ListDominantLanguageDetectionJobsResponse'
  { -- | Identifies the next page of results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | A list containing the properties of each job that is returned.
    dominantLanguageDetectionJobPropertiesList :: Core.Maybe [DominantLanguageDetectionJobProperties],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDominantLanguageDetectionJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDominantLanguageDetectionJobsResponse_nextToken' - Identifies the next page of results to return.
--
-- 'dominantLanguageDetectionJobPropertiesList', 'listDominantLanguageDetectionJobsResponse_dominantLanguageDetectionJobPropertiesList' - A list containing the properties of each job that is returned.
--
-- 'httpStatus', 'listDominantLanguageDetectionJobsResponse_httpStatus' - The response's http status code.
newListDominantLanguageDetectionJobsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDominantLanguageDetectionJobsResponse
newListDominantLanguageDetectionJobsResponse
  pHttpStatus_ =
    ListDominantLanguageDetectionJobsResponse'
      { nextToken =
          Core.Nothing,
        dominantLanguageDetectionJobPropertiesList =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Identifies the next page of results to return.
listDominantLanguageDetectionJobsResponse_nextToken :: Lens.Lens' ListDominantLanguageDetectionJobsResponse (Core.Maybe Core.Text)
listDominantLanguageDetectionJobsResponse_nextToken = Lens.lens (\ListDominantLanguageDetectionJobsResponse' {nextToken} -> nextToken) (\s@ListDominantLanguageDetectionJobsResponse' {} a -> s {nextToken = a} :: ListDominantLanguageDetectionJobsResponse)

-- | A list containing the properties of each job that is returned.
listDominantLanguageDetectionJobsResponse_dominantLanguageDetectionJobPropertiesList :: Lens.Lens' ListDominantLanguageDetectionJobsResponse (Core.Maybe [DominantLanguageDetectionJobProperties])
listDominantLanguageDetectionJobsResponse_dominantLanguageDetectionJobPropertiesList = Lens.lens (\ListDominantLanguageDetectionJobsResponse' {dominantLanguageDetectionJobPropertiesList} -> dominantLanguageDetectionJobPropertiesList) (\s@ListDominantLanguageDetectionJobsResponse' {} a -> s {dominantLanguageDetectionJobPropertiesList = a} :: ListDominantLanguageDetectionJobsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDominantLanguageDetectionJobsResponse_httpStatus :: Lens.Lens' ListDominantLanguageDetectionJobsResponse Core.Int
listDominantLanguageDetectionJobsResponse_httpStatus = Lens.lens (\ListDominantLanguageDetectionJobsResponse' {httpStatus} -> httpStatus) (\s@ListDominantLanguageDetectionJobsResponse' {} a -> s {httpStatus = a} :: ListDominantLanguageDetectionJobsResponse)

instance
  Core.NFData
    ListDominantLanguageDetectionJobsResponse
