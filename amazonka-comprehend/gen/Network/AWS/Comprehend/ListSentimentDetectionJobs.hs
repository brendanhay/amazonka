{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSentimentDetectionJobs' smart constructor.
data ListSentimentDetectionJobs = ListSentimentDetectionJobs'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Filters the jobs that are returned. You can filter jobs on their name,
    -- status, or the date and time that they were submitted. You can only set
    -- one filter at a time.
    filter' :: Prelude.Maybe SentimentDetectionJobFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filter' = Prelude.Nothing
    }

-- | Identifies the next page of results to return.
listSentimentDetectionJobs_nextToken :: Lens.Lens' ListSentimentDetectionJobs (Prelude.Maybe Prelude.Text)
listSentimentDetectionJobs_nextToken = Lens.lens (\ListSentimentDetectionJobs' {nextToken} -> nextToken) (\s@ListSentimentDetectionJobs' {} a -> s {nextToken = a} :: ListSentimentDetectionJobs)

-- | The maximum number of results to return in each page. The default is
-- 100.
listSentimentDetectionJobs_maxResults :: Lens.Lens' ListSentimentDetectionJobs (Prelude.Maybe Prelude.Natural)
listSentimentDetectionJobs_maxResults = Lens.lens (\ListSentimentDetectionJobs' {maxResults} -> maxResults) (\s@ListSentimentDetectionJobs' {} a -> s {maxResults = a} :: ListSentimentDetectionJobs)

-- | Filters the jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
listSentimentDetectionJobs_filter :: Lens.Lens' ListSentimentDetectionJobs (Prelude.Maybe SentimentDetectionJobFilter)
listSentimentDetectionJobs_filter = Lens.lens (\ListSentimentDetectionJobs' {filter'} -> filter') (\s@ListSentimentDetectionJobs' {} a -> s {filter' = a} :: ListSentimentDetectionJobs)

instance Pager.AWSPager ListSentimentDetectionJobs where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listSentimentDetectionJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listSentimentDetectionJobsResponse_sentimentDetectionJobPropertiesList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listSentimentDetectionJobs_nextToken
          Lens..~ rs
          Lens.^? listSentimentDetectionJobsResponse_nextToken
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    ListSentimentDetectionJobs
  where
  type
    Rs ListSentimentDetectionJobs =
      ListSentimentDetectionJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSentimentDetectionJobsResponse'
            Prelude.<$> ( x Prelude..?> "SentimentDetectionJobPropertiesList"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSentimentDetectionJobs

instance Prelude.NFData ListSentimentDetectionJobs

instance Prelude.ToHeaders ListSentimentDetectionJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Comprehend_20171127.ListSentimentDetectionJobs" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListSentimentDetectionJobs where
  toJSON ListSentimentDetectionJobs' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("Filter" Prelude..=) Prelude.<$> filter'
          ]
      )

instance Prelude.ToPath ListSentimentDetectionJobs where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListSentimentDetectionJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSentimentDetectionJobsResponse' smart constructor.
data ListSentimentDetectionJobsResponse = ListSentimentDetectionJobsResponse'
  { -- | A list containing the properties of each job that is returned.
    sentimentDetectionJobPropertiesList :: Prelude.Maybe [SentimentDetectionJobProperties],
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListSentimentDetectionJobsResponse
newListSentimentDetectionJobsResponse pHttpStatus_ =
  ListSentimentDetectionJobsResponse'
    { sentimentDetectionJobPropertiesList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list containing the properties of each job that is returned.
listSentimentDetectionJobsResponse_sentimentDetectionJobPropertiesList :: Lens.Lens' ListSentimentDetectionJobsResponse (Prelude.Maybe [SentimentDetectionJobProperties])
listSentimentDetectionJobsResponse_sentimentDetectionJobPropertiesList = Lens.lens (\ListSentimentDetectionJobsResponse' {sentimentDetectionJobPropertiesList} -> sentimentDetectionJobPropertiesList) (\s@ListSentimentDetectionJobsResponse' {} a -> s {sentimentDetectionJobPropertiesList = a} :: ListSentimentDetectionJobsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | Identifies the next page of results to return.
listSentimentDetectionJobsResponse_nextToken :: Lens.Lens' ListSentimentDetectionJobsResponse (Prelude.Maybe Prelude.Text)
listSentimentDetectionJobsResponse_nextToken = Lens.lens (\ListSentimentDetectionJobsResponse' {nextToken} -> nextToken) (\s@ListSentimentDetectionJobsResponse' {} a -> s {nextToken = a} :: ListSentimentDetectionJobsResponse)

-- | The response's http status code.
listSentimentDetectionJobsResponse_httpStatus :: Lens.Lens' ListSentimentDetectionJobsResponse Prelude.Int
listSentimentDetectionJobsResponse_httpStatus = Lens.lens (\ListSentimentDetectionJobsResponse' {httpStatus} -> httpStatus) (\s@ListSentimentDetectionJobsResponse' {} a -> s {httpStatus = a} :: ListSentimentDetectionJobsResponse)

instance
  Prelude.NFData
    ListSentimentDetectionJobsResponse
