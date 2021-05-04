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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListKeyPhrasesDetectionJobs' smart constructor.
data ListKeyPhrasesDetectionJobs = ListKeyPhrasesDetectionJobs'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Filters the jobs that are returned. You can filter jobs on their name,
    -- status, or the date and time that they were submitted. You can only set
    -- one filter at a time.
    filter' :: Prelude.Maybe KeyPhrasesDetectionJobFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filter' = Prelude.Nothing
    }

-- | Identifies the next page of results to return.
listKeyPhrasesDetectionJobs_nextToken :: Lens.Lens' ListKeyPhrasesDetectionJobs (Prelude.Maybe Prelude.Text)
listKeyPhrasesDetectionJobs_nextToken = Lens.lens (\ListKeyPhrasesDetectionJobs' {nextToken} -> nextToken) (\s@ListKeyPhrasesDetectionJobs' {} a -> s {nextToken = a} :: ListKeyPhrasesDetectionJobs)

-- | The maximum number of results to return in each page. The default is
-- 100.
listKeyPhrasesDetectionJobs_maxResults :: Lens.Lens' ListKeyPhrasesDetectionJobs (Prelude.Maybe Prelude.Natural)
listKeyPhrasesDetectionJobs_maxResults = Lens.lens (\ListKeyPhrasesDetectionJobs' {maxResults} -> maxResults) (\s@ListKeyPhrasesDetectionJobs' {} a -> s {maxResults = a} :: ListKeyPhrasesDetectionJobs)

-- | Filters the jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
listKeyPhrasesDetectionJobs_filter :: Lens.Lens' ListKeyPhrasesDetectionJobs (Prelude.Maybe KeyPhrasesDetectionJobFilter)
listKeyPhrasesDetectionJobs_filter = Lens.lens (\ListKeyPhrasesDetectionJobs' {filter'} -> filter') (\s@ListKeyPhrasesDetectionJobs' {} a -> s {filter' = a} :: ListKeyPhrasesDetectionJobs)

instance Pager.AWSPager ListKeyPhrasesDetectionJobs where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listKeyPhrasesDetectionJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listKeyPhrasesDetectionJobsResponse_keyPhrasesDetectionJobPropertiesList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listKeyPhrasesDetectionJobs_nextToken
          Lens..~ rs
          Lens.^? listKeyPhrasesDetectionJobsResponse_nextToken
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    ListKeyPhrasesDetectionJobs
  where
  type
    Rs ListKeyPhrasesDetectionJobs =
      ListKeyPhrasesDetectionJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListKeyPhrasesDetectionJobsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "KeyPhrasesDetectionJobPropertiesList"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListKeyPhrasesDetectionJobs

instance Prelude.NFData ListKeyPhrasesDetectionJobs

instance
  Prelude.ToHeaders
    ListKeyPhrasesDetectionJobs
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Comprehend_20171127.ListKeyPhrasesDetectionJobs" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListKeyPhrasesDetectionJobs where
  toJSON ListKeyPhrasesDetectionJobs' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("Filter" Prelude..=) Prelude.<$> filter'
          ]
      )

instance Prelude.ToPath ListKeyPhrasesDetectionJobs where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListKeyPhrasesDetectionJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListKeyPhrasesDetectionJobsResponse' smart constructor.
data ListKeyPhrasesDetectionJobsResponse = ListKeyPhrasesDetectionJobsResponse'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list containing the properties of each job that is returned.
    keyPhrasesDetectionJobPropertiesList :: Prelude.Maybe [KeyPhrasesDetectionJobProperties],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListKeyPhrasesDetectionJobsResponse
newListKeyPhrasesDetectionJobsResponse pHttpStatus_ =
  ListKeyPhrasesDetectionJobsResponse'
    { nextToken =
        Prelude.Nothing,
      keyPhrasesDetectionJobPropertiesList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifies the next page of results to return.
listKeyPhrasesDetectionJobsResponse_nextToken :: Lens.Lens' ListKeyPhrasesDetectionJobsResponse (Prelude.Maybe Prelude.Text)
listKeyPhrasesDetectionJobsResponse_nextToken = Lens.lens (\ListKeyPhrasesDetectionJobsResponse' {nextToken} -> nextToken) (\s@ListKeyPhrasesDetectionJobsResponse' {} a -> s {nextToken = a} :: ListKeyPhrasesDetectionJobsResponse)

-- | A list containing the properties of each job that is returned.
listKeyPhrasesDetectionJobsResponse_keyPhrasesDetectionJobPropertiesList :: Lens.Lens' ListKeyPhrasesDetectionJobsResponse (Prelude.Maybe [KeyPhrasesDetectionJobProperties])
listKeyPhrasesDetectionJobsResponse_keyPhrasesDetectionJobPropertiesList = Lens.lens (\ListKeyPhrasesDetectionJobsResponse' {keyPhrasesDetectionJobPropertiesList} -> keyPhrasesDetectionJobPropertiesList) (\s@ListKeyPhrasesDetectionJobsResponse' {} a -> s {keyPhrasesDetectionJobPropertiesList = a} :: ListKeyPhrasesDetectionJobsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listKeyPhrasesDetectionJobsResponse_httpStatus :: Lens.Lens' ListKeyPhrasesDetectionJobsResponse Prelude.Int
listKeyPhrasesDetectionJobsResponse_httpStatus = Lens.lens (\ListKeyPhrasesDetectionJobsResponse' {httpStatus} -> httpStatus) (\s@ListKeyPhrasesDetectionJobsResponse' {} a -> s {httpStatus = a} :: ListKeyPhrasesDetectionJobsResponse)

instance
  Prelude.NFData
    ListKeyPhrasesDetectionJobsResponse
