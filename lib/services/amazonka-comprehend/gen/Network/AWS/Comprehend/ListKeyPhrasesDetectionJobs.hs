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
    listKeyPhrasesDetectionJobs_filter,
    listKeyPhrasesDetectionJobs_maxResults,

    -- * Destructuring the Response
    ListKeyPhrasesDetectionJobsResponse (..),
    newListKeyPhrasesDetectionJobsResponse,

    -- * Response Lenses
    listKeyPhrasesDetectionJobsResponse_keyPhrasesDetectionJobPropertiesList,
    listKeyPhrasesDetectionJobsResponse_nextToken,
    listKeyPhrasesDetectionJobsResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListKeyPhrasesDetectionJobs' smart constructor.
data ListKeyPhrasesDetectionJobs = ListKeyPhrasesDetectionJobs'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters the jobs that are returned. You can filter jobs on their name,
    -- status, or the date and time that they were submitted. You can only set
    -- one filter at a time.
    filter' :: Prelude.Maybe KeyPhrasesDetectionJobFilter,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'filter'', 'listKeyPhrasesDetectionJobs_filter' - Filters the jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
--
-- 'maxResults', 'listKeyPhrasesDetectionJobs_maxResults' - The maximum number of results to return in each page. The default is
-- 100.
newListKeyPhrasesDetectionJobs ::
  ListKeyPhrasesDetectionJobs
newListKeyPhrasesDetectionJobs =
  ListKeyPhrasesDetectionJobs'
    { nextToken =
        Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Identifies the next page of results to return.
listKeyPhrasesDetectionJobs_nextToken :: Lens.Lens' ListKeyPhrasesDetectionJobs (Prelude.Maybe Prelude.Text)
listKeyPhrasesDetectionJobs_nextToken = Lens.lens (\ListKeyPhrasesDetectionJobs' {nextToken} -> nextToken) (\s@ListKeyPhrasesDetectionJobs' {} a -> s {nextToken = a} :: ListKeyPhrasesDetectionJobs)

-- | Filters the jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
listKeyPhrasesDetectionJobs_filter :: Lens.Lens' ListKeyPhrasesDetectionJobs (Prelude.Maybe KeyPhrasesDetectionJobFilter)
listKeyPhrasesDetectionJobs_filter = Lens.lens (\ListKeyPhrasesDetectionJobs' {filter'} -> filter') (\s@ListKeyPhrasesDetectionJobs' {} a -> s {filter' = a} :: ListKeyPhrasesDetectionJobs)

-- | The maximum number of results to return in each page. The default is
-- 100.
listKeyPhrasesDetectionJobs_maxResults :: Lens.Lens' ListKeyPhrasesDetectionJobs (Prelude.Maybe Prelude.Natural)
listKeyPhrasesDetectionJobs_maxResults = Lens.lens (\ListKeyPhrasesDetectionJobs' {maxResults} -> maxResults) (\s@ListKeyPhrasesDetectionJobs' {} a -> s {maxResults = a} :: ListKeyPhrasesDetectionJobs)

instance Core.AWSPager ListKeyPhrasesDetectionJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listKeyPhrasesDetectionJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listKeyPhrasesDetectionJobsResponse_keyPhrasesDetectionJobPropertiesList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listKeyPhrasesDetectionJobs_nextToken
          Lens..~ rs
          Lens.^? listKeyPhrasesDetectionJobsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListKeyPhrasesDetectionJobs where
  type
    AWSResponse ListKeyPhrasesDetectionJobs =
      ListKeyPhrasesDetectionJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListKeyPhrasesDetectionJobsResponse'
            Prelude.<$> ( x Core..?> "KeyPhrasesDetectionJobPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListKeyPhrasesDetectionJobs

instance Prelude.NFData ListKeyPhrasesDetectionJobs

instance Core.ToHeaders ListKeyPhrasesDetectionJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.ListKeyPhrasesDetectionJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListKeyPhrasesDetectionJobs where
  toJSON ListKeyPhrasesDetectionJobs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Filter" Core..=) Prelude.<$> filter',
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListKeyPhrasesDetectionJobs where
  toPath = Prelude.const "/"

instance Core.ToQuery ListKeyPhrasesDetectionJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListKeyPhrasesDetectionJobsResponse' smart constructor.
data ListKeyPhrasesDetectionJobsResponse = ListKeyPhrasesDetectionJobsResponse'
  { -- | A list containing the properties of each job that is returned.
    keyPhrasesDetectionJobPropertiesList :: Prelude.Maybe [KeyPhrasesDetectionJobProperties],
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKeyPhrasesDetectionJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPhrasesDetectionJobPropertiesList', 'listKeyPhrasesDetectionJobsResponse_keyPhrasesDetectionJobPropertiesList' - A list containing the properties of each job that is returned.
--
-- 'nextToken', 'listKeyPhrasesDetectionJobsResponse_nextToken' - Identifies the next page of results to return.
--
-- 'httpStatus', 'listKeyPhrasesDetectionJobsResponse_httpStatus' - The response's http status code.
newListKeyPhrasesDetectionJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListKeyPhrasesDetectionJobsResponse
newListKeyPhrasesDetectionJobsResponse pHttpStatus_ =
  ListKeyPhrasesDetectionJobsResponse'
    { keyPhrasesDetectionJobPropertiesList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list containing the properties of each job that is returned.
listKeyPhrasesDetectionJobsResponse_keyPhrasesDetectionJobPropertiesList :: Lens.Lens' ListKeyPhrasesDetectionJobsResponse (Prelude.Maybe [KeyPhrasesDetectionJobProperties])
listKeyPhrasesDetectionJobsResponse_keyPhrasesDetectionJobPropertiesList = Lens.lens (\ListKeyPhrasesDetectionJobsResponse' {keyPhrasesDetectionJobPropertiesList} -> keyPhrasesDetectionJobPropertiesList) (\s@ListKeyPhrasesDetectionJobsResponse' {} a -> s {keyPhrasesDetectionJobPropertiesList = a} :: ListKeyPhrasesDetectionJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Identifies the next page of results to return.
listKeyPhrasesDetectionJobsResponse_nextToken :: Lens.Lens' ListKeyPhrasesDetectionJobsResponse (Prelude.Maybe Prelude.Text)
listKeyPhrasesDetectionJobsResponse_nextToken = Lens.lens (\ListKeyPhrasesDetectionJobsResponse' {nextToken} -> nextToken) (\s@ListKeyPhrasesDetectionJobsResponse' {} a -> s {nextToken = a} :: ListKeyPhrasesDetectionJobsResponse)

-- | The response's http status code.
listKeyPhrasesDetectionJobsResponse_httpStatus :: Lens.Lens' ListKeyPhrasesDetectionJobsResponse Prelude.Int
listKeyPhrasesDetectionJobsResponse_httpStatus = Lens.lens (\ListKeyPhrasesDetectionJobsResponse' {httpStatus} -> httpStatus) (\s@ListKeyPhrasesDetectionJobsResponse' {} a -> s {httpStatus = a} :: ListKeyPhrasesDetectionJobsResponse)

instance
  Prelude.NFData
    ListKeyPhrasesDetectionJobsResponse
