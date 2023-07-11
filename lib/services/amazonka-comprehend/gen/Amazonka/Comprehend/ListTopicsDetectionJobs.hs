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
-- Module      : Amazonka.Comprehend.ListTopicsDetectionJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the topic detection jobs that you have submitted.
--
-- This operation returns paginated results.
module Amazonka.Comprehend.ListTopicsDetectionJobs
  ( -- * Creating a Request
    ListTopicsDetectionJobs (..),
    newListTopicsDetectionJobs,

    -- * Request Lenses
    listTopicsDetectionJobs_filter,
    listTopicsDetectionJobs_maxResults,
    listTopicsDetectionJobs_nextToken,

    -- * Destructuring the Response
    ListTopicsDetectionJobsResponse (..),
    newListTopicsDetectionJobsResponse,

    -- * Response Lenses
    listTopicsDetectionJobsResponse_nextToken,
    listTopicsDetectionJobsResponse_topicsDetectionJobPropertiesList,
    listTopicsDetectionJobsResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTopicsDetectionJobs' smart constructor.
data ListTopicsDetectionJobs = ListTopicsDetectionJobs'
  { -- | Filters the jobs that are returned. Jobs can be filtered on their name,
    -- status, or the date and time that they were submitted. You can set only
    -- one filter at a time.
    filter' :: Prelude.Maybe TopicsDetectionJobFilter,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTopicsDetectionJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listTopicsDetectionJobs_filter' - Filters the jobs that are returned. Jobs can be filtered on their name,
-- status, or the date and time that they were submitted. You can set only
-- one filter at a time.
--
-- 'maxResults', 'listTopicsDetectionJobs_maxResults' - The maximum number of results to return in each page. The default is
-- 100.
--
-- 'nextToken', 'listTopicsDetectionJobs_nextToken' - Identifies the next page of results to return.
newListTopicsDetectionJobs ::
  ListTopicsDetectionJobs
newListTopicsDetectionJobs =
  ListTopicsDetectionJobs'
    { filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filters the jobs that are returned. Jobs can be filtered on their name,
-- status, or the date and time that they were submitted. You can set only
-- one filter at a time.
listTopicsDetectionJobs_filter :: Lens.Lens' ListTopicsDetectionJobs (Prelude.Maybe TopicsDetectionJobFilter)
listTopicsDetectionJobs_filter = Lens.lens (\ListTopicsDetectionJobs' {filter'} -> filter') (\s@ListTopicsDetectionJobs' {} a -> s {filter' = a} :: ListTopicsDetectionJobs)

-- | The maximum number of results to return in each page. The default is
-- 100.
listTopicsDetectionJobs_maxResults :: Lens.Lens' ListTopicsDetectionJobs (Prelude.Maybe Prelude.Natural)
listTopicsDetectionJobs_maxResults = Lens.lens (\ListTopicsDetectionJobs' {maxResults} -> maxResults) (\s@ListTopicsDetectionJobs' {} a -> s {maxResults = a} :: ListTopicsDetectionJobs)

-- | Identifies the next page of results to return.
listTopicsDetectionJobs_nextToken :: Lens.Lens' ListTopicsDetectionJobs (Prelude.Maybe Prelude.Text)
listTopicsDetectionJobs_nextToken = Lens.lens (\ListTopicsDetectionJobs' {nextToken} -> nextToken) (\s@ListTopicsDetectionJobs' {} a -> s {nextToken = a} :: ListTopicsDetectionJobs)

instance Core.AWSPager ListTopicsDetectionJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTopicsDetectionJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTopicsDetectionJobsResponse_topicsDetectionJobPropertiesList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listTopicsDetectionJobs_nextToken
          Lens..~ rs
          Lens.^? listTopicsDetectionJobsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListTopicsDetectionJobs where
  type
    AWSResponse ListTopicsDetectionJobs =
      ListTopicsDetectionJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTopicsDetectionJobsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "TopicsDetectionJobPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTopicsDetectionJobs where
  hashWithSalt _salt ListTopicsDetectionJobs' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListTopicsDetectionJobs where
  rnf ListTopicsDetectionJobs' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListTopicsDetectionJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.ListTopicsDetectionJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTopicsDetectionJobs where
  toJSON ListTopicsDetectionJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListTopicsDetectionJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTopicsDetectionJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTopicsDetectionJobsResponse' smart constructor.
data ListTopicsDetectionJobsResponse = ListTopicsDetectionJobsResponse'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list containing the properties of each job that is returned.
    topicsDetectionJobPropertiesList :: Prelude.Maybe [TopicsDetectionJobProperties],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListTopicsDetectionJobsResponse
newListTopicsDetectionJobsResponse pHttpStatus_ =
  ListTopicsDetectionJobsResponse'
    { nextToken =
        Prelude.Nothing,
      topicsDetectionJobPropertiesList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifies the next page of results to return.
listTopicsDetectionJobsResponse_nextToken :: Lens.Lens' ListTopicsDetectionJobsResponse (Prelude.Maybe Prelude.Text)
listTopicsDetectionJobsResponse_nextToken = Lens.lens (\ListTopicsDetectionJobsResponse' {nextToken} -> nextToken) (\s@ListTopicsDetectionJobsResponse' {} a -> s {nextToken = a} :: ListTopicsDetectionJobsResponse)

-- | A list containing the properties of each job that is returned.
listTopicsDetectionJobsResponse_topicsDetectionJobPropertiesList :: Lens.Lens' ListTopicsDetectionJobsResponse (Prelude.Maybe [TopicsDetectionJobProperties])
listTopicsDetectionJobsResponse_topicsDetectionJobPropertiesList = Lens.lens (\ListTopicsDetectionJobsResponse' {topicsDetectionJobPropertiesList} -> topicsDetectionJobPropertiesList) (\s@ListTopicsDetectionJobsResponse' {} a -> s {topicsDetectionJobPropertiesList = a} :: ListTopicsDetectionJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTopicsDetectionJobsResponse_httpStatus :: Lens.Lens' ListTopicsDetectionJobsResponse Prelude.Int
listTopicsDetectionJobsResponse_httpStatus = Lens.lens (\ListTopicsDetectionJobsResponse' {httpStatus} -> httpStatus) (\s@ListTopicsDetectionJobsResponse' {} a -> s {httpStatus = a} :: ListTopicsDetectionJobsResponse)

instance
  Prelude.NFData
    ListTopicsDetectionJobsResponse
  where
  rnf ListTopicsDetectionJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf topicsDetectionJobPropertiesList
      `Prelude.seq` Prelude.rnf httpStatus
