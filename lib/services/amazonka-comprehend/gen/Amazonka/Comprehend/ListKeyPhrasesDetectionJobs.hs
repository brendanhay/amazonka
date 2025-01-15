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
-- Module      : Amazonka.Comprehend.ListKeyPhrasesDetectionJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a list of key phrase detection jobs that you have submitted.
--
-- This operation returns paginated results.
module Amazonka.Comprehend.ListKeyPhrasesDetectionJobs
  ( -- * Creating a Request
    ListKeyPhrasesDetectionJobs (..),
    newListKeyPhrasesDetectionJobs,

    -- * Request Lenses
    listKeyPhrasesDetectionJobs_filter,
    listKeyPhrasesDetectionJobs_maxResults,
    listKeyPhrasesDetectionJobs_nextToken,

    -- * Destructuring the Response
    ListKeyPhrasesDetectionJobsResponse (..),
    newListKeyPhrasesDetectionJobsResponse,

    -- * Response Lenses
    listKeyPhrasesDetectionJobsResponse_keyPhrasesDetectionJobPropertiesList,
    listKeyPhrasesDetectionJobsResponse_nextToken,
    listKeyPhrasesDetectionJobsResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListKeyPhrasesDetectionJobs' smart constructor.
data ListKeyPhrasesDetectionJobs = ListKeyPhrasesDetectionJobs'
  { -- | Filters the jobs that are returned. You can filter jobs on their name,
    -- status, or the date and time that they were submitted. You can only set
    -- one filter at a time.
    filter' :: Prelude.Maybe KeyPhrasesDetectionJobFilter,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'filter'', 'listKeyPhrasesDetectionJobs_filter' - Filters the jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
--
-- 'maxResults', 'listKeyPhrasesDetectionJobs_maxResults' - The maximum number of results to return in each page. The default is
-- 100.
--
-- 'nextToken', 'listKeyPhrasesDetectionJobs_nextToken' - Identifies the next page of results to return.
newListKeyPhrasesDetectionJobs ::
  ListKeyPhrasesDetectionJobs
newListKeyPhrasesDetectionJobs =
  ListKeyPhrasesDetectionJobs'
    { filter' =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filters the jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
listKeyPhrasesDetectionJobs_filter :: Lens.Lens' ListKeyPhrasesDetectionJobs (Prelude.Maybe KeyPhrasesDetectionJobFilter)
listKeyPhrasesDetectionJobs_filter = Lens.lens (\ListKeyPhrasesDetectionJobs' {filter'} -> filter') (\s@ListKeyPhrasesDetectionJobs' {} a -> s {filter' = a} :: ListKeyPhrasesDetectionJobs)

-- | The maximum number of results to return in each page. The default is
-- 100.
listKeyPhrasesDetectionJobs_maxResults :: Lens.Lens' ListKeyPhrasesDetectionJobs (Prelude.Maybe Prelude.Natural)
listKeyPhrasesDetectionJobs_maxResults = Lens.lens (\ListKeyPhrasesDetectionJobs' {maxResults} -> maxResults) (\s@ListKeyPhrasesDetectionJobs' {} a -> s {maxResults = a} :: ListKeyPhrasesDetectionJobs)

-- | Identifies the next page of results to return.
listKeyPhrasesDetectionJobs_nextToken :: Lens.Lens' ListKeyPhrasesDetectionJobs (Prelude.Maybe Prelude.Text)
listKeyPhrasesDetectionJobs_nextToken = Lens.lens (\ListKeyPhrasesDetectionJobs' {nextToken} -> nextToken) (\s@ListKeyPhrasesDetectionJobs' {} a -> s {nextToken = a} :: ListKeyPhrasesDetectionJobs)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListKeyPhrasesDetectionJobsResponse'
            Prelude.<$> ( x
                            Data..?> "KeyPhrasesDetectionJobPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListKeyPhrasesDetectionJobs where
  hashWithSalt _salt ListKeyPhrasesDetectionJobs' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListKeyPhrasesDetectionJobs where
  rnf ListKeyPhrasesDetectionJobs' {..} =
    Prelude.rnf filter' `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders ListKeyPhrasesDetectionJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.ListKeyPhrasesDetectionJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListKeyPhrasesDetectionJobs where
  toJSON ListKeyPhrasesDetectionJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListKeyPhrasesDetectionJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListKeyPhrasesDetectionJobs where
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
  where
  rnf ListKeyPhrasesDetectionJobsResponse' {..} =
    Prelude.rnf keyPhrasesDetectionJobPropertiesList `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
