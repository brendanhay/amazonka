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
-- Module      : Amazonka.Comprehend.ListDominantLanguageDetectionJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the dominant language detection jobs that you have
-- submitted.
--
-- This operation returns paginated results.
module Amazonka.Comprehend.ListDominantLanguageDetectionJobs
  ( -- * Creating a Request
    ListDominantLanguageDetectionJobs (..),
    newListDominantLanguageDetectionJobs,

    -- * Request Lenses
    listDominantLanguageDetectionJobs_nextToken,
    listDominantLanguageDetectionJobs_filter,
    listDominantLanguageDetectionJobs_maxResults,

    -- * Destructuring the Response
    ListDominantLanguageDetectionJobsResponse (..),
    newListDominantLanguageDetectionJobsResponse,

    -- * Response Lenses
    listDominantLanguageDetectionJobsResponse_nextToken,
    listDominantLanguageDetectionJobsResponse_dominantLanguageDetectionJobPropertiesList,
    listDominantLanguageDetectionJobsResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDominantLanguageDetectionJobs' smart constructor.
data ListDominantLanguageDetectionJobs = ListDominantLanguageDetectionJobs'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters that jobs that are returned. You can filter jobs on their name,
    -- status, or the date and time that they were submitted. You can only set
    -- one filter at a time.
    filter' :: Prelude.Maybe DominantLanguageDetectionJobFilter,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'filter'', 'listDominantLanguageDetectionJobs_filter' - Filters that jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
--
-- 'maxResults', 'listDominantLanguageDetectionJobs_maxResults' - The maximum number of results to return in each page. The default is
-- 100.
newListDominantLanguageDetectionJobs ::
  ListDominantLanguageDetectionJobs
newListDominantLanguageDetectionJobs =
  ListDominantLanguageDetectionJobs'
    { nextToken =
        Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Identifies the next page of results to return.
listDominantLanguageDetectionJobs_nextToken :: Lens.Lens' ListDominantLanguageDetectionJobs (Prelude.Maybe Prelude.Text)
listDominantLanguageDetectionJobs_nextToken = Lens.lens (\ListDominantLanguageDetectionJobs' {nextToken} -> nextToken) (\s@ListDominantLanguageDetectionJobs' {} a -> s {nextToken = a} :: ListDominantLanguageDetectionJobs)

-- | Filters that jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
listDominantLanguageDetectionJobs_filter :: Lens.Lens' ListDominantLanguageDetectionJobs (Prelude.Maybe DominantLanguageDetectionJobFilter)
listDominantLanguageDetectionJobs_filter = Lens.lens (\ListDominantLanguageDetectionJobs' {filter'} -> filter') (\s@ListDominantLanguageDetectionJobs' {} a -> s {filter' = a} :: ListDominantLanguageDetectionJobs)

-- | The maximum number of results to return in each page. The default is
-- 100.
listDominantLanguageDetectionJobs_maxResults :: Lens.Lens' ListDominantLanguageDetectionJobs (Prelude.Maybe Prelude.Natural)
listDominantLanguageDetectionJobs_maxResults = Lens.lens (\ListDominantLanguageDetectionJobs' {maxResults} -> maxResults) (\s@ListDominantLanguageDetectionJobs' {} a -> s {maxResults = a} :: ListDominantLanguageDetectionJobs)

instance
  Core.AWSPager
    ListDominantLanguageDetectionJobs
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDominantLanguageDetectionJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDominantLanguageDetectionJobsResponse_dominantLanguageDetectionJobPropertiesList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDominantLanguageDetectionJobs_nextToken
          Lens..~ rs
          Lens.^? listDominantLanguageDetectionJobsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListDominantLanguageDetectionJobs
  where
  type
    AWSResponse ListDominantLanguageDetectionJobs =
      ListDominantLanguageDetectionJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDominantLanguageDetectionJobsResponse'
            Prelude.<$> (x Data..?> "NextToken")
              Prelude.<*> ( x
                              Data..?> "DominantLanguageDetectionJobPropertiesList"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListDominantLanguageDetectionJobs
  where
  hashWithSalt
    _salt
    ListDominantLanguageDetectionJobs' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` filter'
        `Prelude.hashWithSalt` maxResults

instance
  Prelude.NFData
    ListDominantLanguageDetectionJobs
  where
  rnf ListDominantLanguageDetectionJobs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults

instance
  Data.ToHeaders
    ListDominantLanguageDetectionJobs
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.ListDominantLanguageDetectionJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ListDominantLanguageDetectionJobs
  where
  toJSON ListDominantLanguageDetectionJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance
  Data.ToPath
    ListDominantLanguageDetectionJobs
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListDominantLanguageDetectionJobs
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDominantLanguageDetectionJobsResponse' smart constructor.
data ListDominantLanguageDetectionJobsResponse = ListDominantLanguageDetectionJobsResponse'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list containing the properties of each job that is returned.
    dominantLanguageDetectionJobPropertiesList :: Prelude.Maybe [DominantLanguageDetectionJobProperties],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListDominantLanguageDetectionJobsResponse
newListDominantLanguageDetectionJobsResponse
  pHttpStatus_ =
    ListDominantLanguageDetectionJobsResponse'
      { nextToken =
          Prelude.Nothing,
        dominantLanguageDetectionJobPropertiesList =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Identifies the next page of results to return.
listDominantLanguageDetectionJobsResponse_nextToken :: Lens.Lens' ListDominantLanguageDetectionJobsResponse (Prelude.Maybe Prelude.Text)
listDominantLanguageDetectionJobsResponse_nextToken = Lens.lens (\ListDominantLanguageDetectionJobsResponse' {nextToken} -> nextToken) (\s@ListDominantLanguageDetectionJobsResponse' {} a -> s {nextToken = a} :: ListDominantLanguageDetectionJobsResponse)

-- | A list containing the properties of each job that is returned.
listDominantLanguageDetectionJobsResponse_dominantLanguageDetectionJobPropertiesList :: Lens.Lens' ListDominantLanguageDetectionJobsResponse (Prelude.Maybe [DominantLanguageDetectionJobProperties])
listDominantLanguageDetectionJobsResponse_dominantLanguageDetectionJobPropertiesList = Lens.lens (\ListDominantLanguageDetectionJobsResponse' {dominantLanguageDetectionJobPropertiesList} -> dominantLanguageDetectionJobPropertiesList) (\s@ListDominantLanguageDetectionJobsResponse' {} a -> s {dominantLanguageDetectionJobPropertiesList = a} :: ListDominantLanguageDetectionJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDominantLanguageDetectionJobsResponse_httpStatus :: Lens.Lens' ListDominantLanguageDetectionJobsResponse Prelude.Int
listDominantLanguageDetectionJobsResponse_httpStatus = Lens.lens (\ListDominantLanguageDetectionJobsResponse' {httpStatus} -> httpStatus) (\s@ListDominantLanguageDetectionJobsResponse' {} a -> s {httpStatus = a} :: ListDominantLanguageDetectionJobsResponse)

instance
  Prelude.NFData
    ListDominantLanguageDetectionJobsResponse
  where
  rnf ListDominantLanguageDetectionJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf
        dominantLanguageDetectionJobPropertiesList
      `Prelude.seq` Prelude.rnf httpStatus
