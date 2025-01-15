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
-- Module      : Amazonka.Comprehend.ListEntitiesDetectionJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the entity detection jobs that you have submitted.
--
-- This operation returns paginated results.
module Amazonka.Comprehend.ListEntitiesDetectionJobs
  ( -- * Creating a Request
    ListEntitiesDetectionJobs (..),
    newListEntitiesDetectionJobs,

    -- * Request Lenses
    listEntitiesDetectionJobs_filter,
    listEntitiesDetectionJobs_maxResults,
    listEntitiesDetectionJobs_nextToken,

    -- * Destructuring the Response
    ListEntitiesDetectionJobsResponse (..),
    newListEntitiesDetectionJobsResponse,

    -- * Response Lenses
    listEntitiesDetectionJobsResponse_entitiesDetectionJobPropertiesList,
    listEntitiesDetectionJobsResponse_nextToken,
    listEntitiesDetectionJobsResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEntitiesDetectionJobs' smart constructor.
data ListEntitiesDetectionJobs = ListEntitiesDetectionJobs'
  { -- | Filters the jobs that are returned. You can filter jobs on their name,
    -- status, or the date and time that they were submitted. You can only set
    -- one filter at a time.
    filter' :: Prelude.Maybe EntitiesDetectionJobFilter,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEntitiesDetectionJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listEntitiesDetectionJobs_filter' - Filters the jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
--
-- 'maxResults', 'listEntitiesDetectionJobs_maxResults' - The maximum number of results to return in each page. The default is
-- 100.
--
-- 'nextToken', 'listEntitiesDetectionJobs_nextToken' - Identifies the next page of results to return.
newListEntitiesDetectionJobs ::
  ListEntitiesDetectionJobs
newListEntitiesDetectionJobs =
  ListEntitiesDetectionJobs'
    { filter' =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filters the jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
listEntitiesDetectionJobs_filter :: Lens.Lens' ListEntitiesDetectionJobs (Prelude.Maybe EntitiesDetectionJobFilter)
listEntitiesDetectionJobs_filter = Lens.lens (\ListEntitiesDetectionJobs' {filter'} -> filter') (\s@ListEntitiesDetectionJobs' {} a -> s {filter' = a} :: ListEntitiesDetectionJobs)

-- | The maximum number of results to return in each page. The default is
-- 100.
listEntitiesDetectionJobs_maxResults :: Lens.Lens' ListEntitiesDetectionJobs (Prelude.Maybe Prelude.Natural)
listEntitiesDetectionJobs_maxResults = Lens.lens (\ListEntitiesDetectionJobs' {maxResults} -> maxResults) (\s@ListEntitiesDetectionJobs' {} a -> s {maxResults = a} :: ListEntitiesDetectionJobs)

-- | Identifies the next page of results to return.
listEntitiesDetectionJobs_nextToken :: Lens.Lens' ListEntitiesDetectionJobs (Prelude.Maybe Prelude.Text)
listEntitiesDetectionJobs_nextToken = Lens.lens (\ListEntitiesDetectionJobs' {nextToken} -> nextToken) (\s@ListEntitiesDetectionJobs' {} a -> s {nextToken = a} :: ListEntitiesDetectionJobs)

instance Core.AWSPager ListEntitiesDetectionJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEntitiesDetectionJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listEntitiesDetectionJobsResponse_entitiesDetectionJobPropertiesList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listEntitiesDetectionJobs_nextToken
              Lens..~ rs
              Lens.^? listEntitiesDetectionJobsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListEntitiesDetectionJobs where
  type
    AWSResponse ListEntitiesDetectionJobs =
      ListEntitiesDetectionJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEntitiesDetectionJobsResponse'
            Prelude.<$> ( x
                            Data..?> "EntitiesDetectionJobPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEntitiesDetectionJobs where
  hashWithSalt _salt ListEntitiesDetectionJobs' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListEntitiesDetectionJobs where
  rnf ListEntitiesDetectionJobs' {..} =
    Prelude.rnf filter' `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders ListEntitiesDetectionJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.ListEntitiesDetectionJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEntitiesDetectionJobs where
  toJSON ListEntitiesDetectionJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListEntitiesDetectionJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListEntitiesDetectionJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEntitiesDetectionJobsResponse' smart constructor.
data ListEntitiesDetectionJobsResponse = ListEntitiesDetectionJobsResponse'
  { -- | A list containing the properties of each job that is returned.
    entitiesDetectionJobPropertiesList :: Prelude.Maybe [EntitiesDetectionJobProperties],
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEntitiesDetectionJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entitiesDetectionJobPropertiesList', 'listEntitiesDetectionJobsResponse_entitiesDetectionJobPropertiesList' - A list containing the properties of each job that is returned.
--
-- 'nextToken', 'listEntitiesDetectionJobsResponse_nextToken' - Identifies the next page of results to return.
--
-- 'httpStatus', 'listEntitiesDetectionJobsResponse_httpStatus' - The response's http status code.
newListEntitiesDetectionJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEntitiesDetectionJobsResponse
newListEntitiesDetectionJobsResponse pHttpStatus_ =
  ListEntitiesDetectionJobsResponse'
    { entitiesDetectionJobPropertiesList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list containing the properties of each job that is returned.
listEntitiesDetectionJobsResponse_entitiesDetectionJobPropertiesList :: Lens.Lens' ListEntitiesDetectionJobsResponse (Prelude.Maybe [EntitiesDetectionJobProperties])
listEntitiesDetectionJobsResponse_entitiesDetectionJobPropertiesList = Lens.lens (\ListEntitiesDetectionJobsResponse' {entitiesDetectionJobPropertiesList} -> entitiesDetectionJobPropertiesList) (\s@ListEntitiesDetectionJobsResponse' {} a -> s {entitiesDetectionJobPropertiesList = a} :: ListEntitiesDetectionJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Identifies the next page of results to return.
listEntitiesDetectionJobsResponse_nextToken :: Lens.Lens' ListEntitiesDetectionJobsResponse (Prelude.Maybe Prelude.Text)
listEntitiesDetectionJobsResponse_nextToken = Lens.lens (\ListEntitiesDetectionJobsResponse' {nextToken} -> nextToken) (\s@ListEntitiesDetectionJobsResponse' {} a -> s {nextToken = a} :: ListEntitiesDetectionJobsResponse)

-- | The response's http status code.
listEntitiesDetectionJobsResponse_httpStatus :: Lens.Lens' ListEntitiesDetectionJobsResponse Prelude.Int
listEntitiesDetectionJobsResponse_httpStatus = Lens.lens (\ListEntitiesDetectionJobsResponse' {httpStatus} -> httpStatus) (\s@ListEntitiesDetectionJobsResponse' {} a -> s {httpStatus = a} :: ListEntitiesDetectionJobsResponse)

instance
  Prelude.NFData
    ListEntitiesDetectionJobsResponse
  where
  rnf ListEntitiesDetectionJobsResponse' {..} =
    Prelude.rnf entitiesDetectionJobPropertiesList `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
