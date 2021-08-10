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
-- Module      : Network.AWS.Comprehend.ListEntitiesDetectionJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the entity detection jobs that you have submitted.
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListEntitiesDetectionJobs
  ( -- * Creating a Request
    ListEntitiesDetectionJobs (..),
    newListEntitiesDetectionJobs,

    -- * Request Lenses
    listEntitiesDetectionJobs_nextToken,
    listEntitiesDetectionJobs_maxResults,
    listEntitiesDetectionJobs_filter,

    -- * Destructuring the Response
    ListEntitiesDetectionJobsResponse (..),
    newListEntitiesDetectionJobsResponse,

    -- * Response Lenses
    listEntitiesDetectionJobsResponse_nextToken,
    listEntitiesDetectionJobsResponse_entitiesDetectionJobPropertiesList,
    listEntitiesDetectionJobsResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListEntitiesDetectionJobs' smart constructor.
data ListEntitiesDetectionJobs = ListEntitiesDetectionJobs'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in each page. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Filters the jobs that are returned. You can filter jobs on their name,
    -- status, or the date and time that they were submitted. You can only set
    -- one filter at a time.
    filter' :: Prelude.Maybe EntitiesDetectionJobFilter
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
-- 'nextToken', 'listEntitiesDetectionJobs_nextToken' - Identifies the next page of results to return.
--
-- 'maxResults', 'listEntitiesDetectionJobs_maxResults' - The maximum number of results to return in each page. The default is
-- 100.
--
-- 'filter'', 'listEntitiesDetectionJobs_filter' - Filters the jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
newListEntitiesDetectionJobs ::
  ListEntitiesDetectionJobs
newListEntitiesDetectionJobs =
  ListEntitiesDetectionJobs'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filter' = Prelude.Nothing
    }

-- | Identifies the next page of results to return.
listEntitiesDetectionJobs_nextToken :: Lens.Lens' ListEntitiesDetectionJobs (Prelude.Maybe Prelude.Text)
listEntitiesDetectionJobs_nextToken = Lens.lens (\ListEntitiesDetectionJobs' {nextToken} -> nextToken) (\s@ListEntitiesDetectionJobs' {} a -> s {nextToken = a} :: ListEntitiesDetectionJobs)

-- | The maximum number of results to return in each page. The default is
-- 100.
listEntitiesDetectionJobs_maxResults :: Lens.Lens' ListEntitiesDetectionJobs (Prelude.Maybe Prelude.Natural)
listEntitiesDetectionJobs_maxResults = Lens.lens (\ListEntitiesDetectionJobs' {maxResults} -> maxResults) (\s@ListEntitiesDetectionJobs' {} a -> s {maxResults = a} :: ListEntitiesDetectionJobs)

-- | Filters the jobs that are returned. You can filter jobs on their name,
-- status, or the date and time that they were submitted. You can only set
-- one filter at a time.
listEntitiesDetectionJobs_filter :: Lens.Lens' ListEntitiesDetectionJobs (Prelude.Maybe EntitiesDetectionJobFilter)
listEntitiesDetectionJobs_filter = Lens.lens (\ListEntitiesDetectionJobs' {filter'} -> filter') (\s@ListEntitiesDetectionJobs' {} a -> s {filter' = a} :: ListEntitiesDetectionJobs)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEntitiesDetectionJobsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "EntitiesDetectionJobPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEntitiesDetectionJobs

instance Prelude.NFData ListEntitiesDetectionJobs

instance Core.ToHeaders ListEntitiesDetectionJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.ListEntitiesDetectionJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListEntitiesDetectionJobs where
  toJSON ListEntitiesDetectionJobs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("Filter" Core..=) Prelude.<$> filter'
          ]
      )

instance Core.ToPath ListEntitiesDetectionJobs where
  toPath = Prelude.const "/"

instance Core.ToQuery ListEntitiesDetectionJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEntitiesDetectionJobsResponse' smart constructor.
data ListEntitiesDetectionJobsResponse = ListEntitiesDetectionJobsResponse'
  { -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list containing the properties of each job that is returned.
    entitiesDetectionJobPropertiesList :: Prelude.Maybe [EntitiesDetectionJobProperties],
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
-- 'nextToken', 'listEntitiesDetectionJobsResponse_nextToken' - Identifies the next page of results to return.
--
-- 'entitiesDetectionJobPropertiesList', 'listEntitiesDetectionJobsResponse_entitiesDetectionJobPropertiesList' - A list containing the properties of each job that is returned.
--
-- 'httpStatus', 'listEntitiesDetectionJobsResponse_httpStatus' - The response's http status code.
newListEntitiesDetectionJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEntitiesDetectionJobsResponse
newListEntitiesDetectionJobsResponse pHttpStatus_ =
  ListEntitiesDetectionJobsResponse'
    { nextToken =
        Prelude.Nothing,
      entitiesDetectionJobPropertiesList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifies the next page of results to return.
listEntitiesDetectionJobsResponse_nextToken :: Lens.Lens' ListEntitiesDetectionJobsResponse (Prelude.Maybe Prelude.Text)
listEntitiesDetectionJobsResponse_nextToken = Lens.lens (\ListEntitiesDetectionJobsResponse' {nextToken} -> nextToken) (\s@ListEntitiesDetectionJobsResponse' {} a -> s {nextToken = a} :: ListEntitiesDetectionJobsResponse)

-- | A list containing the properties of each job that is returned.
listEntitiesDetectionJobsResponse_entitiesDetectionJobPropertiesList :: Lens.Lens' ListEntitiesDetectionJobsResponse (Prelude.Maybe [EntitiesDetectionJobProperties])
listEntitiesDetectionJobsResponse_entitiesDetectionJobPropertiesList = Lens.lens (\ListEntitiesDetectionJobsResponse' {entitiesDetectionJobPropertiesList} -> entitiesDetectionJobPropertiesList) (\s@ListEntitiesDetectionJobsResponse' {} a -> s {entitiesDetectionJobPropertiesList = a} :: ListEntitiesDetectionJobsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listEntitiesDetectionJobsResponse_httpStatus :: Lens.Lens' ListEntitiesDetectionJobsResponse Prelude.Int
listEntitiesDetectionJobsResponse_httpStatus = Lens.lens (\ListEntitiesDetectionJobsResponse' {httpStatus} -> httpStatus) (\s@ListEntitiesDetectionJobsResponse' {} a -> s {httpStatus = a} :: ListEntitiesDetectionJobsResponse)

instance
  Prelude.NFData
    ListEntitiesDetectionJobsResponse
