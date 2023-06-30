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
-- Module      : Amazonka.Snowball.ListClusterJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @JobListEntry@ objects of the specified length. Each
-- @JobListEntry@ object is for a job in the specified cluster and contains
-- a job\'s state, a job\'s ID, and other information.
--
-- This operation returns paginated results.
module Amazonka.Snowball.ListClusterJobs
  ( -- * Creating a Request
    ListClusterJobs (..),
    newListClusterJobs,

    -- * Request Lenses
    listClusterJobs_maxResults,
    listClusterJobs_nextToken,
    listClusterJobs_clusterId,

    -- * Destructuring the Response
    ListClusterJobsResponse (..),
    newListClusterJobsResponse,

    -- * Response Lenses
    listClusterJobsResponse_jobListEntries,
    listClusterJobsResponse_nextToken,
    listClusterJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Snowball.Types

-- | /See:/ 'newListClusterJobs' smart constructor.
data ListClusterJobs = ListClusterJobs'
  { -- | The number of @JobListEntry@ objects to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | HTTP requests are stateless. To identify what object comes \"next\" in
    -- the list of @JobListEntry@ objects, you have the option of specifying
    -- @NextToken@ as the starting point for your returned list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The 39-character ID for the cluster that you want to list, for example
    -- @CID123e4567-e89b-12d3-a456-426655440000@.
    clusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListClusterJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listClusterJobs_maxResults' - The number of @JobListEntry@ objects to return.
--
-- 'nextToken', 'listClusterJobs_nextToken' - HTTP requests are stateless. To identify what object comes \"next\" in
-- the list of @JobListEntry@ objects, you have the option of specifying
-- @NextToken@ as the starting point for your returned list.
--
-- 'clusterId', 'listClusterJobs_clusterId' - The 39-character ID for the cluster that you want to list, for example
-- @CID123e4567-e89b-12d3-a456-426655440000@.
newListClusterJobs ::
  -- | 'clusterId'
  Prelude.Text ->
  ListClusterJobs
newListClusterJobs pClusterId_ =
  ListClusterJobs'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      clusterId = pClusterId_
    }

-- | The number of @JobListEntry@ objects to return.
listClusterJobs_maxResults :: Lens.Lens' ListClusterJobs (Prelude.Maybe Prelude.Natural)
listClusterJobs_maxResults = Lens.lens (\ListClusterJobs' {maxResults} -> maxResults) (\s@ListClusterJobs' {} a -> s {maxResults = a} :: ListClusterJobs)

-- | HTTP requests are stateless. To identify what object comes \"next\" in
-- the list of @JobListEntry@ objects, you have the option of specifying
-- @NextToken@ as the starting point for your returned list.
listClusterJobs_nextToken :: Lens.Lens' ListClusterJobs (Prelude.Maybe Prelude.Text)
listClusterJobs_nextToken = Lens.lens (\ListClusterJobs' {nextToken} -> nextToken) (\s@ListClusterJobs' {} a -> s {nextToken = a} :: ListClusterJobs)

-- | The 39-character ID for the cluster that you want to list, for example
-- @CID123e4567-e89b-12d3-a456-426655440000@.
listClusterJobs_clusterId :: Lens.Lens' ListClusterJobs Prelude.Text
listClusterJobs_clusterId = Lens.lens (\ListClusterJobs' {clusterId} -> clusterId) (\s@ListClusterJobs' {} a -> s {clusterId = a} :: ListClusterJobs)

instance Core.AWSPager ListClusterJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listClusterJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listClusterJobsResponse_jobListEntries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listClusterJobs_nextToken
          Lens..~ rs
          Lens.^? listClusterJobsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListClusterJobs where
  type
    AWSResponse ListClusterJobs =
      ListClusterJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListClusterJobsResponse'
            Prelude.<$> (x Data..?> "JobListEntries" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListClusterJobs where
  hashWithSalt _salt ListClusterJobs' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` clusterId

instance Prelude.NFData ListClusterJobs where
  rnf ListClusterJobs' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf clusterId

instance Data.ToHeaders ListClusterJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIESnowballJobManagementService.ListClusterJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListClusterJobs where
  toJSON ListClusterJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("ClusterId" Data..= clusterId)
          ]
      )

instance Data.ToPath ListClusterJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListClusterJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListClusterJobsResponse' smart constructor.
data ListClusterJobsResponse = ListClusterJobsResponse'
  { -- | Each @JobListEntry@ object contains a job\'s state, a job\'s ID, and a
    -- value that indicates whether the job is a job part, in the case of
    -- export jobs.
    jobListEntries :: Prelude.Maybe [JobListEntry],
    -- | HTTP requests are stateless. If you use the automatically generated
    -- @NextToken@ value in your next @ListClusterJobsResult@ call, your list
    -- of returned jobs will start from this point in the array.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListClusterJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobListEntries', 'listClusterJobsResponse_jobListEntries' - Each @JobListEntry@ object contains a job\'s state, a job\'s ID, and a
-- value that indicates whether the job is a job part, in the case of
-- export jobs.
--
-- 'nextToken', 'listClusterJobsResponse_nextToken' - HTTP requests are stateless. If you use the automatically generated
-- @NextToken@ value in your next @ListClusterJobsResult@ call, your list
-- of returned jobs will start from this point in the array.
--
-- 'httpStatus', 'listClusterJobsResponse_httpStatus' - The response's http status code.
newListClusterJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListClusterJobsResponse
newListClusterJobsResponse pHttpStatus_ =
  ListClusterJobsResponse'
    { jobListEntries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Each @JobListEntry@ object contains a job\'s state, a job\'s ID, and a
-- value that indicates whether the job is a job part, in the case of
-- export jobs.
listClusterJobsResponse_jobListEntries :: Lens.Lens' ListClusterJobsResponse (Prelude.Maybe [JobListEntry])
listClusterJobsResponse_jobListEntries = Lens.lens (\ListClusterJobsResponse' {jobListEntries} -> jobListEntries) (\s@ListClusterJobsResponse' {} a -> s {jobListEntries = a} :: ListClusterJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | HTTP requests are stateless. If you use the automatically generated
-- @NextToken@ value in your next @ListClusterJobsResult@ call, your list
-- of returned jobs will start from this point in the array.
listClusterJobsResponse_nextToken :: Lens.Lens' ListClusterJobsResponse (Prelude.Maybe Prelude.Text)
listClusterJobsResponse_nextToken = Lens.lens (\ListClusterJobsResponse' {nextToken} -> nextToken) (\s@ListClusterJobsResponse' {} a -> s {nextToken = a} :: ListClusterJobsResponse)

-- | The response's http status code.
listClusterJobsResponse_httpStatus :: Lens.Lens' ListClusterJobsResponse Prelude.Int
listClusterJobsResponse_httpStatus = Lens.lens (\ListClusterJobsResponse' {httpStatus} -> httpStatus) (\s@ListClusterJobsResponse' {} a -> s {httpStatus = a} :: ListClusterJobsResponse)

instance Prelude.NFData ListClusterJobsResponse where
  rnf ListClusterJobsResponse' {..} =
    Prelude.rnf jobListEntries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
