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
-- Module      : Amazonka.SMS.GetReplicationJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified replication job or all of your replication jobs.
--
-- This operation returns paginated results.
module Amazonka.SMS.GetReplicationJobs
  ( -- * Creating a Request
    GetReplicationJobs (..),
    newGetReplicationJobs,

    -- * Request Lenses
    getReplicationJobs_maxResults,
    getReplicationJobs_nextToken,
    getReplicationJobs_replicationJobId,

    -- * Destructuring the Response
    GetReplicationJobsResponse (..),
    newGetReplicationJobsResponse,

    -- * Response Lenses
    getReplicationJobsResponse_nextToken,
    getReplicationJobsResponse_replicationJobList,
    getReplicationJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SMS.Types

-- | /See:/ 'newGetReplicationJobs' smart constructor.
data GetReplicationJobs = GetReplicationJobs'
  { -- | The maximum number of results to return in a single call. The default
    -- value is 50. To retrieve the remaining results, make another call with
    -- the returned @NextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the replication job.
    replicationJobId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReplicationJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getReplicationJobs_maxResults' - The maximum number of results to return in a single call. The default
-- value is 50. To retrieve the remaining results, make another call with
-- the returned @NextToken@ value.
--
-- 'nextToken', 'getReplicationJobs_nextToken' - The token for the next set of results.
--
-- 'replicationJobId', 'getReplicationJobs_replicationJobId' - The ID of the replication job.
newGetReplicationJobs ::
  GetReplicationJobs
newGetReplicationJobs =
  GetReplicationJobs'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      replicationJobId = Prelude.Nothing
    }

-- | The maximum number of results to return in a single call. The default
-- value is 50. To retrieve the remaining results, make another call with
-- the returned @NextToken@ value.
getReplicationJobs_maxResults :: Lens.Lens' GetReplicationJobs (Prelude.Maybe Prelude.Int)
getReplicationJobs_maxResults = Lens.lens (\GetReplicationJobs' {maxResults} -> maxResults) (\s@GetReplicationJobs' {} a -> s {maxResults = a} :: GetReplicationJobs)

-- | The token for the next set of results.
getReplicationJobs_nextToken :: Lens.Lens' GetReplicationJobs (Prelude.Maybe Prelude.Text)
getReplicationJobs_nextToken = Lens.lens (\GetReplicationJobs' {nextToken} -> nextToken) (\s@GetReplicationJobs' {} a -> s {nextToken = a} :: GetReplicationJobs)

-- | The ID of the replication job.
getReplicationJobs_replicationJobId :: Lens.Lens' GetReplicationJobs (Prelude.Maybe Prelude.Text)
getReplicationJobs_replicationJobId = Lens.lens (\GetReplicationJobs' {replicationJobId} -> replicationJobId) (\s@GetReplicationJobs' {} a -> s {replicationJobId = a} :: GetReplicationJobs)

instance Core.AWSPager GetReplicationJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getReplicationJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getReplicationJobsResponse_replicationJobList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& getReplicationJobs_nextToken
              Lens..~ rs
              Lens.^? getReplicationJobsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest GetReplicationJobs where
  type
    AWSResponse GetReplicationJobs =
      GetReplicationJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetReplicationJobsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "replicationJobList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetReplicationJobs where
  hashWithSalt _salt GetReplicationJobs' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` replicationJobId

instance Prelude.NFData GetReplicationJobs where
  rnf GetReplicationJobs' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf replicationJobId

instance Data.ToHeaders GetReplicationJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSServerMigrationService_V2016_10_24.GetReplicationJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetReplicationJobs where
  toJSON GetReplicationJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("replicationJobId" Data..=)
              Prelude.<$> replicationJobId
          ]
      )

instance Data.ToPath GetReplicationJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery GetReplicationJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetReplicationJobsResponse' smart constructor.
data GetReplicationJobsResponse = GetReplicationJobsResponse'
  { -- | The token required to retrieve the next set of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the replication jobs.
    replicationJobList :: Prelude.Maybe [ReplicationJob],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReplicationJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getReplicationJobsResponse_nextToken' - The token required to retrieve the next set of results. This value is
-- null when there are no more results to return.
--
-- 'replicationJobList', 'getReplicationJobsResponse_replicationJobList' - Information about the replication jobs.
--
-- 'httpStatus', 'getReplicationJobsResponse_httpStatus' - The response's http status code.
newGetReplicationJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetReplicationJobsResponse
newGetReplicationJobsResponse pHttpStatus_ =
  GetReplicationJobsResponse'
    { nextToken =
        Prelude.Nothing,
      replicationJobList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token required to retrieve the next set of results. This value is
-- null when there are no more results to return.
getReplicationJobsResponse_nextToken :: Lens.Lens' GetReplicationJobsResponse (Prelude.Maybe Prelude.Text)
getReplicationJobsResponse_nextToken = Lens.lens (\GetReplicationJobsResponse' {nextToken} -> nextToken) (\s@GetReplicationJobsResponse' {} a -> s {nextToken = a} :: GetReplicationJobsResponse)

-- | Information about the replication jobs.
getReplicationJobsResponse_replicationJobList :: Lens.Lens' GetReplicationJobsResponse (Prelude.Maybe [ReplicationJob])
getReplicationJobsResponse_replicationJobList = Lens.lens (\GetReplicationJobsResponse' {replicationJobList} -> replicationJobList) (\s@GetReplicationJobsResponse' {} a -> s {replicationJobList = a} :: GetReplicationJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getReplicationJobsResponse_httpStatus :: Lens.Lens' GetReplicationJobsResponse Prelude.Int
getReplicationJobsResponse_httpStatus = Lens.lens (\GetReplicationJobsResponse' {httpStatus} -> httpStatus) (\s@GetReplicationJobsResponse' {} a -> s {httpStatus = a} :: GetReplicationJobsResponse)

instance Prelude.NFData GetReplicationJobsResponse where
  rnf GetReplicationJobsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf replicationJobList `Prelude.seq`
        Prelude.rnf httpStatus
