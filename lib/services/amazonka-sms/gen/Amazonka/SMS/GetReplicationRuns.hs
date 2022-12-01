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
-- Module      : Amazonka.SMS.GetReplicationRuns
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the replication runs for the specified replication job.
--
-- This operation returns paginated results.
module Amazonka.SMS.GetReplicationRuns
  ( -- * Creating a Request
    GetReplicationRuns (..),
    newGetReplicationRuns,

    -- * Request Lenses
    getReplicationRuns_nextToken,
    getReplicationRuns_maxResults,
    getReplicationRuns_replicationJobId,

    -- * Destructuring the Response
    GetReplicationRunsResponse (..),
    newGetReplicationRunsResponse,

    -- * Response Lenses
    getReplicationRunsResponse_replicationRunList,
    getReplicationRunsResponse_nextToken,
    getReplicationRunsResponse_replicationJob,
    getReplicationRunsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SMS.Types

-- | /See:/ 'newGetReplicationRuns' smart constructor.
data GetReplicationRuns = GetReplicationRuns'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in a single call. The default
    -- value is 50. To retrieve the remaining results, make another call with
    -- the returned @NextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The ID of the replication job.
    replicationJobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReplicationRuns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getReplicationRuns_nextToken' - The token for the next set of results.
--
-- 'maxResults', 'getReplicationRuns_maxResults' - The maximum number of results to return in a single call. The default
-- value is 50. To retrieve the remaining results, make another call with
-- the returned @NextToken@ value.
--
-- 'replicationJobId', 'getReplicationRuns_replicationJobId' - The ID of the replication job.
newGetReplicationRuns ::
  -- | 'replicationJobId'
  Prelude.Text ->
  GetReplicationRuns
newGetReplicationRuns pReplicationJobId_ =
  GetReplicationRuns'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      replicationJobId = pReplicationJobId_
    }

-- | The token for the next set of results.
getReplicationRuns_nextToken :: Lens.Lens' GetReplicationRuns (Prelude.Maybe Prelude.Text)
getReplicationRuns_nextToken = Lens.lens (\GetReplicationRuns' {nextToken} -> nextToken) (\s@GetReplicationRuns' {} a -> s {nextToken = a} :: GetReplicationRuns)

-- | The maximum number of results to return in a single call. The default
-- value is 50. To retrieve the remaining results, make another call with
-- the returned @NextToken@ value.
getReplicationRuns_maxResults :: Lens.Lens' GetReplicationRuns (Prelude.Maybe Prelude.Int)
getReplicationRuns_maxResults = Lens.lens (\GetReplicationRuns' {maxResults} -> maxResults) (\s@GetReplicationRuns' {} a -> s {maxResults = a} :: GetReplicationRuns)

-- | The ID of the replication job.
getReplicationRuns_replicationJobId :: Lens.Lens' GetReplicationRuns Prelude.Text
getReplicationRuns_replicationJobId = Lens.lens (\GetReplicationRuns' {replicationJobId} -> replicationJobId) (\s@GetReplicationRuns' {} a -> s {replicationJobId = a} :: GetReplicationRuns)

instance Core.AWSPager GetReplicationRuns where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getReplicationRunsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getReplicationRunsResponse_replicationRunList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getReplicationRuns_nextToken
          Lens..~ rs
          Lens.^? getReplicationRunsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetReplicationRuns where
  type
    AWSResponse GetReplicationRuns =
      GetReplicationRunsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetReplicationRunsResponse'
            Prelude.<$> ( x Core..?> "replicationRunList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "replicationJob")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetReplicationRuns where
  hashWithSalt _salt GetReplicationRuns' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` replicationJobId

instance Prelude.NFData GetReplicationRuns where
  rnf GetReplicationRuns' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf replicationJobId

instance Core.ToHeaders GetReplicationRuns where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.GetReplicationRuns" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetReplicationRuns where
  toJSON GetReplicationRuns' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ("replicationJobId" Core..= replicationJobId)
          ]
      )

instance Core.ToPath GetReplicationRuns where
  toPath = Prelude.const "/"

instance Core.ToQuery GetReplicationRuns where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetReplicationRunsResponse' smart constructor.
data GetReplicationRunsResponse = GetReplicationRunsResponse'
  { -- | Information about the replication runs.
    replicationRunList :: Prelude.Maybe [ReplicationRun],
    -- | The token required to retrieve the next set of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the replication job.
    replicationJob :: Prelude.Maybe ReplicationJob,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReplicationRunsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationRunList', 'getReplicationRunsResponse_replicationRunList' - Information about the replication runs.
--
-- 'nextToken', 'getReplicationRunsResponse_nextToken' - The token required to retrieve the next set of results. This value is
-- null when there are no more results to return.
--
-- 'replicationJob', 'getReplicationRunsResponse_replicationJob' - Information about the replication job.
--
-- 'httpStatus', 'getReplicationRunsResponse_httpStatus' - The response's http status code.
newGetReplicationRunsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetReplicationRunsResponse
newGetReplicationRunsResponse pHttpStatus_ =
  GetReplicationRunsResponse'
    { replicationRunList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      replicationJob = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the replication runs.
getReplicationRunsResponse_replicationRunList :: Lens.Lens' GetReplicationRunsResponse (Prelude.Maybe [ReplicationRun])
getReplicationRunsResponse_replicationRunList = Lens.lens (\GetReplicationRunsResponse' {replicationRunList} -> replicationRunList) (\s@GetReplicationRunsResponse' {} a -> s {replicationRunList = a} :: GetReplicationRunsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token required to retrieve the next set of results. This value is
-- null when there are no more results to return.
getReplicationRunsResponse_nextToken :: Lens.Lens' GetReplicationRunsResponse (Prelude.Maybe Prelude.Text)
getReplicationRunsResponse_nextToken = Lens.lens (\GetReplicationRunsResponse' {nextToken} -> nextToken) (\s@GetReplicationRunsResponse' {} a -> s {nextToken = a} :: GetReplicationRunsResponse)

-- | Information about the replication job.
getReplicationRunsResponse_replicationJob :: Lens.Lens' GetReplicationRunsResponse (Prelude.Maybe ReplicationJob)
getReplicationRunsResponse_replicationJob = Lens.lens (\GetReplicationRunsResponse' {replicationJob} -> replicationJob) (\s@GetReplicationRunsResponse' {} a -> s {replicationJob = a} :: GetReplicationRunsResponse)

-- | The response's http status code.
getReplicationRunsResponse_httpStatus :: Lens.Lens' GetReplicationRunsResponse Prelude.Int
getReplicationRunsResponse_httpStatus = Lens.lens (\GetReplicationRunsResponse' {httpStatus} -> httpStatus) (\s@GetReplicationRunsResponse' {} a -> s {httpStatus = a} :: GetReplicationRunsResponse)

instance Prelude.NFData GetReplicationRunsResponse where
  rnf GetReplicationRunsResponse' {..} =
    Prelude.rnf replicationRunList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf replicationJob
      `Prelude.seq` Prelude.rnf httpStatus
