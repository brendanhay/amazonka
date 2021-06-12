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
-- Module      : Network.AWS.SMS.GetReplicationRuns
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the replication runs for the specified replication job.
--
-- This operation returns paginated results.
module Network.AWS.SMS.GetReplicationRuns
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
    getReplicationRunsResponse_nextToken,
    getReplicationRunsResponse_replicationJob,
    getReplicationRunsResponse_replicationRunList,
    getReplicationRunsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newGetReplicationRuns' smart constructor.
data GetReplicationRuns = GetReplicationRuns'
  { -- | The token for the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in a single call. The default
    -- value is 50. To retrieve the remaining results, make another call with
    -- the returned @NextToken@ value.
    maxResults :: Core.Maybe Core.Int,
    -- | The ID of the replication job.
    replicationJobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetReplicationRuns
newGetReplicationRuns pReplicationJobId_ =
  GetReplicationRuns'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      replicationJobId = pReplicationJobId_
    }

-- | The token for the next set of results.
getReplicationRuns_nextToken :: Lens.Lens' GetReplicationRuns (Core.Maybe Core.Text)
getReplicationRuns_nextToken = Lens.lens (\GetReplicationRuns' {nextToken} -> nextToken) (\s@GetReplicationRuns' {} a -> s {nextToken = a} :: GetReplicationRuns)

-- | The maximum number of results to return in a single call. The default
-- value is 50. To retrieve the remaining results, make another call with
-- the returned @NextToken@ value.
getReplicationRuns_maxResults :: Lens.Lens' GetReplicationRuns (Core.Maybe Core.Int)
getReplicationRuns_maxResults = Lens.lens (\GetReplicationRuns' {maxResults} -> maxResults) (\s@GetReplicationRuns' {} a -> s {maxResults = a} :: GetReplicationRuns)

-- | The ID of the replication job.
getReplicationRuns_replicationJobId :: Lens.Lens' GetReplicationRuns Core.Text
getReplicationRuns_replicationJobId = Lens.lens (\GetReplicationRuns' {replicationJobId} -> replicationJobId) (\s@GetReplicationRuns' {} a -> s {replicationJobId = a} :: GetReplicationRuns)

instance Core.AWSPager GetReplicationRuns where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getReplicationRunsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getReplicationRunsResponse_replicationRunList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getReplicationRuns_nextToken
          Lens..~ rs
          Lens.^? getReplicationRunsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest GetReplicationRuns where
  type
    AWSResponse GetReplicationRuns =
      GetReplicationRunsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetReplicationRunsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "replicationJob")
            Core.<*> ( x Core..?> "replicationRunList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetReplicationRuns

instance Core.NFData GetReplicationRuns

instance Core.ToHeaders GetReplicationRuns where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.GetReplicationRuns" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetReplicationRuns where
  toJSON GetReplicationRuns' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            Core.Just
              ("replicationJobId" Core..= replicationJobId)
          ]
      )

instance Core.ToPath GetReplicationRuns where
  toPath = Core.const "/"

instance Core.ToQuery GetReplicationRuns where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetReplicationRunsResponse' smart constructor.
data GetReplicationRunsResponse = GetReplicationRunsResponse'
  { -- | The token required to retrieve the next set of results. This value is
    -- null when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the replication job.
    replicationJob :: Core.Maybe ReplicationJob,
    -- | Information about the replication runs.
    replicationRunList :: Core.Maybe [ReplicationRun],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetReplicationRunsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getReplicationRunsResponse_nextToken' - The token required to retrieve the next set of results. This value is
-- null when there are no more results to return.
--
-- 'replicationJob', 'getReplicationRunsResponse_replicationJob' - Information about the replication job.
--
-- 'replicationRunList', 'getReplicationRunsResponse_replicationRunList' - Information about the replication runs.
--
-- 'httpStatus', 'getReplicationRunsResponse_httpStatus' - The response's http status code.
newGetReplicationRunsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetReplicationRunsResponse
newGetReplicationRunsResponse pHttpStatus_ =
  GetReplicationRunsResponse'
    { nextToken =
        Core.Nothing,
      replicationJob = Core.Nothing,
      replicationRunList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token required to retrieve the next set of results. This value is
-- null when there are no more results to return.
getReplicationRunsResponse_nextToken :: Lens.Lens' GetReplicationRunsResponse (Core.Maybe Core.Text)
getReplicationRunsResponse_nextToken = Lens.lens (\GetReplicationRunsResponse' {nextToken} -> nextToken) (\s@GetReplicationRunsResponse' {} a -> s {nextToken = a} :: GetReplicationRunsResponse)

-- | Information about the replication job.
getReplicationRunsResponse_replicationJob :: Lens.Lens' GetReplicationRunsResponse (Core.Maybe ReplicationJob)
getReplicationRunsResponse_replicationJob = Lens.lens (\GetReplicationRunsResponse' {replicationJob} -> replicationJob) (\s@GetReplicationRunsResponse' {} a -> s {replicationJob = a} :: GetReplicationRunsResponse)

-- | Information about the replication runs.
getReplicationRunsResponse_replicationRunList :: Lens.Lens' GetReplicationRunsResponse (Core.Maybe [ReplicationRun])
getReplicationRunsResponse_replicationRunList = Lens.lens (\GetReplicationRunsResponse' {replicationRunList} -> replicationRunList) (\s@GetReplicationRunsResponse' {} a -> s {replicationRunList = a} :: GetReplicationRunsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getReplicationRunsResponse_httpStatus :: Lens.Lens' GetReplicationRunsResponse Core.Int
getReplicationRunsResponse_httpStatus = Lens.lens (\GetReplicationRunsResponse' {httpStatus} -> httpStatus) (\s@GetReplicationRunsResponse' {} a -> s {httpStatus = a} :: GetReplicationRunsResponse)

instance Core.NFData GetReplicationRunsResponse
