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
-- Module      : Network.AWS.SMS.GetReplicationJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified replication job or all of your replication jobs.
--
-- This operation returns paginated results.
module Network.AWS.SMS.GetReplicationJobs
  ( -- * Creating a Request
    GetReplicationJobs (..),
    newGetReplicationJobs,

    -- * Request Lenses
    getReplicationJobs_nextToken,
    getReplicationJobs_maxResults,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newGetReplicationJobs' smart constructor.
data GetReplicationJobs = GetReplicationJobs'
  { -- | The token for the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in a single call. The default
    -- value is 50. To retrieve the remaining results, make another call with
    -- the returned @NextToken@ value.
    maxResults :: Core.Maybe Core.Int,
    -- | The ID of the replication job.
    replicationJobId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetReplicationJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getReplicationJobs_nextToken' - The token for the next set of results.
--
-- 'maxResults', 'getReplicationJobs_maxResults' - The maximum number of results to return in a single call. The default
-- value is 50. To retrieve the remaining results, make another call with
-- the returned @NextToken@ value.
--
-- 'replicationJobId', 'getReplicationJobs_replicationJobId' - The ID of the replication job.
newGetReplicationJobs ::
  GetReplicationJobs
newGetReplicationJobs =
  GetReplicationJobs'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      replicationJobId = Core.Nothing
    }

-- | The token for the next set of results.
getReplicationJobs_nextToken :: Lens.Lens' GetReplicationJobs (Core.Maybe Core.Text)
getReplicationJobs_nextToken = Lens.lens (\GetReplicationJobs' {nextToken} -> nextToken) (\s@GetReplicationJobs' {} a -> s {nextToken = a} :: GetReplicationJobs)

-- | The maximum number of results to return in a single call. The default
-- value is 50. To retrieve the remaining results, make another call with
-- the returned @NextToken@ value.
getReplicationJobs_maxResults :: Lens.Lens' GetReplicationJobs (Core.Maybe Core.Int)
getReplicationJobs_maxResults = Lens.lens (\GetReplicationJobs' {maxResults} -> maxResults) (\s@GetReplicationJobs' {} a -> s {maxResults = a} :: GetReplicationJobs)

-- | The ID of the replication job.
getReplicationJobs_replicationJobId :: Lens.Lens' GetReplicationJobs (Core.Maybe Core.Text)
getReplicationJobs_replicationJobId = Lens.lens (\GetReplicationJobs' {replicationJobId} -> replicationJobId) (\s@GetReplicationJobs' {} a -> s {replicationJobId = a} :: GetReplicationJobs)

instance Core.AWSPager GetReplicationJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getReplicationJobsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getReplicationJobsResponse_replicationJobList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getReplicationJobs_nextToken
          Lens..~ rs
          Lens.^? getReplicationJobsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest GetReplicationJobs where
  type
    AWSResponse GetReplicationJobs =
      GetReplicationJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetReplicationJobsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> ( x Core..?> "replicationJobList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetReplicationJobs

instance Core.NFData GetReplicationJobs

instance Core.ToHeaders GetReplicationJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.GetReplicationJobs" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetReplicationJobs where
  toJSON GetReplicationJobs' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("replicationJobId" Core..=)
              Core.<$> replicationJobId
          ]
      )

instance Core.ToPath GetReplicationJobs where
  toPath = Core.const "/"

instance Core.ToQuery GetReplicationJobs where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetReplicationJobsResponse' smart constructor.
data GetReplicationJobsResponse = GetReplicationJobsResponse'
  { -- | The token required to retrieve the next set of results. This value is
    -- null when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the replication jobs.
    replicationJobList :: Core.Maybe [ReplicationJob],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetReplicationJobsResponse
newGetReplicationJobsResponse pHttpStatus_ =
  GetReplicationJobsResponse'
    { nextToken =
        Core.Nothing,
      replicationJobList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token required to retrieve the next set of results. This value is
-- null when there are no more results to return.
getReplicationJobsResponse_nextToken :: Lens.Lens' GetReplicationJobsResponse (Core.Maybe Core.Text)
getReplicationJobsResponse_nextToken = Lens.lens (\GetReplicationJobsResponse' {nextToken} -> nextToken) (\s@GetReplicationJobsResponse' {} a -> s {nextToken = a} :: GetReplicationJobsResponse)

-- | Information about the replication jobs.
getReplicationJobsResponse_replicationJobList :: Lens.Lens' GetReplicationJobsResponse (Core.Maybe [ReplicationJob])
getReplicationJobsResponse_replicationJobList = Lens.lens (\GetReplicationJobsResponse' {replicationJobList} -> replicationJobList) (\s@GetReplicationJobsResponse' {} a -> s {replicationJobList = a} :: GetReplicationJobsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getReplicationJobsResponse_httpStatus :: Lens.Lens' GetReplicationJobsResponse Core.Int
getReplicationJobsResponse_httpStatus = Lens.lens (\GetReplicationJobsResponse' {httpStatus} -> httpStatus) (\s@GetReplicationJobsResponse' {} a -> s {httpStatus = a} :: GetReplicationJobsResponse)

instance Core.NFData GetReplicationJobsResponse
