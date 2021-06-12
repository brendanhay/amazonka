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
-- Module      : Network.AWS.Config.DescribePendingAggregationRequests
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all pending aggregation requests.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribePendingAggregationRequests
  ( -- * Creating a Request
    DescribePendingAggregationRequests (..),
    newDescribePendingAggregationRequests,

    -- * Request Lenses
    describePendingAggregationRequests_nextToken,
    describePendingAggregationRequests_limit,

    -- * Destructuring the Response
    DescribePendingAggregationRequestsResponse (..),
    newDescribePendingAggregationRequestsResponse,

    -- * Response Lenses
    describePendingAggregationRequestsResponse_nextToken,
    describePendingAggregationRequestsResponse_pendingAggregationRequests,
    describePendingAggregationRequestsResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribePendingAggregationRequests' smart constructor.
data DescribePendingAggregationRequests = DescribePendingAggregationRequests'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of evaluation results returned on each page. The
    -- default is maximum. If you specify 0, AWS Config uses the default.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePendingAggregationRequests' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePendingAggregationRequests_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'limit', 'describePendingAggregationRequests_limit' - The maximum number of evaluation results returned on each page. The
-- default is maximum. If you specify 0, AWS Config uses the default.
newDescribePendingAggregationRequests ::
  DescribePendingAggregationRequests
newDescribePendingAggregationRequests =
  DescribePendingAggregationRequests'
    { nextToken =
        Core.Nothing,
      limit = Core.Nothing
    }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describePendingAggregationRequests_nextToken :: Lens.Lens' DescribePendingAggregationRequests (Core.Maybe Core.Text)
describePendingAggregationRequests_nextToken = Lens.lens (\DescribePendingAggregationRequests' {nextToken} -> nextToken) (\s@DescribePendingAggregationRequests' {} a -> s {nextToken = a} :: DescribePendingAggregationRequests)

-- | The maximum number of evaluation results returned on each page. The
-- default is maximum. If you specify 0, AWS Config uses the default.
describePendingAggregationRequests_limit :: Lens.Lens' DescribePendingAggregationRequests (Core.Maybe Core.Natural)
describePendingAggregationRequests_limit = Lens.lens (\DescribePendingAggregationRequests' {limit} -> limit) (\s@DescribePendingAggregationRequests' {} a -> s {limit = a} :: DescribePendingAggregationRequests)

instance
  Core.AWSPager
    DescribePendingAggregationRequests
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describePendingAggregationRequestsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describePendingAggregationRequestsResponse_pendingAggregationRequests
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describePendingAggregationRequests_nextToken
          Lens..~ rs
          Lens.^? describePendingAggregationRequestsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribePendingAggregationRequests
  where
  type
    AWSResponse DescribePendingAggregationRequests =
      DescribePendingAggregationRequestsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePendingAggregationRequestsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "PendingAggregationRequests"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribePendingAggregationRequests

instance
  Core.NFData
    DescribePendingAggregationRequests

instance
  Core.ToHeaders
    DescribePendingAggregationRequests
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribePendingAggregationRequests" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribePendingAggregationRequests
  where
  toJSON DescribePendingAggregationRequests' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance
  Core.ToPath
    DescribePendingAggregationRequests
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribePendingAggregationRequests
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribePendingAggregationRequestsResponse' smart constructor.
data DescribePendingAggregationRequestsResponse = DescribePendingAggregationRequestsResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | Returns a PendingAggregationRequests object.
    pendingAggregationRequests :: Core.Maybe [PendingAggregationRequest],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePendingAggregationRequestsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePendingAggregationRequestsResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'pendingAggregationRequests', 'describePendingAggregationRequestsResponse_pendingAggregationRequests' - Returns a PendingAggregationRequests object.
--
-- 'httpStatus', 'describePendingAggregationRequestsResponse_httpStatus' - The response's http status code.
newDescribePendingAggregationRequestsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribePendingAggregationRequestsResponse
newDescribePendingAggregationRequestsResponse
  pHttpStatus_ =
    DescribePendingAggregationRequestsResponse'
      { nextToken =
          Core.Nothing,
        pendingAggregationRequests =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describePendingAggregationRequestsResponse_nextToken :: Lens.Lens' DescribePendingAggregationRequestsResponse (Core.Maybe Core.Text)
describePendingAggregationRequestsResponse_nextToken = Lens.lens (\DescribePendingAggregationRequestsResponse' {nextToken} -> nextToken) (\s@DescribePendingAggregationRequestsResponse' {} a -> s {nextToken = a} :: DescribePendingAggregationRequestsResponse)

-- | Returns a PendingAggregationRequests object.
describePendingAggregationRequestsResponse_pendingAggregationRequests :: Lens.Lens' DescribePendingAggregationRequestsResponse (Core.Maybe [PendingAggregationRequest])
describePendingAggregationRequestsResponse_pendingAggregationRequests = Lens.lens (\DescribePendingAggregationRequestsResponse' {pendingAggregationRequests} -> pendingAggregationRequests) (\s@DescribePendingAggregationRequestsResponse' {} a -> s {pendingAggregationRequests = a} :: DescribePendingAggregationRequestsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describePendingAggregationRequestsResponse_httpStatus :: Lens.Lens' DescribePendingAggregationRequestsResponse Core.Int
describePendingAggregationRequestsResponse_httpStatus = Lens.lens (\DescribePendingAggregationRequestsResponse' {httpStatus} -> httpStatus) (\s@DescribePendingAggregationRequestsResponse' {} a -> s {httpStatus = a} :: DescribePendingAggregationRequestsResponse)

instance
  Core.NFData
    DescribePendingAggregationRequestsResponse
