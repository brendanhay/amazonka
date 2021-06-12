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
-- Module      : Network.AWS.Config.DescribeAggregationAuthorizations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of authorizations granted to various aggregator accounts
-- and regions.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeAggregationAuthorizations
  ( -- * Creating a Request
    DescribeAggregationAuthorizations (..),
    newDescribeAggregationAuthorizations,

    -- * Request Lenses
    describeAggregationAuthorizations_nextToken,
    describeAggregationAuthorizations_limit,

    -- * Destructuring the Response
    DescribeAggregationAuthorizationsResponse (..),
    newDescribeAggregationAuthorizationsResponse,

    -- * Response Lenses
    describeAggregationAuthorizationsResponse_nextToken,
    describeAggregationAuthorizationsResponse_aggregationAuthorizations,
    describeAggregationAuthorizationsResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAggregationAuthorizations' smart constructor.
data DescribeAggregationAuthorizations = DescribeAggregationAuthorizations'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of AggregationAuthorizations returned on each page.
    -- The default is maximum. If you specify 0, AWS Config uses the default.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAggregationAuthorizations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAggregationAuthorizations_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'limit', 'describeAggregationAuthorizations_limit' - The maximum number of AggregationAuthorizations returned on each page.
-- The default is maximum. If you specify 0, AWS Config uses the default.
newDescribeAggregationAuthorizations ::
  DescribeAggregationAuthorizations
newDescribeAggregationAuthorizations =
  DescribeAggregationAuthorizations'
    { nextToken =
        Core.Nothing,
      limit = Core.Nothing
    }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeAggregationAuthorizations_nextToken :: Lens.Lens' DescribeAggregationAuthorizations (Core.Maybe Core.Text)
describeAggregationAuthorizations_nextToken = Lens.lens (\DescribeAggregationAuthorizations' {nextToken} -> nextToken) (\s@DescribeAggregationAuthorizations' {} a -> s {nextToken = a} :: DescribeAggregationAuthorizations)

-- | The maximum number of AggregationAuthorizations returned on each page.
-- The default is maximum. If you specify 0, AWS Config uses the default.
describeAggregationAuthorizations_limit :: Lens.Lens' DescribeAggregationAuthorizations (Core.Maybe Core.Natural)
describeAggregationAuthorizations_limit = Lens.lens (\DescribeAggregationAuthorizations' {limit} -> limit) (\s@DescribeAggregationAuthorizations' {} a -> s {limit = a} :: DescribeAggregationAuthorizations)

instance
  Core.AWSPager
    DescribeAggregationAuthorizations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAggregationAuthorizationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAggregationAuthorizationsResponse_aggregationAuthorizations
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeAggregationAuthorizations_nextToken
          Lens..~ rs
          Lens.^? describeAggregationAuthorizationsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeAggregationAuthorizations
  where
  type
    AWSResponse DescribeAggregationAuthorizations =
      DescribeAggregationAuthorizationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAggregationAuthorizationsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "AggregationAuthorizations"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeAggregationAuthorizations

instance
  Core.NFData
    DescribeAggregationAuthorizations

instance
  Core.ToHeaders
    DescribeAggregationAuthorizations
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeAggregationAuthorizations" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeAggregationAuthorizations
  where
  toJSON DescribeAggregationAuthorizations' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance
  Core.ToPath
    DescribeAggregationAuthorizations
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeAggregationAuthorizations
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeAggregationAuthorizationsResponse' smart constructor.
data DescribeAggregationAuthorizationsResponse = DescribeAggregationAuthorizationsResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | Returns a list of authorizations granted to various aggregator accounts
    -- and regions.
    aggregationAuthorizations :: Core.Maybe [AggregationAuthorization],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAggregationAuthorizationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAggregationAuthorizationsResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'aggregationAuthorizations', 'describeAggregationAuthorizationsResponse_aggregationAuthorizations' - Returns a list of authorizations granted to various aggregator accounts
-- and regions.
--
-- 'httpStatus', 'describeAggregationAuthorizationsResponse_httpStatus' - The response's http status code.
newDescribeAggregationAuthorizationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeAggregationAuthorizationsResponse
newDescribeAggregationAuthorizationsResponse
  pHttpStatus_ =
    DescribeAggregationAuthorizationsResponse'
      { nextToken =
          Core.Nothing,
        aggregationAuthorizations =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeAggregationAuthorizationsResponse_nextToken :: Lens.Lens' DescribeAggregationAuthorizationsResponse (Core.Maybe Core.Text)
describeAggregationAuthorizationsResponse_nextToken = Lens.lens (\DescribeAggregationAuthorizationsResponse' {nextToken} -> nextToken) (\s@DescribeAggregationAuthorizationsResponse' {} a -> s {nextToken = a} :: DescribeAggregationAuthorizationsResponse)

-- | Returns a list of authorizations granted to various aggregator accounts
-- and regions.
describeAggregationAuthorizationsResponse_aggregationAuthorizations :: Lens.Lens' DescribeAggregationAuthorizationsResponse (Core.Maybe [AggregationAuthorization])
describeAggregationAuthorizationsResponse_aggregationAuthorizations = Lens.lens (\DescribeAggregationAuthorizationsResponse' {aggregationAuthorizations} -> aggregationAuthorizations) (\s@DescribeAggregationAuthorizationsResponse' {} a -> s {aggregationAuthorizations = a} :: DescribeAggregationAuthorizationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAggregationAuthorizationsResponse_httpStatus :: Lens.Lens' DescribeAggregationAuthorizationsResponse Core.Int
describeAggregationAuthorizationsResponse_httpStatus = Lens.lens (\DescribeAggregationAuthorizationsResponse' {httpStatus} -> httpStatus) (\s@DescribeAggregationAuthorizationsResponse' {} a -> s {httpStatus = a} :: DescribeAggregationAuthorizationsResponse)

instance
  Core.NFData
    DescribeAggregationAuthorizationsResponse
