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
-- Module      : Network.AWS.EC2.DescribeTrafficMirrorFilters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Traffic Mirror filters.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTrafficMirrorFilters
  ( -- * Creating a Request
    DescribeTrafficMirrorFilters (..),
    newDescribeTrafficMirrorFilters,

    -- * Request Lenses
    describeTrafficMirrorFilters_nextToken,
    describeTrafficMirrorFilters_dryRun,
    describeTrafficMirrorFilters_maxResults,
    describeTrafficMirrorFilters_trafficMirrorFilterIds,
    describeTrafficMirrorFilters_filters,

    -- * Destructuring the Response
    DescribeTrafficMirrorFiltersResponse (..),
    newDescribeTrafficMirrorFiltersResponse,

    -- * Response Lenses
    describeTrafficMirrorFiltersResponse_trafficMirrorFilters,
    describeTrafficMirrorFiltersResponse_nextToken,
    describeTrafficMirrorFiltersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTrafficMirrorFilters' smart constructor.
data DescribeTrafficMirrorFilters = DescribeTrafficMirrorFilters'
  { -- | The token for the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The ID of the Traffic Mirror filter.
    trafficMirrorFilterIds :: Core.Maybe [Core.Text],
    -- | One or more filters. The possible values are:
    --
    -- -   @description@: The Traffic Mirror filter description.
    --
    -- -   @traffic-mirror-filter-id@: The ID of the Traffic Mirror filter.
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTrafficMirrorFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTrafficMirrorFilters_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeTrafficMirrorFilters_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeTrafficMirrorFilters_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'trafficMirrorFilterIds', 'describeTrafficMirrorFilters_trafficMirrorFilterIds' - The ID of the Traffic Mirror filter.
--
-- 'filters', 'describeTrafficMirrorFilters_filters' - One or more filters. The possible values are:
--
-- -   @description@: The Traffic Mirror filter description.
--
-- -   @traffic-mirror-filter-id@: The ID of the Traffic Mirror filter.
newDescribeTrafficMirrorFilters ::
  DescribeTrafficMirrorFilters
newDescribeTrafficMirrorFilters =
  DescribeTrafficMirrorFilters'
    { nextToken =
        Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      trafficMirrorFilterIds = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token for the next page of results.
describeTrafficMirrorFilters_nextToken :: Lens.Lens' DescribeTrafficMirrorFilters (Core.Maybe Core.Text)
describeTrafficMirrorFilters_nextToken = Lens.lens (\DescribeTrafficMirrorFilters' {nextToken} -> nextToken) (\s@DescribeTrafficMirrorFilters' {} a -> s {nextToken = a} :: DescribeTrafficMirrorFilters)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeTrafficMirrorFilters_dryRun :: Lens.Lens' DescribeTrafficMirrorFilters (Core.Maybe Core.Bool)
describeTrafficMirrorFilters_dryRun = Lens.lens (\DescribeTrafficMirrorFilters' {dryRun} -> dryRun) (\s@DescribeTrafficMirrorFilters' {} a -> s {dryRun = a} :: DescribeTrafficMirrorFilters)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeTrafficMirrorFilters_maxResults :: Lens.Lens' DescribeTrafficMirrorFilters (Core.Maybe Core.Natural)
describeTrafficMirrorFilters_maxResults = Lens.lens (\DescribeTrafficMirrorFilters' {maxResults} -> maxResults) (\s@DescribeTrafficMirrorFilters' {} a -> s {maxResults = a} :: DescribeTrafficMirrorFilters)

-- | The ID of the Traffic Mirror filter.
describeTrafficMirrorFilters_trafficMirrorFilterIds :: Lens.Lens' DescribeTrafficMirrorFilters (Core.Maybe [Core.Text])
describeTrafficMirrorFilters_trafficMirrorFilterIds = Lens.lens (\DescribeTrafficMirrorFilters' {trafficMirrorFilterIds} -> trafficMirrorFilterIds) (\s@DescribeTrafficMirrorFilters' {} a -> s {trafficMirrorFilterIds = a} :: DescribeTrafficMirrorFilters) Core.. Lens.mapping Lens._Coerce

-- | One or more filters. The possible values are:
--
-- -   @description@: The Traffic Mirror filter description.
--
-- -   @traffic-mirror-filter-id@: The ID of the Traffic Mirror filter.
describeTrafficMirrorFilters_filters :: Lens.Lens' DescribeTrafficMirrorFilters (Core.Maybe [Filter])
describeTrafficMirrorFilters_filters = Lens.lens (\DescribeTrafficMirrorFilters' {filters} -> filters) (\s@DescribeTrafficMirrorFilters' {} a -> s {filters = a} :: DescribeTrafficMirrorFilters) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeTrafficMirrorFilters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTrafficMirrorFiltersResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTrafficMirrorFiltersResponse_trafficMirrorFilters
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeTrafficMirrorFilters_nextToken
          Lens..~ rs
          Lens.^? describeTrafficMirrorFiltersResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeTrafficMirrorFilters where
  type
    AWSResponse DescribeTrafficMirrorFilters =
      DescribeTrafficMirrorFiltersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTrafficMirrorFiltersResponse'
            Core.<$> ( x Core..@? "trafficMirrorFilterSet"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeTrafficMirrorFilters

instance Core.NFData DescribeTrafficMirrorFilters

instance Core.ToHeaders DescribeTrafficMirrorFilters where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeTrafficMirrorFilters where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTrafficMirrorFilters where
  toQuery DescribeTrafficMirrorFilters' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeTrafficMirrorFilters" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "TrafficMirrorFilterId"
              Core.<$> trafficMirrorFilterIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | /See:/ 'newDescribeTrafficMirrorFiltersResponse' smart constructor.
data DescribeTrafficMirrorFiltersResponse = DescribeTrafficMirrorFiltersResponse'
  { -- | Information about one or more Traffic Mirror filters.
    trafficMirrorFilters :: Core.Maybe [TrafficMirrorFilter],
    -- | The token to use to retrieve the next page of results. The value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTrafficMirrorFiltersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trafficMirrorFilters', 'describeTrafficMirrorFiltersResponse_trafficMirrorFilters' - Information about one or more Traffic Mirror filters.
--
-- 'nextToken', 'describeTrafficMirrorFiltersResponse_nextToken' - The token to use to retrieve the next page of results. The value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeTrafficMirrorFiltersResponse_httpStatus' - The response's http status code.
newDescribeTrafficMirrorFiltersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeTrafficMirrorFiltersResponse
newDescribeTrafficMirrorFiltersResponse pHttpStatus_ =
  DescribeTrafficMirrorFiltersResponse'
    { trafficMirrorFilters =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about one or more Traffic Mirror filters.
describeTrafficMirrorFiltersResponse_trafficMirrorFilters :: Lens.Lens' DescribeTrafficMirrorFiltersResponse (Core.Maybe [TrafficMirrorFilter])
describeTrafficMirrorFiltersResponse_trafficMirrorFilters = Lens.lens (\DescribeTrafficMirrorFiltersResponse' {trafficMirrorFilters} -> trafficMirrorFilters) (\s@DescribeTrafficMirrorFiltersResponse' {} a -> s {trafficMirrorFilters = a} :: DescribeTrafficMirrorFiltersResponse) Core.. Lens.mapping Lens._Coerce

-- | The token to use to retrieve the next page of results. The value is
-- @null@ when there are no more results to return.
describeTrafficMirrorFiltersResponse_nextToken :: Lens.Lens' DescribeTrafficMirrorFiltersResponse (Core.Maybe Core.Text)
describeTrafficMirrorFiltersResponse_nextToken = Lens.lens (\DescribeTrafficMirrorFiltersResponse' {nextToken} -> nextToken) (\s@DescribeTrafficMirrorFiltersResponse' {} a -> s {nextToken = a} :: DescribeTrafficMirrorFiltersResponse)

-- | The response's http status code.
describeTrafficMirrorFiltersResponse_httpStatus :: Lens.Lens' DescribeTrafficMirrorFiltersResponse Core.Int
describeTrafficMirrorFiltersResponse_httpStatus = Lens.lens (\DescribeTrafficMirrorFiltersResponse' {httpStatus} -> httpStatus) (\s@DescribeTrafficMirrorFiltersResponse' {} a -> s {httpStatus = a} :: DescribeTrafficMirrorFiltersResponse)

instance
  Core.NFData
    DescribeTrafficMirrorFiltersResponse
