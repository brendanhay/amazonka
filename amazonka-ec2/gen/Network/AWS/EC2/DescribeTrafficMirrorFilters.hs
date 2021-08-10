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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTrafficMirrorFilters' smart constructor.
data DescribeTrafficMirrorFilters = DescribeTrafficMirrorFilters'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the Traffic Mirror filter.
    trafficMirrorFilterIds :: Prelude.Maybe [Prelude.Text],
    -- | One or more filters. The possible values are:
    --
    -- -   @description@: The Traffic Mirror filter description.
    --
    -- -   @traffic-mirror-filter-id@: The ID of the Traffic Mirror filter.
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      trafficMirrorFilterIds = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The token for the next page of results.
describeTrafficMirrorFilters_nextToken :: Lens.Lens' DescribeTrafficMirrorFilters (Prelude.Maybe Prelude.Text)
describeTrafficMirrorFilters_nextToken = Lens.lens (\DescribeTrafficMirrorFilters' {nextToken} -> nextToken) (\s@DescribeTrafficMirrorFilters' {} a -> s {nextToken = a} :: DescribeTrafficMirrorFilters)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeTrafficMirrorFilters_dryRun :: Lens.Lens' DescribeTrafficMirrorFilters (Prelude.Maybe Prelude.Bool)
describeTrafficMirrorFilters_dryRun = Lens.lens (\DescribeTrafficMirrorFilters' {dryRun} -> dryRun) (\s@DescribeTrafficMirrorFilters' {} a -> s {dryRun = a} :: DescribeTrafficMirrorFilters)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeTrafficMirrorFilters_maxResults :: Lens.Lens' DescribeTrafficMirrorFilters (Prelude.Maybe Prelude.Natural)
describeTrafficMirrorFilters_maxResults = Lens.lens (\DescribeTrafficMirrorFilters' {maxResults} -> maxResults) (\s@DescribeTrafficMirrorFilters' {} a -> s {maxResults = a} :: DescribeTrafficMirrorFilters)

-- | The ID of the Traffic Mirror filter.
describeTrafficMirrorFilters_trafficMirrorFilterIds :: Lens.Lens' DescribeTrafficMirrorFilters (Prelude.Maybe [Prelude.Text])
describeTrafficMirrorFilters_trafficMirrorFilterIds = Lens.lens (\DescribeTrafficMirrorFilters' {trafficMirrorFilterIds} -> trafficMirrorFilterIds) (\s@DescribeTrafficMirrorFilters' {} a -> s {trafficMirrorFilterIds = a} :: DescribeTrafficMirrorFilters) Prelude.. Lens.mapping Lens._Coerce

-- | One or more filters. The possible values are:
--
-- -   @description@: The Traffic Mirror filter description.
--
-- -   @traffic-mirror-filter-id@: The ID of the Traffic Mirror filter.
describeTrafficMirrorFilters_filters :: Lens.Lens' DescribeTrafficMirrorFilters (Prelude.Maybe [Filter])
describeTrafficMirrorFilters_filters = Lens.lens (\DescribeTrafficMirrorFilters' {filters} -> filters) (\s@DescribeTrafficMirrorFilters' {} a -> s {filters = a} :: DescribeTrafficMirrorFilters) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeTrafficMirrorFilters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTrafficMirrorFiltersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTrafficMirrorFiltersResponse_trafficMirrorFilters
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeTrafficMirrorFilters_nextToken
          Lens..~ rs
          Lens.^? describeTrafficMirrorFiltersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeTrafficMirrorFilters where
  type
    AWSResponse DescribeTrafficMirrorFilters =
      DescribeTrafficMirrorFiltersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTrafficMirrorFiltersResponse'
            Prelude.<$> ( x Core..@? "trafficMirrorFilterSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (x Core..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTrafficMirrorFilters

instance Prelude.NFData DescribeTrafficMirrorFilters

instance Core.ToHeaders DescribeTrafficMirrorFilters where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeTrafficMirrorFilters where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeTrafficMirrorFilters where
  toQuery DescribeTrafficMirrorFilters' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeTrafficMirrorFilters" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "TrafficMirrorFilterId"
              Prelude.<$> trafficMirrorFilterIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters)
      ]

-- | /See:/ 'newDescribeTrafficMirrorFiltersResponse' smart constructor.
data DescribeTrafficMirrorFiltersResponse = DescribeTrafficMirrorFiltersResponse'
  { -- | Information about one or more Traffic Mirror filters.
    trafficMirrorFilters :: Prelude.Maybe [TrafficMirrorFilter],
    -- | The token to use to retrieve the next page of results. The value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeTrafficMirrorFiltersResponse
newDescribeTrafficMirrorFiltersResponse pHttpStatus_ =
  DescribeTrafficMirrorFiltersResponse'
    { trafficMirrorFilters =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about one or more Traffic Mirror filters.
describeTrafficMirrorFiltersResponse_trafficMirrorFilters :: Lens.Lens' DescribeTrafficMirrorFiltersResponse (Prelude.Maybe [TrafficMirrorFilter])
describeTrafficMirrorFiltersResponse_trafficMirrorFilters = Lens.lens (\DescribeTrafficMirrorFiltersResponse' {trafficMirrorFilters} -> trafficMirrorFilters) (\s@DescribeTrafficMirrorFiltersResponse' {} a -> s {trafficMirrorFilters = a} :: DescribeTrafficMirrorFiltersResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The token to use to retrieve the next page of results. The value is
-- @null@ when there are no more results to return.
describeTrafficMirrorFiltersResponse_nextToken :: Lens.Lens' DescribeTrafficMirrorFiltersResponse (Prelude.Maybe Prelude.Text)
describeTrafficMirrorFiltersResponse_nextToken = Lens.lens (\DescribeTrafficMirrorFiltersResponse' {nextToken} -> nextToken) (\s@DescribeTrafficMirrorFiltersResponse' {} a -> s {nextToken = a} :: DescribeTrafficMirrorFiltersResponse)

-- | The response's http status code.
describeTrafficMirrorFiltersResponse_httpStatus :: Lens.Lens' DescribeTrafficMirrorFiltersResponse Prelude.Int
describeTrafficMirrorFiltersResponse_httpStatus = Lens.lens (\DescribeTrafficMirrorFiltersResponse' {httpStatus} -> httpStatus) (\s@DescribeTrafficMirrorFiltersResponse' {} a -> s {httpStatus = a} :: DescribeTrafficMirrorFiltersResponse)

instance
  Prelude.NFData
    DescribeTrafficMirrorFiltersResponse
