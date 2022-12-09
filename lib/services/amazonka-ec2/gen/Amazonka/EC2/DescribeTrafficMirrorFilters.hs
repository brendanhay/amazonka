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
-- Module      : Amazonka.EC2.DescribeTrafficMirrorFilters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Traffic Mirror filters.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeTrafficMirrorFilters
  ( -- * Creating a Request
    DescribeTrafficMirrorFilters (..),
    newDescribeTrafficMirrorFilters,

    -- * Request Lenses
    describeTrafficMirrorFilters_dryRun,
    describeTrafficMirrorFilters_filters,
    describeTrafficMirrorFilters_maxResults,
    describeTrafficMirrorFilters_nextToken,
    describeTrafficMirrorFilters_trafficMirrorFilterIds,

    -- * Destructuring the Response
    DescribeTrafficMirrorFiltersResponse (..),
    newDescribeTrafficMirrorFiltersResponse,

    -- * Response Lenses
    describeTrafficMirrorFiltersResponse_nextToken,
    describeTrafficMirrorFiltersResponse_trafficMirrorFilters,
    describeTrafficMirrorFiltersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTrafficMirrorFilters' smart constructor.
data DescribeTrafficMirrorFilters = DescribeTrafficMirrorFilters'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters. The possible values are:
    --
    -- -   @description@: The Traffic Mirror filter description.
    --
    -- -   @traffic-mirror-filter-id@: The ID of the Traffic Mirror filter.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Traffic Mirror filter.
    trafficMirrorFilterIds :: Prelude.Maybe [Prelude.Text]
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
-- 'dryRun', 'describeTrafficMirrorFilters_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeTrafficMirrorFilters_filters' - One or more filters. The possible values are:
--
-- -   @description@: The Traffic Mirror filter description.
--
-- -   @traffic-mirror-filter-id@: The ID of the Traffic Mirror filter.
--
-- 'maxResults', 'describeTrafficMirrorFilters_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeTrafficMirrorFilters_nextToken' - The token for the next page of results.
--
-- 'trafficMirrorFilterIds', 'describeTrafficMirrorFilters_trafficMirrorFilterIds' - The ID of the Traffic Mirror filter.
newDescribeTrafficMirrorFilters ::
  DescribeTrafficMirrorFilters
newDescribeTrafficMirrorFilters =
  DescribeTrafficMirrorFilters'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      trafficMirrorFilterIds = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeTrafficMirrorFilters_dryRun :: Lens.Lens' DescribeTrafficMirrorFilters (Prelude.Maybe Prelude.Bool)
describeTrafficMirrorFilters_dryRun = Lens.lens (\DescribeTrafficMirrorFilters' {dryRun} -> dryRun) (\s@DescribeTrafficMirrorFilters' {} a -> s {dryRun = a} :: DescribeTrafficMirrorFilters)

-- | One or more filters. The possible values are:
--
-- -   @description@: The Traffic Mirror filter description.
--
-- -   @traffic-mirror-filter-id@: The ID of the Traffic Mirror filter.
describeTrafficMirrorFilters_filters :: Lens.Lens' DescribeTrafficMirrorFilters (Prelude.Maybe [Filter])
describeTrafficMirrorFilters_filters = Lens.lens (\DescribeTrafficMirrorFilters' {filters} -> filters) (\s@DescribeTrafficMirrorFilters' {} a -> s {filters = a} :: DescribeTrafficMirrorFilters) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeTrafficMirrorFilters_maxResults :: Lens.Lens' DescribeTrafficMirrorFilters (Prelude.Maybe Prelude.Natural)
describeTrafficMirrorFilters_maxResults = Lens.lens (\DescribeTrafficMirrorFilters' {maxResults} -> maxResults) (\s@DescribeTrafficMirrorFilters' {} a -> s {maxResults = a} :: DescribeTrafficMirrorFilters)

-- | The token for the next page of results.
describeTrafficMirrorFilters_nextToken :: Lens.Lens' DescribeTrafficMirrorFilters (Prelude.Maybe Prelude.Text)
describeTrafficMirrorFilters_nextToken = Lens.lens (\DescribeTrafficMirrorFilters' {nextToken} -> nextToken) (\s@DescribeTrafficMirrorFilters' {} a -> s {nextToken = a} :: DescribeTrafficMirrorFilters)

-- | The ID of the Traffic Mirror filter.
describeTrafficMirrorFilters_trafficMirrorFilterIds :: Lens.Lens' DescribeTrafficMirrorFilters (Prelude.Maybe [Prelude.Text])
describeTrafficMirrorFilters_trafficMirrorFilterIds = Lens.lens (\DescribeTrafficMirrorFilters' {trafficMirrorFilterIds} -> trafficMirrorFilterIds) (\s@DescribeTrafficMirrorFilters' {} a -> s {trafficMirrorFilterIds = a} :: DescribeTrafficMirrorFilters) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTrafficMirrorFiltersResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x Data..@? "trafficMirrorFilterSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTrafficMirrorFilters
  where
  hashWithSalt _salt DescribeTrafficMirrorFilters' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` trafficMirrorFilterIds

instance Prelude.NFData DescribeTrafficMirrorFilters where
  rnf DescribeTrafficMirrorFilters' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf trafficMirrorFilterIds

instance Data.ToHeaders DescribeTrafficMirrorFilters where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeTrafficMirrorFilters where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTrafficMirrorFilters where
  toQuery DescribeTrafficMirrorFilters' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeTrafficMirrorFilters" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        Data.toQuery
          ( Data.toQueryList "TrafficMirrorFilterId"
              Prelude.<$> trafficMirrorFilterIds
          )
      ]

-- | /See:/ 'newDescribeTrafficMirrorFiltersResponse' smart constructor.
data DescribeTrafficMirrorFiltersResponse = DescribeTrafficMirrorFiltersResponse'
  { -- | The token to use to retrieve the next page of results. The value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about one or more Traffic Mirror filters.
    trafficMirrorFilters :: Prelude.Maybe [TrafficMirrorFilter],
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
-- 'nextToken', 'describeTrafficMirrorFiltersResponse_nextToken' - The token to use to retrieve the next page of results. The value is
-- @null@ when there are no more results to return.
--
-- 'trafficMirrorFilters', 'describeTrafficMirrorFiltersResponse_trafficMirrorFilters' - Information about one or more Traffic Mirror filters.
--
-- 'httpStatus', 'describeTrafficMirrorFiltersResponse_httpStatus' - The response's http status code.
newDescribeTrafficMirrorFiltersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTrafficMirrorFiltersResponse
newDescribeTrafficMirrorFiltersResponse pHttpStatus_ =
  DescribeTrafficMirrorFiltersResponse'
    { nextToken =
        Prelude.Nothing,
      trafficMirrorFilters =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. The value is
-- @null@ when there are no more results to return.
describeTrafficMirrorFiltersResponse_nextToken :: Lens.Lens' DescribeTrafficMirrorFiltersResponse (Prelude.Maybe Prelude.Text)
describeTrafficMirrorFiltersResponse_nextToken = Lens.lens (\DescribeTrafficMirrorFiltersResponse' {nextToken} -> nextToken) (\s@DescribeTrafficMirrorFiltersResponse' {} a -> s {nextToken = a} :: DescribeTrafficMirrorFiltersResponse)

-- | Information about one or more Traffic Mirror filters.
describeTrafficMirrorFiltersResponse_trafficMirrorFilters :: Lens.Lens' DescribeTrafficMirrorFiltersResponse (Prelude.Maybe [TrafficMirrorFilter])
describeTrafficMirrorFiltersResponse_trafficMirrorFilters = Lens.lens (\DescribeTrafficMirrorFiltersResponse' {trafficMirrorFilters} -> trafficMirrorFilters) (\s@DescribeTrafficMirrorFiltersResponse' {} a -> s {trafficMirrorFilters = a} :: DescribeTrafficMirrorFiltersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTrafficMirrorFiltersResponse_httpStatus :: Lens.Lens' DescribeTrafficMirrorFiltersResponse Prelude.Int
describeTrafficMirrorFiltersResponse_httpStatus = Lens.lens (\DescribeTrafficMirrorFiltersResponse' {httpStatus} -> httpStatus) (\s@DescribeTrafficMirrorFiltersResponse' {} a -> s {httpStatus = a} :: DescribeTrafficMirrorFiltersResponse)

instance
  Prelude.NFData
    DescribeTrafficMirrorFiltersResponse
  where
  rnf DescribeTrafficMirrorFiltersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf trafficMirrorFilters
      `Prelude.seq` Prelude.rnf httpStatus
