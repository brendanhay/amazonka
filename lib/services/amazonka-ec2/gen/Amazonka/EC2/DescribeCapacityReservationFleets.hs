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
-- Module      : Amazonka.EC2.DescribeCapacityReservationFleets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Capacity Reservation Fleets.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeCapacityReservationFleets
  ( -- * Creating a Request
    DescribeCapacityReservationFleets (..),
    newDescribeCapacityReservationFleets,

    -- * Request Lenses
    describeCapacityReservationFleets_capacityReservationFleetIds,
    describeCapacityReservationFleets_nextToken,
    describeCapacityReservationFleets_filters,
    describeCapacityReservationFleets_dryRun,
    describeCapacityReservationFleets_maxResults,

    -- * Destructuring the Response
    DescribeCapacityReservationFleetsResponse (..),
    newDescribeCapacityReservationFleetsResponse,

    -- * Response Lenses
    describeCapacityReservationFleetsResponse_nextToken,
    describeCapacityReservationFleetsResponse_capacityReservationFleets,
    describeCapacityReservationFleetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeCapacityReservationFleets' smart constructor.
data DescribeCapacityReservationFleets = DescribeCapacityReservationFleets'
  { -- | The IDs of the Capacity Reservation Fleets to describe.
    capacityReservationFleetIds :: Prelude.Maybe [Prelude.Text],
    -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more filters.
    --
    -- -   @state@ - The state of the Fleet (@submitted@ | @modifying@ |
    --     @active@ | @partially_fulfilled@ | @expiring@ | @expired@ |
    --     @cancelling@ | @cancelled@ | @failed@).
    --
    -- -   @instance-match-criteria@ - The instance matching criteria for the
    --     Fleet. Only @open@ is supported.
    --
    -- -   @tenancy@ - The tenancy of the Fleet (@default@ | @dedicated@).
    --
    -- -   @allocation-strategy@ - The allocation strategy used by the Fleet.
    --     Only @prioritized@ is supported.
    filters :: Prelude.Maybe [Filter],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results can be seen by sending another request with
    -- the returned @nextToken@ value. This value can be between 5 and 500. If
    -- @maxResults@ is given a larger value than 500, you receive an error.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCapacityReservationFleets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityReservationFleetIds', 'describeCapacityReservationFleets_capacityReservationFleetIds' - The IDs of the Capacity Reservation Fleets to describe.
--
-- 'nextToken', 'describeCapacityReservationFleets_nextToken' - The token to use to retrieve the next page of results.
--
-- 'filters', 'describeCapacityReservationFleets_filters' - One or more filters.
--
-- -   @state@ - The state of the Fleet (@submitted@ | @modifying@ |
--     @active@ | @partially_fulfilled@ | @expiring@ | @expired@ |
--     @cancelling@ | @cancelled@ | @failed@).
--
-- -   @instance-match-criteria@ - The instance matching criteria for the
--     Fleet. Only @open@ is supported.
--
-- -   @tenancy@ - The tenancy of the Fleet (@default@ | @dedicated@).
--
-- -   @allocation-strategy@ - The allocation strategy used by the Fleet.
--     Only @prioritized@ is supported.
--
-- 'dryRun', 'describeCapacityReservationFleets_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeCapacityReservationFleets_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the returned @nextToken@ value. This value can be between 5 and 500. If
-- @maxResults@ is given a larger value than 500, you receive an error.
newDescribeCapacityReservationFleets ::
  DescribeCapacityReservationFleets
newDescribeCapacityReservationFleets =
  DescribeCapacityReservationFleets'
    { capacityReservationFleetIds =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The IDs of the Capacity Reservation Fleets to describe.
describeCapacityReservationFleets_capacityReservationFleetIds :: Lens.Lens' DescribeCapacityReservationFleets (Prelude.Maybe [Prelude.Text])
describeCapacityReservationFleets_capacityReservationFleetIds = Lens.lens (\DescribeCapacityReservationFleets' {capacityReservationFleetIds} -> capacityReservationFleetIds) (\s@DescribeCapacityReservationFleets' {} a -> s {capacityReservationFleetIds = a} :: DescribeCapacityReservationFleets) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results.
describeCapacityReservationFleets_nextToken :: Lens.Lens' DescribeCapacityReservationFleets (Prelude.Maybe Prelude.Text)
describeCapacityReservationFleets_nextToken = Lens.lens (\DescribeCapacityReservationFleets' {nextToken} -> nextToken) (\s@DescribeCapacityReservationFleets' {} a -> s {nextToken = a} :: DescribeCapacityReservationFleets)

-- | One or more filters.
--
-- -   @state@ - The state of the Fleet (@submitted@ | @modifying@ |
--     @active@ | @partially_fulfilled@ | @expiring@ | @expired@ |
--     @cancelling@ | @cancelled@ | @failed@).
--
-- -   @instance-match-criteria@ - The instance matching criteria for the
--     Fleet. Only @open@ is supported.
--
-- -   @tenancy@ - The tenancy of the Fleet (@default@ | @dedicated@).
--
-- -   @allocation-strategy@ - The allocation strategy used by the Fleet.
--     Only @prioritized@ is supported.
describeCapacityReservationFleets_filters :: Lens.Lens' DescribeCapacityReservationFleets (Prelude.Maybe [Filter])
describeCapacityReservationFleets_filters = Lens.lens (\DescribeCapacityReservationFleets' {filters} -> filters) (\s@DescribeCapacityReservationFleets' {} a -> s {filters = a} :: DescribeCapacityReservationFleets) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeCapacityReservationFleets_dryRun :: Lens.Lens' DescribeCapacityReservationFleets (Prelude.Maybe Prelude.Bool)
describeCapacityReservationFleets_dryRun = Lens.lens (\DescribeCapacityReservationFleets' {dryRun} -> dryRun) (\s@DescribeCapacityReservationFleets' {} a -> s {dryRun = a} :: DescribeCapacityReservationFleets)

-- | The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the returned @nextToken@ value. This value can be between 5 and 500. If
-- @maxResults@ is given a larger value than 500, you receive an error.
describeCapacityReservationFleets_maxResults :: Lens.Lens' DescribeCapacityReservationFleets (Prelude.Maybe Prelude.Natural)
describeCapacityReservationFleets_maxResults = Lens.lens (\DescribeCapacityReservationFleets' {maxResults} -> maxResults) (\s@DescribeCapacityReservationFleets' {} a -> s {maxResults = a} :: DescribeCapacityReservationFleets)

instance
  Core.AWSPager
    DescribeCapacityReservationFleets
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeCapacityReservationFleetsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeCapacityReservationFleetsResponse_capacityReservationFleets
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeCapacityReservationFleets_nextToken
          Lens..~ rs
          Lens.^? describeCapacityReservationFleetsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeCapacityReservationFleets
  where
  type
    AWSResponse DescribeCapacityReservationFleets =
      DescribeCapacityReservationFleetsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeCapacityReservationFleetsResponse'
            Prelude.<$> (x Core..@? "nextToken")
              Prelude.<*> ( x Core..@? "capacityReservationFleetSet"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Core.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeCapacityReservationFleets
  where
  hashWithSalt
    _salt
    DescribeCapacityReservationFleets' {..} =
      _salt
        `Prelude.hashWithSalt` capacityReservationFleetIds
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` maxResults

instance
  Prelude.NFData
    DescribeCapacityReservationFleets
  where
  rnf DescribeCapacityReservationFleets' {..} =
    Prelude.rnf capacityReservationFleetIds
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults

instance
  Core.ToHeaders
    DescribeCapacityReservationFleets
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DescribeCapacityReservationFleets
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeCapacityReservationFleets
  where
  toQuery DescribeCapacityReservationFleets' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeCapacityReservationFleets" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          ( Core.toQueryList "CapacityReservationFleetId"
              Prelude.<$> capacityReservationFleetIds
          ),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newDescribeCapacityReservationFleetsResponse' smart constructor.
data DescribeCapacityReservationFleetsResponse = DescribeCapacityReservationFleetsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the Capacity Reservation Fleets.
    capacityReservationFleets :: Prelude.Maybe [CapacityReservationFleet],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCapacityReservationFleetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeCapacityReservationFleetsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'capacityReservationFleets', 'describeCapacityReservationFleetsResponse_capacityReservationFleets' - Information about the Capacity Reservation Fleets.
--
-- 'httpStatus', 'describeCapacityReservationFleetsResponse_httpStatus' - The response's http status code.
newDescribeCapacityReservationFleetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCapacityReservationFleetsResponse
newDescribeCapacityReservationFleetsResponse
  pHttpStatus_ =
    DescribeCapacityReservationFleetsResponse'
      { nextToken =
          Prelude.Nothing,
        capacityReservationFleets =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeCapacityReservationFleetsResponse_nextToken :: Lens.Lens' DescribeCapacityReservationFleetsResponse (Prelude.Maybe Prelude.Text)
describeCapacityReservationFleetsResponse_nextToken = Lens.lens (\DescribeCapacityReservationFleetsResponse' {nextToken} -> nextToken) (\s@DescribeCapacityReservationFleetsResponse' {} a -> s {nextToken = a} :: DescribeCapacityReservationFleetsResponse)

-- | Information about the Capacity Reservation Fleets.
describeCapacityReservationFleetsResponse_capacityReservationFleets :: Lens.Lens' DescribeCapacityReservationFleetsResponse (Prelude.Maybe [CapacityReservationFleet])
describeCapacityReservationFleetsResponse_capacityReservationFleets = Lens.lens (\DescribeCapacityReservationFleetsResponse' {capacityReservationFleets} -> capacityReservationFleets) (\s@DescribeCapacityReservationFleetsResponse' {} a -> s {capacityReservationFleets = a} :: DescribeCapacityReservationFleetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeCapacityReservationFleetsResponse_httpStatus :: Lens.Lens' DescribeCapacityReservationFleetsResponse Prelude.Int
describeCapacityReservationFleetsResponse_httpStatus = Lens.lens (\DescribeCapacityReservationFleetsResponse' {httpStatus} -> httpStatus) (\s@DescribeCapacityReservationFleetsResponse' {} a -> s {httpStatus = a} :: DescribeCapacityReservationFleetsResponse)

instance
  Prelude.NFData
    DescribeCapacityReservationFleetsResponse
  where
  rnf DescribeCapacityReservationFleetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf capacityReservationFleets
      `Prelude.seq` Prelude.rnf httpStatus
