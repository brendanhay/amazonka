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
-- Module      : Network.AWS.EC2.DescribeCapacityReservations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your Capacity Reservations. The results
-- describe only the Capacity Reservations in the AWS Region that you\'re
-- currently using.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeCapacityReservations
  ( -- * Creating a Request
    DescribeCapacityReservations (..),
    newDescribeCapacityReservations,

    -- * Request Lenses
    describeCapacityReservations_nextToken,
    describeCapacityReservations_dryRun,
    describeCapacityReservations_maxResults,
    describeCapacityReservations_capacityReservationIds,
    describeCapacityReservations_filters,

    -- * Destructuring the Response
    DescribeCapacityReservationsResponse (..),
    newDescribeCapacityReservationsResponse,

    -- * Response Lenses
    describeCapacityReservationsResponse_capacityReservations,
    describeCapacityReservationsResponse_nextToken,
    describeCapacityReservationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeCapacityReservations' smart constructor.
data DescribeCapacityReservations = DescribeCapacityReservations'
  { -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results can be seen by sending another request with
    -- the returned @nextToken@ value. This value can be between 5 and 500. If
    -- @maxResults@ is given a larger value than 500, you receive an error.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the Capacity Reservation.
    capacityReservationIds :: Prelude.Maybe [Prelude.Text],
    -- | One or more filters.
    --
    -- -   @instance-type@ - The type of instance for which the Capacity
    --     Reservation reserves capacity.
    --
    -- -   @owner-id@ - The ID of the AWS account that owns the Capacity
    --     Reservation.
    --
    -- -   @availability-zone-id@ - The Availability Zone ID of the Capacity
    --     Reservation.
    --
    -- -   @instance-platform@ - The type of operating system for which the
    --     Capacity Reservation reserves capacity.
    --
    -- -   @availability-zone@ - The Availability Zone ID of the Capacity
    --     Reservation.
    --
    -- -   @tenancy@ - Indicates the tenancy of the Capacity Reservation. A
    --     Capacity Reservation can have one of the following tenancy settings:
    --
    --     -   @default@ - The Capacity Reservation is created on hardware that
    --         is shared with other AWS accounts.
    --
    --     -   @dedicated@ - The Capacity Reservation is created on
    --         single-tenant hardware that is dedicated to a single AWS
    --         account.
    --
    -- -   @state@ - The current state of the Capacity Reservation. A Capacity
    --     Reservation can be in one of the following states:
    --
    --     -   @active@- The Capacity Reservation is active and the capacity is
    --         available for your use.
    --
    --     -   @expired@ - The Capacity Reservation expired automatically at
    --         the date and time specified in your request. The reserved
    --         capacity is no longer available for your use.
    --
    --     -   @cancelled@ - The Capacity Reservation was cancelled. The
    --         reserved capacity is no longer available for your use.
    --
    --     -   @pending@ - The Capacity Reservation request was successful but
    --         the capacity provisioning is still pending.
    --
    --     -   @failed@ - The Capacity Reservation request has failed. A
    --         request might fail due to invalid request parameters, capacity
    --         constraints, or instance limit constraints. Failed requests are
    --         retained for 60 minutes.
    --
    -- -   @end-date@ - The date and time at which the Capacity Reservation
    --     expires. When a Capacity Reservation expires, the reserved capacity
    --     is released and you can no longer launch instances into it. The
    --     Capacity Reservation\'s state changes to expired when it reaches its
    --     end date and time.
    --
    -- -   @end-date-type@ - Indicates the way in which the Capacity
    --     Reservation ends. A Capacity Reservation can have one of the
    --     following end types:
    --
    --     -   @unlimited@ - The Capacity Reservation remains active until you
    --         explicitly cancel it.
    --
    --     -   @limited@ - The Capacity Reservation expires automatically at a
    --         specified date and time.
    --
    -- -   @instance-match-criteria@ - Indicates the type of instance launches
    --     that the Capacity Reservation accepts. The options include:
    --
    --     -   @open@ - The Capacity Reservation accepts all instances that
    --         have matching attributes (instance type, platform, and
    --         Availability Zone). Instances that have matching attributes
    --         launch into the Capacity Reservation automatically without
    --         specifying any additional parameters.
    --
    --     -   @targeted@ - The Capacity Reservation only accepts instances
    --         that have matching attributes (instance type, platform, and
    --         Availability Zone), and explicitly target the Capacity
    --         Reservation. This ensures that only permitted instances can use
    --         the reserved capacity.
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCapacityReservations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeCapacityReservations_nextToken' - The token to use to retrieve the next page of results.
--
-- 'dryRun', 'describeCapacityReservations_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeCapacityReservations_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the returned @nextToken@ value. This value can be between 5 and 500. If
-- @maxResults@ is given a larger value than 500, you receive an error.
--
-- 'capacityReservationIds', 'describeCapacityReservations_capacityReservationIds' - The ID of the Capacity Reservation.
--
-- 'filters', 'describeCapacityReservations_filters' - One or more filters.
--
-- -   @instance-type@ - The type of instance for which the Capacity
--     Reservation reserves capacity.
--
-- -   @owner-id@ - The ID of the AWS account that owns the Capacity
--     Reservation.
--
-- -   @availability-zone-id@ - The Availability Zone ID of the Capacity
--     Reservation.
--
-- -   @instance-platform@ - The type of operating system for which the
--     Capacity Reservation reserves capacity.
--
-- -   @availability-zone@ - The Availability Zone ID of the Capacity
--     Reservation.
--
-- -   @tenancy@ - Indicates the tenancy of the Capacity Reservation. A
--     Capacity Reservation can have one of the following tenancy settings:
--
--     -   @default@ - The Capacity Reservation is created on hardware that
--         is shared with other AWS accounts.
--
--     -   @dedicated@ - The Capacity Reservation is created on
--         single-tenant hardware that is dedicated to a single AWS
--         account.
--
-- -   @state@ - The current state of the Capacity Reservation. A Capacity
--     Reservation can be in one of the following states:
--
--     -   @active@- The Capacity Reservation is active and the capacity is
--         available for your use.
--
--     -   @expired@ - The Capacity Reservation expired automatically at
--         the date and time specified in your request. The reserved
--         capacity is no longer available for your use.
--
--     -   @cancelled@ - The Capacity Reservation was cancelled. The
--         reserved capacity is no longer available for your use.
--
--     -   @pending@ - The Capacity Reservation request was successful but
--         the capacity provisioning is still pending.
--
--     -   @failed@ - The Capacity Reservation request has failed. A
--         request might fail due to invalid request parameters, capacity
--         constraints, or instance limit constraints. Failed requests are
--         retained for 60 minutes.
--
-- -   @end-date@ - The date and time at which the Capacity Reservation
--     expires. When a Capacity Reservation expires, the reserved capacity
--     is released and you can no longer launch instances into it. The
--     Capacity Reservation\'s state changes to expired when it reaches its
--     end date and time.
--
-- -   @end-date-type@ - Indicates the way in which the Capacity
--     Reservation ends. A Capacity Reservation can have one of the
--     following end types:
--
--     -   @unlimited@ - The Capacity Reservation remains active until you
--         explicitly cancel it.
--
--     -   @limited@ - The Capacity Reservation expires automatically at a
--         specified date and time.
--
-- -   @instance-match-criteria@ - Indicates the type of instance launches
--     that the Capacity Reservation accepts. The options include:
--
--     -   @open@ - The Capacity Reservation accepts all instances that
--         have matching attributes (instance type, platform, and
--         Availability Zone). Instances that have matching attributes
--         launch into the Capacity Reservation automatically without
--         specifying any additional parameters.
--
--     -   @targeted@ - The Capacity Reservation only accepts instances
--         that have matching attributes (instance type, platform, and
--         Availability Zone), and explicitly target the Capacity
--         Reservation. This ensures that only permitted instances can use
--         the reserved capacity.
newDescribeCapacityReservations ::
  DescribeCapacityReservations
newDescribeCapacityReservations =
  DescribeCapacityReservations'
    { nextToken =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      capacityReservationIds = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The token to use to retrieve the next page of results.
describeCapacityReservations_nextToken :: Lens.Lens' DescribeCapacityReservations (Prelude.Maybe Prelude.Text)
describeCapacityReservations_nextToken = Lens.lens (\DescribeCapacityReservations' {nextToken} -> nextToken) (\s@DescribeCapacityReservations' {} a -> s {nextToken = a} :: DescribeCapacityReservations)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeCapacityReservations_dryRun :: Lens.Lens' DescribeCapacityReservations (Prelude.Maybe Prelude.Bool)
describeCapacityReservations_dryRun = Lens.lens (\DescribeCapacityReservations' {dryRun} -> dryRun) (\s@DescribeCapacityReservations' {} a -> s {dryRun = a} :: DescribeCapacityReservations)

-- | The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the returned @nextToken@ value. This value can be between 5 and 500. If
-- @maxResults@ is given a larger value than 500, you receive an error.
describeCapacityReservations_maxResults :: Lens.Lens' DescribeCapacityReservations (Prelude.Maybe Prelude.Natural)
describeCapacityReservations_maxResults = Lens.lens (\DescribeCapacityReservations' {maxResults} -> maxResults) (\s@DescribeCapacityReservations' {} a -> s {maxResults = a} :: DescribeCapacityReservations)

-- | The ID of the Capacity Reservation.
describeCapacityReservations_capacityReservationIds :: Lens.Lens' DescribeCapacityReservations (Prelude.Maybe [Prelude.Text])
describeCapacityReservations_capacityReservationIds = Lens.lens (\DescribeCapacityReservations' {capacityReservationIds} -> capacityReservationIds) (\s@DescribeCapacityReservations' {} a -> s {capacityReservationIds = a} :: DescribeCapacityReservations) Prelude.. Lens.mapping Lens._Coerce

-- | One or more filters.
--
-- -   @instance-type@ - The type of instance for which the Capacity
--     Reservation reserves capacity.
--
-- -   @owner-id@ - The ID of the AWS account that owns the Capacity
--     Reservation.
--
-- -   @availability-zone-id@ - The Availability Zone ID of the Capacity
--     Reservation.
--
-- -   @instance-platform@ - The type of operating system for which the
--     Capacity Reservation reserves capacity.
--
-- -   @availability-zone@ - The Availability Zone ID of the Capacity
--     Reservation.
--
-- -   @tenancy@ - Indicates the tenancy of the Capacity Reservation. A
--     Capacity Reservation can have one of the following tenancy settings:
--
--     -   @default@ - The Capacity Reservation is created on hardware that
--         is shared with other AWS accounts.
--
--     -   @dedicated@ - The Capacity Reservation is created on
--         single-tenant hardware that is dedicated to a single AWS
--         account.
--
-- -   @state@ - The current state of the Capacity Reservation. A Capacity
--     Reservation can be in one of the following states:
--
--     -   @active@- The Capacity Reservation is active and the capacity is
--         available for your use.
--
--     -   @expired@ - The Capacity Reservation expired automatically at
--         the date and time specified in your request. The reserved
--         capacity is no longer available for your use.
--
--     -   @cancelled@ - The Capacity Reservation was cancelled. The
--         reserved capacity is no longer available for your use.
--
--     -   @pending@ - The Capacity Reservation request was successful but
--         the capacity provisioning is still pending.
--
--     -   @failed@ - The Capacity Reservation request has failed. A
--         request might fail due to invalid request parameters, capacity
--         constraints, or instance limit constraints. Failed requests are
--         retained for 60 minutes.
--
-- -   @end-date@ - The date and time at which the Capacity Reservation
--     expires. When a Capacity Reservation expires, the reserved capacity
--     is released and you can no longer launch instances into it. The
--     Capacity Reservation\'s state changes to expired when it reaches its
--     end date and time.
--
-- -   @end-date-type@ - Indicates the way in which the Capacity
--     Reservation ends. A Capacity Reservation can have one of the
--     following end types:
--
--     -   @unlimited@ - The Capacity Reservation remains active until you
--         explicitly cancel it.
--
--     -   @limited@ - The Capacity Reservation expires automatically at a
--         specified date and time.
--
-- -   @instance-match-criteria@ - Indicates the type of instance launches
--     that the Capacity Reservation accepts. The options include:
--
--     -   @open@ - The Capacity Reservation accepts all instances that
--         have matching attributes (instance type, platform, and
--         Availability Zone). Instances that have matching attributes
--         launch into the Capacity Reservation automatically without
--         specifying any additional parameters.
--
--     -   @targeted@ - The Capacity Reservation only accepts instances
--         that have matching attributes (instance type, platform, and
--         Availability Zone), and explicitly target the Capacity
--         Reservation. This ensures that only permitted instances can use
--         the reserved capacity.
describeCapacityReservations_filters :: Lens.Lens' DescribeCapacityReservations (Prelude.Maybe [Filter])
describeCapacityReservations_filters = Lens.lens (\DescribeCapacityReservations' {filters} -> filters) (\s@DescribeCapacityReservations' {} a -> s {filters = a} :: DescribeCapacityReservations) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeCapacityReservations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeCapacityReservationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeCapacityReservationsResponse_capacityReservations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeCapacityReservations_nextToken
          Lens..~ rs
          Lens.^? describeCapacityReservationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeCapacityReservations where
  type
    AWSResponse DescribeCapacityReservations =
      DescribeCapacityReservationsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeCapacityReservationsResponse'
            Prelude.<$> ( x Core..@? "capacityReservationSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (x Core..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeCapacityReservations

instance Prelude.NFData DescribeCapacityReservations

instance Core.ToHeaders DescribeCapacityReservations where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeCapacityReservations where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeCapacityReservations where
  toQuery DescribeCapacityReservations' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeCapacityReservations" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "CapacityReservationId"
              Prelude.<$> capacityReservationIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters)
      ]

-- | /See:/ 'newDescribeCapacityReservationsResponse' smart constructor.
data DescribeCapacityReservationsResponse = DescribeCapacityReservationsResponse'
  { -- | Information about the Capacity Reservations.
    capacityReservations :: Prelude.Maybe [CapacityReservation],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCapacityReservationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityReservations', 'describeCapacityReservationsResponse_capacityReservations' - Information about the Capacity Reservations.
--
-- 'nextToken', 'describeCapacityReservationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeCapacityReservationsResponse_httpStatus' - The response's http status code.
newDescribeCapacityReservationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCapacityReservationsResponse
newDescribeCapacityReservationsResponse pHttpStatus_ =
  DescribeCapacityReservationsResponse'
    { capacityReservations =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Capacity Reservations.
describeCapacityReservationsResponse_capacityReservations :: Lens.Lens' DescribeCapacityReservationsResponse (Prelude.Maybe [CapacityReservation])
describeCapacityReservationsResponse_capacityReservations = Lens.lens (\DescribeCapacityReservationsResponse' {capacityReservations} -> capacityReservations) (\s@DescribeCapacityReservationsResponse' {} a -> s {capacityReservations = a} :: DescribeCapacityReservationsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeCapacityReservationsResponse_nextToken :: Lens.Lens' DescribeCapacityReservationsResponse (Prelude.Maybe Prelude.Text)
describeCapacityReservationsResponse_nextToken = Lens.lens (\DescribeCapacityReservationsResponse' {nextToken} -> nextToken) (\s@DescribeCapacityReservationsResponse' {} a -> s {nextToken = a} :: DescribeCapacityReservationsResponse)

-- | The response's http status code.
describeCapacityReservationsResponse_httpStatus :: Lens.Lens' DescribeCapacityReservationsResponse Prelude.Int
describeCapacityReservationsResponse_httpStatus = Lens.lens (\DescribeCapacityReservationsResponse' {httpStatus} -> httpStatus) (\s@DescribeCapacityReservationsResponse' {} a -> s {httpStatus = a} :: DescribeCapacityReservationsResponse)

instance
  Prelude.NFData
    DescribeCapacityReservationsResponse
