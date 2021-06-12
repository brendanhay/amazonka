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
-- Module      : Network.AWS.EC2.DescribeHostReservations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes reservations that are associated with Dedicated Hosts in your
-- account.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeHostReservations
  ( -- * Creating a Request
    DescribeHostReservations (..),
    newDescribeHostReservations,

    -- * Request Lenses
    describeHostReservations_nextToken,
    describeHostReservations_maxResults,
    describeHostReservations_hostReservationIdSet,
    describeHostReservations_filter,

    -- * Destructuring the Response
    DescribeHostReservationsResponse (..),
    newDescribeHostReservationsResponse,

    -- * Response Lenses
    describeHostReservationsResponse_nextToken,
    describeHostReservationsResponse_hostReservationSet,
    describeHostReservationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeHostReservations' smart constructor.
data DescribeHostReservations = DescribeHostReservations'
  { -- | The token to use to retrieve the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results can be seen by sending another request with
    -- the returned @nextToken@ value. This value can be between 5 and 500. If
    -- @maxResults@ is given a larger value than 500, you receive an error.
    maxResults :: Core.Maybe Core.Int,
    -- | The host reservation IDs.
    hostReservationIdSet :: Core.Maybe [Core.Text],
    -- | The filters.
    --
    -- -   @instance-family@ - The instance family (for example, @m4@).
    --
    -- -   @payment-option@ - The payment option (@NoUpfront@ |
    --     @PartialUpfront@ | @AllUpfront@).
    --
    -- -   @state@ - The state of the reservation (@payment-pending@ |
    --     @payment-failed@ | @active@ | @retired@).
    --
    -- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
    --     resource. Use the tag key in the filter name and the tag value as
    --     the filter value. For example, to find all resources that have a tag
    --     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
    --     the filter name and @TeamA@ for the filter value.
    --
    -- -   @tag-key@ - The key of a tag assigned to the resource. Use this
    --     filter to find all resources assigned a tag with a specific key,
    --     regardless of the tag value.
    filter' :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeHostReservations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeHostReservations_nextToken' - The token to use to retrieve the next page of results.
--
-- 'maxResults', 'describeHostReservations_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the returned @nextToken@ value. This value can be between 5 and 500. If
-- @maxResults@ is given a larger value than 500, you receive an error.
--
-- 'hostReservationIdSet', 'describeHostReservations_hostReservationIdSet' - The host reservation IDs.
--
-- 'filter'', 'describeHostReservations_filter' - The filters.
--
-- -   @instance-family@ - The instance family (for example, @m4@).
--
-- -   @payment-option@ - The payment option (@NoUpfront@ |
--     @PartialUpfront@ | @AllUpfront@).
--
-- -   @state@ - The state of the reservation (@payment-pending@ |
--     @payment-failed@ | @active@ | @retired@).
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
newDescribeHostReservations ::
  DescribeHostReservations
newDescribeHostReservations =
  DescribeHostReservations'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      hostReservationIdSet = Core.Nothing,
      filter' = Core.Nothing
    }

-- | The token to use to retrieve the next page of results.
describeHostReservations_nextToken :: Lens.Lens' DescribeHostReservations (Core.Maybe Core.Text)
describeHostReservations_nextToken = Lens.lens (\DescribeHostReservations' {nextToken} -> nextToken) (\s@DescribeHostReservations' {} a -> s {nextToken = a} :: DescribeHostReservations)

-- | The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the returned @nextToken@ value. This value can be between 5 and 500. If
-- @maxResults@ is given a larger value than 500, you receive an error.
describeHostReservations_maxResults :: Lens.Lens' DescribeHostReservations (Core.Maybe Core.Int)
describeHostReservations_maxResults = Lens.lens (\DescribeHostReservations' {maxResults} -> maxResults) (\s@DescribeHostReservations' {} a -> s {maxResults = a} :: DescribeHostReservations)

-- | The host reservation IDs.
describeHostReservations_hostReservationIdSet :: Lens.Lens' DescribeHostReservations (Core.Maybe [Core.Text])
describeHostReservations_hostReservationIdSet = Lens.lens (\DescribeHostReservations' {hostReservationIdSet} -> hostReservationIdSet) (\s@DescribeHostReservations' {} a -> s {hostReservationIdSet = a} :: DescribeHostReservations) Core.. Lens.mapping Lens._Coerce

-- | The filters.
--
-- -   @instance-family@ - The instance family (for example, @m4@).
--
-- -   @payment-option@ - The payment option (@NoUpfront@ |
--     @PartialUpfront@ | @AllUpfront@).
--
-- -   @state@ - The state of the reservation (@payment-pending@ |
--     @payment-failed@ | @active@ | @retired@).
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
describeHostReservations_filter :: Lens.Lens' DescribeHostReservations (Core.Maybe [Filter])
describeHostReservations_filter = Lens.lens (\DescribeHostReservations' {filter'} -> filter') (\s@DescribeHostReservations' {} a -> s {filter' = a} :: DescribeHostReservations) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeHostReservations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeHostReservationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeHostReservationsResponse_hostReservationSet
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeHostReservations_nextToken
          Lens..~ rs
          Lens.^? describeHostReservationsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeHostReservations where
  type
    AWSResponse DescribeHostReservations =
      DescribeHostReservationsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeHostReservationsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "hostReservationSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeHostReservations

instance Core.NFData DescribeHostReservations

instance Core.ToHeaders DescribeHostReservations where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeHostReservations where
  toPath = Core.const "/"

instance Core.ToQuery DescribeHostReservations where
  toQuery DescribeHostReservations' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeHostReservations" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "HostReservationIdSet"
              Core.<$> hostReservationIdSet
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filter')
      ]

-- | /See:/ 'newDescribeHostReservationsResponse' smart constructor.
data DescribeHostReservationsResponse = DescribeHostReservationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Details about the reservation\'s configuration.
    hostReservationSet :: Core.Maybe [HostReservation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeHostReservationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeHostReservationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'hostReservationSet', 'describeHostReservationsResponse_hostReservationSet' - Details about the reservation\'s configuration.
--
-- 'httpStatus', 'describeHostReservationsResponse_httpStatus' - The response's http status code.
newDescribeHostReservationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeHostReservationsResponse
newDescribeHostReservationsResponse pHttpStatus_ =
  DescribeHostReservationsResponse'
    { nextToken =
        Core.Nothing,
      hostReservationSet = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeHostReservationsResponse_nextToken :: Lens.Lens' DescribeHostReservationsResponse (Core.Maybe Core.Text)
describeHostReservationsResponse_nextToken = Lens.lens (\DescribeHostReservationsResponse' {nextToken} -> nextToken) (\s@DescribeHostReservationsResponse' {} a -> s {nextToken = a} :: DescribeHostReservationsResponse)

-- | Details about the reservation\'s configuration.
describeHostReservationsResponse_hostReservationSet :: Lens.Lens' DescribeHostReservationsResponse (Core.Maybe [HostReservation])
describeHostReservationsResponse_hostReservationSet = Lens.lens (\DescribeHostReservationsResponse' {hostReservationSet} -> hostReservationSet) (\s@DescribeHostReservationsResponse' {} a -> s {hostReservationSet = a} :: DescribeHostReservationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeHostReservationsResponse_httpStatus :: Lens.Lens' DescribeHostReservationsResponse Core.Int
describeHostReservationsResponse_httpStatus = Lens.lens (\DescribeHostReservationsResponse' {httpStatus} -> httpStatus) (\s@DescribeHostReservationsResponse' {} a -> s {httpStatus = a} :: DescribeHostReservationsResponse)

instance Core.NFData DescribeHostReservationsResponse
