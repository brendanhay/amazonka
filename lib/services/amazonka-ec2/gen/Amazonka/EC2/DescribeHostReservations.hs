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
-- Module      : Amazonka.EC2.DescribeHostReservations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes reservations that are associated with Dedicated Hosts in your
-- account.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeHostReservations
  ( -- * Creating a Request
    DescribeHostReservations (..),
    newDescribeHostReservations,

    -- * Request Lenses
    describeHostReservations_nextToken,
    describeHostReservations_filter,
    describeHostReservations_hostReservationIdSet,
    describeHostReservations_maxResults,

    -- * Destructuring the Response
    DescribeHostReservationsResponse (..),
    newDescribeHostReservationsResponse,

    -- * Response Lenses
    describeHostReservationsResponse_nextToken,
    describeHostReservationsResponse_hostReservationSet,
    describeHostReservationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeHostReservations' smart constructor.
data DescribeHostReservations = DescribeHostReservations'
  { -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
    -- -   @tag:\<key>@ - The key\/value combination of a tag assigned to the
    --     resource. Use the tag key in the filter name and the tag value as
    --     the filter value. For example, to find all resources that have a tag
    --     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
    --     the filter name and @TeamA@ for the filter value.
    --
    -- -   @tag-key@ - The key of a tag assigned to the resource. Use this
    --     filter to find all resources assigned a tag with a specific key,
    --     regardless of the tag value.
    filter' :: Prelude.Maybe [Filter],
    -- | The host reservation IDs.
    hostReservationIdSet :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results can be seen by sending another request with
    -- the returned @nextToken@ value. This value can be between 5 and 500. If
    -- @maxResults@ is given a larger value than 500, you receive an error.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- -   @tag:\<key>@ - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- 'hostReservationIdSet', 'describeHostReservations_hostReservationIdSet' - The host reservation IDs.
--
-- 'maxResults', 'describeHostReservations_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the returned @nextToken@ value. This value can be between 5 and 500. If
-- @maxResults@ is given a larger value than 500, you receive an error.
newDescribeHostReservations ::
  DescribeHostReservations
newDescribeHostReservations =
  DescribeHostReservations'
    { nextToken =
        Prelude.Nothing,
      filter' = Prelude.Nothing,
      hostReservationIdSet = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token to use to retrieve the next page of results.
describeHostReservations_nextToken :: Lens.Lens' DescribeHostReservations (Prelude.Maybe Prelude.Text)
describeHostReservations_nextToken = Lens.lens (\DescribeHostReservations' {nextToken} -> nextToken) (\s@DescribeHostReservations' {} a -> s {nextToken = a} :: DescribeHostReservations)

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
-- -   @tag:\<key>@ - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
describeHostReservations_filter :: Lens.Lens' DescribeHostReservations (Prelude.Maybe [Filter])
describeHostReservations_filter = Lens.lens (\DescribeHostReservations' {filter'} -> filter') (\s@DescribeHostReservations' {} a -> s {filter' = a} :: DescribeHostReservations) Prelude.. Lens.mapping Lens.coerced

-- | The host reservation IDs.
describeHostReservations_hostReservationIdSet :: Lens.Lens' DescribeHostReservations (Prelude.Maybe [Prelude.Text])
describeHostReservations_hostReservationIdSet = Lens.lens (\DescribeHostReservations' {hostReservationIdSet} -> hostReservationIdSet) (\s@DescribeHostReservations' {} a -> s {hostReservationIdSet = a} :: DescribeHostReservations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the returned @nextToken@ value. This value can be between 5 and 500. If
-- @maxResults@ is given a larger value than 500, you receive an error.
describeHostReservations_maxResults :: Lens.Lens' DescribeHostReservations (Prelude.Maybe Prelude.Int)
describeHostReservations_maxResults = Lens.lens (\DescribeHostReservations' {maxResults} -> maxResults) (\s@DescribeHostReservations' {} a -> s {maxResults = a} :: DescribeHostReservations)

instance Core.AWSPager DescribeHostReservations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeHostReservationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeHostReservationsResponse_hostReservationSet
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeHostReservations_nextToken
          Lens..~ rs
          Lens.^? describeHostReservationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeHostReservations where
  type
    AWSResponse DescribeHostReservations =
      DescribeHostReservationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeHostReservationsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x Data..@? "hostReservationSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeHostReservations where
  hashWithSalt _salt DescribeHostReservations' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` hostReservationIdSet
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeHostReservations where
  rnf DescribeHostReservations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf hostReservationIdSet
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders DescribeHostReservations where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeHostReservations where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeHostReservations where
  toQuery DescribeHostReservations' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeHostReservations" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Data.=: nextToken,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filter'),
        Data.toQuery
          ( Data.toQueryList "HostReservationIdSet"
              Prelude.<$> hostReservationIdSet
          ),
        "MaxResults" Data.=: maxResults
      ]

-- | /See:/ 'newDescribeHostReservationsResponse' smart constructor.
data DescribeHostReservationsResponse = DescribeHostReservationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Details about the reservation\'s configuration.
    hostReservationSet :: Prelude.Maybe [HostReservation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeHostReservationsResponse
newDescribeHostReservationsResponse pHttpStatus_ =
  DescribeHostReservationsResponse'
    { nextToken =
        Prelude.Nothing,
      hostReservationSet = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeHostReservationsResponse_nextToken :: Lens.Lens' DescribeHostReservationsResponse (Prelude.Maybe Prelude.Text)
describeHostReservationsResponse_nextToken = Lens.lens (\DescribeHostReservationsResponse' {nextToken} -> nextToken) (\s@DescribeHostReservationsResponse' {} a -> s {nextToken = a} :: DescribeHostReservationsResponse)

-- | Details about the reservation\'s configuration.
describeHostReservationsResponse_hostReservationSet :: Lens.Lens' DescribeHostReservationsResponse (Prelude.Maybe [HostReservation])
describeHostReservationsResponse_hostReservationSet = Lens.lens (\DescribeHostReservationsResponse' {hostReservationSet} -> hostReservationSet) (\s@DescribeHostReservationsResponse' {} a -> s {hostReservationSet = a} :: DescribeHostReservationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeHostReservationsResponse_httpStatus :: Lens.Lens' DescribeHostReservationsResponse Prelude.Int
describeHostReservationsResponse_httpStatus = Lens.lens (\DescribeHostReservationsResponse' {httpStatus} -> httpStatus) (\s@DescribeHostReservationsResponse' {} a -> s {httpStatus = a} :: DescribeHostReservationsResponse)

instance
  Prelude.NFData
    DescribeHostReservationsResponse
  where
  rnf DescribeHostReservationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf hostReservationSet
      `Prelude.seq` Prelude.rnf httpStatus
