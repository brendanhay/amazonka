{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.DescribeHostReservationOfferings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Dedicated Host reservations that are available to
-- purchase.
--
-- The results describe all of the Dedicated Host reservation offerings,
-- including offerings that might not match the instance family and Region
-- of your Dedicated Hosts. When purchasing an offering, ensure that the
-- instance family and Region of the offering matches that of the Dedicated
-- Hosts with which it is to be associated. For more information about
-- supported instance types, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-overview.html Dedicated Hosts>
-- in the /Amazon EC2 User Guide/.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeHostReservationOfferings
  ( -- * Creating a Request
    DescribeHostReservationOfferings (..),
    newDescribeHostReservationOfferings,

    -- * Request Lenses
    describeHostReservationOfferings_minDuration,
    describeHostReservationOfferings_nextToken,
    describeHostReservationOfferings_maxResults,
    describeHostReservationOfferings_offeringId,
    describeHostReservationOfferings_filter,
    describeHostReservationOfferings_maxDuration,

    -- * Destructuring the Response
    DescribeHostReservationOfferingsResponse (..),
    newDescribeHostReservationOfferingsResponse,

    -- * Response Lenses
    describeHostReservationOfferingsResponse_nextToken,
    describeHostReservationOfferingsResponse_offeringSet,
    describeHostReservationOfferingsResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeHostReservationOfferings' smart constructor.
data DescribeHostReservationOfferings = DescribeHostReservationOfferings'
  { -- | This is the minimum duration of the reservation you\'d like to purchase,
    -- specified in seconds. Reservations are available in one-year and
    -- three-year terms. The number of seconds specified must be the number of
    -- seconds in a year (365x24x60x60) times one of the supported durations (1
    -- or 3). For example, specify 31536000 for one year.
    minDuration :: Prelude.Maybe Prelude.Int,
    -- | The token to use to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results can be seen by sending another request with
    -- the returned @nextToken@ value. This value can be between 5 and 500. If
    -- @maxResults@ is given a larger value than 500, you receive an error.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the reservation offering.
    offeringId :: Prelude.Maybe Prelude.Text,
    -- | The filters.
    --
    -- -   @instance-family@ - The instance family of the offering (for
    --     example, @m4@).
    --
    -- -   @payment-option@ - The payment option (@NoUpfront@ |
    --     @PartialUpfront@ | @AllUpfront@).
    filter' :: Prelude.Maybe [Filter],
    -- | This is the maximum duration of the reservation to purchase, specified
    -- in seconds. Reservations are available in one-year and three-year terms.
    -- The number of seconds specified must be the number of seconds in a year
    -- (365x24x60x60) times one of the supported durations (1 or 3). For
    -- example, specify 94608000 for three years.
    maxDuration :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeHostReservationOfferings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minDuration', 'describeHostReservationOfferings_minDuration' - This is the minimum duration of the reservation you\'d like to purchase,
-- specified in seconds. Reservations are available in one-year and
-- three-year terms. The number of seconds specified must be the number of
-- seconds in a year (365x24x60x60) times one of the supported durations (1
-- or 3). For example, specify 31536000 for one year.
--
-- 'nextToken', 'describeHostReservationOfferings_nextToken' - The token to use to retrieve the next page of results.
--
-- 'maxResults', 'describeHostReservationOfferings_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the returned @nextToken@ value. This value can be between 5 and 500. If
-- @maxResults@ is given a larger value than 500, you receive an error.
--
-- 'offeringId', 'describeHostReservationOfferings_offeringId' - The ID of the reservation offering.
--
-- 'filter'', 'describeHostReservationOfferings_filter' - The filters.
--
-- -   @instance-family@ - The instance family of the offering (for
--     example, @m4@).
--
-- -   @payment-option@ - The payment option (@NoUpfront@ |
--     @PartialUpfront@ | @AllUpfront@).
--
-- 'maxDuration', 'describeHostReservationOfferings_maxDuration' - This is the maximum duration of the reservation to purchase, specified
-- in seconds. Reservations are available in one-year and three-year terms.
-- The number of seconds specified must be the number of seconds in a year
-- (365x24x60x60) times one of the supported durations (1 or 3). For
-- example, specify 94608000 for three years.
newDescribeHostReservationOfferings ::
  DescribeHostReservationOfferings
newDescribeHostReservationOfferings =
  DescribeHostReservationOfferings'
    { minDuration =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      offeringId = Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxDuration = Prelude.Nothing
    }

-- | This is the minimum duration of the reservation you\'d like to purchase,
-- specified in seconds. Reservations are available in one-year and
-- three-year terms. The number of seconds specified must be the number of
-- seconds in a year (365x24x60x60) times one of the supported durations (1
-- or 3). For example, specify 31536000 for one year.
describeHostReservationOfferings_minDuration :: Lens.Lens' DescribeHostReservationOfferings (Prelude.Maybe Prelude.Int)
describeHostReservationOfferings_minDuration = Lens.lens (\DescribeHostReservationOfferings' {minDuration} -> minDuration) (\s@DescribeHostReservationOfferings' {} a -> s {minDuration = a} :: DescribeHostReservationOfferings)

-- | The token to use to retrieve the next page of results.
describeHostReservationOfferings_nextToken :: Lens.Lens' DescribeHostReservationOfferings (Prelude.Maybe Prelude.Text)
describeHostReservationOfferings_nextToken = Lens.lens (\DescribeHostReservationOfferings' {nextToken} -> nextToken) (\s@DescribeHostReservationOfferings' {} a -> s {nextToken = a} :: DescribeHostReservationOfferings)

-- | The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the returned @nextToken@ value. This value can be between 5 and 500. If
-- @maxResults@ is given a larger value than 500, you receive an error.
describeHostReservationOfferings_maxResults :: Lens.Lens' DescribeHostReservationOfferings (Prelude.Maybe Prelude.Natural)
describeHostReservationOfferings_maxResults = Lens.lens (\DescribeHostReservationOfferings' {maxResults} -> maxResults) (\s@DescribeHostReservationOfferings' {} a -> s {maxResults = a} :: DescribeHostReservationOfferings)

-- | The ID of the reservation offering.
describeHostReservationOfferings_offeringId :: Lens.Lens' DescribeHostReservationOfferings (Prelude.Maybe Prelude.Text)
describeHostReservationOfferings_offeringId = Lens.lens (\DescribeHostReservationOfferings' {offeringId} -> offeringId) (\s@DescribeHostReservationOfferings' {} a -> s {offeringId = a} :: DescribeHostReservationOfferings)

-- | The filters.
--
-- -   @instance-family@ - The instance family of the offering (for
--     example, @m4@).
--
-- -   @payment-option@ - The payment option (@NoUpfront@ |
--     @PartialUpfront@ | @AllUpfront@).
describeHostReservationOfferings_filter :: Lens.Lens' DescribeHostReservationOfferings (Prelude.Maybe [Filter])
describeHostReservationOfferings_filter = Lens.lens (\DescribeHostReservationOfferings' {filter'} -> filter') (\s@DescribeHostReservationOfferings' {} a -> s {filter' = a} :: DescribeHostReservationOfferings) Prelude.. Lens.mapping Prelude._Coerce

-- | This is the maximum duration of the reservation to purchase, specified
-- in seconds. Reservations are available in one-year and three-year terms.
-- The number of seconds specified must be the number of seconds in a year
-- (365x24x60x60) times one of the supported durations (1 or 3). For
-- example, specify 94608000 for three years.
describeHostReservationOfferings_maxDuration :: Lens.Lens' DescribeHostReservationOfferings (Prelude.Maybe Prelude.Int)
describeHostReservationOfferings_maxDuration = Lens.lens (\DescribeHostReservationOfferings' {maxDuration} -> maxDuration) (\s@DescribeHostReservationOfferings' {} a -> s {maxDuration = a} :: DescribeHostReservationOfferings)

instance
  Pager.AWSPager
    DescribeHostReservationOfferings
  where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeHostReservationOfferingsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeHostReservationOfferingsResponse_offeringSet
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeHostReservationOfferings_nextToken
          Lens..~ rs
          Lens.^? describeHostReservationOfferingsResponse_nextToken
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    DescribeHostReservationOfferings
  where
  type
    Rs DescribeHostReservationOfferings =
      DescribeHostReservationOfferingsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeHostReservationOfferingsResponse'
            Prelude.<$> (x Prelude..@? "nextToken")
            Prelude.<*> ( x Prelude..@? "offeringSet"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeHostReservationOfferings

instance
  Prelude.NFData
    DescribeHostReservationOfferings

instance
  Prelude.ToHeaders
    DescribeHostReservationOfferings
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DescribeHostReservationOfferings
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribeHostReservationOfferings
  where
  toQuery DescribeHostReservationOfferings' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DescribeHostReservationOfferings" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "MinDuration" Prelude.=: minDuration,
        "NextToken" Prelude.=: nextToken,
        "MaxResults" Prelude.=: maxResults,
        "OfferingId" Prelude.=: offeringId,
        Prelude.toQuery
          (Prelude.toQueryList "Filter" Prelude.<$> filter'),
        "MaxDuration" Prelude.=: maxDuration
      ]

-- | /See:/ 'newDescribeHostReservationOfferingsResponse' smart constructor.
data DescribeHostReservationOfferingsResponse = DescribeHostReservationOfferingsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the offerings.
    offeringSet :: Prelude.Maybe [HostOffering],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeHostReservationOfferingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeHostReservationOfferingsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'offeringSet', 'describeHostReservationOfferingsResponse_offeringSet' - Information about the offerings.
--
-- 'httpStatus', 'describeHostReservationOfferingsResponse_httpStatus' - The response's http status code.
newDescribeHostReservationOfferingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeHostReservationOfferingsResponse
newDescribeHostReservationOfferingsResponse
  pHttpStatus_ =
    DescribeHostReservationOfferingsResponse'
      { nextToken =
          Prelude.Nothing,
        offeringSet = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeHostReservationOfferingsResponse_nextToken :: Lens.Lens' DescribeHostReservationOfferingsResponse (Prelude.Maybe Prelude.Text)
describeHostReservationOfferingsResponse_nextToken = Lens.lens (\DescribeHostReservationOfferingsResponse' {nextToken} -> nextToken) (\s@DescribeHostReservationOfferingsResponse' {} a -> s {nextToken = a} :: DescribeHostReservationOfferingsResponse)

-- | Information about the offerings.
describeHostReservationOfferingsResponse_offeringSet :: Lens.Lens' DescribeHostReservationOfferingsResponse (Prelude.Maybe [HostOffering])
describeHostReservationOfferingsResponse_offeringSet = Lens.lens (\DescribeHostReservationOfferingsResponse' {offeringSet} -> offeringSet) (\s@DescribeHostReservationOfferingsResponse' {} a -> s {offeringSet = a} :: DescribeHostReservationOfferingsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeHostReservationOfferingsResponse_httpStatus :: Lens.Lens' DescribeHostReservationOfferingsResponse Prelude.Int
describeHostReservationOfferingsResponse_httpStatus = Lens.lens (\DescribeHostReservationOfferingsResponse' {httpStatus} -> httpStatus) (\s@DescribeHostReservationOfferingsResponse' {} a -> s {httpStatus = a} :: DescribeHostReservationOfferingsResponse)

instance
  Prelude.NFData
    DescribeHostReservationOfferingsResponse
