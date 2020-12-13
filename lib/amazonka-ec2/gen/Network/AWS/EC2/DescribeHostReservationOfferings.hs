{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeHostReservationOfferings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Dedicated Host reservations that are available to purchase.
--
-- The results describe all of the Dedicated Host reservation offerings, including offerings that might not match the instance family and Region of your Dedicated Hosts. When purchasing an offering, ensure that the instance family and Region of the offering matches that of the Dedicated Hosts with which it is to be associated. For more information about supported instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-hosts-overview.html Dedicated Hosts Overview> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeHostReservationOfferings
  ( -- * Creating a request
    DescribeHostReservationOfferings (..),
    mkDescribeHostReservationOfferings,

    -- ** Request lenses
    dhroMaxDuration,
    dhroNextToken,
    dhroMinDuration,
    dhroOfferingId,
    dhroFilter,
    dhroMaxResults,

    -- * Destructuring the response
    DescribeHostReservationOfferingsResponse (..),
    mkDescribeHostReservationOfferingsResponse,

    -- ** Response lenses
    dhrorsOfferingSet,
    dhrorsNextToken,
    dhrorsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeHostReservationOfferings' smart constructor.
data DescribeHostReservationOfferings = DescribeHostReservationOfferings'
  { -- | This is the maximum duration of the reservation to purchase, specified in seconds. Reservations are available in one-year and three-year terms. The number of seconds specified must be the number of seconds in a year (365x24x60x60) times one of the supported durations (1 or 3). For example, specify 94608000 for three years.
    maxDuration :: Lude.Maybe Lude.Int,
    -- | The token to use to retrieve the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | This is the minimum duration of the reservation you'd like to purchase, specified in seconds. Reservations are available in one-year and three-year terms. The number of seconds specified must be the number of seconds in a year (365x24x60x60) times one of the supported durations (1 or 3). For example, specify 31536000 for one year.
    minDuration :: Lude.Maybe Lude.Int,
    -- | The ID of the reservation offering.
    offeringId :: Lude.Maybe Lude.Text,
    -- | The filters.
    --
    --
    --     * @instance-family@ - The instance family of the offering (for example, @m4@ ).
    --
    --
    --     * @payment-option@ - The payment option (@NoUpfront@ | @PartialUpfront@ | @AllUpfront@ ).
    filter :: Lude.Maybe [Filter],
    -- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHostReservationOfferings' with the minimum fields required to make a request.
--
-- * 'maxDuration' - This is the maximum duration of the reservation to purchase, specified in seconds. Reservations are available in one-year and three-year terms. The number of seconds specified must be the number of seconds in a year (365x24x60x60) times one of the supported durations (1 or 3). For example, specify 94608000 for three years.
-- * 'nextToken' - The token to use to retrieve the next page of results.
-- * 'minDuration' - This is the minimum duration of the reservation you'd like to purchase, specified in seconds. Reservations are available in one-year and three-year terms. The number of seconds specified must be the number of seconds in a year (365x24x60x60) times one of the supported durations (1 or 3). For example, specify 31536000 for one year.
-- * 'offeringId' - The ID of the reservation offering.
-- * 'filter' - The filters.
--
--
--     * @instance-family@ - The instance family of the offering (for example, @m4@ ).
--
--
--     * @payment-option@ - The payment option (@NoUpfront@ | @PartialUpfront@ | @AllUpfront@ ).
--
--
-- * 'maxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
mkDescribeHostReservationOfferings ::
  DescribeHostReservationOfferings
mkDescribeHostReservationOfferings =
  DescribeHostReservationOfferings'
    { maxDuration = Lude.Nothing,
      nextToken = Lude.Nothing,
      minDuration = Lude.Nothing,
      offeringId = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | This is the maximum duration of the reservation to purchase, specified in seconds. Reservations are available in one-year and three-year terms. The number of seconds specified must be the number of seconds in a year (365x24x60x60) times one of the supported durations (1 or 3). For example, specify 94608000 for three years.
--
-- /Note:/ Consider using 'maxDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhroMaxDuration :: Lens.Lens' DescribeHostReservationOfferings (Lude.Maybe Lude.Int)
dhroMaxDuration = Lens.lens (maxDuration :: DescribeHostReservationOfferings -> Lude.Maybe Lude.Int) (\s a -> s {maxDuration = a} :: DescribeHostReservationOfferings)
{-# DEPRECATED dhroMaxDuration "Use generic-lens or generic-optics with 'maxDuration' instead." #-}

-- | The token to use to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhroNextToken :: Lens.Lens' DescribeHostReservationOfferings (Lude.Maybe Lude.Text)
dhroNextToken = Lens.lens (nextToken :: DescribeHostReservationOfferings -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeHostReservationOfferings)
{-# DEPRECATED dhroNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | This is the minimum duration of the reservation you'd like to purchase, specified in seconds. Reservations are available in one-year and three-year terms. The number of seconds specified must be the number of seconds in a year (365x24x60x60) times one of the supported durations (1 or 3). For example, specify 31536000 for one year.
--
-- /Note:/ Consider using 'minDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhroMinDuration :: Lens.Lens' DescribeHostReservationOfferings (Lude.Maybe Lude.Int)
dhroMinDuration = Lens.lens (minDuration :: DescribeHostReservationOfferings -> Lude.Maybe Lude.Int) (\s a -> s {minDuration = a} :: DescribeHostReservationOfferings)
{-# DEPRECATED dhroMinDuration "Use generic-lens or generic-optics with 'minDuration' instead." #-}

-- | The ID of the reservation offering.
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhroOfferingId :: Lens.Lens' DescribeHostReservationOfferings (Lude.Maybe Lude.Text)
dhroOfferingId = Lens.lens (offeringId :: DescribeHostReservationOfferings -> Lude.Maybe Lude.Text) (\s a -> s {offeringId = a} :: DescribeHostReservationOfferings)
{-# DEPRECATED dhroOfferingId "Use generic-lens or generic-optics with 'offeringId' instead." #-}

-- | The filters.
--
--
--     * @instance-family@ - The instance family of the offering (for example, @m4@ ).
--
--
--     * @payment-option@ - The payment option (@NoUpfront@ | @PartialUpfront@ | @AllUpfront@ ).
--
--
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhroFilter :: Lens.Lens' DescribeHostReservationOfferings (Lude.Maybe [Filter])
dhroFilter = Lens.lens (filter :: DescribeHostReservationOfferings -> Lude.Maybe [Filter]) (\s a -> s {filter = a} :: DescribeHostReservationOfferings)
{-# DEPRECATED dhroFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhroMaxResults :: Lens.Lens' DescribeHostReservationOfferings (Lude.Maybe Lude.Natural)
dhroMaxResults = Lens.lens (maxResults :: DescribeHostReservationOfferings -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeHostReservationOfferings)
{-# DEPRECATED dhroMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeHostReservationOfferings where
  page rq rs
    | Page.stop (rs Lens.^. dhrorsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dhrorsOfferingSet) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dhroNextToken Lens..~ rs Lens.^. dhrorsNextToken

instance Lude.AWSRequest DescribeHostReservationOfferings where
  type
    Rs DescribeHostReservationOfferings =
      DescribeHostReservationOfferingsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeHostReservationOfferingsResponse'
            Lude.<$> ( x Lude..@? "offeringSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeHostReservationOfferings where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeHostReservationOfferings where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeHostReservationOfferings where
  toQuery DescribeHostReservationOfferings' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeHostReservationOfferings" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "MaxDuration" Lude.=: maxDuration,
        "NextToken" Lude.=: nextToken,
        "MinDuration" Lude.=: minDuration,
        "OfferingId" Lude.=: offeringId,
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filter),
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeHostReservationOfferingsResponse' smart constructor.
data DescribeHostReservationOfferingsResponse = DescribeHostReservationOfferingsResponse'
  { -- | Information about the offerings.
    offeringSet :: Lude.Maybe [HostOffering],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHostReservationOfferingsResponse' with the minimum fields required to make a request.
--
-- * 'offeringSet' - Information about the offerings.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeHostReservationOfferingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeHostReservationOfferingsResponse
mkDescribeHostReservationOfferingsResponse pResponseStatus_ =
  DescribeHostReservationOfferingsResponse'
    { offeringSet =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the offerings.
--
-- /Note:/ Consider using 'offeringSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrorsOfferingSet :: Lens.Lens' DescribeHostReservationOfferingsResponse (Lude.Maybe [HostOffering])
dhrorsOfferingSet = Lens.lens (offeringSet :: DescribeHostReservationOfferingsResponse -> Lude.Maybe [HostOffering]) (\s a -> s {offeringSet = a} :: DescribeHostReservationOfferingsResponse)
{-# DEPRECATED dhrorsOfferingSet "Use generic-lens or generic-optics with 'offeringSet' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrorsNextToken :: Lens.Lens' DescribeHostReservationOfferingsResponse (Lude.Maybe Lude.Text)
dhrorsNextToken = Lens.lens (nextToken :: DescribeHostReservationOfferingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeHostReservationOfferingsResponse)
{-# DEPRECATED dhrorsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrorsResponseStatus :: Lens.Lens' DescribeHostReservationOfferingsResponse Lude.Int
dhrorsResponseStatus = Lens.lens (responseStatus :: DescribeHostReservationOfferingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeHostReservationOfferingsResponse)
{-# DEPRECATED dhrorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
