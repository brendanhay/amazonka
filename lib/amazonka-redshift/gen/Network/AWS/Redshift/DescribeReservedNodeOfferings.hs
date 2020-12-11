{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeReservedNodeOfferings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the available reserved node offerings by Amazon Redshift with their descriptions including the node type, the fixed and recurring costs of reserving the node and duration the node will be reserved for you. These descriptions help you determine which reserve node offering you want to purchase. You then use the unique offering ID in you call to 'PurchaseReservedNodeOffering' to reserve one or more nodes for your Amazon Redshift cluster.
--
-- For more information about reserved node offerings, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/purchase-reserved-node-instance.html Purchasing Reserved Nodes> in the /Amazon Redshift Cluster Management Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeReservedNodeOfferings
  ( -- * Creating a request
    DescribeReservedNodeOfferings (..),
    mkDescribeReservedNodeOfferings,

    -- ** Request lenses
    drnoReservedNodeOfferingId,
    drnoMarker,
    drnoMaxRecords,

    -- * Destructuring the response
    DescribeReservedNodeOfferingsResponse (..),
    mkDescribeReservedNodeOfferingsResponse,

    -- ** Response lenses
    drnorsReservedNodeOfferings,
    drnorsMarker,
    drnorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeReservedNodeOfferings' smart constructor.
data DescribeReservedNodeOfferings = DescribeReservedNodeOfferings'
  { reservedNodeOfferingId ::
      Lude.Maybe Lude.Text,
    marker :: Lude.Maybe Lude.Text,
    maxRecords ::
      Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReservedNodeOfferings' with the minimum fields required to make a request.
--
-- * 'marker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeReservedNodeOfferings' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
-- * 'maxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
-- * 'reservedNodeOfferingId' - The unique identifier for the offering.
mkDescribeReservedNodeOfferings ::
  DescribeReservedNodeOfferings
mkDescribeReservedNodeOfferings =
  DescribeReservedNodeOfferings'
    { reservedNodeOfferingId =
        Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | The unique identifier for the offering.
--
-- /Note:/ Consider using 'reservedNodeOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drnoReservedNodeOfferingId :: Lens.Lens' DescribeReservedNodeOfferings (Lude.Maybe Lude.Text)
drnoReservedNodeOfferingId = Lens.lens (reservedNodeOfferingId :: DescribeReservedNodeOfferings -> Lude.Maybe Lude.Text) (\s a -> s {reservedNodeOfferingId = a} :: DescribeReservedNodeOfferings)
{-# DEPRECATED drnoReservedNodeOfferingId "Use generic-lens or generic-optics with 'reservedNodeOfferingId' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeReservedNodeOfferings' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drnoMarker :: Lens.Lens' DescribeReservedNodeOfferings (Lude.Maybe Lude.Text)
drnoMarker = Lens.lens (marker :: DescribeReservedNodeOfferings -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReservedNodeOfferings)
{-# DEPRECATED drnoMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drnoMaxRecords :: Lens.Lens' DescribeReservedNodeOfferings (Lude.Maybe Lude.Int)
drnoMaxRecords = Lens.lens (maxRecords :: DescribeReservedNodeOfferings -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeReservedNodeOfferings)
{-# DEPRECATED drnoMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeReservedNodeOfferings where
  page rq rs
    | Page.stop (rs Lens.^. drnorsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. drnorsReservedNodeOfferings) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& drnoMarker Lens..~ rs Lens.^. drnorsMarker

instance Lude.AWSRequest DescribeReservedNodeOfferings where
  type
    Rs DescribeReservedNodeOfferings =
      DescribeReservedNodeOfferingsResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeReservedNodeOfferingsResult"
      ( \s h x ->
          DescribeReservedNodeOfferingsResponse'
            Lude.<$> ( x Lude..@? "ReservedNodeOfferings" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "ReservedNodeOffering")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeReservedNodeOfferings where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeReservedNodeOfferings where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeReservedNodeOfferings where
  toQuery DescribeReservedNodeOfferings' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeReservedNodeOfferings" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ReservedNodeOfferingId" Lude.=: reservedNodeOfferingId,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords
      ]

-- |
--
-- /See:/ 'mkDescribeReservedNodeOfferingsResponse' smart constructor.
data DescribeReservedNodeOfferingsResponse = DescribeReservedNodeOfferingsResponse'
  { reservedNodeOfferings ::
      Lude.Maybe
        [ReservedNodeOffering],
    marker ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReservedNodeOfferingsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
-- * 'reservedNodeOfferings' - A list of @ReservedNodeOffering@ objects.
-- * 'responseStatus' - The response status code.
mkDescribeReservedNodeOfferingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReservedNodeOfferingsResponse
mkDescribeReservedNodeOfferingsResponse pResponseStatus_ =
  DescribeReservedNodeOfferingsResponse'
    { reservedNodeOfferings =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @ReservedNodeOffering@ objects.
--
-- /Note:/ Consider using 'reservedNodeOfferings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drnorsReservedNodeOfferings :: Lens.Lens' DescribeReservedNodeOfferingsResponse (Lude.Maybe [ReservedNodeOffering])
drnorsReservedNodeOfferings = Lens.lens (reservedNodeOfferings :: DescribeReservedNodeOfferingsResponse -> Lude.Maybe [ReservedNodeOffering]) (\s a -> s {reservedNodeOfferings = a} :: DescribeReservedNodeOfferingsResponse)
{-# DEPRECATED drnorsReservedNodeOfferings "Use generic-lens or generic-optics with 'reservedNodeOfferings' instead." #-}

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drnorsMarker :: Lens.Lens' DescribeReservedNodeOfferingsResponse (Lude.Maybe Lude.Text)
drnorsMarker = Lens.lens (marker :: DescribeReservedNodeOfferingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReservedNodeOfferingsResponse)
{-# DEPRECATED drnorsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drnorsResponseStatus :: Lens.Lens' DescribeReservedNodeOfferingsResponse Lude.Int
drnorsResponseStatus = Lens.lens (responseStatus :: DescribeReservedNodeOfferingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReservedNodeOfferingsResponse)
{-# DEPRECATED drnorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
