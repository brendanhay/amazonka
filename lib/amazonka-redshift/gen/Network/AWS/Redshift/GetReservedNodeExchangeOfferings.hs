{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.GetReservedNodeExchangeOfferings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of DC2 ReservedNodeOfferings that matches the payment type, term, and usage price of the given DC1 reserved node.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.GetReservedNodeExchangeOfferings
  ( -- * Creating a request
    GetReservedNodeExchangeOfferings (..),
    mkGetReservedNodeExchangeOfferings,

    -- ** Request lenses
    grneoMarker,
    grneoMaxRecords,
    grneoReservedNodeId,

    -- * Destructuring the response
    GetReservedNodeExchangeOfferingsResponse (..),
    mkGetReservedNodeExchangeOfferingsResponse,

    -- ** Response lenses
    grneorsReservedNodeOfferings,
    grneorsMarker,
    grneorsResponseStatus,
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
-- /See:/ 'mkGetReservedNodeExchangeOfferings' smart constructor.
data GetReservedNodeExchangeOfferings = GetReservedNodeExchangeOfferings'
  { marker ::
      Lude.Maybe Lude.Text,
    maxRecords ::
      Lude.Maybe Lude.Int,
    reservedNodeId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetReservedNodeExchangeOfferings' with the minimum fields required to make a request.
--
-- * 'marker' - A value that indicates the starting point for the next set of ReservedNodeOfferings.
-- * 'maxRecords' - An integer setting the maximum number of ReservedNodeOfferings to retrieve.
-- * 'reservedNodeId' - A string representing the node identifier for the DC1 Reserved Node to be exchanged.
mkGetReservedNodeExchangeOfferings ::
  -- | 'reservedNodeId'
  Lude.Text ->
  GetReservedNodeExchangeOfferings
mkGetReservedNodeExchangeOfferings pReservedNodeId_ =
  GetReservedNodeExchangeOfferings'
    { marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      reservedNodeId = pReservedNodeId_
    }

-- | A value that indicates the starting point for the next set of ReservedNodeOfferings.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grneoMarker :: Lens.Lens' GetReservedNodeExchangeOfferings (Lude.Maybe Lude.Text)
grneoMarker = Lens.lens (marker :: GetReservedNodeExchangeOfferings -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: GetReservedNodeExchangeOfferings)
{-# DEPRECATED grneoMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | An integer setting the maximum number of ReservedNodeOfferings to retrieve.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grneoMaxRecords :: Lens.Lens' GetReservedNodeExchangeOfferings (Lude.Maybe Lude.Int)
grneoMaxRecords = Lens.lens (maxRecords :: GetReservedNodeExchangeOfferings -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: GetReservedNodeExchangeOfferings)
{-# DEPRECATED grneoMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | A string representing the node identifier for the DC1 Reserved Node to be exchanged.
--
-- /Note:/ Consider using 'reservedNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grneoReservedNodeId :: Lens.Lens' GetReservedNodeExchangeOfferings Lude.Text
grneoReservedNodeId = Lens.lens (reservedNodeId :: GetReservedNodeExchangeOfferings -> Lude.Text) (\s a -> s {reservedNodeId = a} :: GetReservedNodeExchangeOfferings)
{-# DEPRECATED grneoReservedNodeId "Use generic-lens or generic-optics with 'reservedNodeId' instead." #-}

instance Page.AWSPager GetReservedNodeExchangeOfferings where
  page rq rs
    | Page.stop (rs Lens.^. grneorsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. grneorsReservedNodeOfferings) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& grneoMarker Lens..~ rs Lens.^. grneorsMarker

instance Lude.AWSRequest GetReservedNodeExchangeOfferings where
  type
    Rs GetReservedNodeExchangeOfferings =
      GetReservedNodeExchangeOfferingsResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "GetReservedNodeExchangeOfferingsResult"
      ( \s h x ->
          GetReservedNodeExchangeOfferingsResponse'
            Lude.<$> ( x Lude..@? "ReservedNodeOfferings" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "ReservedNodeOffering")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetReservedNodeExchangeOfferings where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetReservedNodeExchangeOfferings where
  toPath = Lude.const "/"

instance Lude.ToQuery GetReservedNodeExchangeOfferings where
  toQuery GetReservedNodeExchangeOfferings' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("GetReservedNodeExchangeOfferings" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "ReservedNodeId" Lude.=: reservedNodeId
      ]

-- | /See:/ 'mkGetReservedNodeExchangeOfferingsResponse' smart constructor.
data GetReservedNodeExchangeOfferingsResponse = GetReservedNodeExchangeOfferingsResponse'
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

-- | Creates a value of 'GetReservedNodeExchangeOfferingsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An optional parameter that specifies the starting point for returning a set of response records. When the results of a @GetReservedNodeExchangeOfferings@ request exceed the value specified in MaxRecords, Amazon Redshift returns a value in the marker field of the response. You can retrieve the next set of response records by providing the returned marker value in the marker parameter and retrying the request.
-- * 'reservedNodeOfferings' - Returns an array of 'ReservedNodeOffering' objects.
-- * 'responseStatus' - The response status code.
mkGetReservedNodeExchangeOfferingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetReservedNodeExchangeOfferingsResponse
mkGetReservedNodeExchangeOfferingsResponse pResponseStatus_ =
  GetReservedNodeExchangeOfferingsResponse'
    { reservedNodeOfferings =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns an array of 'ReservedNodeOffering' objects.
--
-- /Note:/ Consider using 'reservedNodeOfferings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grneorsReservedNodeOfferings :: Lens.Lens' GetReservedNodeExchangeOfferingsResponse (Lude.Maybe [ReservedNodeOffering])
grneorsReservedNodeOfferings = Lens.lens (reservedNodeOfferings :: GetReservedNodeExchangeOfferingsResponse -> Lude.Maybe [ReservedNodeOffering]) (\s a -> s {reservedNodeOfferings = a} :: GetReservedNodeExchangeOfferingsResponse)
{-# DEPRECATED grneorsReservedNodeOfferings "Use generic-lens or generic-optics with 'reservedNodeOfferings' instead." #-}

-- | An optional parameter that specifies the starting point for returning a set of response records. When the results of a @GetReservedNodeExchangeOfferings@ request exceed the value specified in MaxRecords, Amazon Redshift returns a value in the marker field of the response. You can retrieve the next set of response records by providing the returned marker value in the marker parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grneorsMarker :: Lens.Lens' GetReservedNodeExchangeOfferingsResponse (Lude.Maybe Lude.Text)
grneorsMarker = Lens.lens (marker :: GetReservedNodeExchangeOfferingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: GetReservedNodeExchangeOfferingsResponse)
{-# DEPRECATED grneorsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grneorsResponseStatus :: Lens.Lens' GetReservedNodeExchangeOfferingsResponse Lude.Int
grneorsResponseStatus = Lens.lens (responseStatus :: GetReservedNodeExchangeOfferingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetReservedNodeExchangeOfferingsResponse)
{-# DEPRECATED grneorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
