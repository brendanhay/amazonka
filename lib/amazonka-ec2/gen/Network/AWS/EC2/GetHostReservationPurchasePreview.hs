{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetHostReservationPurchasePreview
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Preview a reservation purchase with configurations that match those of your Dedicated Host. You must have active Dedicated Hosts in your account before you purchase a reservation.
--
-- This is a preview of the 'PurchaseHostReservation' action and does not result in the offering being purchased.
module Network.AWS.EC2.GetHostReservationPurchasePreview
  ( -- * Creating a request
    GetHostReservationPurchasePreview (..),
    mkGetHostReservationPurchasePreview,

    -- ** Request lenses
    ghrppHostIdSet,
    ghrppOfferingId,

    -- * Destructuring the response
    GetHostReservationPurchasePreviewResponse (..),
    mkGetHostReservationPurchasePreviewResponse,

    -- ** Response lenses
    ghrpprsCurrencyCode,
    ghrpprsTotalHourlyPrice,
    ghrpprsTotalUpfrontPrice,
    ghrpprsPurchase,
    ghrpprsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetHostReservationPurchasePreview' smart constructor.
data GetHostReservationPurchasePreview = GetHostReservationPurchasePreview'
  { hostIdSet ::
      [Lude.Text],
    offeringId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetHostReservationPurchasePreview' with the minimum fields required to make a request.
--
-- * 'hostIdSet' - The IDs of the Dedicated Hosts with which the reservation is associated.
-- * 'offeringId' - The offering ID of the reservation.
mkGetHostReservationPurchasePreview ::
  -- | 'offeringId'
  Lude.Text ->
  GetHostReservationPurchasePreview
mkGetHostReservationPurchasePreview pOfferingId_ =
  GetHostReservationPurchasePreview'
    { hostIdSet = Lude.mempty,
      offeringId = pOfferingId_
    }

-- | The IDs of the Dedicated Hosts with which the reservation is associated.
--
-- /Note:/ Consider using 'hostIdSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghrppHostIdSet :: Lens.Lens' GetHostReservationPurchasePreview [Lude.Text]
ghrppHostIdSet = Lens.lens (hostIdSet :: GetHostReservationPurchasePreview -> [Lude.Text]) (\s a -> s {hostIdSet = a} :: GetHostReservationPurchasePreview)
{-# DEPRECATED ghrppHostIdSet "Use generic-lens or generic-optics with 'hostIdSet' instead." #-}

-- | The offering ID of the reservation.
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghrppOfferingId :: Lens.Lens' GetHostReservationPurchasePreview Lude.Text
ghrppOfferingId = Lens.lens (offeringId :: GetHostReservationPurchasePreview -> Lude.Text) (\s a -> s {offeringId = a} :: GetHostReservationPurchasePreview)
{-# DEPRECATED ghrppOfferingId "Use generic-lens or generic-optics with 'offeringId' instead." #-}

instance Lude.AWSRequest GetHostReservationPurchasePreview where
  type
    Rs GetHostReservationPurchasePreview =
      GetHostReservationPurchasePreviewResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetHostReservationPurchasePreviewResponse'
            Lude.<$> (x Lude..@? "currencyCode")
            Lude.<*> (x Lude..@? "totalHourlyPrice")
            Lude.<*> (x Lude..@? "totalUpfrontPrice")
            Lude.<*> ( x Lude..@? "purchase" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetHostReservationPurchasePreview where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetHostReservationPurchasePreview where
  toPath = Lude.const "/"

instance Lude.ToQuery GetHostReservationPurchasePreview where
  toQuery GetHostReservationPurchasePreview' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("GetHostReservationPurchasePreview" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQueryList "HostIdSet" hostIdSet,
        "OfferingId" Lude.=: offeringId
      ]

-- | /See:/ 'mkGetHostReservationPurchasePreviewResponse' smart constructor.
data GetHostReservationPurchasePreviewResponse = GetHostReservationPurchasePreviewResponse'
  { currencyCode ::
      Lude.Maybe
        CurrencyCodeValues,
    totalHourlyPrice ::
      Lude.Maybe
        Lude.Text,
    totalUpfrontPrice ::
      Lude.Maybe
        Lude.Text,
    purchase ::
      Lude.Maybe
        [Purchase],
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

-- | Creates a value of 'GetHostReservationPurchasePreviewResponse' with the minimum fields required to make a request.
--
-- * 'currencyCode' - The currency in which the @totalUpfrontPrice@ and @totalHourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
-- * 'purchase' - The purchase information of the Dedicated Host reservation and the Dedicated Hosts associated with it.
-- * 'responseStatus' - The response status code.
-- * 'totalHourlyPrice' - The potential total hourly price of the reservation per hour.
-- * 'totalUpfrontPrice' - The potential total upfront price. This is billed immediately.
mkGetHostReservationPurchasePreviewResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetHostReservationPurchasePreviewResponse
mkGetHostReservationPurchasePreviewResponse pResponseStatus_ =
  GetHostReservationPurchasePreviewResponse'
    { currencyCode =
        Lude.Nothing,
      totalHourlyPrice = Lude.Nothing,
      totalUpfrontPrice = Lude.Nothing,
      purchase = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The currency in which the @totalUpfrontPrice@ and @totalHourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghrpprsCurrencyCode :: Lens.Lens' GetHostReservationPurchasePreviewResponse (Lude.Maybe CurrencyCodeValues)
ghrpprsCurrencyCode = Lens.lens (currencyCode :: GetHostReservationPurchasePreviewResponse -> Lude.Maybe CurrencyCodeValues) (\s a -> s {currencyCode = a} :: GetHostReservationPurchasePreviewResponse)
{-# DEPRECATED ghrpprsCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The potential total hourly price of the reservation per hour.
--
-- /Note:/ Consider using 'totalHourlyPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghrpprsTotalHourlyPrice :: Lens.Lens' GetHostReservationPurchasePreviewResponse (Lude.Maybe Lude.Text)
ghrpprsTotalHourlyPrice = Lens.lens (totalHourlyPrice :: GetHostReservationPurchasePreviewResponse -> Lude.Maybe Lude.Text) (\s a -> s {totalHourlyPrice = a} :: GetHostReservationPurchasePreviewResponse)
{-# DEPRECATED ghrpprsTotalHourlyPrice "Use generic-lens or generic-optics with 'totalHourlyPrice' instead." #-}

-- | The potential total upfront price. This is billed immediately.
--
-- /Note:/ Consider using 'totalUpfrontPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghrpprsTotalUpfrontPrice :: Lens.Lens' GetHostReservationPurchasePreviewResponse (Lude.Maybe Lude.Text)
ghrpprsTotalUpfrontPrice = Lens.lens (totalUpfrontPrice :: GetHostReservationPurchasePreviewResponse -> Lude.Maybe Lude.Text) (\s a -> s {totalUpfrontPrice = a} :: GetHostReservationPurchasePreviewResponse)
{-# DEPRECATED ghrpprsTotalUpfrontPrice "Use generic-lens or generic-optics with 'totalUpfrontPrice' instead." #-}

-- | The purchase information of the Dedicated Host reservation and the Dedicated Hosts associated with it.
--
-- /Note:/ Consider using 'purchase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghrpprsPurchase :: Lens.Lens' GetHostReservationPurchasePreviewResponse (Lude.Maybe [Purchase])
ghrpprsPurchase = Lens.lens (purchase :: GetHostReservationPurchasePreviewResponse -> Lude.Maybe [Purchase]) (\s a -> s {purchase = a} :: GetHostReservationPurchasePreviewResponse)
{-# DEPRECATED ghrpprsPurchase "Use generic-lens or generic-optics with 'purchase' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghrpprsResponseStatus :: Lens.Lens' GetHostReservationPurchasePreviewResponse Lude.Int
ghrpprsResponseStatus = Lens.lens (responseStatus :: GetHostReservationPurchasePreviewResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetHostReservationPurchasePreviewResponse)
{-# DEPRECATED ghrpprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
