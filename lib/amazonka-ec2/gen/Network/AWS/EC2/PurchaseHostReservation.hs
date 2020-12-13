{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.PurchaseHostReservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Purchase a reservation with configurations that match those of your Dedicated Host. You must have active Dedicated Hosts in your account before you purchase a reservation. This action results in the specified reservation being purchased and charged to your account.
module Network.AWS.EC2.PurchaseHostReservation
  ( -- * Creating a request
    PurchaseHostReservation (..),
    mkPurchaseHostReservation,

    -- ** Request lenses
    phrCurrencyCode,
    phrClientToken,
    phrTagSpecifications,
    phrHostIdSet,
    phrOfferingId,
    phrLimitPrice,

    -- * Destructuring the response
    PurchaseHostReservationResponse (..),
    mkPurchaseHostReservationResponse,

    -- ** Response lenses
    phrrsCurrencyCode,
    phrrsClientToken,
    phrrsTotalHourlyPrice,
    phrrsTotalUpfrontPrice,
    phrrsPurchase,
    phrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPurchaseHostReservation' smart constructor.
data PurchaseHostReservation = PurchaseHostReservation'
  { -- | The currency in which the @totalUpfrontPrice@ , @LimitPrice@ , and @totalHourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
    currencyCode :: Lude.Maybe CurrencyCodeValues,
    -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
    clientToken :: Lude.Maybe Lude.Text,
    -- | The tags to apply to the Dedicated Host Reservation during purchase.
    tagSpecifications :: Lude.Maybe [TagSpecification],
    -- | The IDs of the Dedicated Hosts with which the reservation will be associated.
    hostIdSet :: [Lude.Text],
    -- | The ID of the offering.
    offeringId :: Lude.Text,
    -- | The specified limit is checked against the total upfront cost of the reservation (calculated as the offering's upfront cost multiplied by the host count). If the total upfront cost is greater than the specified price limit, the request fails. This is used to ensure that the purchase does not exceed the expected upfront cost of the purchase. At this time, the only supported currency is @USD@ . For example, to indicate a limit price of USD 100, specify 100.00.
    limitPrice :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PurchaseHostReservation' with the minimum fields required to make a request.
--
-- * 'currencyCode' - The currency in which the @totalUpfrontPrice@ , @LimitPrice@ , and @totalHourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
-- * 'tagSpecifications' - The tags to apply to the Dedicated Host Reservation during purchase.
-- * 'hostIdSet' - The IDs of the Dedicated Hosts with which the reservation will be associated.
-- * 'offeringId' - The ID of the offering.
-- * 'limitPrice' - The specified limit is checked against the total upfront cost of the reservation (calculated as the offering's upfront cost multiplied by the host count). If the total upfront cost is greater than the specified price limit, the request fails. This is used to ensure that the purchase does not exceed the expected upfront cost of the purchase. At this time, the only supported currency is @USD@ . For example, to indicate a limit price of USD 100, specify 100.00.
mkPurchaseHostReservation ::
  -- | 'offeringId'
  Lude.Text ->
  PurchaseHostReservation
mkPurchaseHostReservation pOfferingId_ =
  PurchaseHostReservation'
    { currencyCode = Lude.Nothing,
      clientToken = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      hostIdSet = Lude.mempty,
      offeringId = pOfferingId_,
      limitPrice = Lude.Nothing
    }

-- | The currency in which the @totalUpfrontPrice@ , @LimitPrice@ , and @totalHourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrCurrencyCode :: Lens.Lens' PurchaseHostReservation (Lude.Maybe CurrencyCodeValues)
phrCurrencyCode = Lens.lens (currencyCode :: PurchaseHostReservation -> Lude.Maybe CurrencyCodeValues) (\s a -> s {currencyCode = a} :: PurchaseHostReservation)
{-# DEPRECATED phrCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrClientToken :: Lens.Lens' PurchaseHostReservation (Lude.Maybe Lude.Text)
phrClientToken = Lens.lens (clientToken :: PurchaseHostReservation -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: PurchaseHostReservation)
{-# DEPRECATED phrClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The tags to apply to the Dedicated Host Reservation during purchase.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrTagSpecifications :: Lens.Lens' PurchaseHostReservation (Lude.Maybe [TagSpecification])
phrTagSpecifications = Lens.lens (tagSpecifications :: PurchaseHostReservation -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: PurchaseHostReservation)
{-# DEPRECATED phrTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The IDs of the Dedicated Hosts with which the reservation will be associated.
--
-- /Note:/ Consider using 'hostIdSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrHostIdSet :: Lens.Lens' PurchaseHostReservation [Lude.Text]
phrHostIdSet = Lens.lens (hostIdSet :: PurchaseHostReservation -> [Lude.Text]) (\s a -> s {hostIdSet = a} :: PurchaseHostReservation)
{-# DEPRECATED phrHostIdSet "Use generic-lens or generic-optics with 'hostIdSet' instead." #-}

-- | The ID of the offering.
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrOfferingId :: Lens.Lens' PurchaseHostReservation Lude.Text
phrOfferingId = Lens.lens (offeringId :: PurchaseHostReservation -> Lude.Text) (\s a -> s {offeringId = a} :: PurchaseHostReservation)
{-# DEPRECATED phrOfferingId "Use generic-lens or generic-optics with 'offeringId' instead." #-}

-- | The specified limit is checked against the total upfront cost of the reservation (calculated as the offering's upfront cost multiplied by the host count). If the total upfront cost is greater than the specified price limit, the request fails. This is used to ensure that the purchase does not exceed the expected upfront cost of the purchase. At this time, the only supported currency is @USD@ . For example, to indicate a limit price of USD 100, specify 100.00.
--
-- /Note:/ Consider using 'limitPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrLimitPrice :: Lens.Lens' PurchaseHostReservation (Lude.Maybe Lude.Text)
phrLimitPrice = Lens.lens (limitPrice :: PurchaseHostReservation -> Lude.Maybe Lude.Text) (\s a -> s {limitPrice = a} :: PurchaseHostReservation)
{-# DEPRECATED phrLimitPrice "Use generic-lens or generic-optics with 'limitPrice' instead." #-}

instance Lude.AWSRequest PurchaseHostReservation where
  type Rs PurchaseHostReservation = PurchaseHostReservationResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          PurchaseHostReservationResponse'
            Lude.<$> (x Lude..@? "currencyCode")
            Lude.<*> (x Lude..@? "clientToken")
            Lude.<*> (x Lude..@? "totalHourlyPrice")
            Lude.<*> (x Lude..@? "totalUpfrontPrice")
            Lude.<*> ( x Lude..@? "purchase" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PurchaseHostReservation where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PurchaseHostReservation where
  toPath = Lude.const "/"

instance Lude.ToQuery PurchaseHostReservation where
  toQuery PurchaseHostReservation' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("PurchaseHostReservation" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "CurrencyCode" Lude.=: currencyCode,
        "ClientToken" Lude.=: clientToken,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        Lude.toQueryList "HostIdSet" hostIdSet,
        "OfferingId" Lude.=: offeringId,
        "LimitPrice" Lude.=: limitPrice
      ]

-- | /See:/ 'mkPurchaseHostReservationResponse' smart constructor.
data PurchaseHostReservationResponse = PurchaseHostReservationResponse'
  { -- | The currency in which the @totalUpfrontPrice@ and @totalHourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
    currencyCode :: Lude.Maybe CurrencyCodeValues,
    -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
    clientToken :: Lude.Maybe Lude.Text,
    -- | The total hourly price of the reservation calculated per hour.
    totalHourlyPrice :: Lude.Maybe Lude.Text,
    -- | The total amount charged to your account when you purchase the reservation.
    totalUpfrontPrice :: Lude.Maybe Lude.Text,
    -- | Describes the details of the purchase.
    purchase :: Lude.Maybe [Purchase],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PurchaseHostReservationResponse' with the minimum fields required to make a request.
--
-- * 'currencyCode' - The currency in which the @totalUpfrontPrice@ and @totalHourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
-- * 'totalHourlyPrice' - The total hourly price of the reservation calculated per hour.
-- * 'totalUpfrontPrice' - The total amount charged to your account when you purchase the reservation.
-- * 'purchase' - Describes the details of the purchase.
-- * 'responseStatus' - The response status code.
mkPurchaseHostReservationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PurchaseHostReservationResponse
mkPurchaseHostReservationResponse pResponseStatus_ =
  PurchaseHostReservationResponse'
    { currencyCode = Lude.Nothing,
      clientToken = Lude.Nothing,
      totalHourlyPrice = Lude.Nothing,
      totalUpfrontPrice = Lude.Nothing,
      purchase = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The currency in which the @totalUpfrontPrice@ and @totalHourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrrsCurrencyCode :: Lens.Lens' PurchaseHostReservationResponse (Lude.Maybe CurrencyCodeValues)
phrrsCurrencyCode = Lens.lens (currencyCode :: PurchaseHostReservationResponse -> Lude.Maybe CurrencyCodeValues) (\s a -> s {currencyCode = a} :: PurchaseHostReservationResponse)
{-# DEPRECATED phrrsCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrrsClientToken :: Lens.Lens' PurchaseHostReservationResponse (Lude.Maybe Lude.Text)
phrrsClientToken = Lens.lens (clientToken :: PurchaseHostReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: PurchaseHostReservationResponse)
{-# DEPRECATED phrrsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The total hourly price of the reservation calculated per hour.
--
-- /Note:/ Consider using 'totalHourlyPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrrsTotalHourlyPrice :: Lens.Lens' PurchaseHostReservationResponse (Lude.Maybe Lude.Text)
phrrsTotalHourlyPrice = Lens.lens (totalHourlyPrice :: PurchaseHostReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {totalHourlyPrice = a} :: PurchaseHostReservationResponse)
{-# DEPRECATED phrrsTotalHourlyPrice "Use generic-lens or generic-optics with 'totalHourlyPrice' instead." #-}

-- | The total amount charged to your account when you purchase the reservation.
--
-- /Note:/ Consider using 'totalUpfrontPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrrsTotalUpfrontPrice :: Lens.Lens' PurchaseHostReservationResponse (Lude.Maybe Lude.Text)
phrrsTotalUpfrontPrice = Lens.lens (totalUpfrontPrice :: PurchaseHostReservationResponse -> Lude.Maybe Lude.Text) (\s a -> s {totalUpfrontPrice = a} :: PurchaseHostReservationResponse)
{-# DEPRECATED phrrsTotalUpfrontPrice "Use generic-lens or generic-optics with 'totalUpfrontPrice' instead." #-}

-- | Describes the details of the purchase.
--
-- /Note:/ Consider using 'purchase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrrsPurchase :: Lens.Lens' PurchaseHostReservationResponse (Lude.Maybe [Purchase])
phrrsPurchase = Lens.lens (purchase :: PurchaseHostReservationResponse -> Lude.Maybe [Purchase]) (\s a -> s {purchase = a} :: PurchaseHostReservationResponse)
{-# DEPRECATED phrrsPurchase "Use generic-lens or generic-optics with 'purchase' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phrrsResponseStatus :: Lens.Lens' PurchaseHostReservationResponse Lude.Int
phrrsResponseStatus = Lens.lens (responseStatus :: PurchaseHostReservationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PurchaseHostReservationResponse)
{-# DEPRECATED phrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
