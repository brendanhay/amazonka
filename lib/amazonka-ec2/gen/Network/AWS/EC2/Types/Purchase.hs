-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Purchase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Purchase
  ( Purchase (..),

    -- * Smart constructor
    mkPurchase,

    -- * Lenses
    pInstanceFamily,
    pCurrencyCode,
    pHostReservationId,
    pHourlyPrice,
    pUpfrontPrice,
    pHostIdSet,
    pDuration,
    pPaymentOption,
  )
where

import Network.AWS.EC2.Types.CurrencyCodeValues
import Network.AWS.EC2.Types.PaymentOption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the result of the purchase.
--
-- /See:/ 'mkPurchase' smart constructor.
data Purchase = Purchase'
  { instanceFamily :: Lude.Maybe Lude.Text,
    currencyCode :: Lude.Maybe CurrencyCodeValues,
    hostReservationId :: Lude.Maybe Lude.Text,
    hourlyPrice :: Lude.Maybe Lude.Text,
    upfrontPrice :: Lude.Maybe Lude.Text,
    hostIdSet :: Lude.Maybe [Lude.Text],
    duration :: Lude.Maybe Lude.Int,
    paymentOption :: Lude.Maybe PaymentOption
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Purchase' with the minimum fields required to make a request.
--
-- * 'currencyCode' - The currency in which the @UpfrontPrice@ and @HourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
-- * 'duration' - The duration of the reservation's term in seconds.
-- * 'hostIdSet' - The IDs of the Dedicated Hosts associated with the reservation.
-- * 'hostReservationId' - The ID of the reservation.
-- * 'hourlyPrice' - The hourly price of the reservation per hour.
-- * 'instanceFamily' - The instance family on the Dedicated Host that the reservation can be associated with.
-- * 'paymentOption' - The payment option for the reservation.
-- * 'upfrontPrice' - The upfront price of the reservation.
mkPurchase ::
  Purchase
mkPurchase =
  Purchase'
    { instanceFamily = Lude.Nothing,
      currencyCode = Lude.Nothing,
      hostReservationId = Lude.Nothing,
      hourlyPrice = Lude.Nothing,
      upfrontPrice = Lude.Nothing,
      hostIdSet = Lude.Nothing,
      duration = Lude.Nothing,
      paymentOption = Lude.Nothing
    }

-- | The instance family on the Dedicated Host that the reservation can be associated with.
--
-- /Note:/ Consider using 'instanceFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pInstanceFamily :: Lens.Lens' Purchase (Lude.Maybe Lude.Text)
pInstanceFamily = Lens.lens (instanceFamily :: Purchase -> Lude.Maybe Lude.Text) (\s a -> s {instanceFamily = a} :: Purchase)
{-# DEPRECATED pInstanceFamily "Use generic-lens or generic-optics with 'instanceFamily' instead." #-}

-- | The currency in which the @UpfrontPrice@ and @HourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCurrencyCode :: Lens.Lens' Purchase (Lude.Maybe CurrencyCodeValues)
pCurrencyCode = Lens.lens (currencyCode :: Purchase -> Lude.Maybe CurrencyCodeValues) (\s a -> s {currencyCode = a} :: Purchase)
{-# DEPRECATED pCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The ID of the reservation.
--
-- /Note:/ Consider using 'hostReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pHostReservationId :: Lens.Lens' Purchase (Lude.Maybe Lude.Text)
pHostReservationId = Lens.lens (hostReservationId :: Purchase -> Lude.Maybe Lude.Text) (\s a -> s {hostReservationId = a} :: Purchase)
{-# DEPRECATED pHostReservationId "Use generic-lens or generic-optics with 'hostReservationId' instead." #-}

-- | The hourly price of the reservation per hour.
--
-- /Note:/ Consider using 'hourlyPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pHourlyPrice :: Lens.Lens' Purchase (Lude.Maybe Lude.Text)
pHourlyPrice = Lens.lens (hourlyPrice :: Purchase -> Lude.Maybe Lude.Text) (\s a -> s {hourlyPrice = a} :: Purchase)
{-# DEPRECATED pHourlyPrice "Use generic-lens or generic-optics with 'hourlyPrice' instead." #-}

-- | The upfront price of the reservation.
--
-- /Note:/ Consider using 'upfrontPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pUpfrontPrice :: Lens.Lens' Purchase (Lude.Maybe Lude.Text)
pUpfrontPrice = Lens.lens (upfrontPrice :: Purchase -> Lude.Maybe Lude.Text) (\s a -> s {upfrontPrice = a} :: Purchase)
{-# DEPRECATED pUpfrontPrice "Use generic-lens or generic-optics with 'upfrontPrice' instead." #-}

-- | The IDs of the Dedicated Hosts associated with the reservation.
--
-- /Note:/ Consider using 'hostIdSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pHostIdSet :: Lens.Lens' Purchase (Lude.Maybe [Lude.Text])
pHostIdSet = Lens.lens (hostIdSet :: Purchase -> Lude.Maybe [Lude.Text]) (\s a -> s {hostIdSet = a} :: Purchase)
{-# DEPRECATED pHostIdSet "Use generic-lens or generic-optics with 'hostIdSet' instead." #-}

-- | The duration of the reservation's term in seconds.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDuration :: Lens.Lens' Purchase (Lude.Maybe Lude.Int)
pDuration = Lens.lens (duration :: Purchase -> Lude.Maybe Lude.Int) (\s a -> s {duration = a} :: Purchase)
{-# DEPRECATED pDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The payment option for the reservation.
--
-- /Note:/ Consider using 'paymentOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPaymentOption :: Lens.Lens' Purchase (Lude.Maybe PaymentOption)
pPaymentOption = Lens.lens (paymentOption :: Purchase -> Lude.Maybe PaymentOption) (\s a -> s {paymentOption = a} :: Purchase)
{-# DEPRECATED pPaymentOption "Use generic-lens or generic-optics with 'paymentOption' instead." #-}

instance Lude.FromXML Purchase where
  parseXML x =
    Purchase'
      Lude.<$> (x Lude..@? "instanceFamily")
      Lude.<*> (x Lude..@? "currencyCode")
      Lude.<*> (x Lude..@? "hostReservationId")
      Lude.<*> (x Lude..@? "hourlyPrice")
      Lude.<*> (x Lude..@? "upfrontPrice")
      Lude.<*> ( x Lude..@? "hostIdSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "duration")
      Lude.<*> (x Lude..@? "paymentOption")
