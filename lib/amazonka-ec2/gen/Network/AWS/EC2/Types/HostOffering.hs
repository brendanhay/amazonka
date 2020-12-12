{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HostOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HostOffering
  ( HostOffering (..),

    -- * Smart constructor
    mkHostOffering,

    -- * Lenses
    hoInstanceFamily,
    hoCurrencyCode,
    hoHourlyPrice,
    hoUpfrontPrice,
    hoOfferingId,
    hoDuration,
    hoPaymentOption,
  )
where

import Network.AWS.EC2.Types.CurrencyCodeValues
import Network.AWS.EC2.Types.PaymentOption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about the Dedicated Host Reservation offering.
--
-- /See:/ 'mkHostOffering' smart constructor.
data HostOffering = HostOffering'
  { instanceFamily ::
      Lude.Maybe Lude.Text,
    currencyCode :: Lude.Maybe CurrencyCodeValues,
    hourlyPrice :: Lude.Maybe Lude.Text,
    upfrontPrice :: Lude.Maybe Lude.Text,
    offeringId :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'HostOffering' with the minimum fields required to make a request.
--
-- * 'currencyCode' - The currency of the offering.
-- * 'duration' - The duration of the offering (in seconds).
-- * 'hourlyPrice' - The hourly price of the offering.
-- * 'instanceFamily' - The instance family of the offering.
-- * 'offeringId' - The ID of the offering.
-- * 'paymentOption' - The available payment option.
-- * 'upfrontPrice' - The upfront price of the offering. Does not apply to No Upfront offerings.
mkHostOffering ::
  HostOffering
mkHostOffering =
  HostOffering'
    { instanceFamily = Lude.Nothing,
      currencyCode = Lude.Nothing,
      hourlyPrice = Lude.Nothing,
      upfrontPrice = Lude.Nothing,
      offeringId = Lude.Nothing,
      duration = Lude.Nothing,
      paymentOption = Lude.Nothing
    }

-- | The instance family of the offering.
--
-- /Note:/ Consider using 'instanceFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoInstanceFamily :: Lens.Lens' HostOffering (Lude.Maybe Lude.Text)
hoInstanceFamily = Lens.lens (instanceFamily :: HostOffering -> Lude.Maybe Lude.Text) (\s a -> s {instanceFamily = a} :: HostOffering)
{-# DEPRECATED hoInstanceFamily "Use generic-lens or generic-optics with 'instanceFamily' instead." #-}

-- | The currency of the offering.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoCurrencyCode :: Lens.Lens' HostOffering (Lude.Maybe CurrencyCodeValues)
hoCurrencyCode = Lens.lens (currencyCode :: HostOffering -> Lude.Maybe CurrencyCodeValues) (\s a -> s {currencyCode = a} :: HostOffering)
{-# DEPRECATED hoCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The hourly price of the offering.
--
-- /Note:/ Consider using 'hourlyPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoHourlyPrice :: Lens.Lens' HostOffering (Lude.Maybe Lude.Text)
hoHourlyPrice = Lens.lens (hourlyPrice :: HostOffering -> Lude.Maybe Lude.Text) (\s a -> s {hourlyPrice = a} :: HostOffering)
{-# DEPRECATED hoHourlyPrice "Use generic-lens or generic-optics with 'hourlyPrice' instead." #-}

-- | The upfront price of the offering. Does not apply to No Upfront offerings.
--
-- /Note:/ Consider using 'upfrontPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoUpfrontPrice :: Lens.Lens' HostOffering (Lude.Maybe Lude.Text)
hoUpfrontPrice = Lens.lens (upfrontPrice :: HostOffering -> Lude.Maybe Lude.Text) (\s a -> s {upfrontPrice = a} :: HostOffering)
{-# DEPRECATED hoUpfrontPrice "Use generic-lens or generic-optics with 'upfrontPrice' instead." #-}

-- | The ID of the offering.
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoOfferingId :: Lens.Lens' HostOffering (Lude.Maybe Lude.Text)
hoOfferingId = Lens.lens (offeringId :: HostOffering -> Lude.Maybe Lude.Text) (\s a -> s {offeringId = a} :: HostOffering)
{-# DEPRECATED hoOfferingId "Use generic-lens or generic-optics with 'offeringId' instead." #-}

-- | The duration of the offering (in seconds).
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoDuration :: Lens.Lens' HostOffering (Lude.Maybe Lude.Int)
hoDuration = Lens.lens (duration :: HostOffering -> Lude.Maybe Lude.Int) (\s a -> s {duration = a} :: HostOffering)
{-# DEPRECATED hoDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The available payment option.
--
-- /Note:/ Consider using 'paymentOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoPaymentOption :: Lens.Lens' HostOffering (Lude.Maybe PaymentOption)
hoPaymentOption = Lens.lens (paymentOption :: HostOffering -> Lude.Maybe PaymentOption) (\s a -> s {paymentOption = a} :: HostOffering)
{-# DEPRECATED hoPaymentOption "Use generic-lens or generic-optics with 'paymentOption' instead." #-}

instance Lude.FromXML HostOffering where
  parseXML x =
    HostOffering'
      Lude.<$> (x Lude..@? "instanceFamily")
      Lude.<*> (x Lude..@? "currencyCode")
      Lude.<*> (x Lude..@? "hourlyPrice")
      Lude.<*> (x Lude..@? "upfrontPrice")
      Lude.<*> (x Lude..@? "offeringId")
      Lude.<*> (x Lude..@? "duration")
      Lude.<*> (x Lude..@? "paymentOption")
