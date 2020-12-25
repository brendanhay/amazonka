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
    hoCurrencyCode,
    hoDuration,
    hoHourlyPrice,
    hoInstanceFamily,
    hoOfferingId,
    hoPaymentOption,
    hoUpfrontPrice,
  )
where

import qualified Network.AWS.EC2.Types.CurrencyCodeValues as Types
import qualified Network.AWS.EC2.Types.PaymentOption as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about the Dedicated Host Reservation offering.
--
-- /See:/ 'mkHostOffering' smart constructor.
data HostOffering = HostOffering'
  { -- | The currency of the offering.
    currencyCode :: Core.Maybe Types.CurrencyCodeValues,
    -- | The duration of the offering (in seconds).
    duration :: Core.Maybe Core.Int,
    -- | The hourly price of the offering.
    hourlyPrice :: Core.Maybe Types.String,
    -- | The instance family of the offering.
    instanceFamily :: Core.Maybe Types.String,
    -- | The ID of the offering.
    offeringId :: Core.Maybe Types.String,
    -- | The available payment option.
    paymentOption :: Core.Maybe Types.PaymentOption,
    -- | The upfront price of the offering. Does not apply to No Upfront offerings.
    upfrontPrice :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HostOffering' value with any optional fields omitted.
mkHostOffering ::
  HostOffering
mkHostOffering =
  HostOffering'
    { currencyCode = Core.Nothing,
      duration = Core.Nothing,
      hourlyPrice = Core.Nothing,
      instanceFamily = Core.Nothing,
      offeringId = Core.Nothing,
      paymentOption = Core.Nothing,
      upfrontPrice = Core.Nothing
    }

-- | The currency of the offering.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoCurrencyCode :: Lens.Lens' HostOffering (Core.Maybe Types.CurrencyCodeValues)
hoCurrencyCode = Lens.field @"currencyCode"
{-# DEPRECATED hoCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The duration of the offering (in seconds).
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoDuration :: Lens.Lens' HostOffering (Core.Maybe Core.Int)
hoDuration = Lens.field @"duration"
{-# DEPRECATED hoDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The hourly price of the offering.
--
-- /Note:/ Consider using 'hourlyPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoHourlyPrice :: Lens.Lens' HostOffering (Core.Maybe Types.String)
hoHourlyPrice = Lens.field @"hourlyPrice"
{-# DEPRECATED hoHourlyPrice "Use generic-lens or generic-optics with 'hourlyPrice' instead." #-}

-- | The instance family of the offering.
--
-- /Note:/ Consider using 'instanceFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoInstanceFamily :: Lens.Lens' HostOffering (Core.Maybe Types.String)
hoInstanceFamily = Lens.field @"instanceFamily"
{-# DEPRECATED hoInstanceFamily "Use generic-lens or generic-optics with 'instanceFamily' instead." #-}

-- | The ID of the offering.
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoOfferingId :: Lens.Lens' HostOffering (Core.Maybe Types.String)
hoOfferingId = Lens.field @"offeringId"
{-# DEPRECATED hoOfferingId "Use generic-lens or generic-optics with 'offeringId' instead." #-}

-- | The available payment option.
--
-- /Note:/ Consider using 'paymentOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoPaymentOption :: Lens.Lens' HostOffering (Core.Maybe Types.PaymentOption)
hoPaymentOption = Lens.field @"paymentOption"
{-# DEPRECATED hoPaymentOption "Use generic-lens or generic-optics with 'paymentOption' instead." #-}

-- | The upfront price of the offering. Does not apply to No Upfront offerings.
--
-- /Note:/ Consider using 'upfrontPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoUpfrontPrice :: Lens.Lens' HostOffering (Core.Maybe Types.String)
hoUpfrontPrice = Lens.field @"upfrontPrice"
{-# DEPRECATED hoUpfrontPrice "Use generic-lens or generic-optics with 'upfrontPrice' instead." #-}

instance Core.FromXML HostOffering where
  parseXML x =
    HostOffering'
      Core.<$> (x Core..@? "currencyCode")
      Core.<*> (x Core..@? "duration")
      Core.<*> (x Core..@? "hourlyPrice")
      Core.<*> (x Core..@? "instanceFamily")
      Core.<*> (x Core..@? "offeringId")
      Core.<*> (x Core..@? "paymentOption")
      Core.<*> (x Core..@? "upfrontPrice")
