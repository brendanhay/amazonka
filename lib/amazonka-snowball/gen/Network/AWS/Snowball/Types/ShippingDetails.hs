{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.ShippingDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.ShippingDetails
  ( ShippingDetails (..),

    -- * Smart constructor
    mkShippingDetails,

    -- * Lenses
    sdInboundShipment,
    sdOutboundShipment,
    sdShippingOption,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Snowball.Types.Shipment as Types
import qualified Network.AWS.Snowball.Types.ShippingOption as Types

-- | A job's shipping information, including inbound and outbound tracking numbers and shipping speed options.
--
-- /See:/ 'mkShippingDetails' smart constructor.
data ShippingDetails = ShippingDetails'
  { -- | The @Status@ and @TrackingNumber@ values for a Snow device being returned to AWS for a particular job.
    inboundShipment :: Core.Maybe Types.Shipment,
    -- | The @Status@ and @TrackingNumber@ values for a Snow device being delivered to the address that you specified for a particular job.
    outboundShipment :: Core.Maybe Types.Shipment,
    -- | The shipping speed for a particular job. This speed doesn't dictate how soon you'll get the Snow device from the job's creation date. This speed represents how quickly it moves to its destination while in transit. Regional shipping speeds are as follows:
    --
    --
    --     * In Australia, you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day.
    --
    --
    --     * In the European Union (EU), you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.
    --
    --
    --     * In India, Snow device are delivered in one to seven days.
    --
    --
    --     * In the United States of America (US), you have access to one-day shipping and two-day shipping.
    shippingOption :: Core.Maybe Types.ShippingOption
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ShippingDetails' value with any optional fields omitted.
mkShippingDetails ::
  ShippingDetails
mkShippingDetails =
  ShippingDetails'
    { inboundShipment = Core.Nothing,
      outboundShipment = Core.Nothing,
      shippingOption = Core.Nothing
    }

-- | The @Status@ and @TrackingNumber@ values for a Snow device being returned to AWS for a particular job.
--
-- /Note:/ Consider using 'inboundShipment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdInboundShipment :: Lens.Lens' ShippingDetails (Core.Maybe Types.Shipment)
sdInboundShipment = Lens.field @"inboundShipment"
{-# DEPRECATED sdInboundShipment "Use generic-lens or generic-optics with 'inboundShipment' instead." #-}

-- | The @Status@ and @TrackingNumber@ values for a Snow device being delivered to the address that you specified for a particular job.
--
-- /Note:/ Consider using 'outboundShipment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdOutboundShipment :: Lens.Lens' ShippingDetails (Core.Maybe Types.Shipment)
sdOutboundShipment = Lens.field @"outboundShipment"
{-# DEPRECATED sdOutboundShipment "Use generic-lens or generic-optics with 'outboundShipment' instead." #-}

-- | The shipping speed for a particular job. This speed doesn't dictate how soon you'll get the Snow device from the job's creation date. This speed represents how quickly it moves to its destination while in transit. Regional shipping speeds are as follows:
--
--
--     * In Australia, you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day.
--
--
--     * In the European Union (EU), you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.
--
--
--     * In India, Snow device are delivered in one to seven days.
--
--
--     * In the United States of America (US), you have access to one-day shipping and two-day shipping.
--
--
--
-- /Note:/ Consider using 'shippingOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdShippingOption :: Lens.Lens' ShippingDetails (Core.Maybe Types.ShippingOption)
sdShippingOption = Lens.field @"shippingOption"
{-# DEPRECATED sdShippingOption "Use generic-lens or generic-optics with 'shippingOption' instead." #-}

instance Core.FromJSON ShippingDetails where
  parseJSON =
    Core.withObject "ShippingDetails" Core.$
      \x ->
        ShippingDetails'
          Core.<$> (x Core..:? "InboundShipment")
          Core.<*> (x Core..:? "OutboundShipment")
          Core.<*> (x Core..:? "ShippingOption")
