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
    sdShippingOption,
    sdOutboundShipment,
    sdInboundShipment,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Snowball.Types.Shipment
import Network.AWS.Snowball.Types.ShippingOption

-- | A job's shipping information, including inbound and outbound tracking numbers and shipping speed options.
--
-- /See:/ 'mkShippingDetails' smart constructor.
data ShippingDetails = ShippingDetails'
  { -- | The shipping speed for a particular job. This speed doesn't dictate how soon you'll get the Snow device from the job's creation date. This speed represents how quickly it moves to its destination while in transit. Regional shipping speeds are as follows:
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
    shippingOption :: Lude.Maybe ShippingOption,
    -- | The @Status@ and @TrackingNumber@ values for a Snow device being delivered to the address that you specified for a particular job.
    outboundShipment :: Lude.Maybe Shipment,
    -- | The @Status@ and @TrackingNumber@ values for a Snow device being returned to AWS for a particular job.
    inboundShipment :: Lude.Maybe Shipment
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ShippingDetails' with the minimum fields required to make a request.
--
-- * 'shippingOption' - The shipping speed for a particular job. This speed doesn't dictate how soon you'll get the Snow device from the job's creation date. This speed represents how quickly it moves to its destination while in transit. Regional shipping speeds are as follows:
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
-- * 'outboundShipment' - The @Status@ and @TrackingNumber@ values for a Snow device being delivered to the address that you specified for a particular job.
-- * 'inboundShipment' - The @Status@ and @TrackingNumber@ values for a Snow device being returned to AWS for a particular job.
mkShippingDetails ::
  ShippingDetails
mkShippingDetails =
  ShippingDetails'
    { shippingOption = Lude.Nothing,
      outboundShipment = Lude.Nothing,
      inboundShipment = Lude.Nothing
    }

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
sdShippingOption :: Lens.Lens' ShippingDetails (Lude.Maybe ShippingOption)
sdShippingOption = Lens.lens (shippingOption :: ShippingDetails -> Lude.Maybe ShippingOption) (\s a -> s {shippingOption = a} :: ShippingDetails)
{-# DEPRECATED sdShippingOption "Use generic-lens or generic-optics with 'shippingOption' instead." #-}

-- | The @Status@ and @TrackingNumber@ values for a Snow device being delivered to the address that you specified for a particular job.
--
-- /Note:/ Consider using 'outboundShipment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdOutboundShipment :: Lens.Lens' ShippingDetails (Lude.Maybe Shipment)
sdOutboundShipment = Lens.lens (outboundShipment :: ShippingDetails -> Lude.Maybe Shipment) (\s a -> s {outboundShipment = a} :: ShippingDetails)
{-# DEPRECATED sdOutboundShipment "Use generic-lens or generic-optics with 'outboundShipment' instead." #-}

-- | The @Status@ and @TrackingNumber@ values for a Snow device being returned to AWS for a particular job.
--
-- /Note:/ Consider using 'inboundShipment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdInboundShipment :: Lens.Lens' ShippingDetails (Lude.Maybe Shipment)
sdInboundShipment = Lens.lens (inboundShipment :: ShippingDetails -> Lude.Maybe Shipment) (\s a -> s {inboundShipment = a} :: ShippingDetails)
{-# DEPRECATED sdInboundShipment "Use generic-lens or generic-optics with 'inboundShipment' instead." #-}

instance Lude.FromJSON ShippingDetails where
  parseJSON =
    Lude.withObject
      "ShippingDetails"
      ( \x ->
          ShippingDetails'
            Lude.<$> (x Lude..:? "ShippingOption")
            Lude.<*> (x Lude..:? "OutboundShipment")
            Lude.<*> (x Lude..:? "InboundShipment")
      )
