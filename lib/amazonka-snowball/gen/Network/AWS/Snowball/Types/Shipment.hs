{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.Shipment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.Shipment
  ( Shipment (..),

    -- * Smart constructor
    mkShipment,

    -- * Lenses
    sStatus,
    sTrackingNumber,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Snowball.Types.String as Types

-- | The @Status@ and @TrackingNumber@ information for an inbound or outbound shipment.
--
-- /See:/ 'mkShipment' smart constructor.
data Shipment = Shipment'
  { -- | Status information for a shipment.
    status :: Core.Maybe Types.String,
    -- | The tracking number for this job. Using this tracking number with your region's carrier's website, you can track a Snow device as the carrier transports it.
    --
    -- For India, the carrier is Amazon Logistics. For all other regions, UPS is the carrier.
    trackingNumber :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Shipment' value with any optional fields omitted.
mkShipment ::
  Shipment
mkShipment =
  Shipment' {status = Core.Nothing, trackingNumber = Core.Nothing}

-- | Status information for a shipment.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStatus :: Lens.Lens' Shipment (Core.Maybe Types.String)
sStatus = Lens.field @"status"
{-# DEPRECATED sStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The tracking number for this job. Using this tracking number with your region's carrier's website, you can track a Snow device as the carrier transports it.
--
-- For India, the carrier is Amazon Logistics. For all other regions, UPS is the carrier.
--
-- /Note:/ Consider using 'trackingNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTrackingNumber :: Lens.Lens' Shipment (Core.Maybe Types.String)
sTrackingNumber = Lens.field @"trackingNumber"
{-# DEPRECATED sTrackingNumber "Use generic-lens or generic-optics with 'trackingNumber' instead." #-}

instance Core.FromJSON Shipment where
  parseJSON =
    Core.withObject "Shipment" Core.$
      \x ->
        Shipment'
          Core.<$> (x Core..:? "Status") Core.<*> (x Core..:? "TrackingNumber")
