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
import qualified Network.AWS.Prelude as Lude

-- | The @Status@ and @TrackingNumber@ information for an inbound or outbound shipment.
--
-- /See:/ 'mkShipment' smart constructor.
data Shipment = Shipment'
  { -- | Status information for a shipment.
    status :: Lude.Maybe Lude.Text,
    -- | The tracking number for this job. Using this tracking number with your region's carrier's website, you can track a Snow device as the carrier transports it.
    --
    -- For India, the carrier is Amazon Logistics. For all other regions, UPS is the carrier.
    trackingNumber :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Shipment' with the minimum fields required to make a request.
--
-- * 'status' - Status information for a shipment.
-- * 'trackingNumber' - The tracking number for this job. Using this tracking number with your region's carrier's website, you can track a Snow device as the carrier transports it.
--
-- For India, the carrier is Amazon Logistics. For all other regions, UPS is the carrier.
mkShipment ::
  Shipment
mkShipment =
  Shipment' {status = Lude.Nothing, trackingNumber = Lude.Nothing}

-- | Status information for a shipment.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStatus :: Lens.Lens' Shipment (Lude.Maybe Lude.Text)
sStatus = Lens.lens (status :: Shipment -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: Shipment)
{-# DEPRECATED sStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The tracking number for this job. Using this tracking number with your region's carrier's website, you can track a Snow device as the carrier transports it.
--
-- For India, the carrier is Amazon Logistics. For all other regions, UPS is the carrier.
--
-- /Note:/ Consider using 'trackingNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTrackingNumber :: Lens.Lens' Shipment (Lude.Maybe Lude.Text)
sTrackingNumber = Lens.lens (trackingNumber :: Shipment -> Lude.Maybe Lude.Text) (\s a -> s {trackingNumber = a} :: Shipment)
{-# DEPRECATED sTrackingNumber "Use generic-lens or generic-optics with 'trackingNumber' instead." #-}

instance Lude.FromJSON Shipment where
  parseJSON =
    Lude.withObject
      "Shipment"
      ( \x ->
          Shipment'
            Lude.<$> (x Lude..:? "Status") Lude.<*> (x Lude..:? "TrackingNumber")
      )
