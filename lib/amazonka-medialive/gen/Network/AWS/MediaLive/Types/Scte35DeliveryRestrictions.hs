{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35DeliveryRestrictions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35DeliveryRestrictions
  ( Scte35DeliveryRestrictions (..),

    -- * Smart constructor
    mkScte35DeliveryRestrictions,

    -- * Lenses
    sdrArchiveAllowedFlag,
    sdrDeviceRestrictions,
    sdrWebDeliveryAllowedFlag,
    sdrNoRegionalBlackoutFlag,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Scte35ArchiveAllowedFlag
import Network.AWS.MediaLive.Types.Scte35DeviceRestrictions
import Network.AWS.MediaLive.Types.Scte35NoRegionalBlackoutFlag
import Network.AWS.MediaLive.Types.Scte35WebDeliveryAllowedFlag
import qualified Network.AWS.Prelude as Lude

-- | Corresponds to SCTE-35 delivery_not_restricted_flag parameter. To declare delivery restrictions, include this element and its four "restriction" flags. To declare that there are no restrictions, omit this element.
--
-- /See:/ 'mkScte35DeliveryRestrictions' smart constructor.
data Scte35DeliveryRestrictions = Scte35DeliveryRestrictions'
  { -- | Corresponds to SCTE-35 archive_allowed_flag.
    archiveAllowedFlag :: Scte35ArchiveAllowedFlag,
    -- | Corresponds to SCTE-35 device_restrictions parameter.
    deviceRestrictions :: Scte35DeviceRestrictions,
    -- | Corresponds to SCTE-35 web_delivery_allowed_flag parameter.
    webDeliveryAllowedFlag :: Scte35WebDeliveryAllowedFlag,
    -- | Corresponds to SCTE-35 no_regional_blackout_flag parameter.
    noRegionalBlackoutFlag :: Scte35NoRegionalBlackoutFlag
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Scte35DeliveryRestrictions' with the minimum fields required to make a request.
--
-- * 'archiveAllowedFlag' - Corresponds to SCTE-35 archive_allowed_flag.
-- * 'deviceRestrictions' - Corresponds to SCTE-35 device_restrictions parameter.
-- * 'webDeliveryAllowedFlag' - Corresponds to SCTE-35 web_delivery_allowed_flag parameter.
-- * 'noRegionalBlackoutFlag' - Corresponds to SCTE-35 no_regional_blackout_flag parameter.
mkScte35DeliveryRestrictions ::
  -- | 'archiveAllowedFlag'
  Scte35ArchiveAllowedFlag ->
  -- | 'deviceRestrictions'
  Scte35DeviceRestrictions ->
  -- | 'webDeliveryAllowedFlag'
  Scte35WebDeliveryAllowedFlag ->
  -- | 'noRegionalBlackoutFlag'
  Scte35NoRegionalBlackoutFlag ->
  Scte35DeliveryRestrictions
mkScte35DeliveryRestrictions
  pArchiveAllowedFlag_
  pDeviceRestrictions_
  pWebDeliveryAllowedFlag_
  pNoRegionalBlackoutFlag_ =
    Scte35DeliveryRestrictions'
      { archiveAllowedFlag =
          pArchiveAllowedFlag_,
        deviceRestrictions = pDeviceRestrictions_,
        webDeliveryAllowedFlag = pWebDeliveryAllowedFlag_,
        noRegionalBlackoutFlag = pNoRegionalBlackoutFlag_
      }

-- | Corresponds to SCTE-35 archive_allowed_flag.
--
-- /Note:/ Consider using 'archiveAllowedFlag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrArchiveAllowedFlag :: Lens.Lens' Scte35DeliveryRestrictions Scte35ArchiveAllowedFlag
sdrArchiveAllowedFlag = Lens.lens (archiveAllowedFlag :: Scte35DeliveryRestrictions -> Scte35ArchiveAllowedFlag) (\s a -> s {archiveAllowedFlag = a} :: Scte35DeliveryRestrictions)
{-# DEPRECATED sdrArchiveAllowedFlag "Use generic-lens or generic-optics with 'archiveAllowedFlag' instead." #-}

-- | Corresponds to SCTE-35 device_restrictions parameter.
--
-- /Note:/ Consider using 'deviceRestrictions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrDeviceRestrictions :: Lens.Lens' Scte35DeliveryRestrictions Scte35DeviceRestrictions
sdrDeviceRestrictions = Lens.lens (deviceRestrictions :: Scte35DeliveryRestrictions -> Scte35DeviceRestrictions) (\s a -> s {deviceRestrictions = a} :: Scte35DeliveryRestrictions)
{-# DEPRECATED sdrDeviceRestrictions "Use generic-lens or generic-optics with 'deviceRestrictions' instead." #-}

-- | Corresponds to SCTE-35 web_delivery_allowed_flag parameter.
--
-- /Note:/ Consider using 'webDeliveryAllowedFlag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrWebDeliveryAllowedFlag :: Lens.Lens' Scte35DeliveryRestrictions Scte35WebDeliveryAllowedFlag
sdrWebDeliveryAllowedFlag = Lens.lens (webDeliveryAllowedFlag :: Scte35DeliveryRestrictions -> Scte35WebDeliveryAllowedFlag) (\s a -> s {webDeliveryAllowedFlag = a} :: Scte35DeliveryRestrictions)
{-# DEPRECATED sdrWebDeliveryAllowedFlag "Use generic-lens or generic-optics with 'webDeliveryAllowedFlag' instead." #-}

-- | Corresponds to SCTE-35 no_regional_blackout_flag parameter.
--
-- /Note:/ Consider using 'noRegionalBlackoutFlag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrNoRegionalBlackoutFlag :: Lens.Lens' Scte35DeliveryRestrictions Scte35NoRegionalBlackoutFlag
sdrNoRegionalBlackoutFlag = Lens.lens (noRegionalBlackoutFlag :: Scte35DeliveryRestrictions -> Scte35NoRegionalBlackoutFlag) (\s a -> s {noRegionalBlackoutFlag = a} :: Scte35DeliveryRestrictions)
{-# DEPRECATED sdrNoRegionalBlackoutFlag "Use generic-lens or generic-optics with 'noRegionalBlackoutFlag' instead." #-}

instance Lude.FromJSON Scte35DeliveryRestrictions where
  parseJSON =
    Lude.withObject
      "Scte35DeliveryRestrictions"
      ( \x ->
          Scte35DeliveryRestrictions'
            Lude.<$> (x Lude..: "archiveAllowedFlag")
            Lude.<*> (x Lude..: "deviceRestrictions")
            Lude.<*> (x Lude..: "webDeliveryAllowedFlag")
            Lude.<*> (x Lude..: "noRegionalBlackoutFlag")
      )

instance Lude.ToJSON Scte35DeliveryRestrictions where
  toJSON Scte35DeliveryRestrictions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("archiveAllowedFlag" Lude..= archiveAllowedFlag),
            Lude.Just ("deviceRestrictions" Lude..= deviceRestrictions),
            Lude.Just
              ("webDeliveryAllowedFlag" Lude..= webDeliveryAllowedFlag),
            Lude.Just
              ("noRegionalBlackoutFlag" Lude..= noRegionalBlackoutFlag)
          ]
      )
