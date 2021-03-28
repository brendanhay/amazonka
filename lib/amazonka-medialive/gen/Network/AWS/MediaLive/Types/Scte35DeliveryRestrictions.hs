{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35DeliveryRestrictions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Scte35DeliveryRestrictions
  ( Scte35DeliveryRestrictions (..)
  -- * Smart constructor
  , mkScte35DeliveryRestrictions
  -- * Lenses
  , sdrDeviceRestrictions
  , sdrArchiveAllowedFlag
  , sdrWebDeliveryAllowedFlag
  , sdrNoRegionalBlackoutFlag
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.Scte35ArchiveAllowedFlag as Types
import qualified Network.AWS.MediaLive.Types.Scte35DeviceRestrictions as Types
import qualified Network.AWS.MediaLive.Types.Scte35NoRegionalBlackoutFlag as Types
import qualified Network.AWS.MediaLive.Types.Scte35WebDeliveryAllowedFlag as Types
import qualified Network.AWS.Prelude as Core

-- | Corresponds to SCTE-35 delivery_not_restricted_flag parameter. To declare delivery restrictions, include this element and its four "restriction" flags. To declare that there are no restrictions, omit this element.
--
-- /See:/ 'mkScte35DeliveryRestrictions' smart constructor.
data Scte35DeliveryRestrictions = Scte35DeliveryRestrictions'
  { deviceRestrictions :: Types.Scte35DeviceRestrictions
    -- ^ Corresponds to SCTE-35 device_restrictions parameter.
  , archiveAllowedFlag :: Types.Scte35ArchiveAllowedFlag
    -- ^ Corresponds to SCTE-35 archive_allowed_flag.
  , webDeliveryAllowedFlag :: Types.Scte35WebDeliveryAllowedFlag
    -- ^ Corresponds to SCTE-35 web_delivery_allowed_flag parameter.
  , noRegionalBlackoutFlag :: Types.Scte35NoRegionalBlackoutFlag
    -- ^ Corresponds to SCTE-35 no_regional_blackout_flag parameter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Scte35DeliveryRestrictions' value with any optional fields omitted.
mkScte35DeliveryRestrictions
    :: Types.Scte35DeviceRestrictions -- ^ 'deviceRestrictions'
    -> Types.Scte35ArchiveAllowedFlag -- ^ 'archiveAllowedFlag'
    -> Types.Scte35WebDeliveryAllowedFlag -- ^ 'webDeliveryAllowedFlag'
    -> Types.Scte35NoRegionalBlackoutFlag -- ^ 'noRegionalBlackoutFlag'
    -> Scte35DeliveryRestrictions
mkScte35DeliveryRestrictions deviceRestrictions archiveAllowedFlag
  webDeliveryAllowedFlag noRegionalBlackoutFlag
  = Scte35DeliveryRestrictions'{deviceRestrictions,
                                archiveAllowedFlag, webDeliveryAllowedFlag, noRegionalBlackoutFlag}

-- | Corresponds to SCTE-35 device_restrictions parameter.
--
-- /Note:/ Consider using 'deviceRestrictions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrDeviceRestrictions :: Lens.Lens' Scte35DeliveryRestrictions Types.Scte35DeviceRestrictions
sdrDeviceRestrictions = Lens.field @"deviceRestrictions"
{-# INLINEABLE sdrDeviceRestrictions #-}
{-# DEPRECATED deviceRestrictions "Use generic-lens or generic-optics with 'deviceRestrictions' instead"  #-}

-- | Corresponds to SCTE-35 archive_allowed_flag.
--
-- /Note:/ Consider using 'archiveAllowedFlag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrArchiveAllowedFlag :: Lens.Lens' Scte35DeliveryRestrictions Types.Scte35ArchiveAllowedFlag
sdrArchiveAllowedFlag = Lens.field @"archiveAllowedFlag"
{-# INLINEABLE sdrArchiveAllowedFlag #-}
{-# DEPRECATED archiveAllowedFlag "Use generic-lens or generic-optics with 'archiveAllowedFlag' instead"  #-}

-- | Corresponds to SCTE-35 web_delivery_allowed_flag parameter.
--
-- /Note:/ Consider using 'webDeliveryAllowedFlag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrWebDeliveryAllowedFlag :: Lens.Lens' Scte35DeliveryRestrictions Types.Scte35WebDeliveryAllowedFlag
sdrWebDeliveryAllowedFlag = Lens.field @"webDeliveryAllowedFlag"
{-# INLINEABLE sdrWebDeliveryAllowedFlag #-}
{-# DEPRECATED webDeliveryAllowedFlag "Use generic-lens or generic-optics with 'webDeliveryAllowedFlag' instead"  #-}

-- | Corresponds to SCTE-35 no_regional_blackout_flag parameter.
--
-- /Note:/ Consider using 'noRegionalBlackoutFlag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrNoRegionalBlackoutFlag :: Lens.Lens' Scte35DeliveryRestrictions Types.Scte35NoRegionalBlackoutFlag
sdrNoRegionalBlackoutFlag = Lens.field @"noRegionalBlackoutFlag"
{-# INLINEABLE sdrNoRegionalBlackoutFlag #-}
{-# DEPRECATED noRegionalBlackoutFlag "Use generic-lens or generic-optics with 'noRegionalBlackoutFlag' instead"  #-}

instance Core.FromJSON Scte35DeliveryRestrictions where
        toJSON Scte35DeliveryRestrictions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("deviceRestrictions" Core..= deviceRestrictions),
                  Core.Just ("archiveAllowedFlag" Core..= archiveAllowedFlag),
                  Core.Just
                    ("webDeliveryAllowedFlag" Core..= webDeliveryAllowedFlag),
                  Core.Just
                    ("noRegionalBlackoutFlag" Core..= noRegionalBlackoutFlag)])

instance Core.FromJSON Scte35DeliveryRestrictions where
        parseJSON
          = Core.withObject "Scte35DeliveryRestrictions" Core.$
              \ x ->
                Scte35DeliveryRestrictions' Core.<$>
                  (x Core..: "deviceRestrictions") Core.<*>
                    x Core..: "archiveAllowedFlag"
                    Core.<*> x Core..: "webDeliveryAllowedFlag"
                    Core.<*> x Core..: "noRegionalBlackoutFlag"
