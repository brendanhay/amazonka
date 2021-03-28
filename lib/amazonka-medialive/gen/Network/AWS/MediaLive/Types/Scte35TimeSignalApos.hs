{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35TimeSignalApos
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Scte35TimeSignalApos
  ( Scte35TimeSignalApos (..)
  -- * Smart constructor
  , mkScte35TimeSignalApos
  -- * Lenses
  , stsaAdAvailOffset
  , stsaNoRegionalBlackoutFlag
  , stsaWebDeliveryAllowedFlag
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.Scte35AposNoRegionalBlackoutBehavior as Types
import qualified Network.AWS.MediaLive.Types.Scte35AposWebDeliveryAllowedBehavior as Types
import qualified Network.AWS.Prelude as Core

-- | Scte35 Time Signal Apos
--
-- /See:/ 'mkScte35TimeSignalApos' smart constructor.
data Scte35TimeSignalApos = Scte35TimeSignalApos'
  { adAvailOffset :: Core.Maybe Core.Int
    -- ^ When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time. This only applies to embedded SCTE 104/35 messages and does not apply to OOB messages.
  , noRegionalBlackoutFlag :: Core.Maybe Types.Scte35AposNoRegionalBlackoutBehavior
    -- ^ When set to ignore, Segment Descriptors with noRegionalBlackoutFlag set to 0 will no longer trigger blackouts or Ad Avail slates
  , webDeliveryAllowedFlag :: Core.Maybe Types.Scte35AposWebDeliveryAllowedBehavior
    -- ^ When set to ignore, Segment Descriptors with webDeliveryAllowedFlag set to 0 will no longer trigger blackouts or Ad Avail slates
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Scte35TimeSignalApos' value with any optional fields omitted.
mkScte35TimeSignalApos
    :: Scte35TimeSignalApos
mkScte35TimeSignalApos
  = Scte35TimeSignalApos'{adAvailOffset = Core.Nothing,
                          noRegionalBlackoutFlag = Core.Nothing,
                          webDeliveryAllowedFlag = Core.Nothing}

-- | When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time. This only applies to embedded SCTE 104/35 messages and does not apply to OOB messages.
--
-- /Note:/ Consider using 'adAvailOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsaAdAvailOffset :: Lens.Lens' Scte35TimeSignalApos (Core.Maybe Core.Int)
stsaAdAvailOffset = Lens.field @"adAvailOffset"
{-# INLINEABLE stsaAdAvailOffset #-}
{-# DEPRECATED adAvailOffset "Use generic-lens or generic-optics with 'adAvailOffset' instead"  #-}

-- | When set to ignore, Segment Descriptors with noRegionalBlackoutFlag set to 0 will no longer trigger blackouts or Ad Avail slates
--
-- /Note:/ Consider using 'noRegionalBlackoutFlag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsaNoRegionalBlackoutFlag :: Lens.Lens' Scte35TimeSignalApos (Core.Maybe Types.Scte35AposNoRegionalBlackoutBehavior)
stsaNoRegionalBlackoutFlag = Lens.field @"noRegionalBlackoutFlag"
{-# INLINEABLE stsaNoRegionalBlackoutFlag #-}
{-# DEPRECATED noRegionalBlackoutFlag "Use generic-lens or generic-optics with 'noRegionalBlackoutFlag' instead"  #-}

-- | When set to ignore, Segment Descriptors with webDeliveryAllowedFlag set to 0 will no longer trigger blackouts or Ad Avail slates
--
-- /Note:/ Consider using 'webDeliveryAllowedFlag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsaWebDeliveryAllowedFlag :: Lens.Lens' Scte35TimeSignalApos (Core.Maybe Types.Scte35AposWebDeliveryAllowedBehavior)
stsaWebDeliveryAllowedFlag = Lens.field @"webDeliveryAllowedFlag"
{-# INLINEABLE stsaWebDeliveryAllowedFlag #-}
{-# DEPRECATED webDeliveryAllowedFlag "Use generic-lens or generic-optics with 'webDeliveryAllowedFlag' instead"  #-}

instance Core.FromJSON Scte35TimeSignalApos where
        toJSON Scte35TimeSignalApos{..}
          = Core.object
              (Core.catMaybes
                 [("adAvailOffset" Core..=) Core.<$> adAvailOffset,
                  ("noRegionalBlackoutFlag" Core..=) Core.<$> noRegionalBlackoutFlag,
                  ("webDeliveryAllowedFlag" Core..=) Core.<$>
                    webDeliveryAllowedFlag])

instance Core.FromJSON Scte35TimeSignalApos where
        parseJSON
          = Core.withObject "Scte35TimeSignalApos" Core.$
              \ x ->
                Scte35TimeSignalApos' Core.<$>
                  (x Core..:? "adAvailOffset") Core.<*>
                    x Core..:? "noRegionalBlackoutFlag"
                    Core.<*> x Core..:? "webDeliveryAllowedFlag"
