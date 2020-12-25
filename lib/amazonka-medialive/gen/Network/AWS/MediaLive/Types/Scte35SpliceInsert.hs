{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35SpliceInsert
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35SpliceInsert
  ( Scte35SpliceInsert (..),

    -- * Smart constructor
    mkScte35SpliceInsert,

    -- * Lenses
    ssiAdAvailOffset,
    ssiNoRegionalBlackoutFlag,
    ssiWebDeliveryAllowedFlag,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.Scte35SpliceInsertNoRegionalBlackoutBehavior as Types
import qualified Network.AWS.MediaLive.Types.Scte35SpliceInsertWebDeliveryAllowedBehavior as Types
import qualified Network.AWS.Prelude as Core

-- | Scte35 Splice Insert
--
-- /See:/ 'mkScte35SpliceInsert' smart constructor.
data Scte35SpliceInsert = Scte35SpliceInsert'
  { -- | When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time. This only applies to embedded SCTE 104/35 messages and does not apply to OOB messages.
    adAvailOffset :: Core.Maybe Core.Int,
    -- | When set to ignore, Segment Descriptors with noRegionalBlackoutFlag set to 0 will no longer trigger blackouts or Ad Avail slates
    noRegionalBlackoutFlag :: Core.Maybe Types.Scte35SpliceInsertNoRegionalBlackoutBehavior,
    -- | When set to ignore, Segment Descriptors with webDeliveryAllowedFlag set to 0 will no longer trigger blackouts or Ad Avail slates
    webDeliveryAllowedFlag :: Core.Maybe Types.Scte35SpliceInsertWebDeliveryAllowedBehavior
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Scte35SpliceInsert' value with any optional fields omitted.
mkScte35SpliceInsert ::
  Scte35SpliceInsert
mkScte35SpliceInsert =
  Scte35SpliceInsert'
    { adAvailOffset = Core.Nothing,
      noRegionalBlackoutFlag = Core.Nothing,
      webDeliveryAllowedFlag = Core.Nothing
    }

-- | When specified, this offset (in milliseconds) is added to the input Ad Avail PTS time. This only applies to embedded SCTE 104/35 messages and does not apply to OOB messages.
--
-- /Note:/ Consider using 'adAvailOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssiAdAvailOffset :: Lens.Lens' Scte35SpliceInsert (Core.Maybe Core.Int)
ssiAdAvailOffset = Lens.field @"adAvailOffset"
{-# DEPRECATED ssiAdAvailOffset "Use generic-lens or generic-optics with 'adAvailOffset' instead." #-}

-- | When set to ignore, Segment Descriptors with noRegionalBlackoutFlag set to 0 will no longer trigger blackouts or Ad Avail slates
--
-- /Note:/ Consider using 'noRegionalBlackoutFlag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssiNoRegionalBlackoutFlag :: Lens.Lens' Scte35SpliceInsert (Core.Maybe Types.Scte35SpliceInsertNoRegionalBlackoutBehavior)
ssiNoRegionalBlackoutFlag = Lens.field @"noRegionalBlackoutFlag"
{-# DEPRECATED ssiNoRegionalBlackoutFlag "Use generic-lens or generic-optics with 'noRegionalBlackoutFlag' instead." #-}

-- | When set to ignore, Segment Descriptors with webDeliveryAllowedFlag set to 0 will no longer trigger blackouts or Ad Avail slates
--
-- /Note:/ Consider using 'webDeliveryAllowedFlag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssiWebDeliveryAllowedFlag :: Lens.Lens' Scte35SpliceInsert (Core.Maybe Types.Scte35SpliceInsertWebDeliveryAllowedBehavior)
ssiWebDeliveryAllowedFlag = Lens.field @"webDeliveryAllowedFlag"
{-# DEPRECATED ssiWebDeliveryAllowedFlag "Use generic-lens or generic-optics with 'webDeliveryAllowedFlag' instead." #-}

instance Core.FromJSON Scte35SpliceInsert where
  toJSON Scte35SpliceInsert {..} =
    Core.object
      ( Core.catMaybes
          [ ("adAvailOffset" Core..=) Core.<$> adAvailOffset,
            ("noRegionalBlackoutFlag" Core..=) Core.<$> noRegionalBlackoutFlag,
            ("webDeliveryAllowedFlag" Core..=)
              Core.<$> webDeliveryAllowedFlag
          ]
      )

instance Core.FromJSON Scte35SpliceInsert where
  parseJSON =
    Core.withObject "Scte35SpliceInsert" Core.$
      \x ->
        Scte35SpliceInsert'
          Core.<$> (x Core..:? "adAvailOffset")
          Core.<*> (x Core..:? "noRegionalBlackoutFlag")
          Core.<*> (x Core..:? "webDeliveryAllowedFlag")
