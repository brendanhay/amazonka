{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ScheduleActionStartSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.ScheduleActionStartSettings
  ( ScheduleActionStartSettings (..)
  -- * Smart constructor
  , mkScheduleActionStartSettings
  -- * Lenses
  , sassFixedModeScheduleActionStartSettings
  , sassFollowModeScheduleActionStartSettings
  , sassImmediateModeScheduleActionStartSettings
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.FixedModeScheduleActionStartSettings as Types
import qualified Network.AWS.MediaLive.Types.FollowModeScheduleActionStartSettings as Types
import qualified Network.AWS.MediaLive.Types.ImmediateModeScheduleActionStartSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Settings to specify when an action should occur. Only one of the options must be selected.
--
-- /See:/ 'mkScheduleActionStartSettings' smart constructor.
data ScheduleActionStartSettings = ScheduleActionStartSettings'
  { fixedModeScheduleActionStartSettings :: Core.Maybe Types.FixedModeScheduleActionStartSettings
    -- ^ Option for specifying the start time for an action.
  , followModeScheduleActionStartSettings :: Core.Maybe Types.FollowModeScheduleActionStartSettings
    -- ^ Option for specifying an action as relative to another action.
  , immediateModeScheduleActionStartSettings :: Core.Maybe Types.ImmediateModeScheduleActionStartSettings
    -- ^ Option for specifying an action that should be applied immediately.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduleActionStartSettings' value with any optional fields omitted.
mkScheduleActionStartSettings
    :: ScheduleActionStartSettings
mkScheduleActionStartSettings
  = ScheduleActionStartSettings'{fixedModeScheduleActionStartSettings
                                   = Core.Nothing,
                                 followModeScheduleActionStartSettings = Core.Nothing,
                                 immediateModeScheduleActionStartSettings = Core.Nothing}

-- | Option for specifying the start time for an action.
--
-- /Note:/ Consider using 'fixedModeScheduleActionStartSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sassFixedModeScheduleActionStartSettings :: Lens.Lens' ScheduleActionStartSettings (Core.Maybe Types.FixedModeScheduleActionStartSettings)
sassFixedModeScheduleActionStartSettings = Lens.field @"fixedModeScheduleActionStartSettings"
{-# INLINEABLE sassFixedModeScheduleActionStartSettings #-}
{-# DEPRECATED fixedModeScheduleActionStartSettings "Use generic-lens or generic-optics with 'fixedModeScheduleActionStartSettings' instead"  #-}

-- | Option for specifying an action as relative to another action.
--
-- /Note:/ Consider using 'followModeScheduleActionStartSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sassFollowModeScheduleActionStartSettings :: Lens.Lens' ScheduleActionStartSettings (Core.Maybe Types.FollowModeScheduleActionStartSettings)
sassFollowModeScheduleActionStartSettings = Lens.field @"followModeScheduleActionStartSettings"
{-# INLINEABLE sassFollowModeScheduleActionStartSettings #-}
{-# DEPRECATED followModeScheduleActionStartSettings "Use generic-lens or generic-optics with 'followModeScheduleActionStartSettings' instead"  #-}

-- | Option for specifying an action that should be applied immediately.
--
-- /Note:/ Consider using 'immediateModeScheduleActionStartSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sassImmediateModeScheduleActionStartSettings :: Lens.Lens' ScheduleActionStartSettings (Core.Maybe Types.ImmediateModeScheduleActionStartSettings)
sassImmediateModeScheduleActionStartSettings = Lens.field @"immediateModeScheduleActionStartSettings"
{-# INLINEABLE sassImmediateModeScheduleActionStartSettings #-}
{-# DEPRECATED immediateModeScheduleActionStartSettings "Use generic-lens or generic-optics with 'immediateModeScheduleActionStartSettings' instead"  #-}

instance Core.FromJSON ScheduleActionStartSettings where
        toJSON ScheduleActionStartSettings{..}
          = Core.object
              (Core.catMaybes
                 [("fixedModeScheduleActionStartSettings" Core..=) Core.<$>
                    fixedModeScheduleActionStartSettings,
                  ("followModeScheduleActionStartSettings" Core..=) Core.<$>
                    followModeScheduleActionStartSettings,
                  ("immediateModeScheduleActionStartSettings" Core..=) Core.<$>
                    immediateModeScheduleActionStartSettings])

instance Core.FromJSON ScheduleActionStartSettings where
        parseJSON
          = Core.withObject "ScheduleActionStartSettings" Core.$
              \ x ->
                ScheduleActionStartSettings' Core.<$>
                  (x Core..:? "fixedModeScheduleActionStartSettings") Core.<*>
                    x Core..:? "followModeScheduleActionStartSettings"
                    Core.<*> x Core..:? "immediateModeScheduleActionStartSettings"
