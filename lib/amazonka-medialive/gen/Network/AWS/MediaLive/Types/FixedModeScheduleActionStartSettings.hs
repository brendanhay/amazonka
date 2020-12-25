{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FixedModeScheduleActionStartSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FixedModeScheduleActionStartSettings
  ( FixedModeScheduleActionStartSettings (..),

    -- * Smart constructor
    mkFixedModeScheduleActionStartSettings,

    -- * Lenses
    fmsassTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Start time for the action.
--
-- /See:/ 'mkFixedModeScheduleActionStartSettings' smart constructor.
newtype FixedModeScheduleActionStartSettings = FixedModeScheduleActionStartSettings'
  { -- | Start time for the action to start in the channel. (Not the time for the action to be added to the schedule: actions are always added to the schedule immediately.) UTC format: yyyy-mm-ddThh:mm:ss.nnnZ. All the letters are digits (for example, mm might be 01) except for the two constants "T" for time and "Z" for "UTC format".
    time :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'FixedModeScheduleActionStartSettings' value with any optional fields omitted.
mkFixedModeScheduleActionStartSettings ::
  -- | 'time'
  Core.Text ->
  FixedModeScheduleActionStartSettings
mkFixedModeScheduleActionStartSettings time =
  FixedModeScheduleActionStartSettings' {time}

-- | Start time for the action to start in the channel. (Not the time for the action to be added to the schedule: actions are always added to the schedule immediately.) UTC format: yyyy-mm-ddThh:mm:ss.nnnZ. All the letters are digits (for example, mm might be 01) except for the two constants "T" for time and "Z" for "UTC format".
--
-- /Note:/ Consider using 'time' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmsassTime :: Lens.Lens' FixedModeScheduleActionStartSettings Core.Text
fmsassTime = Lens.field @"time"
{-# DEPRECATED fmsassTime "Use generic-lens or generic-optics with 'time' instead." #-}

instance Core.FromJSON FixedModeScheduleActionStartSettings where
  toJSON FixedModeScheduleActionStartSettings {..} =
    Core.object (Core.catMaybes [Core.Just ("time" Core..= time)])

instance Core.FromJSON FixedModeScheduleActionStartSettings where
  parseJSON =
    Core.withObject "FixedModeScheduleActionStartSettings" Core.$
      \x ->
        FixedModeScheduleActionStartSettings' Core.<$> (x Core..: "time")
