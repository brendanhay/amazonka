{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ScheduleAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ScheduleAction
  ( ScheduleAction (..),

    -- * Smart constructor
    mkScheduleAction,

    -- * Lenses
    saActionName,
    saScheduleActionStartSettings,
    saScheduleActionSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.ScheduleActionSettings as Types
import qualified Network.AWS.MediaLive.Types.ScheduleActionStartSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Contains information on a single schedule action.
--
-- /See:/ 'mkScheduleAction' smart constructor.
data ScheduleAction = ScheduleAction'
  { -- | The name of the action, must be unique within the schedule. This name provides the main reference to an action once it is added to the schedule. A name is unique if it is no longer in the schedule. The schedule is automatically cleaned up to remove actions with a start time of more than 1 hour ago (approximately) so at that point a name can be reused.
    actionName :: Core.Text,
    -- | The time for the action to start in the channel.
    scheduleActionStartSettings :: Types.ScheduleActionStartSettings,
    -- | Settings for this schedule action.
    scheduleActionSettings :: Types.ScheduleActionSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduleAction' value with any optional fields omitted.
mkScheduleAction ::
  -- | 'actionName'
  Core.Text ->
  -- | 'scheduleActionStartSettings'
  Types.ScheduleActionStartSettings ->
  -- | 'scheduleActionSettings'
  Types.ScheduleActionSettings ->
  ScheduleAction
mkScheduleAction
  actionName
  scheduleActionStartSettings
  scheduleActionSettings =
    ScheduleAction'
      { actionName,
        scheduleActionStartSettings,
        scheduleActionSettings
      }

-- | The name of the action, must be unique within the schedule. This name provides the main reference to an action once it is added to the schedule. A name is unique if it is no longer in the schedule. The schedule is automatically cleaned up to remove actions with a start time of more than 1 hour ago (approximately) so at that point a name can be reused.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saActionName :: Lens.Lens' ScheduleAction Core.Text
saActionName = Lens.field @"actionName"
{-# DEPRECATED saActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

-- | The time for the action to start in the channel.
--
-- /Note:/ Consider using 'scheduleActionStartSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saScheduleActionStartSettings :: Lens.Lens' ScheduleAction Types.ScheduleActionStartSettings
saScheduleActionStartSettings = Lens.field @"scheduleActionStartSettings"
{-# DEPRECATED saScheduleActionStartSettings "Use generic-lens or generic-optics with 'scheduleActionStartSettings' instead." #-}

-- | Settings for this schedule action.
--
-- /Note:/ Consider using 'scheduleActionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saScheduleActionSettings :: Lens.Lens' ScheduleAction Types.ScheduleActionSettings
saScheduleActionSettings = Lens.field @"scheduleActionSettings"
{-# DEPRECATED saScheduleActionSettings "Use generic-lens or generic-optics with 'scheduleActionSettings' instead." #-}

instance Core.FromJSON ScheduleAction where
  toJSON ScheduleAction {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("actionName" Core..= actionName),
            Core.Just
              ( "scheduleActionStartSettings"
                  Core..= scheduleActionStartSettings
              ),
            Core.Just
              ("scheduleActionSettings" Core..= scheduleActionSettings)
          ]
      )

instance Core.FromJSON ScheduleAction where
  parseJSON =
    Core.withObject "ScheduleAction" Core.$
      \x ->
        ScheduleAction'
          Core.<$> (x Core..: "actionName")
          Core.<*> (x Core..: "scheduleActionStartSettings")
          Core.<*> (x Core..: "scheduleActionSettings")
