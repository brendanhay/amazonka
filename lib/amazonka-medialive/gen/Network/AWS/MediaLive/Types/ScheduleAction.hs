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
import Network.AWS.MediaLive.Types.ScheduleActionSettings
import Network.AWS.MediaLive.Types.ScheduleActionStartSettings
import qualified Network.AWS.Prelude as Lude

-- | Contains information on a single schedule action.
--
-- /See:/ 'mkScheduleAction' smart constructor.
data ScheduleAction = ScheduleAction'
  { actionName :: Lude.Text,
    scheduleActionStartSettings :: ScheduleActionStartSettings,
    scheduleActionSettings :: ScheduleActionSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduleAction' with the minimum fields required to make a request.
--
-- * 'actionName' - The name of the action, must be unique within the schedule. This name provides the main reference to an action once it is added to the schedule. A name is unique if it is no longer in the schedule. The schedule is automatically cleaned up to remove actions with a start time of more than 1 hour ago (approximately) so at that point a name can be reused.
-- * 'scheduleActionSettings' - Settings for this schedule action.
-- * 'scheduleActionStartSettings' - The time for the action to start in the channel.
mkScheduleAction ::
  -- | 'actionName'
  Lude.Text ->
  -- | 'scheduleActionStartSettings'
  ScheduleActionStartSettings ->
  -- | 'scheduleActionSettings'
  ScheduleActionSettings ->
  ScheduleAction
mkScheduleAction
  pActionName_
  pScheduleActionStartSettings_
  pScheduleActionSettings_ =
    ScheduleAction'
      { actionName = pActionName_,
        scheduleActionStartSettings = pScheduleActionStartSettings_,
        scheduleActionSettings = pScheduleActionSettings_
      }

-- | The name of the action, must be unique within the schedule. This name provides the main reference to an action once it is added to the schedule. A name is unique if it is no longer in the schedule. The schedule is automatically cleaned up to remove actions with a start time of more than 1 hour ago (approximately) so at that point a name can be reused.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saActionName :: Lens.Lens' ScheduleAction Lude.Text
saActionName = Lens.lens (actionName :: ScheduleAction -> Lude.Text) (\s a -> s {actionName = a} :: ScheduleAction)
{-# DEPRECATED saActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

-- | The time for the action to start in the channel.
--
-- /Note:/ Consider using 'scheduleActionStartSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saScheduleActionStartSettings :: Lens.Lens' ScheduleAction ScheduleActionStartSettings
saScheduleActionStartSettings = Lens.lens (scheduleActionStartSettings :: ScheduleAction -> ScheduleActionStartSettings) (\s a -> s {scheduleActionStartSettings = a} :: ScheduleAction)
{-# DEPRECATED saScheduleActionStartSettings "Use generic-lens or generic-optics with 'scheduleActionStartSettings' instead." #-}

-- | Settings for this schedule action.
--
-- /Note:/ Consider using 'scheduleActionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saScheduleActionSettings :: Lens.Lens' ScheduleAction ScheduleActionSettings
saScheduleActionSettings = Lens.lens (scheduleActionSettings :: ScheduleAction -> ScheduleActionSettings) (\s a -> s {scheduleActionSettings = a} :: ScheduleAction)
{-# DEPRECATED saScheduleActionSettings "Use generic-lens or generic-optics with 'scheduleActionSettings' instead." #-}

instance Lude.FromJSON ScheduleAction where
  parseJSON =
    Lude.withObject
      "ScheduleAction"
      ( \x ->
          ScheduleAction'
            Lude.<$> (x Lude..: "actionName")
            Lude.<*> (x Lude..: "scheduleActionStartSettings")
            Lude.<*> (x Lude..: "scheduleActionSettings")
      )

instance Lude.ToJSON ScheduleAction where
  toJSON ScheduleAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("actionName" Lude..= actionName),
            Lude.Just
              ( "scheduleActionStartSettings"
                  Lude..= scheduleActionStartSettings
              ),
            Lude.Just
              ("scheduleActionSettings" Lude..= scheduleActionSettings)
          ]
      )
