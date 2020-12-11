-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ScheduleActionStartSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ScheduleActionStartSettings
  ( ScheduleActionStartSettings (..),

    -- * Smart constructor
    mkScheduleActionStartSettings,

    -- * Lenses
    sassImmediateModeScheduleActionStartSettings,
    sassFollowModeScheduleActionStartSettings,
    sassFixedModeScheduleActionStartSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.FixedModeScheduleActionStartSettings
import Network.AWS.MediaLive.Types.FollowModeScheduleActionStartSettings
import Network.AWS.MediaLive.Types.ImmediateModeScheduleActionStartSettings
import qualified Network.AWS.Prelude as Lude

-- | Settings to specify when an action should occur. Only one of the options must be selected.
--
-- /See:/ 'mkScheduleActionStartSettings' smart constructor.
data ScheduleActionStartSettings = ScheduleActionStartSettings'
  { immediateModeScheduleActionStartSettings ::
      Lude.Maybe
        ImmediateModeScheduleActionStartSettings,
    followModeScheduleActionStartSettings ::
      Lude.Maybe
        FollowModeScheduleActionStartSettings,
    fixedModeScheduleActionStartSettings ::
      Lude.Maybe
        FixedModeScheduleActionStartSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduleActionStartSettings' with the minimum fields required to make a request.
--
-- * 'fixedModeScheduleActionStartSettings' - Option for specifying the start time for an action.
-- * 'followModeScheduleActionStartSettings' - Option for specifying an action as relative to another action.
-- * 'immediateModeScheduleActionStartSettings' - Option for specifying an action that should be applied immediately.
mkScheduleActionStartSettings ::
  ScheduleActionStartSettings
mkScheduleActionStartSettings =
  ScheduleActionStartSettings'
    { immediateModeScheduleActionStartSettings =
        Lude.Nothing,
      followModeScheduleActionStartSettings = Lude.Nothing,
      fixedModeScheduleActionStartSettings = Lude.Nothing
    }

-- | Option for specifying an action that should be applied immediately.
--
-- /Note:/ Consider using 'immediateModeScheduleActionStartSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sassImmediateModeScheduleActionStartSettings :: Lens.Lens' ScheduleActionStartSettings (Lude.Maybe ImmediateModeScheduleActionStartSettings)
sassImmediateModeScheduleActionStartSettings = Lens.lens (immediateModeScheduleActionStartSettings :: ScheduleActionStartSettings -> Lude.Maybe ImmediateModeScheduleActionStartSettings) (\s a -> s {immediateModeScheduleActionStartSettings = a} :: ScheduleActionStartSettings)
{-# DEPRECATED sassImmediateModeScheduleActionStartSettings "Use generic-lens or generic-optics with 'immediateModeScheduleActionStartSettings' instead." #-}

-- | Option for specifying an action as relative to another action.
--
-- /Note:/ Consider using 'followModeScheduleActionStartSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sassFollowModeScheduleActionStartSettings :: Lens.Lens' ScheduleActionStartSettings (Lude.Maybe FollowModeScheduleActionStartSettings)
sassFollowModeScheduleActionStartSettings = Lens.lens (followModeScheduleActionStartSettings :: ScheduleActionStartSettings -> Lude.Maybe FollowModeScheduleActionStartSettings) (\s a -> s {followModeScheduleActionStartSettings = a} :: ScheduleActionStartSettings)
{-# DEPRECATED sassFollowModeScheduleActionStartSettings "Use generic-lens or generic-optics with 'followModeScheduleActionStartSettings' instead." #-}

-- | Option for specifying the start time for an action.
--
-- /Note:/ Consider using 'fixedModeScheduleActionStartSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sassFixedModeScheduleActionStartSettings :: Lens.Lens' ScheduleActionStartSettings (Lude.Maybe FixedModeScheduleActionStartSettings)
sassFixedModeScheduleActionStartSettings = Lens.lens (fixedModeScheduleActionStartSettings :: ScheduleActionStartSettings -> Lude.Maybe FixedModeScheduleActionStartSettings) (\s a -> s {fixedModeScheduleActionStartSettings = a} :: ScheduleActionStartSettings)
{-# DEPRECATED sassFixedModeScheduleActionStartSettings "Use generic-lens or generic-optics with 'fixedModeScheduleActionStartSettings' instead." #-}

instance Lude.FromJSON ScheduleActionStartSettings where
  parseJSON =
    Lude.withObject
      "ScheduleActionStartSettings"
      ( \x ->
          ScheduleActionStartSettings'
            Lude.<$> (x Lude..:? "immediateModeScheduleActionStartSettings")
            Lude.<*> (x Lude..:? "followModeScheduleActionStartSettings")
            Lude.<*> (x Lude..:? "fixedModeScheduleActionStartSettings")
      )

instance Lude.ToJSON ScheduleActionStartSettings where
  toJSON ScheduleActionStartSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("immediateModeScheduleActionStartSettings" Lude..=)
              Lude.<$> immediateModeScheduleActionStartSettings,
            ("followModeScheduleActionStartSettings" Lude..=)
              Lude.<$> followModeScheduleActionStartSettings,
            ("fixedModeScheduleActionStartSettings" Lude..=)
              Lude.<$> fixedModeScheduleActionStartSettings
          ]
      )
