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
import qualified Network.AWS.Prelude as Lude

-- | Start time for the action.
--
-- /See:/ 'mkFixedModeScheduleActionStartSettings' smart constructor.
newtype FixedModeScheduleActionStartSettings = FixedModeScheduleActionStartSettings'
  { time ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FixedModeScheduleActionStartSettings' with the minimum fields required to make a request.
--
-- * 'time' - Start time for the action to start in the channel. (Not the time for the action to be added to the schedule: actions are always added to the schedule immediately.) UTC format: yyyy-mm-ddThh:mm:ss.nnnZ. All the letters are digits (for example, mm might be 01) except for the two constants "T" for time and "Z" for "UTC format".
mkFixedModeScheduleActionStartSettings ::
  -- | 'time'
  Lude.Text ->
  FixedModeScheduleActionStartSettings
mkFixedModeScheduleActionStartSettings pTime_ =
  FixedModeScheduleActionStartSettings' {time = pTime_}

-- | Start time for the action to start in the channel. (Not the time for the action to be added to the schedule: actions are always added to the schedule immediately.) UTC format: yyyy-mm-ddThh:mm:ss.nnnZ. All the letters are digits (for example, mm might be 01) except for the two constants "T" for time and "Z" for "UTC format".
--
-- /Note:/ Consider using 'time' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmsassTime :: Lens.Lens' FixedModeScheduleActionStartSettings Lude.Text
fmsassTime = Lens.lens (time :: FixedModeScheduleActionStartSettings -> Lude.Text) (\s a -> s {time = a} :: FixedModeScheduleActionStartSettings)
{-# DEPRECATED fmsassTime "Use generic-lens or generic-optics with 'time' instead." #-}

instance Lude.FromJSON FixedModeScheduleActionStartSettings where
  parseJSON =
    Lude.withObject
      "FixedModeScheduleActionStartSettings"
      ( \x ->
          FixedModeScheduleActionStartSettings' Lude.<$> (x Lude..: "time")
      )

instance Lude.ToJSON FixedModeScheduleActionStartSettings where
  toJSON FixedModeScheduleActionStartSettings' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("time" Lude..= time)])
