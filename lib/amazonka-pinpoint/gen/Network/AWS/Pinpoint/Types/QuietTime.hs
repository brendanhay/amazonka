{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.QuietTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.QuietTime
  ( QuietTime (..),

    -- * Smart constructor
    mkQuietTime,

    -- * Lenses
    qtStart,
    qtEnd,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the start and end times that define a time range when messages aren't sent to endpoints.
--
-- /See:/ 'mkQuietTime' smart constructor.
data QuietTime = QuietTime'
  { -- | The specific time when quiet time begins. This value has to use 24-hour notation and be in HH:MM format, where HH is the hour (with a leading zero, if applicable) and MM is the minutes. For example, use 02:30 to represent 2:30 AM, or 14:30 to represent 2:30 PM.
    start :: Lude.Maybe Lude.Text,
    -- | The specific time when quiet time ends. This value has to use 24-hour notation and be in HH:MM format, where HH is the hour (with a leading zero, if applicable) and MM is the minutes. For example, use 02:30 to represent 2:30 AM, or 14:30 to represent 2:30 PM.
    end :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QuietTime' with the minimum fields required to make a request.
--
-- * 'start' - The specific time when quiet time begins. This value has to use 24-hour notation and be in HH:MM format, where HH is the hour (with a leading zero, if applicable) and MM is the minutes. For example, use 02:30 to represent 2:30 AM, or 14:30 to represent 2:30 PM.
-- * 'end' - The specific time when quiet time ends. This value has to use 24-hour notation and be in HH:MM format, where HH is the hour (with a leading zero, if applicable) and MM is the minutes. For example, use 02:30 to represent 2:30 AM, or 14:30 to represent 2:30 PM.
mkQuietTime ::
  QuietTime
mkQuietTime = QuietTime' {start = Lude.Nothing, end = Lude.Nothing}

-- | The specific time when quiet time begins. This value has to use 24-hour notation and be in HH:MM format, where HH is the hour (with a leading zero, if applicable) and MM is the minutes. For example, use 02:30 to represent 2:30 AM, or 14:30 to represent 2:30 PM.
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtStart :: Lens.Lens' QuietTime (Lude.Maybe Lude.Text)
qtStart = Lens.lens (start :: QuietTime -> Lude.Maybe Lude.Text) (\s a -> s {start = a} :: QuietTime)
{-# DEPRECATED qtStart "Use generic-lens or generic-optics with 'start' instead." #-}

-- | The specific time when quiet time ends. This value has to use 24-hour notation and be in HH:MM format, where HH is the hour (with a leading zero, if applicable) and MM is the minutes. For example, use 02:30 to represent 2:30 AM, or 14:30 to represent 2:30 PM.
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qtEnd :: Lens.Lens' QuietTime (Lude.Maybe Lude.Text)
qtEnd = Lens.lens (end :: QuietTime -> Lude.Maybe Lude.Text) (\s a -> s {end = a} :: QuietTime)
{-# DEPRECATED qtEnd "Use generic-lens or generic-optics with 'end' instead." #-}

instance Lude.FromJSON QuietTime where
  parseJSON =
    Lude.withObject
      "QuietTime"
      ( \x ->
          QuietTime'
            Lude.<$> (x Lude..:? "Start") Lude.<*> (x Lude..:? "End")
      )

instance Lude.ToJSON QuietTime where
  toJSON QuietTime' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Start" Lude..=) Lude.<$> start, ("End" Lude..=) Lude.<$> end]
      )
