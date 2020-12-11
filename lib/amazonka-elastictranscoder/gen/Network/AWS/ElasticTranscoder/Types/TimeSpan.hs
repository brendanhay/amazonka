-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.TimeSpan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.TimeSpan
  ( TimeSpan (..),

    -- * Smart constructor
    mkTimeSpan,

    -- * Lenses
    tsStartTime,
    tsDuration,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Settings that determine when a clip begins and how long it lasts.
--
-- /See:/ 'mkTimeSpan' smart constructor.
data TimeSpan = TimeSpan'
  { startTime :: Lude.Maybe Lude.Text,
    duration :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TimeSpan' with the minimum fields required to make a request.
--
-- * 'duration' - The duration of the clip. The format can be either HH:mm:ss.SSS (maximum value: 23:59:59.999; SSS is thousandths of a second) or sssss.SSS (maximum value: 86399.999). If you don't specify a value, Elastic Transcoder creates an output file from StartTime to the end of the file.
--
-- If you specify a value longer than the duration of the input file, Elastic Transcoder transcodes the file and returns a warning message.
-- * 'startTime' - The place in the input file where you want a clip to start. The format can be either HH:mm:ss.SSS (maximum value: 23:59:59.999; SSS is thousandths of a second) or sssss.SSS (maximum value: 86399.999). If you don't specify a value, Elastic Transcoder starts at the beginning of the input file.
mkTimeSpan ::
  TimeSpan
mkTimeSpan =
  TimeSpan' {startTime = Lude.Nothing, duration = Lude.Nothing}

-- | The place in the input file where you want a clip to start. The format can be either HH:mm:ss.SSS (maximum value: 23:59:59.999; SSS is thousandths of a second) or sssss.SSS (maximum value: 86399.999). If you don't specify a value, Elastic Transcoder starts at the beginning of the input file.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsStartTime :: Lens.Lens' TimeSpan (Lude.Maybe Lude.Text)
tsStartTime = Lens.lens (startTime :: TimeSpan -> Lude.Maybe Lude.Text) (\s a -> s {startTime = a} :: TimeSpan)
{-# DEPRECATED tsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The duration of the clip. The format can be either HH:mm:ss.SSS (maximum value: 23:59:59.999; SSS is thousandths of a second) or sssss.SSS (maximum value: 86399.999). If you don't specify a value, Elastic Transcoder creates an output file from StartTime to the end of the file.
--
-- If you specify a value longer than the duration of the input file, Elastic Transcoder transcodes the file and returns a warning message.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsDuration :: Lens.Lens' TimeSpan (Lude.Maybe Lude.Text)
tsDuration = Lens.lens (duration :: TimeSpan -> Lude.Maybe Lude.Text) (\s a -> s {duration = a} :: TimeSpan)
{-# DEPRECATED tsDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

instance Lude.FromJSON TimeSpan where
  parseJSON =
    Lude.withObject
      "TimeSpan"
      ( \x ->
          TimeSpan'
            Lude.<$> (x Lude..:? "StartTime") Lude.<*> (x Lude..:? "Duration")
      )

instance Lude.ToJSON TimeSpan where
  toJSON TimeSpan' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StartTime" Lude..=) Lude.<$> startTime,
            ("Duration" Lude..=) Lude.<$> duration
          ]
      )
