-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InputClipping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.InputClipping
  ( InputClipping (..),

    -- * Smart constructor
    mkInputClipping,

    -- * Lenses
    icEndTimecode,
    icStartTimecode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | To transcode only portions of your input (clips), include one Input clipping (one instance of InputClipping in the JSON job file) for each input clip. All input clips you specify will be included in every output of the job.
--
-- /See:/ 'mkInputClipping' smart constructor.
data InputClipping = InputClipping'
  { endTimecode ::
      Lude.Maybe Lude.Text,
    startTimecode :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputClipping' with the minimum fields required to make a request.
--
-- * 'endTimecode' - Set End timecode (EndTimecode) to the end of the portion of the input you are clipping. The frame corresponding to the End timecode value is included in the clip. Start timecode or End timecode may be left blank, but not both. Use the format HH:MM:SS:FF or HH:MM:SS;FF, where HH is the hour, MM is the minute, SS is the second, and FF is the frame number. When choosing this value, take into account your setting for timecode source under input settings (InputTimecodeSource). For example, if you have embedded timecodes that start at 01:00:00:00 and you want your clip to end six minutes into the video, use 01:06:00:00.
-- * 'startTimecode' - Set Start timecode (StartTimecode) to the beginning of the portion of the input you are clipping. The frame corresponding to the Start timecode value is included in the clip. Start timecode or End timecode may be left blank, but not both. Use the format HH:MM:SS:FF or HH:MM:SS;FF, where HH is the hour, MM is the minute, SS is the second, and FF is the frame number. When choosing this value, take into account your setting for Input timecode source. For example, if you have embedded timecodes that start at 01:00:00:00 and you want your clip to begin five minutes into the video, use 01:05:00:00.
mkInputClipping ::
  InputClipping
mkInputClipping =
  InputClipping'
    { endTimecode = Lude.Nothing,
      startTimecode = Lude.Nothing
    }

-- | Set End timecode (EndTimecode) to the end of the portion of the input you are clipping. The frame corresponding to the End timecode value is included in the clip. Start timecode or End timecode may be left blank, but not both. Use the format HH:MM:SS:FF or HH:MM:SS;FF, where HH is the hour, MM is the minute, SS is the second, and FF is the frame number. When choosing this value, take into account your setting for timecode source under input settings (InputTimecodeSource). For example, if you have embedded timecodes that start at 01:00:00:00 and you want your clip to end six minutes into the video, use 01:06:00:00.
--
-- /Note:/ Consider using 'endTimecode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icEndTimecode :: Lens.Lens' InputClipping (Lude.Maybe Lude.Text)
icEndTimecode = Lens.lens (endTimecode :: InputClipping -> Lude.Maybe Lude.Text) (\s a -> s {endTimecode = a} :: InputClipping)
{-# DEPRECATED icEndTimecode "Use generic-lens or generic-optics with 'endTimecode' instead." #-}

-- | Set Start timecode (StartTimecode) to the beginning of the portion of the input you are clipping. The frame corresponding to the Start timecode value is included in the clip. Start timecode or End timecode may be left blank, but not both. Use the format HH:MM:SS:FF or HH:MM:SS;FF, where HH is the hour, MM is the minute, SS is the second, and FF is the frame number. When choosing this value, take into account your setting for Input timecode source. For example, if you have embedded timecodes that start at 01:00:00:00 and you want your clip to begin five minutes into the video, use 01:05:00:00.
--
-- /Note:/ Consider using 'startTimecode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icStartTimecode :: Lens.Lens' InputClipping (Lude.Maybe Lude.Text)
icStartTimecode = Lens.lens (startTimecode :: InputClipping -> Lude.Maybe Lude.Text) (\s a -> s {startTimecode = a} :: InputClipping)
{-# DEPRECATED icStartTimecode "Use generic-lens or generic-optics with 'startTimecode' instead." #-}

instance Lude.FromJSON InputClipping where
  parseJSON =
    Lude.withObject
      "InputClipping"
      ( \x ->
          InputClipping'
            Lude.<$> (x Lude..:? "endTimecode") Lude.<*> (x Lude..:? "startTimecode")
      )

instance Lude.ToJSON InputClipping where
  toJSON InputClipping' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("endTimecode" Lude..=) Lude.<$> endTimecode,
            ("startTimecode" Lude..=) Lude.<$> startTimecode
          ]
      )
