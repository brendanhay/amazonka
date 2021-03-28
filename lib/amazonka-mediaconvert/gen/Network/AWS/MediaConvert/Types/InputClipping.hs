{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InputClipping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.InputClipping
  ( InputClipping (..)
  -- * Smart constructor
  , mkInputClipping
  -- * Lenses
  , icEndTimecode
  , icStartTimecode
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | To transcode only portions of your input (clips), include one Input clipping (one instance of InputClipping in the JSON job file) for each input clip. All input clips you specify will be included in every output of the job.
--
-- /See:/ 'mkInputClipping' smart constructor.
data InputClipping = InputClipping'
  { endTimecode :: Core.Maybe Core.Text
    -- ^ Set End timecode (EndTimecode) to the end of the portion of the input you are clipping. The frame corresponding to the End timecode value is included in the clip. Start timecode or End timecode may be left blank, but not both. Use the format HH:MM:SS:FF or HH:MM:SS;FF, where HH is the hour, MM is the minute, SS is the second, and FF is the frame number. When choosing this value, take into account your setting for timecode source under input settings (InputTimecodeSource). For example, if you have embedded timecodes that start at 01:00:00:00 and you want your clip to end six minutes into the video, use 01:06:00:00.
  , startTimecode :: Core.Maybe Core.Text
    -- ^ Set Start timecode (StartTimecode) to the beginning of the portion of the input you are clipping. The frame corresponding to the Start timecode value is included in the clip. Start timecode or End timecode may be left blank, but not both. Use the format HH:MM:SS:FF or HH:MM:SS;FF, where HH is the hour, MM is the minute, SS is the second, and FF is the frame number. When choosing this value, take into account your setting for Input timecode source. For example, if you have embedded timecodes that start at 01:00:00:00 and you want your clip to begin five minutes into the video, use 01:05:00:00.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputClipping' value with any optional fields omitted.
mkInputClipping
    :: InputClipping
mkInputClipping
  = InputClipping'{endTimecode = Core.Nothing,
                   startTimecode = Core.Nothing}

-- | Set End timecode (EndTimecode) to the end of the portion of the input you are clipping. The frame corresponding to the End timecode value is included in the clip. Start timecode or End timecode may be left blank, but not both. Use the format HH:MM:SS:FF or HH:MM:SS;FF, where HH is the hour, MM is the minute, SS is the second, and FF is the frame number. When choosing this value, take into account your setting for timecode source under input settings (InputTimecodeSource). For example, if you have embedded timecodes that start at 01:00:00:00 and you want your clip to end six minutes into the video, use 01:06:00:00.
--
-- /Note:/ Consider using 'endTimecode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icEndTimecode :: Lens.Lens' InputClipping (Core.Maybe Core.Text)
icEndTimecode = Lens.field @"endTimecode"
{-# INLINEABLE icEndTimecode #-}
{-# DEPRECATED endTimecode "Use generic-lens or generic-optics with 'endTimecode' instead"  #-}

-- | Set Start timecode (StartTimecode) to the beginning of the portion of the input you are clipping. The frame corresponding to the Start timecode value is included in the clip. Start timecode or End timecode may be left blank, but not both. Use the format HH:MM:SS:FF or HH:MM:SS;FF, where HH is the hour, MM is the minute, SS is the second, and FF is the frame number. When choosing this value, take into account your setting for Input timecode source. For example, if you have embedded timecodes that start at 01:00:00:00 and you want your clip to begin five minutes into the video, use 01:05:00:00.
--
-- /Note:/ Consider using 'startTimecode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icStartTimecode :: Lens.Lens' InputClipping (Core.Maybe Core.Text)
icStartTimecode = Lens.field @"startTimecode"
{-# INLINEABLE icStartTimecode #-}
{-# DEPRECATED startTimecode "Use generic-lens or generic-optics with 'startTimecode' instead"  #-}

instance Core.FromJSON InputClipping where
        toJSON InputClipping{..}
          = Core.object
              (Core.catMaybes
                 [("endTimecode" Core..=) Core.<$> endTimecode,
                  ("startTimecode" Core..=) Core.<$> startTimecode])

instance Core.FromJSON InputClipping where
        parseJSON
          = Core.withObject "InputClipping" Core.$
              \ x ->
                InputClipping' Core.<$>
                  (x Core..:? "endTimecode") Core.<*> x Core..:? "startTimecode"
