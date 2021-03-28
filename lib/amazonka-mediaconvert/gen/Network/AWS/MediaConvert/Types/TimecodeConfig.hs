{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TimecodeConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.TimecodeConfig
  ( TimecodeConfig (..)
  -- * Smart constructor
  , mkTimecodeConfig
  -- * Lenses
  , tcAnchor
  , tcSource
  , tcStart
  , tcTimestampOffset
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.TimecodeSource as Types
import qualified Network.AWS.Prelude as Core

-- | These settings control how the service handles timecodes throughout the job. These settings don't affect input clipping.
--
-- /See:/ 'mkTimecodeConfig' smart constructor.
data TimecodeConfig = TimecodeConfig'
  { anchor :: Core.Maybe Core.Text
    -- ^ If you use an editing platform that relies on an anchor timecode, use Anchor Timecode (Anchor) to specify a timecode that will match the input video frame to the output video frame. Use 24-hour format with frame number, (HH:MM:SS:FF) or (HH:MM:SS;FF). This setting ignores frame rate conversion. System behavior for Anchor Timecode varies depending on your setting for Source (TimecodeSource). * If Source (TimecodeSource) is set to Specified Start (SPECIFIEDSTART), the first input frame is the specified value in Start Timecode (Start). Anchor Timecode (Anchor) and Start Timecode (Start) are used calculate output timecode. * If Source (TimecodeSource) is set to Start at 0 (ZEROBASED)  the  first frame is 00:00:00:00. * If Source (TimecodeSource) is set to Embedded (EMBEDDED), the  first frame is the timecode value on the first input frame of the input.
  , source :: Core.Maybe Types.TimecodeSource
    -- ^ Use Source (TimecodeSource) to set how timecodes are handled within this job. To make sure that your video, audio, captions, and markers are synchronized and that time-based features, such as image inserter, work correctly, choose the Timecode source option that matches your assets. All timecodes are in a 24-hour format with frame number (HH:MM:SS:FF). * Embedded (EMBEDDED) - Use the timecode that is in the input video. If no embedded timecode is in the source, the service will use Start at 0 (ZEROBASED) instead. * Start at 0 (ZEROBASED) - Set the timecode of the initial frame to 00:00:00:00. * Specified Start (SPECIFIEDSTART) - Set the timecode of the initial frame to a value other than zero. You use Start timecode (Start) to provide this value.
  , start :: Core.Maybe Core.Text
    -- ^ Only use when you set Source (TimecodeSource) to Specified start (SPECIFIEDSTART). Use Start timecode (Start) to specify the timecode for the initial frame. Use 24-hour format with frame number, (HH:MM:SS:FF) or (HH:MM:SS;FF).
  , timestampOffset :: Core.Maybe Core.Text
    -- ^ Only applies to outputs that support program-date-time stamp. Use Timestamp offset (TimestampOffset) to overwrite the timecode date without affecting the time and frame number. Provide the new date as a string in the format "yyyy-mm-dd".  To use Time stamp offset, you must also enable Insert program-date-time (InsertProgramDateTime) in the output settings. For example, if the date part of your timecodes is 2002-1-25 and you want to change it to one year later, set Timestamp offset (TimestampOffset) to 2003-1-25.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TimecodeConfig' value with any optional fields omitted.
mkTimecodeConfig
    :: TimecodeConfig
mkTimecodeConfig
  = TimecodeConfig'{anchor = Core.Nothing, source = Core.Nothing,
                    start = Core.Nothing, timestampOffset = Core.Nothing}

-- | If you use an editing platform that relies on an anchor timecode, use Anchor Timecode (Anchor) to specify a timecode that will match the input video frame to the output video frame. Use 24-hour format with frame number, (HH:MM:SS:FF) or (HH:MM:SS;FF). This setting ignores frame rate conversion. System behavior for Anchor Timecode varies depending on your setting for Source (TimecodeSource). * If Source (TimecodeSource) is set to Specified Start (SPECIFIEDSTART), the first input frame is the specified value in Start Timecode (Start). Anchor Timecode (Anchor) and Start Timecode (Start) are used calculate output timecode. * If Source (TimecodeSource) is set to Start at 0 (ZEROBASED)  the  first frame is 00:00:00:00. * If Source (TimecodeSource) is set to Embedded (EMBEDDED), the  first frame is the timecode value on the first input frame of the input.
--
-- /Note:/ Consider using 'anchor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcAnchor :: Lens.Lens' TimecodeConfig (Core.Maybe Core.Text)
tcAnchor = Lens.field @"anchor"
{-# INLINEABLE tcAnchor #-}
{-# DEPRECATED anchor "Use generic-lens or generic-optics with 'anchor' instead"  #-}

-- | Use Source (TimecodeSource) to set how timecodes are handled within this job. To make sure that your video, audio, captions, and markers are synchronized and that time-based features, such as image inserter, work correctly, choose the Timecode source option that matches your assets. All timecodes are in a 24-hour format with frame number (HH:MM:SS:FF). * Embedded (EMBEDDED) - Use the timecode that is in the input video. If no embedded timecode is in the source, the service will use Start at 0 (ZEROBASED) instead. * Start at 0 (ZEROBASED) - Set the timecode of the initial frame to 00:00:00:00. * Specified Start (SPECIFIEDSTART) - Set the timecode of the initial frame to a value other than zero. You use Start timecode (Start) to provide this value.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcSource :: Lens.Lens' TimecodeConfig (Core.Maybe Types.TimecodeSource)
tcSource = Lens.field @"source"
{-# INLINEABLE tcSource #-}
{-# DEPRECATED source "Use generic-lens or generic-optics with 'source' instead"  #-}

-- | Only use when you set Source (TimecodeSource) to Specified start (SPECIFIEDSTART). Use Start timecode (Start) to specify the timecode for the initial frame. Use 24-hour format with frame number, (HH:MM:SS:FF) or (HH:MM:SS;FF).
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcStart :: Lens.Lens' TimecodeConfig (Core.Maybe Core.Text)
tcStart = Lens.field @"start"
{-# INLINEABLE tcStart #-}
{-# DEPRECATED start "Use generic-lens or generic-optics with 'start' instead"  #-}

-- | Only applies to outputs that support program-date-time stamp. Use Timestamp offset (TimestampOffset) to overwrite the timecode date without affecting the time and frame number. Provide the new date as a string in the format "yyyy-mm-dd".  To use Time stamp offset, you must also enable Insert program-date-time (InsertProgramDateTime) in the output settings. For example, if the date part of your timecodes is 2002-1-25 and you want to change it to one year later, set Timestamp offset (TimestampOffset) to 2003-1-25.
--
-- /Note:/ Consider using 'timestampOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTimestampOffset :: Lens.Lens' TimecodeConfig (Core.Maybe Core.Text)
tcTimestampOffset = Lens.field @"timestampOffset"
{-# INLINEABLE tcTimestampOffset #-}
{-# DEPRECATED timestampOffset "Use generic-lens or generic-optics with 'timestampOffset' instead"  #-}

instance Core.FromJSON TimecodeConfig where
        toJSON TimecodeConfig{..}
          = Core.object
              (Core.catMaybes
                 [("anchor" Core..=) Core.<$> anchor,
                  ("source" Core..=) Core.<$> source,
                  ("start" Core..=) Core.<$> start,
                  ("timestampOffset" Core..=) Core.<$> timestampOffset])

instance Core.FromJSON TimecodeConfig where
        parseJSON
          = Core.withObject "TimecodeConfig" Core.$
              \ x ->
                TimecodeConfig' Core.<$>
                  (x Core..:? "anchor") Core.<*> x Core..:? "source" Core.<*>
                    x Core..:? "start"
                    Core.<*> x Core..:? "timestampOffset"
