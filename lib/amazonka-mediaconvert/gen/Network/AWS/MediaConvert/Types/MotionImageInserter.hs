{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MotionImageInserter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MotionImageInserter
  ( MotionImageInserter (..),

    -- * Smart constructor
    mkMotionImageInserter,

    -- * Lenses
    miiFramerate,
    miiInput,
    miiInsertionMode,
    miiOffset,
    miiPlayback,
    miiStartTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.MotionImageInsertionFramerate as Types
import qualified Network.AWS.MediaConvert.Types.MotionImageInsertionMode as Types
import qualified Network.AWS.MediaConvert.Types.MotionImageInsertionOffset as Types
import qualified Network.AWS.MediaConvert.Types.MotionImagePlayback as Types
import qualified Network.AWS.Prelude as Core

-- | Overlay motion graphics on top of your video at the time that you specify.
--
-- /See:/ 'mkMotionImageInserter' smart constructor.
data MotionImageInserter = MotionImageInserter'
  { -- | If your motion graphic asset is a .mov file, keep this setting unspecified. If your motion graphic asset is a series of .png files, specify the frame rate of the overlay in frames per second, as a fraction. For example, specify 24 fps as 24/1. Make sure that the number of images in your series matches the frame rate and your intended overlay duration. For example, if you want a 30-second overlay at 30 fps, you should have 900 .png images. This overlay frame rate doesn't need to match the frame rate of the underlying video.
    framerate :: Core.Maybe Types.MotionImageInsertionFramerate,
    -- | Specify the .mov file or series of .png files that you want to overlay on your video. For .png files, provide the file name of the first file in the series. Make sure that the names of the .png files end with sequential numbers that specify the order that they are played in. For example, overlay_000.png, overlay_001.png, overlay_002.png, and so on. The sequence must start at zero, and each image file name must have the same number of digits. Pad your initial file names with enough zeros to complete the sequence. For example, if the first image is overlay_0.png, there can be only 10 images in the sequence, with the last image being overlay_9.png. But if the first image is overlay_00.png, there can be 100 images in the sequence.
    input :: Core.Maybe Core.Text,
    -- | Choose the type of motion graphic asset that you are providing for your overlay. You can choose either a .mov file or a series of .png files.
    insertionMode :: Core.Maybe Types.MotionImageInsertionMode,
    -- | Use Offset to specify the placement of your motion graphic overlay on the video frame. Specify in pixels, from the upper-left corner of the frame. If you don't specify an offset, the service scales your overlay to the full size of the frame. Otherwise, the service inserts the overlay at its native resolution and scales the size up or down with any video scaling.
    offset :: Core.Maybe Types.MotionImageInsertionOffset,
    -- | Specify whether your motion graphic overlay repeats on a loop or plays only once.
    playback :: Core.Maybe Types.MotionImagePlayback,
    -- | Specify when the motion overlay begins. Use timecode format (HH:MM:SS:FF or HH:MM:SS;FF). Make sure that the timecode you provide here takes into account how you have set up your timecode configuration under both job settings and input settings. The simplest way to do that is to set both to start at 0. If you need to set up your job to follow timecodes embedded in your source that don't start at zero, make sure that you specify a start time that is after the first embedded timecode. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/setting-up-timecode.html Find job-wide and input timecode configuration settings in your JSON job settings specification at settings>timecodeConfig>source and settings>inputs>timecodeSource.
    startTime :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MotionImageInserter' value with any optional fields omitted.
mkMotionImageInserter ::
  MotionImageInserter
mkMotionImageInserter =
  MotionImageInserter'
    { framerate = Core.Nothing,
      input = Core.Nothing,
      insertionMode = Core.Nothing,
      offset = Core.Nothing,
      playback = Core.Nothing,
      startTime = Core.Nothing
    }

-- | If your motion graphic asset is a .mov file, keep this setting unspecified. If your motion graphic asset is a series of .png files, specify the frame rate of the overlay in frames per second, as a fraction. For example, specify 24 fps as 24/1. Make sure that the number of images in your series matches the frame rate and your intended overlay duration. For example, if you want a 30-second overlay at 30 fps, you should have 900 .png images. This overlay frame rate doesn't need to match the frame rate of the underlying video.
--
-- /Note:/ Consider using 'framerate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miiFramerate :: Lens.Lens' MotionImageInserter (Core.Maybe Types.MotionImageInsertionFramerate)
miiFramerate = Lens.field @"framerate"
{-# DEPRECATED miiFramerate "Use generic-lens or generic-optics with 'framerate' instead." #-}

-- | Specify the .mov file or series of .png files that you want to overlay on your video. For .png files, provide the file name of the first file in the series. Make sure that the names of the .png files end with sequential numbers that specify the order that they are played in. For example, overlay_000.png, overlay_001.png, overlay_002.png, and so on. The sequence must start at zero, and each image file name must have the same number of digits. Pad your initial file names with enough zeros to complete the sequence. For example, if the first image is overlay_0.png, there can be only 10 images in the sequence, with the last image being overlay_9.png. But if the first image is overlay_00.png, there can be 100 images in the sequence.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miiInput :: Lens.Lens' MotionImageInserter (Core.Maybe Core.Text)
miiInput = Lens.field @"input"
{-# DEPRECATED miiInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | Choose the type of motion graphic asset that you are providing for your overlay. You can choose either a .mov file or a series of .png files.
--
-- /Note:/ Consider using 'insertionMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miiInsertionMode :: Lens.Lens' MotionImageInserter (Core.Maybe Types.MotionImageInsertionMode)
miiInsertionMode = Lens.field @"insertionMode"
{-# DEPRECATED miiInsertionMode "Use generic-lens or generic-optics with 'insertionMode' instead." #-}

-- | Use Offset to specify the placement of your motion graphic overlay on the video frame. Specify in pixels, from the upper-left corner of the frame. If you don't specify an offset, the service scales your overlay to the full size of the frame. Otherwise, the service inserts the overlay at its native resolution and scales the size up or down with any video scaling.
--
-- /Note:/ Consider using 'offset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miiOffset :: Lens.Lens' MotionImageInserter (Core.Maybe Types.MotionImageInsertionOffset)
miiOffset = Lens.field @"offset"
{-# DEPRECATED miiOffset "Use generic-lens or generic-optics with 'offset' instead." #-}

-- | Specify whether your motion graphic overlay repeats on a loop or plays only once.
--
-- /Note:/ Consider using 'playback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miiPlayback :: Lens.Lens' MotionImageInserter (Core.Maybe Types.MotionImagePlayback)
miiPlayback = Lens.field @"playback"
{-# DEPRECATED miiPlayback "Use generic-lens or generic-optics with 'playback' instead." #-}

-- | Specify when the motion overlay begins. Use timecode format (HH:MM:SS:FF or HH:MM:SS;FF). Make sure that the timecode you provide here takes into account how you have set up your timecode configuration under both job settings and input settings. The simplest way to do that is to set both to start at 0. If you need to set up your job to follow timecodes embedded in your source that don't start at zero, make sure that you specify a start time that is after the first embedded timecode. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/setting-up-timecode.html Find job-wide and input timecode configuration settings in your JSON job settings specification at settings>timecodeConfig>source and settings>inputs>timecodeSource.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miiStartTime :: Lens.Lens' MotionImageInserter (Core.Maybe Core.Text)
miiStartTime = Lens.field @"startTime"
{-# DEPRECATED miiStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

instance Core.FromJSON MotionImageInserter where
  toJSON MotionImageInserter {..} =
    Core.object
      ( Core.catMaybes
          [ ("framerate" Core..=) Core.<$> framerate,
            ("input" Core..=) Core.<$> input,
            ("insertionMode" Core..=) Core.<$> insertionMode,
            ("offset" Core..=) Core.<$> offset,
            ("playback" Core..=) Core.<$> playback,
            ("startTime" Core..=) Core.<$> startTime
          ]
      )

instance Core.FromJSON MotionImageInserter where
  parseJSON =
    Core.withObject "MotionImageInserter" Core.$
      \x ->
        MotionImageInserter'
          Core.<$> (x Core..:? "framerate")
          Core.<*> (x Core..:? "input")
          Core.<*> (x Core..:? "insertionMode")
          Core.<*> (x Core..:? "offset")
          Core.<*> (x Core..:? "playback")
          Core.<*> (x Core..:? "startTime")
