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
    miiStartTime,
    miiOffset,
    miiInput,
    miiInsertionMode,
    miiPlayback,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.MotionImageInsertionFramerate
import Network.AWS.MediaConvert.Types.MotionImageInsertionMode
import Network.AWS.MediaConvert.Types.MotionImageInsertionOffset
import Network.AWS.MediaConvert.Types.MotionImagePlayback
import qualified Network.AWS.Prelude as Lude

-- | Overlay motion graphics on top of your video at the time that you specify.
--
-- /See:/ 'mkMotionImageInserter' smart constructor.
data MotionImageInserter = MotionImageInserter'
  { framerate ::
      Lude.Maybe MotionImageInsertionFramerate,
    startTime :: Lude.Maybe Lude.Text,
    offset :: Lude.Maybe MotionImageInsertionOffset,
    input :: Lude.Maybe Lude.Text,
    insertionMode ::
      Lude.Maybe MotionImageInsertionMode,
    playback :: Lude.Maybe MotionImagePlayback
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MotionImageInserter' with the minimum fields required to make a request.
--
-- * 'framerate' - If your motion graphic asset is a .mov file, keep this setting unspecified. If your motion graphic asset is a series of .png files, specify the frame rate of the overlay in frames per second, as a fraction. For example, specify 24 fps as 24/1. Make sure that the number of images in your series matches the frame rate and your intended overlay duration. For example, if you want a 30-second overlay at 30 fps, you should have 900 .png images. This overlay frame rate doesn't need to match the frame rate of the underlying video.
-- * 'input' - Specify the .mov file or series of .png files that you want to overlay on your video. For .png files, provide the file name of the first file in the series. Make sure that the names of the .png files end with sequential numbers that specify the order that they are played in. For example, overlay_000.png, overlay_001.png, overlay_002.png, and so on. The sequence must start at zero, and each image file name must have the same number of digits. Pad your initial file names with enough zeros to complete the sequence. For example, if the first image is overlay_0.png, there can be only 10 images in the sequence, with the last image being overlay_9.png. But if the first image is overlay_00.png, there can be 100 images in the sequence.
-- * 'insertionMode' - Choose the type of motion graphic asset that you are providing for your overlay. You can choose either a .mov file or a series of .png files.
-- * 'offset' - Use Offset to specify the placement of your motion graphic overlay on the video frame. Specify in pixels, from the upper-left corner of the frame. If you don't specify an offset, the service scales your overlay to the full size of the frame. Otherwise, the service inserts the overlay at its native resolution and scales the size up or down with any video scaling.
-- * 'playback' - Specify whether your motion graphic overlay repeats on a loop or plays only once.
-- * 'startTime' - Specify when the motion overlay begins. Use timecode format (HH:MM:SS:FF or HH:MM:SS;FF). Make sure that the timecode you provide here takes into account how you have set up your timecode configuration under both job settings and input settings. The simplest way to do that is to set both to start at 0. If you need to set up your job to follow timecodes embedded in your source that don't start at zero, make sure that you specify a start time that is after the first embedded timecode. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/setting-up-timecode.html Find job-wide and input timecode configuration settings in your JSON job settings specification at settings>timecodeConfig>source and settings>inputs>timecodeSource.
mkMotionImageInserter ::
  MotionImageInserter
mkMotionImageInserter =
  MotionImageInserter'
    { framerate = Lude.Nothing,
      startTime = Lude.Nothing,
      offset = Lude.Nothing,
      input = Lude.Nothing,
      insertionMode = Lude.Nothing,
      playback = Lude.Nothing
    }

-- | If your motion graphic asset is a .mov file, keep this setting unspecified. If your motion graphic asset is a series of .png files, specify the frame rate of the overlay in frames per second, as a fraction. For example, specify 24 fps as 24/1. Make sure that the number of images in your series matches the frame rate and your intended overlay duration. For example, if you want a 30-second overlay at 30 fps, you should have 900 .png images. This overlay frame rate doesn't need to match the frame rate of the underlying video.
--
-- /Note:/ Consider using 'framerate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miiFramerate :: Lens.Lens' MotionImageInserter (Lude.Maybe MotionImageInsertionFramerate)
miiFramerate = Lens.lens (framerate :: MotionImageInserter -> Lude.Maybe MotionImageInsertionFramerate) (\s a -> s {framerate = a} :: MotionImageInserter)
{-# DEPRECATED miiFramerate "Use generic-lens or generic-optics with 'framerate' instead." #-}

-- | Specify when the motion overlay begins. Use timecode format (HH:MM:SS:FF or HH:MM:SS;FF). Make sure that the timecode you provide here takes into account how you have set up your timecode configuration under both job settings and input settings. The simplest way to do that is to set both to start at 0. If you need to set up your job to follow timecodes embedded in your source that don't start at zero, make sure that you specify a start time that is after the first embedded timecode. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/setting-up-timecode.html Find job-wide and input timecode configuration settings in your JSON job settings specification at settings>timecodeConfig>source and settings>inputs>timecodeSource.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miiStartTime :: Lens.Lens' MotionImageInserter (Lude.Maybe Lude.Text)
miiStartTime = Lens.lens (startTime :: MotionImageInserter -> Lude.Maybe Lude.Text) (\s a -> s {startTime = a} :: MotionImageInserter)
{-# DEPRECATED miiStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Use Offset to specify the placement of your motion graphic overlay on the video frame. Specify in pixels, from the upper-left corner of the frame. If you don't specify an offset, the service scales your overlay to the full size of the frame. Otherwise, the service inserts the overlay at its native resolution and scales the size up or down with any video scaling.
--
-- /Note:/ Consider using 'offset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miiOffset :: Lens.Lens' MotionImageInserter (Lude.Maybe MotionImageInsertionOffset)
miiOffset = Lens.lens (offset :: MotionImageInserter -> Lude.Maybe MotionImageInsertionOffset) (\s a -> s {offset = a} :: MotionImageInserter)
{-# DEPRECATED miiOffset "Use generic-lens or generic-optics with 'offset' instead." #-}

-- | Specify the .mov file or series of .png files that you want to overlay on your video. For .png files, provide the file name of the first file in the series. Make sure that the names of the .png files end with sequential numbers that specify the order that they are played in. For example, overlay_000.png, overlay_001.png, overlay_002.png, and so on. The sequence must start at zero, and each image file name must have the same number of digits. Pad your initial file names with enough zeros to complete the sequence. For example, if the first image is overlay_0.png, there can be only 10 images in the sequence, with the last image being overlay_9.png. But if the first image is overlay_00.png, there can be 100 images in the sequence.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miiInput :: Lens.Lens' MotionImageInserter (Lude.Maybe Lude.Text)
miiInput = Lens.lens (input :: MotionImageInserter -> Lude.Maybe Lude.Text) (\s a -> s {input = a} :: MotionImageInserter)
{-# DEPRECATED miiInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | Choose the type of motion graphic asset that you are providing for your overlay. You can choose either a .mov file or a series of .png files.
--
-- /Note:/ Consider using 'insertionMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miiInsertionMode :: Lens.Lens' MotionImageInserter (Lude.Maybe MotionImageInsertionMode)
miiInsertionMode = Lens.lens (insertionMode :: MotionImageInserter -> Lude.Maybe MotionImageInsertionMode) (\s a -> s {insertionMode = a} :: MotionImageInserter)
{-# DEPRECATED miiInsertionMode "Use generic-lens or generic-optics with 'insertionMode' instead." #-}

-- | Specify whether your motion graphic overlay repeats on a loop or plays only once.
--
-- /Note:/ Consider using 'playback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miiPlayback :: Lens.Lens' MotionImageInserter (Lude.Maybe MotionImagePlayback)
miiPlayback = Lens.lens (playback :: MotionImageInserter -> Lude.Maybe MotionImagePlayback) (\s a -> s {playback = a} :: MotionImageInserter)
{-# DEPRECATED miiPlayback "Use generic-lens or generic-optics with 'playback' instead." #-}

instance Lude.FromJSON MotionImageInserter where
  parseJSON =
    Lude.withObject
      "MotionImageInserter"
      ( \x ->
          MotionImageInserter'
            Lude.<$> (x Lude..:? "framerate")
            Lude.<*> (x Lude..:? "startTime")
            Lude.<*> (x Lude..:? "offset")
            Lude.<*> (x Lude..:? "input")
            Lude.<*> (x Lude..:? "insertionMode")
            Lude.<*> (x Lude..:? "playback")
      )

instance Lude.ToJSON MotionImageInserter where
  toJSON MotionImageInserter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("framerate" Lude..=) Lude.<$> framerate,
            ("startTime" Lude..=) Lude.<$> startTime,
            ("offset" Lude..=) Lude.<$> offset,
            ("input" Lude..=) Lude.<$> input,
            ("insertionMode" Lude..=) Lude.<$> insertionMode,
            ("playback" Lude..=) Lude.<$> playback
          ]
      )
