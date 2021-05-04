{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MotionImageInserter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MotionImageInserter where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.MotionImageInsertionFramerate
import Network.AWS.MediaConvert.Types.MotionImageInsertionMode
import Network.AWS.MediaConvert.Types.MotionImageInsertionOffset
import Network.AWS.MediaConvert.Types.MotionImagePlayback
import qualified Network.AWS.Prelude as Prelude

-- | Overlay motion graphics on top of your video at the time that you
-- specify.
--
-- /See:/ 'newMotionImageInserter' smart constructor.
data MotionImageInserter = MotionImageInserter'
  { -- | Choose the type of motion graphic asset that you are providing for your
    -- overlay. You can choose either a .mov file or a series of .png files.
    insertionMode :: Prelude.Maybe MotionImageInsertionMode,
    -- | Specify the .mov file or series of .png files that you want to overlay
    -- on your video. For .png files, provide the file name of the first file
    -- in the series. Make sure that the names of the .png files end with
    -- sequential numbers that specify the order that they are played in. For
    -- example, overlay_000.png, overlay_001.png, overlay_002.png, and so on.
    -- The sequence must start at zero, and each image file name must have the
    -- same number of digits. Pad your initial file names with enough zeros to
    -- complete the sequence. For example, if the first image is overlay_0.png,
    -- there can be only 10 images in the sequence, with the last image being
    -- overlay_9.png. But if the first image is overlay_00.png, there can be
    -- 100 images in the sequence.
    input :: Prelude.Maybe Prelude.Text,
    -- | Specify when the motion overlay begins. Use timecode format (HH:MM:SS:FF
    -- or HH:MM:SS;FF). Make sure that the timecode you provide here takes into
    -- account how you have set up your timecode configuration under both job
    -- settings and input settings. The simplest way to do that is to set both
    -- to start at 0. If you need to set up your job to follow timecodes
    -- embedded in your source that don\'t start at zero, make sure that you
    -- specify a start time that is after the first embedded timecode. For more
    -- information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/setting-up-timecode.html
    -- Find job-wide and input timecode configuration settings in your JSON job
    -- settings specification at settings>timecodeConfig>source and
    -- settings>inputs>timecodeSource.
    startTime :: Prelude.Maybe Prelude.Text,
    -- | Specify whether your motion graphic overlay repeats on a loop or plays
    -- only once.
    playback :: Prelude.Maybe MotionImagePlayback,
    -- | If your motion graphic asset is a .mov file, keep this setting
    -- unspecified. If your motion graphic asset is a series of .png files,
    -- specify the frame rate of the overlay in frames per second, as a
    -- fraction. For example, specify 24 fps as 24\/1. Make sure that the
    -- number of images in your series matches the frame rate and your intended
    -- overlay duration. For example, if you want a 30-second overlay at 30
    -- fps, you should have 900 .png images. This overlay frame rate doesn\'t
    -- need to match the frame rate of the underlying video.
    framerate :: Prelude.Maybe MotionImageInsertionFramerate,
    -- | Use Offset to specify the placement of your motion graphic overlay on
    -- the video frame. Specify in pixels, from the upper-left corner of the
    -- frame. If you don\'t specify an offset, the service scales your overlay
    -- to the full size of the frame. Otherwise, the service inserts the
    -- overlay at its native resolution and scales the size up or down with any
    -- video scaling.
    offset :: Prelude.Maybe MotionImageInsertionOffset
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MotionImageInserter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insertionMode', 'motionImageInserter_insertionMode' - Choose the type of motion graphic asset that you are providing for your
-- overlay. You can choose either a .mov file or a series of .png files.
--
-- 'input', 'motionImageInserter_input' - Specify the .mov file or series of .png files that you want to overlay
-- on your video. For .png files, provide the file name of the first file
-- in the series. Make sure that the names of the .png files end with
-- sequential numbers that specify the order that they are played in. For
-- example, overlay_000.png, overlay_001.png, overlay_002.png, and so on.
-- The sequence must start at zero, and each image file name must have the
-- same number of digits. Pad your initial file names with enough zeros to
-- complete the sequence. For example, if the first image is overlay_0.png,
-- there can be only 10 images in the sequence, with the last image being
-- overlay_9.png. But if the first image is overlay_00.png, there can be
-- 100 images in the sequence.
--
-- 'startTime', 'motionImageInserter_startTime' - Specify when the motion overlay begins. Use timecode format (HH:MM:SS:FF
-- or HH:MM:SS;FF). Make sure that the timecode you provide here takes into
-- account how you have set up your timecode configuration under both job
-- settings and input settings. The simplest way to do that is to set both
-- to start at 0. If you need to set up your job to follow timecodes
-- embedded in your source that don\'t start at zero, make sure that you
-- specify a start time that is after the first embedded timecode. For more
-- information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/setting-up-timecode.html
-- Find job-wide and input timecode configuration settings in your JSON job
-- settings specification at settings>timecodeConfig>source and
-- settings>inputs>timecodeSource.
--
-- 'playback', 'motionImageInserter_playback' - Specify whether your motion graphic overlay repeats on a loop or plays
-- only once.
--
-- 'framerate', 'motionImageInserter_framerate' - If your motion graphic asset is a .mov file, keep this setting
-- unspecified. If your motion graphic asset is a series of .png files,
-- specify the frame rate of the overlay in frames per second, as a
-- fraction. For example, specify 24 fps as 24\/1. Make sure that the
-- number of images in your series matches the frame rate and your intended
-- overlay duration. For example, if you want a 30-second overlay at 30
-- fps, you should have 900 .png images. This overlay frame rate doesn\'t
-- need to match the frame rate of the underlying video.
--
-- 'offset', 'motionImageInserter_offset' - Use Offset to specify the placement of your motion graphic overlay on
-- the video frame. Specify in pixels, from the upper-left corner of the
-- frame. If you don\'t specify an offset, the service scales your overlay
-- to the full size of the frame. Otherwise, the service inserts the
-- overlay at its native resolution and scales the size up or down with any
-- video scaling.
newMotionImageInserter ::
  MotionImageInserter
newMotionImageInserter =
  MotionImageInserter'
    { insertionMode =
        Prelude.Nothing,
      input = Prelude.Nothing,
      startTime = Prelude.Nothing,
      playback = Prelude.Nothing,
      framerate = Prelude.Nothing,
      offset = Prelude.Nothing
    }

-- | Choose the type of motion graphic asset that you are providing for your
-- overlay. You can choose either a .mov file or a series of .png files.
motionImageInserter_insertionMode :: Lens.Lens' MotionImageInserter (Prelude.Maybe MotionImageInsertionMode)
motionImageInserter_insertionMode = Lens.lens (\MotionImageInserter' {insertionMode} -> insertionMode) (\s@MotionImageInserter' {} a -> s {insertionMode = a} :: MotionImageInserter)

-- | Specify the .mov file or series of .png files that you want to overlay
-- on your video. For .png files, provide the file name of the first file
-- in the series. Make sure that the names of the .png files end with
-- sequential numbers that specify the order that they are played in. For
-- example, overlay_000.png, overlay_001.png, overlay_002.png, and so on.
-- The sequence must start at zero, and each image file name must have the
-- same number of digits. Pad your initial file names with enough zeros to
-- complete the sequence. For example, if the first image is overlay_0.png,
-- there can be only 10 images in the sequence, with the last image being
-- overlay_9.png. But if the first image is overlay_00.png, there can be
-- 100 images in the sequence.
motionImageInserter_input :: Lens.Lens' MotionImageInserter (Prelude.Maybe Prelude.Text)
motionImageInserter_input = Lens.lens (\MotionImageInserter' {input} -> input) (\s@MotionImageInserter' {} a -> s {input = a} :: MotionImageInserter)

-- | Specify when the motion overlay begins. Use timecode format (HH:MM:SS:FF
-- or HH:MM:SS;FF). Make sure that the timecode you provide here takes into
-- account how you have set up your timecode configuration under both job
-- settings and input settings. The simplest way to do that is to set both
-- to start at 0. If you need to set up your job to follow timecodes
-- embedded in your source that don\'t start at zero, make sure that you
-- specify a start time that is after the first embedded timecode. For more
-- information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/setting-up-timecode.html
-- Find job-wide and input timecode configuration settings in your JSON job
-- settings specification at settings>timecodeConfig>source and
-- settings>inputs>timecodeSource.
motionImageInserter_startTime :: Lens.Lens' MotionImageInserter (Prelude.Maybe Prelude.Text)
motionImageInserter_startTime = Lens.lens (\MotionImageInserter' {startTime} -> startTime) (\s@MotionImageInserter' {} a -> s {startTime = a} :: MotionImageInserter)

-- | Specify whether your motion graphic overlay repeats on a loop or plays
-- only once.
motionImageInserter_playback :: Lens.Lens' MotionImageInserter (Prelude.Maybe MotionImagePlayback)
motionImageInserter_playback = Lens.lens (\MotionImageInserter' {playback} -> playback) (\s@MotionImageInserter' {} a -> s {playback = a} :: MotionImageInserter)

-- | If your motion graphic asset is a .mov file, keep this setting
-- unspecified. If your motion graphic asset is a series of .png files,
-- specify the frame rate of the overlay in frames per second, as a
-- fraction. For example, specify 24 fps as 24\/1. Make sure that the
-- number of images in your series matches the frame rate and your intended
-- overlay duration. For example, if you want a 30-second overlay at 30
-- fps, you should have 900 .png images. This overlay frame rate doesn\'t
-- need to match the frame rate of the underlying video.
motionImageInserter_framerate :: Lens.Lens' MotionImageInserter (Prelude.Maybe MotionImageInsertionFramerate)
motionImageInserter_framerate = Lens.lens (\MotionImageInserter' {framerate} -> framerate) (\s@MotionImageInserter' {} a -> s {framerate = a} :: MotionImageInserter)

-- | Use Offset to specify the placement of your motion graphic overlay on
-- the video frame. Specify in pixels, from the upper-left corner of the
-- frame. If you don\'t specify an offset, the service scales your overlay
-- to the full size of the frame. Otherwise, the service inserts the
-- overlay at its native resolution and scales the size up or down with any
-- video scaling.
motionImageInserter_offset :: Lens.Lens' MotionImageInserter (Prelude.Maybe MotionImageInsertionOffset)
motionImageInserter_offset = Lens.lens (\MotionImageInserter' {offset} -> offset) (\s@MotionImageInserter' {} a -> s {offset = a} :: MotionImageInserter)

instance Prelude.FromJSON MotionImageInserter where
  parseJSON =
    Prelude.withObject
      "MotionImageInserter"
      ( \x ->
          MotionImageInserter'
            Prelude.<$> (x Prelude..:? "insertionMode")
            Prelude.<*> (x Prelude..:? "input")
            Prelude.<*> (x Prelude..:? "startTime")
            Prelude.<*> (x Prelude..:? "playback")
            Prelude.<*> (x Prelude..:? "framerate")
            Prelude.<*> (x Prelude..:? "offset")
      )

instance Prelude.Hashable MotionImageInserter

instance Prelude.NFData MotionImageInserter

instance Prelude.ToJSON MotionImageInserter where
  toJSON MotionImageInserter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("insertionMode" Prelude..=)
              Prelude.<$> insertionMode,
            ("input" Prelude..=) Prelude.<$> input,
            ("startTime" Prelude..=) Prelude.<$> startTime,
            ("playback" Prelude..=) Prelude.<$> playback,
            ("framerate" Prelude..=) Prelude.<$> framerate,
            ("offset" Prelude..=) Prelude.<$> offset
          ]
      )
