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
-- Module      : Amazonka.MediaConvert.Types.MotionImageInserter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.MotionImageInserter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.MotionImageInsertionFramerate
import Amazonka.MediaConvert.Types.MotionImageInsertionMode
import Amazonka.MediaConvert.Types.MotionImageInsertionOffset
import Amazonka.MediaConvert.Types.MotionImagePlayback
import qualified Amazonka.Prelude as Prelude

-- | Overlay motion graphics on top of your video. The motion graphics that
-- you specify here appear on all outputs in all output groups. For more
-- information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/motion-graphic-overlay.html.
--
-- /See:/ 'newMotionImageInserter' smart constructor.
data MotionImageInserter = MotionImageInserter'
  { -- | If your motion graphic asset is a .mov file, keep this setting
    -- unspecified. If your motion graphic asset is a series of .png files,
    -- specify the frame rate of the overlay in frames per second, as a
    -- fraction. For example, specify 24 fps as 24\/1. Make sure that the
    -- number of images in your series matches the frame rate and your intended
    -- overlay duration. For example, if you want a 30-second overlay at 30
    -- fps, you should have 900 .png images. This overlay frame rate doesn\'t
    -- need to match the frame rate of the underlying video.
    framerate :: Prelude.Maybe MotionImageInsertionFramerate,
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
    -- | Choose the type of motion graphic asset that you are providing for your
    -- overlay. You can choose either a .mov file or a series of .png files.
    insertionMode :: Prelude.Maybe MotionImageInsertionMode,
    -- | Use Offset to specify the placement of your motion graphic overlay on
    -- the video frame. Specify in pixels, from the upper-left corner of the
    -- frame. If you don\'t specify an offset, the service scales your overlay
    -- to the full size of the frame. Otherwise, the service inserts the
    -- overlay at its native resolution and scales the size up or down with any
    -- video scaling.
    offset :: Prelude.Maybe MotionImageInsertionOffset,
    -- | Specify whether your motion graphic overlay repeats on a loop or plays
    -- only once.
    playback :: Prelude.Maybe MotionImagePlayback,
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
    startTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MotionImageInserter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'insertionMode', 'motionImageInserter_insertionMode' - Choose the type of motion graphic asset that you are providing for your
-- overlay. You can choose either a .mov file or a series of .png files.
--
-- 'offset', 'motionImageInserter_offset' - Use Offset to specify the placement of your motion graphic overlay on
-- the video frame. Specify in pixels, from the upper-left corner of the
-- frame. If you don\'t specify an offset, the service scales your overlay
-- to the full size of the frame. Otherwise, the service inserts the
-- overlay at its native resolution and scales the size up or down with any
-- video scaling.
--
-- 'playback', 'motionImageInserter_playback' - Specify whether your motion graphic overlay repeats on a loop or plays
-- only once.
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
newMotionImageInserter ::
  MotionImageInserter
newMotionImageInserter =
  MotionImageInserter'
    { framerate = Prelude.Nothing,
      input = Prelude.Nothing,
      insertionMode = Prelude.Nothing,
      offset = Prelude.Nothing,
      playback = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

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

-- | Choose the type of motion graphic asset that you are providing for your
-- overlay. You can choose either a .mov file or a series of .png files.
motionImageInserter_insertionMode :: Lens.Lens' MotionImageInserter (Prelude.Maybe MotionImageInsertionMode)
motionImageInserter_insertionMode = Lens.lens (\MotionImageInserter' {insertionMode} -> insertionMode) (\s@MotionImageInserter' {} a -> s {insertionMode = a} :: MotionImageInserter)

-- | Use Offset to specify the placement of your motion graphic overlay on
-- the video frame. Specify in pixels, from the upper-left corner of the
-- frame. If you don\'t specify an offset, the service scales your overlay
-- to the full size of the frame. Otherwise, the service inserts the
-- overlay at its native resolution and scales the size up or down with any
-- video scaling.
motionImageInserter_offset :: Lens.Lens' MotionImageInserter (Prelude.Maybe MotionImageInsertionOffset)
motionImageInserter_offset = Lens.lens (\MotionImageInserter' {offset} -> offset) (\s@MotionImageInserter' {} a -> s {offset = a} :: MotionImageInserter)

-- | Specify whether your motion graphic overlay repeats on a loop or plays
-- only once.
motionImageInserter_playback :: Lens.Lens' MotionImageInserter (Prelude.Maybe MotionImagePlayback)
motionImageInserter_playback = Lens.lens (\MotionImageInserter' {playback} -> playback) (\s@MotionImageInserter' {} a -> s {playback = a} :: MotionImageInserter)

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

instance Data.FromJSON MotionImageInserter where
  parseJSON =
    Data.withObject
      "MotionImageInserter"
      ( \x ->
          MotionImageInserter'
            Prelude.<$> (x Data..:? "framerate")
            Prelude.<*> (x Data..:? "input")
            Prelude.<*> (x Data..:? "insertionMode")
            Prelude.<*> (x Data..:? "offset")
            Prelude.<*> (x Data..:? "playback")
            Prelude.<*> (x Data..:? "startTime")
      )

instance Prelude.Hashable MotionImageInserter where
  hashWithSalt _salt MotionImageInserter' {..} =
    _salt
      `Prelude.hashWithSalt` framerate
      `Prelude.hashWithSalt` input
      `Prelude.hashWithSalt` insertionMode
      `Prelude.hashWithSalt` offset
      `Prelude.hashWithSalt` playback
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData MotionImageInserter where
  rnf MotionImageInserter' {..} =
    Prelude.rnf framerate `Prelude.seq`
      Prelude.rnf input `Prelude.seq`
        Prelude.rnf insertionMode `Prelude.seq`
          Prelude.rnf offset `Prelude.seq`
            Prelude.rnf playback `Prelude.seq`
              Prelude.rnf startTime

instance Data.ToJSON MotionImageInserter where
  toJSON MotionImageInserter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("framerate" Data..=) Prelude.<$> framerate,
            ("input" Data..=) Prelude.<$> input,
            ("insertionMode" Data..=) Prelude.<$> insertionMode,
            ("offset" Data..=) Prelude.<$> offset,
            ("playback" Data..=) Prelude.<$> playback,
            ("startTime" Data..=) Prelude.<$> startTime
          ]
      )
