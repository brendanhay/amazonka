{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MotionImageInserter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MotionImageInserter where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.MotionImageInsertionFramerate
import Network.AWS.MediaConvert.Types.MotionImageInsertionMode
import Network.AWS.MediaConvert.Types.MotionImageInsertionOffset
import Network.AWS.MediaConvert.Types.MotionImagePlayback
import Network.AWS.Prelude

-- | Overlay motion graphics on top of your video at the time that you specify.
--
-- /See:/ 'motionImageInserter' smart constructor.
data MotionImageInserter = MotionImageInserter'
  { _miiFramerate ::
      !(Maybe MotionImageInsertionFramerate),
    _miiStartTime :: !(Maybe Text),
    _miiOffset :: !(Maybe MotionImageInsertionOffset),
    _miiInput :: !(Maybe Text),
    _miiInsertionMode ::
      !(Maybe MotionImageInsertionMode),
    _miiPlayback :: !(Maybe MotionImagePlayback)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MotionImageInserter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'miiFramerate' - If your motion graphic asset is a .mov file, keep this setting unspecified. If your motion graphic asset is a series of .png files, specify the frame rate of the overlay in frames per second, as a fraction. For example, specify 24 fps as 24/1. Make sure that the number of images in your series matches the frame rate and your intended overlay duration. For example, if you want a 30-second overlay at 30 fps, you should have 900 .png images. This overlay frame rate doesn't need to match the frame rate of the underlying video.
--
-- * 'miiStartTime' - Specify when the motion overlay begins. Use timecode format (HH:MM:SS:FF or HH:MM:SS;FF). Make sure that the timecode you provide here takes into account how you have set up your timecode configuration under both job settings and input settings. The simplest way to do that is to set both to start at 0. If you need to set up your job to follow timecodes embedded in your source that don't start at zero, make sure that you specify a start time that is after the first embedded timecode. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/setting-up-timecode.html Find job-wide and input timecode configuration settings in your JSON job settings specification at settings>timecodeConfig>source and settings>inputs>timecodeSource.
--
-- * 'miiOffset' - Use Offset to specify the placement of your motion graphic overlay on the video frame. Specify in pixels, from the upper-left corner of the frame. If you don't specify an offset, the service scales your overlay to the full size of the frame. Otherwise, the service inserts the overlay at its native resolution and scales the size up or down with any video scaling.
--
-- * 'miiInput' - Specify the .mov file or series of .png files that you want to overlay on your video. For .png files, provide the file name of the first file in the series. Make sure that the names of the .png files end with sequential numbers that specify the order that they are played in. For example, overlay_000.png, overlay_001.png, overlay_002.png, and so on. The sequence must start at zero, and each image file name must have the same number of digits. Pad your initial file names with enough zeros to complete the sequence. For example, if the first image is overlay_0.png, there can be only 10 images in the sequence, with the last image being overlay_9.png. But if the first image is overlay_00.png, there can be 100 images in the sequence.
--
-- * 'miiInsertionMode' - Choose the type of motion graphic asset that you are providing for your overlay. You can choose either a .mov file or a series of .png files.
--
-- * 'miiPlayback' - Specify whether your motion graphic overlay repeats on a loop or plays only once.
motionImageInserter ::
  MotionImageInserter
motionImageInserter =
  MotionImageInserter'
    { _miiFramerate = Nothing,
      _miiStartTime = Nothing,
      _miiOffset = Nothing,
      _miiInput = Nothing,
      _miiInsertionMode = Nothing,
      _miiPlayback = Nothing
    }

-- | If your motion graphic asset is a .mov file, keep this setting unspecified. If your motion graphic asset is a series of .png files, specify the frame rate of the overlay in frames per second, as a fraction. For example, specify 24 fps as 24/1. Make sure that the number of images in your series matches the frame rate and your intended overlay duration. For example, if you want a 30-second overlay at 30 fps, you should have 900 .png images. This overlay frame rate doesn't need to match the frame rate of the underlying video.
miiFramerate :: Lens' MotionImageInserter (Maybe MotionImageInsertionFramerate)
miiFramerate = lens _miiFramerate (\s a -> s {_miiFramerate = a})

-- | Specify when the motion overlay begins. Use timecode format (HH:MM:SS:FF or HH:MM:SS;FF). Make sure that the timecode you provide here takes into account how you have set up your timecode configuration under both job settings and input settings. The simplest way to do that is to set both to start at 0. If you need to set up your job to follow timecodes embedded in your source that don't start at zero, make sure that you specify a start time that is after the first embedded timecode. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/setting-up-timecode.html Find job-wide and input timecode configuration settings in your JSON job settings specification at settings>timecodeConfig>source and settings>inputs>timecodeSource.
miiStartTime :: Lens' MotionImageInserter (Maybe Text)
miiStartTime = lens _miiStartTime (\s a -> s {_miiStartTime = a})

-- | Use Offset to specify the placement of your motion graphic overlay on the video frame. Specify in pixels, from the upper-left corner of the frame. If you don't specify an offset, the service scales your overlay to the full size of the frame. Otherwise, the service inserts the overlay at its native resolution and scales the size up or down with any video scaling.
miiOffset :: Lens' MotionImageInserter (Maybe MotionImageInsertionOffset)
miiOffset = lens _miiOffset (\s a -> s {_miiOffset = a})

-- | Specify the .mov file or series of .png files that you want to overlay on your video. For .png files, provide the file name of the first file in the series. Make sure that the names of the .png files end with sequential numbers that specify the order that they are played in. For example, overlay_000.png, overlay_001.png, overlay_002.png, and so on. The sequence must start at zero, and each image file name must have the same number of digits. Pad your initial file names with enough zeros to complete the sequence. For example, if the first image is overlay_0.png, there can be only 10 images in the sequence, with the last image being overlay_9.png. But if the first image is overlay_00.png, there can be 100 images in the sequence.
miiInput :: Lens' MotionImageInserter (Maybe Text)
miiInput = lens _miiInput (\s a -> s {_miiInput = a})

-- | Choose the type of motion graphic asset that you are providing for your overlay. You can choose either a .mov file or a series of .png files.
miiInsertionMode :: Lens' MotionImageInserter (Maybe MotionImageInsertionMode)
miiInsertionMode = lens _miiInsertionMode (\s a -> s {_miiInsertionMode = a})

-- | Specify whether your motion graphic overlay repeats on a loop or plays only once.
miiPlayback :: Lens' MotionImageInserter (Maybe MotionImagePlayback)
miiPlayback = lens _miiPlayback (\s a -> s {_miiPlayback = a})

instance FromJSON MotionImageInserter where
  parseJSON =
    withObject
      "MotionImageInserter"
      ( \x ->
          MotionImageInserter'
            <$> (x .:? "framerate")
            <*> (x .:? "startTime")
            <*> (x .:? "offset")
            <*> (x .:? "input")
            <*> (x .:? "insertionMode")
            <*> (x .:? "playback")
      )

instance Hashable MotionImageInserter

instance NFData MotionImageInserter

instance ToJSON MotionImageInserter where
  toJSON MotionImageInserter' {..} =
    object
      ( catMaybes
          [ ("framerate" .=) <$> _miiFramerate,
            ("startTime" .=) <$> _miiStartTime,
            ("offset" .=) <$> _miiOffset,
            ("input" .=) <$> _miiInput,
            ("insertionMode" .=) <$> _miiInsertionMode,
            ("playback" .=) <$> _miiPlayback
          ]
      )
