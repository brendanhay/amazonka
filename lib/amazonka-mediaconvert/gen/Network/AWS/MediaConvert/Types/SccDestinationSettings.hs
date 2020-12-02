{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.SccDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.SccDestinationSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.SccDestinationFramerate
import Network.AWS.Prelude

-- | Settings for SCC caption output.
--
-- /See:/ 'sccDestinationSettings' smart constructor.
newtype SccDestinationSettings = SccDestinationSettings'
  { _sdsFramerate ::
      Maybe SccDestinationFramerate
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SccDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdsFramerate' - Set Framerate (SccDestinationFramerate) to make sure that the captions and the video are synchronized in the output. Specify a frame rate that matches the frame rate of the associated video. If the video frame rate is 29.97, choose 29.97 dropframe (FRAMERATE_29_97_DROPFRAME) only if the video has video_insertion=true and drop_frame_timecode=true; otherwise, choose 29.97 non-dropframe (FRAMERATE_29_97_NON_DROPFRAME).
sccDestinationSettings ::
  SccDestinationSettings
sccDestinationSettings =
  SccDestinationSettings' {_sdsFramerate = Nothing}

-- | Set Framerate (SccDestinationFramerate) to make sure that the captions and the video are synchronized in the output. Specify a frame rate that matches the frame rate of the associated video. If the video frame rate is 29.97, choose 29.97 dropframe (FRAMERATE_29_97_DROPFRAME) only if the video has video_insertion=true and drop_frame_timecode=true; otherwise, choose 29.97 non-dropframe (FRAMERATE_29_97_NON_DROPFRAME).
sdsFramerate :: Lens' SccDestinationSettings (Maybe SccDestinationFramerate)
sdsFramerate = lens _sdsFramerate (\s a -> s {_sdsFramerate = a})

instance FromJSON SccDestinationSettings where
  parseJSON =
    withObject
      "SccDestinationSettings"
      (\x -> SccDestinationSettings' <$> (x .:? "framerate"))

instance Hashable SccDestinationSettings

instance NFData SccDestinationSettings

instance ToJSON SccDestinationSettings where
  toJSON SccDestinationSettings' {..} =
    object (catMaybes [("framerate" .=) <$> _sdsFramerate])
