{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoCodecSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoCodecSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.FrameCaptureSettings
import Network.AWS.MediaLive.Types.H264Settings
import Network.AWS.MediaLive.Types.H265Settings
import Network.AWS.MediaLive.Types.Mpeg2Settings
import Network.AWS.Prelude

-- | Video Codec Settings
--
-- /See:/ 'videoCodecSettings' smart constructor.
data VideoCodecSettings = VideoCodecSettings'
  { _vcsFrameCaptureSettings ::
      !(Maybe FrameCaptureSettings),
    _vcsH265Settings :: !(Maybe H265Settings),
    _vcsH264Settings :: !(Maybe H264Settings),
    _vcsMpeg2Settings :: !(Maybe Mpeg2Settings)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VideoCodecSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcsFrameCaptureSettings' - Undocumented member.
--
-- * 'vcsH265Settings' - Undocumented member.
--
-- * 'vcsH264Settings' - Undocumented member.
--
-- * 'vcsMpeg2Settings' - Undocumented member.
videoCodecSettings ::
  VideoCodecSettings
videoCodecSettings =
  VideoCodecSettings'
    { _vcsFrameCaptureSettings = Nothing,
      _vcsH265Settings = Nothing,
      _vcsH264Settings = Nothing,
      _vcsMpeg2Settings = Nothing
    }

-- | Undocumented member.
vcsFrameCaptureSettings :: Lens' VideoCodecSettings (Maybe FrameCaptureSettings)
vcsFrameCaptureSettings = lens _vcsFrameCaptureSettings (\s a -> s {_vcsFrameCaptureSettings = a})

-- | Undocumented member.
vcsH265Settings :: Lens' VideoCodecSettings (Maybe H265Settings)
vcsH265Settings = lens _vcsH265Settings (\s a -> s {_vcsH265Settings = a})

-- | Undocumented member.
vcsH264Settings :: Lens' VideoCodecSettings (Maybe H264Settings)
vcsH264Settings = lens _vcsH264Settings (\s a -> s {_vcsH264Settings = a})

-- | Undocumented member.
vcsMpeg2Settings :: Lens' VideoCodecSettings (Maybe Mpeg2Settings)
vcsMpeg2Settings = lens _vcsMpeg2Settings (\s a -> s {_vcsMpeg2Settings = a})

instance FromJSON VideoCodecSettings where
  parseJSON =
    withObject
      "VideoCodecSettings"
      ( \x ->
          VideoCodecSettings'
            <$> (x .:? "frameCaptureSettings")
            <*> (x .:? "h265Settings")
            <*> (x .:? "h264Settings")
            <*> (x .:? "mpeg2Settings")
      )

instance Hashable VideoCodecSettings

instance NFData VideoCodecSettings

instance ToJSON VideoCodecSettings where
  toJSON VideoCodecSettings' {..} =
    object
      ( catMaybes
          [ ("frameCaptureSettings" .=) <$> _vcsFrameCaptureSettings,
            ("h265Settings" .=) <$> _vcsH265Settings,
            ("h264Settings" .=) <$> _vcsH264Settings,
            ("mpeg2Settings" .=) <$> _vcsMpeg2Settings
          ]
      )
