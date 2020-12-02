{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.OutputGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.OutputGroupSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.ArchiveGroupSettings
import Network.AWS.MediaLive.Types.FrameCaptureGroupSettings
import Network.AWS.MediaLive.Types.HlsGroupSettings
import Network.AWS.MediaLive.Types.MediaPackageGroupSettings
import Network.AWS.MediaLive.Types.MsSmoothGroupSettings
import Network.AWS.MediaLive.Types.MultiplexGroupSettings
import Network.AWS.MediaLive.Types.RtmpGroupSettings
import Network.AWS.MediaLive.Types.UdpGroupSettings
import Network.AWS.Prelude

-- | Output Group Settings
--
-- /See:/ 'outputGroupSettings' smart constructor.
data OutputGroupSettings = OutputGroupSettings'
  { _ogsMediaPackageGroupSettings ::
      !(Maybe MediaPackageGroupSettings),
    _ogsMsSmoothGroupSettings ::
      !(Maybe MsSmoothGroupSettings),
    _ogsRtmpGroupSettings :: !(Maybe RtmpGroupSettings),
    _ogsMultiplexGroupSettings ::
      !(Maybe MultiplexGroupSettings),
    _ogsHlsGroupSettings :: !(Maybe HlsGroupSettings),
    _ogsArchiveGroupSettings ::
      !(Maybe ArchiveGroupSettings),
    _ogsUdpGroupSettings :: !(Maybe UdpGroupSettings),
    _ogsFrameCaptureGroupSettings ::
      !(Maybe FrameCaptureGroupSettings)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogsMediaPackageGroupSettings' - Undocumented member.
--
-- * 'ogsMsSmoothGroupSettings' - Undocumented member.
--
-- * 'ogsRtmpGroupSettings' - Undocumented member.
--
-- * 'ogsMultiplexGroupSettings' - Undocumented member.
--
-- * 'ogsHlsGroupSettings' - Undocumented member.
--
-- * 'ogsArchiveGroupSettings' - Undocumented member.
--
-- * 'ogsUdpGroupSettings' - Undocumented member.
--
-- * 'ogsFrameCaptureGroupSettings' - Undocumented member.
outputGroupSettings ::
  OutputGroupSettings
outputGroupSettings =
  OutputGroupSettings'
    { _ogsMediaPackageGroupSettings = Nothing,
      _ogsMsSmoothGroupSettings = Nothing,
      _ogsRtmpGroupSettings = Nothing,
      _ogsMultiplexGroupSettings = Nothing,
      _ogsHlsGroupSettings = Nothing,
      _ogsArchiveGroupSettings = Nothing,
      _ogsUdpGroupSettings = Nothing,
      _ogsFrameCaptureGroupSettings = Nothing
    }

-- | Undocumented member.
ogsMediaPackageGroupSettings :: Lens' OutputGroupSettings (Maybe MediaPackageGroupSettings)
ogsMediaPackageGroupSettings = lens _ogsMediaPackageGroupSettings (\s a -> s {_ogsMediaPackageGroupSettings = a})

-- | Undocumented member.
ogsMsSmoothGroupSettings :: Lens' OutputGroupSettings (Maybe MsSmoothGroupSettings)
ogsMsSmoothGroupSettings = lens _ogsMsSmoothGroupSettings (\s a -> s {_ogsMsSmoothGroupSettings = a})

-- | Undocumented member.
ogsRtmpGroupSettings :: Lens' OutputGroupSettings (Maybe RtmpGroupSettings)
ogsRtmpGroupSettings = lens _ogsRtmpGroupSettings (\s a -> s {_ogsRtmpGroupSettings = a})

-- | Undocumented member.
ogsMultiplexGroupSettings :: Lens' OutputGroupSettings (Maybe MultiplexGroupSettings)
ogsMultiplexGroupSettings = lens _ogsMultiplexGroupSettings (\s a -> s {_ogsMultiplexGroupSettings = a})

-- | Undocumented member.
ogsHlsGroupSettings :: Lens' OutputGroupSettings (Maybe HlsGroupSettings)
ogsHlsGroupSettings = lens _ogsHlsGroupSettings (\s a -> s {_ogsHlsGroupSettings = a})

-- | Undocumented member.
ogsArchiveGroupSettings :: Lens' OutputGroupSettings (Maybe ArchiveGroupSettings)
ogsArchiveGroupSettings = lens _ogsArchiveGroupSettings (\s a -> s {_ogsArchiveGroupSettings = a})

-- | Undocumented member.
ogsUdpGroupSettings :: Lens' OutputGroupSettings (Maybe UdpGroupSettings)
ogsUdpGroupSettings = lens _ogsUdpGroupSettings (\s a -> s {_ogsUdpGroupSettings = a})

-- | Undocumented member.
ogsFrameCaptureGroupSettings :: Lens' OutputGroupSettings (Maybe FrameCaptureGroupSettings)
ogsFrameCaptureGroupSettings = lens _ogsFrameCaptureGroupSettings (\s a -> s {_ogsFrameCaptureGroupSettings = a})

instance FromJSON OutputGroupSettings where
  parseJSON =
    withObject
      "OutputGroupSettings"
      ( \x ->
          OutputGroupSettings'
            <$> (x .:? "mediaPackageGroupSettings")
            <*> (x .:? "msSmoothGroupSettings")
            <*> (x .:? "rtmpGroupSettings")
            <*> (x .:? "multiplexGroupSettings")
            <*> (x .:? "hlsGroupSettings")
            <*> (x .:? "archiveGroupSettings")
            <*> (x .:? "udpGroupSettings")
            <*> (x .:? "frameCaptureGroupSettings")
      )

instance Hashable OutputGroupSettings

instance NFData OutputGroupSettings

instance ToJSON OutputGroupSettings where
  toJSON OutputGroupSettings' {..} =
    object
      ( catMaybes
          [ ("mediaPackageGroupSettings" .=)
              <$> _ogsMediaPackageGroupSettings,
            ("msSmoothGroupSettings" .=) <$> _ogsMsSmoothGroupSettings,
            ("rtmpGroupSettings" .=) <$> _ogsRtmpGroupSettings,
            ("multiplexGroupSettings" .=) <$> _ogsMultiplexGroupSettings,
            ("hlsGroupSettings" .=) <$> _ogsHlsGroupSettings,
            ("archiveGroupSettings" .=) <$> _ogsArchiveGroupSettings,
            ("udpGroupSettings" .=) <$> _ogsUdpGroupSettings,
            ("frameCaptureGroupSettings" .=)
              <$> _ogsFrameCaptureGroupSettings
          ]
      )
