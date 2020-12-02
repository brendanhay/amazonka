{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.OutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.OutputSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.ArchiveOutputSettings
import Network.AWS.MediaLive.Types.FrameCaptureOutputSettings
import Network.AWS.MediaLive.Types.HlsOutputSettings
import Network.AWS.MediaLive.Types.MediaPackageOutputSettings
import Network.AWS.MediaLive.Types.MsSmoothOutputSettings
import Network.AWS.MediaLive.Types.MultiplexOutputSettings
import Network.AWS.MediaLive.Types.RtmpOutputSettings
import Network.AWS.MediaLive.Types.UdpOutputSettings
import Network.AWS.Prelude

-- | Output Settings
--
-- /See:/ 'outputSettings' smart constructor.
data OutputSettings = OutputSettings'
  { _osMultiplexOutputSettings ::
      !(Maybe MultiplexOutputSettings),
    _osArchiveOutputSettings :: !(Maybe ArchiveOutputSettings),
    _osRtmpOutputSettings :: !(Maybe RtmpOutputSettings),
    _osMediaPackageOutputSettings ::
      !(Maybe MediaPackageOutputSettings),
    _osHlsOutputSettings :: !(Maybe HlsOutputSettings),
    _osFrameCaptureOutputSettings ::
      !(Maybe FrameCaptureOutputSettings),
    _osUdpOutputSettings :: !(Maybe UdpOutputSettings),
    _osMsSmoothOutputSettings :: !(Maybe MsSmoothOutputSettings)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osMultiplexOutputSettings' - Undocumented member.
--
-- * 'osArchiveOutputSettings' - Undocumented member.
--
-- * 'osRtmpOutputSettings' - Undocumented member.
--
-- * 'osMediaPackageOutputSettings' - Undocumented member.
--
-- * 'osHlsOutputSettings' - Undocumented member.
--
-- * 'osFrameCaptureOutputSettings' - Undocumented member.
--
-- * 'osUdpOutputSettings' - Undocumented member.
--
-- * 'osMsSmoothOutputSettings' - Undocumented member.
outputSettings ::
  OutputSettings
outputSettings =
  OutputSettings'
    { _osMultiplexOutputSettings = Nothing,
      _osArchiveOutputSettings = Nothing,
      _osRtmpOutputSettings = Nothing,
      _osMediaPackageOutputSettings = Nothing,
      _osHlsOutputSettings = Nothing,
      _osFrameCaptureOutputSettings = Nothing,
      _osUdpOutputSettings = Nothing,
      _osMsSmoothOutputSettings = Nothing
    }

-- | Undocumented member.
osMultiplexOutputSettings :: Lens' OutputSettings (Maybe MultiplexOutputSettings)
osMultiplexOutputSettings = lens _osMultiplexOutputSettings (\s a -> s {_osMultiplexOutputSettings = a})

-- | Undocumented member.
osArchiveOutputSettings :: Lens' OutputSettings (Maybe ArchiveOutputSettings)
osArchiveOutputSettings = lens _osArchiveOutputSettings (\s a -> s {_osArchiveOutputSettings = a})

-- | Undocumented member.
osRtmpOutputSettings :: Lens' OutputSettings (Maybe RtmpOutputSettings)
osRtmpOutputSettings = lens _osRtmpOutputSettings (\s a -> s {_osRtmpOutputSettings = a})

-- | Undocumented member.
osMediaPackageOutputSettings :: Lens' OutputSettings (Maybe MediaPackageOutputSettings)
osMediaPackageOutputSettings = lens _osMediaPackageOutputSettings (\s a -> s {_osMediaPackageOutputSettings = a})

-- | Undocumented member.
osHlsOutputSettings :: Lens' OutputSettings (Maybe HlsOutputSettings)
osHlsOutputSettings = lens _osHlsOutputSettings (\s a -> s {_osHlsOutputSettings = a})

-- | Undocumented member.
osFrameCaptureOutputSettings :: Lens' OutputSettings (Maybe FrameCaptureOutputSettings)
osFrameCaptureOutputSettings = lens _osFrameCaptureOutputSettings (\s a -> s {_osFrameCaptureOutputSettings = a})

-- | Undocumented member.
osUdpOutputSettings :: Lens' OutputSettings (Maybe UdpOutputSettings)
osUdpOutputSettings = lens _osUdpOutputSettings (\s a -> s {_osUdpOutputSettings = a})

-- | Undocumented member.
osMsSmoothOutputSettings :: Lens' OutputSettings (Maybe MsSmoothOutputSettings)
osMsSmoothOutputSettings = lens _osMsSmoothOutputSettings (\s a -> s {_osMsSmoothOutputSettings = a})

instance FromJSON OutputSettings where
  parseJSON =
    withObject
      "OutputSettings"
      ( \x ->
          OutputSettings'
            <$> (x .:? "multiplexOutputSettings")
            <*> (x .:? "archiveOutputSettings")
            <*> (x .:? "rtmpOutputSettings")
            <*> (x .:? "mediaPackageOutputSettings")
            <*> (x .:? "hlsOutputSettings")
            <*> (x .:? "frameCaptureOutputSettings")
            <*> (x .:? "udpOutputSettings")
            <*> (x .:? "msSmoothOutputSettings")
      )

instance Hashable OutputSettings

instance NFData OutputSettings

instance ToJSON OutputSettings where
  toJSON OutputSettings' {..} =
    object
      ( catMaybes
          [ ("multiplexOutputSettings" .=) <$> _osMultiplexOutputSettings,
            ("archiveOutputSettings" .=) <$> _osArchiveOutputSettings,
            ("rtmpOutputSettings" .=) <$> _osRtmpOutputSettings,
            ("mediaPackageOutputSettings" .=)
              <$> _osMediaPackageOutputSettings,
            ("hlsOutputSettings" .=) <$> _osHlsOutputSettings,
            ("frameCaptureOutputSettings" .=)
              <$> _osFrameCaptureOutputSettings,
            ("udpOutputSettings" .=) <$> _osUdpOutputSettings,
            ("msSmoothOutputSettings" .=) <$> _osMsSmoothOutputSettings
          ]
      )
