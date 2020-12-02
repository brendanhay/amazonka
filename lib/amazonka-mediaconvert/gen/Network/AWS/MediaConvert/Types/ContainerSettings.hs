{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ContainerSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ContainerSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.CmfcSettings
import Network.AWS.MediaConvert.Types.ContainerType
import Network.AWS.MediaConvert.Types.F4vSettings
import Network.AWS.MediaConvert.Types.M2tsSettings
import Network.AWS.MediaConvert.Types.M3u8Settings
import Network.AWS.MediaConvert.Types.MovSettings
import Network.AWS.MediaConvert.Types.Mp4Settings
import Network.AWS.MediaConvert.Types.MpdSettings
import Network.AWS.MediaConvert.Types.MxfSettings
import Network.AWS.Prelude

-- | Container specific settings.
--
-- /See:/ 'containerSettings' smart constructor.
data ContainerSettings = ContainerSettings'
  { _csM2tsSettings ::
      !(Maybe M2tsSettings),
    _csMxfSettings :: !(Maybe MxfSettings),
    _csM3u8Settings :: !(Maybe M3u8Settings),
    _csCmfcSettings :: !(Maybe CmfcSettings),
    _csMovSettings :: !(Maybe MovSettings),
    _csMp4Settings :: !(Maybe Mp4Settings),
    _csMpdSettings :: !(Maybe MpdSettings),
    _csContainer :: !(Maybe ContainerType),
    _csF4vSettings :: !(Maybe F4vSettings)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContainerSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csM2tsSettings' - MPEG-2 TS container settings. These apply to outputs in a File output group when the output's container (ContainerType) is MPEG-2 Transport Stream (M2TS). In these assets, data is organized by the program map table (PMT). Each transport stream program contains subsets of data, including audio, video, and metadata. Each of these subsets of data has a numerical label called a packet identifier (PID). Each transport stream program corresponds to one MediaConvert output. The PMT lists the types of data in a program along with their PID. Downstream systems and players use the program map table to look up the PID for each type of data it accesses and then uses the PIDs to locate specific data within the asset.
--
-- * 'csMxfSettings' - MXF settings
--
-- * 'csM3u8Settings' - Settings for TS segments in HLS
--
-- * 'csCmfcSettings' - Settings for MP4 segments in CMAF
--
-- * 'csMovSettings' - Settings for MOV Container.
--
-- * 'csMp4Settings' - Settings for MP4 container. You can create audio-only AAC outputs with this container.
--
-- * 'csMpdSettings' - Settings for MP4 segments in DASH
--
-- * 'csContainer' - Container for this output. Some containers require a container settings object. If not specified, the default object will be created.
--
-- * 'csF4vSettings' - Settings for F4v container
containerSettings ::
  ContainerSettings
containerSettings =
  ContainerSettings'
    { _csM2tsSettings = Nothing,
      _csMxfSettings = Nothing,
      _csM3u8Settings = Nothing,
      _csCmfcSettings = Nothing,
      _csMovSettings = Nothing,
      _csMp4Settings = Nothing,
      _csMpdSettings = Nothing,
      _csContainer = Nothing,
      _csF4vSettings = Nothing
    }

-- | MPEG-2 TS container settings. These apply to outputs in a File output group when the output's container (ContainerType) is MPEG-2 Transport Stream (M2TS). In these assets, data is organized by the program map table (PMT). Each transport stream program contains subsets of data, including audio, video, and metadata. Each of these subsets of data has a numerical label called a packet identifier (PID). Each transport stream program corresponds to one MediaConvert output. The PMT lists the types of data in a program along with their PID. Downstream systems and players use the program map table to look up the PID for each type of data it accesses and then uses the PIDs to locate specific data within the asset.
csM2tsSettings :: Lens' ContainerSettings (Maybe M2tsSettings)
csM2tsSettings = lens _csM2tsSettings (\s a -> s {_csM2tsSettings = a})

-- | MXF settings
csMxfSettings :: Lens' ContainerSettings (Maybe MxfSettings)
csMxfSettings = lens _csMxfSettings (\s a -> s {_csMxfSettings = a})

-- | Settings for TS segments in HLS
csM3u8Settings :: Lens' ContainerSettings (Maybe M3u8Settings)
csM3u8Settings = lens _csM3u8Settings (\s a -> s {_csM3u8Settings = a})

-- | Settings for MP4 segments in CMAF
csCmfcSettings :: Lens' ContainerSettings (Maybe CmfcSettings)
csCmfcSettings = lens _csCmfcSettings (\s a -> s {_csCmfcSettings = a})

-- | Settings for MOV Container.
csMovSettings :: Lens' ContainerSettings (Maybe MovSettings)
csMovSettings = lens _csMovSettings (\s a -> s {_csMovSettings = a})

-- | Settings for MP4 container. You can create audio-only AAC outputs with this container.
csMp4Settings :: Lens' ContainerSettings (Maybe Mp4Settings)
csMp4Settings = lens _csMp4Settings (\s a -> s {_csMp4Settings = a})

-- | Settings for MP4 segments in DASH
csMpdSettings :: Lens' ContainerSettings (Maybe MpdSettings)
csMpdSettings = lens _csMpdSettings (\s a -> s {_csMpdSettings = a})

-- | Container for this output. Some containers require a container settings object. If not specified, the default object will be created.
csContainer :: Lens' ContainerSettings (Maybe ContainerType)
csContainer = lens _csContainer (\s a -> s {_csContainer = a})

-- | Settings for F4v container
csF4vSettings :: Lens' ContainerSettings (Maybe F4vSettings)
csF4vSettings = lens _csF4vSettings (\s a -> s {_csF4vSettings = a})

instance FromJSON ContainerSettings where
  parseJSON =
    withObject
      "ContainerSettings"
      ( \x ->
          ContainerSettings'
            <$> (x .:? "m2tsSettings")
            <*> (x .:? "mxfSettings")
            <*> (x .:? "m3u8Settings")
            <*> (x .:? "cmfcSettings")
            <*> (x .:? "movSettings")
            <*> (x .:? "mp4Settings")
            <*> (x .:? "mpdSettings")
            <*> (x .:? "container")
            <*> (x .:? "f4vSettings")
      )

instance Hashable ContainerSettings

instance NFData ContainerSettings

instance ToJSON ContainerSettings where
  toJSON ContainerSettings' {..} =
    object
      ( catMaybes
          [ ("m2tsSettings" .=) <$> _csM2tsSettings,
            ("mxfSettings" .=) <$> _csMxfSettings,
            ("m3u8Settings" .=) <$> _csM3u8Settings,
            ("cmfcSettings" .=) <$> _csCmfcSettings,
            ("movSettings" .=) <$> _csMovSettings,
            ("mp4Settings" .=) <$> _csMp4Settings,
            ("mpdSettings" .=) <$> _csMpdSettings,
            ("container" .=) <$> _csContainer,
            ("f4vSettings" .=) <$> _csF4vSettings
          ]
      )
