{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ArchiveOutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ArchiveOutputSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.ArchiveContainerSettings
import Network.AWS.Prelude

-- | Archive Output Settings
--
-- /See:/ 'archiveOutputSettings' smart constructor.
data ArchiveOutputSettings = ArchiveOutputSettings'
  { _aosExtension ::
      !(Maybe Text),
    _aosNameModifier :: !(Maybe Text),
    _aosContainerSettings ::
      !ArchiveContainerSettings
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ArchiveOutputSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aosExtension' - Output file extension. If excluded, this will be auto-selected from the container type.
--
-- * 'aosNameModifier' - String concatenated to the end of the destination filename.  Required for multiple outputs of the same type.
--
-- * 'aosContainerSettings' - Settings specific to the container type of the file.
archiveOutputSettings ::
  -- | 'aosContainerSettings'
  ArchiveContainerSettings ->
  ArchiveOutputSettings
archiveOutputSettings pContainerSettings_ =
  ArchiveOutputSettings'
    { _aosExtension = Nothing,
      _aosNameModifier = Nothing,
      _aosContainerSettings = pContainerSettings_
    }

-- | Output file extension. If excluded, this will be auto-selected from the container type.
aosExtension :: Lens' ArchiveOutputSettings (Maybe Text)
aosExtension = lens _aosExtension (\s a -> s {_aosExtension = a})

-- | String concatenated to the end of the destination filename.  Required for multiple outputs of the same type.
aosNameModifier :: Lens' ArchiveOutputSettings (Maybe Text)
aosNameModifier = lens _aosNameModifier (\s a -> s {_aosNameModifier = a})

-- | Settings specific to the container type of the file.
aosContainerSettings :: Lens' ArchiveOutputSettings ArchiveContainerSettings
aosContainerSettings = lens _aosContainerSettings (\s a -> s {_aosContainerSettings = a})

instance FromJSON ArchiveOutputSettings where
  parseJSON =
    withObject
      "ArchiveOutputSettings"
      ( \x ->
          ArchiveOutputSettings'
            <$> (x .:? "extension")
            <*> (x .:? "nameModifier")
            <*> (x .: "containerSettings")
      )

instance Hashable ArchiveOutputSettings

instance NFData ArchiveOutputSettings

instance ToJSON ArchiveOutputSettings where
  toJSON ArchiveOutputSettings' {..} =
    object
      ( catMaybes
          [ ("extension" .=) <$> _aosExtension,
            ("nameModifier" .=) <$> _aosNameModifier,
            Just ("containerSettings" .= _aosContainerSettings)
          ]
      )
