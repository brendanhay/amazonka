{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.FileGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.FileGroupSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.DestinationSettings
import Network.AWS.Prelude

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to FILE_GROUP_SETTINGS.
--
-- /See:/ 'fileGroupSettings' smart constructor.
data FileGroupSettings = FileGroupSettings'
  { _fgsDestination ::
      !(Maybe Text),
    _fgsDestinationSettings :: !(Maybe DestinationSettings)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FileGroupSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fgsDestination' - Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
--
-- * 'fgsDestinationSettings' - Settings associated with the destination. Will vary based on the type of destination
fileGroupSettings ::
  FileGroupSettings
fileGroupSettings =
  FileGroupSettings'
    { _fgsDestination = Nothing,
      _fgsDestinationSettings = Nothing
    }

-- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
fgsDestination :: Lens' FileGroupSettings (Maybe Text)
fgsDestination = lens _fgsDestination (\s a -> s {_fgsDestination = a})

-- | Settings associated with the destination. Will vary based on the type of destination
fgsDestinationSettings :: Lens' FileGroupSettings (Maybe DestinationSettings)
fgsDestinationSettings = lens _fgsDestinationSettings (\s a -> s {_fgsDestinationSettings = a})

instance FromJSON FileGroupSettings where
  parseJSON =
    withObject
      "FileGroupSettings"
      ( \x ->
          FileGroupSettings'
            <$> (x .:? "destination") <*> (x .:? "destinationSettings")
      )

instance Hashable FileGroupSettings

instance NFData FileGroupSettings

instance ToJSON FileGroupSettings where
  toJSON FileGroupSettings' {..} =
    object
      ( catMaybes
          [ ("destination" .=) <$> _fgsDestination,
            ("destinationSettings" .=) <$> _fgsDestinationSettings
          ]
      )
