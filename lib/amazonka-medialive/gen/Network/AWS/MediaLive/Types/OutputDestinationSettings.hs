{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.OutputDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.OutputDestinationSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Placeholder documentation for OutputDestinationSettings
--
-- /See:/ 'outputDestinationSettings' smart constructor.
data OutputDestinationSettings = OutputDestinationSettings'
  { _odsURL ::
      !(Maybe Text),
    _odsUsername :: !(Maybe Text),
    _odsPasswordParam :: !(Maybe Text),
    _odsStreamName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputDestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'odsURL' - A URL specifying a destination
--
-- * 'odsUsername' - username for destination
--
-- * 'odsPasswordParam' - key used to extract the password from EC2 Parameter store
--
-- * 'odsStreamName' - Stream name for RTMP destinations (URLs of type rtmp://)
outputDestinationSettings ::
  OutputDestinationSettings
outputDestinationSettings =
  OutputDestinationSettings'
    { _odsURL = Nothing,
      _odsUsername = Nothing,
      _odsPasswordParam = Nothing,
      _odsStreamName = Nothing
    }

-- | A URL specifying a destination
odsURL :: Lens' OutputDestinationSettings (Maybe Text)
odsURL = lens _odsURL (\s a -> s {_odsURL = a})

-- | username for destination
odsUsername :: Lens' OutputDestinationSettings (Maybe Text)
odsUsername = lens _odsUsername (\s a -> s {_odsUsername = a})

-- | key used to extract the password from EC2 Parameter store
odsPasswordParam :: Lens' OutputDestinationSettings (Maybe Text)
odsPasswordParam = lens _odsPasswordParam (\s a -> s {_odsPasswordParam = a})

-- | Stream name for RTMP destinations (URLs of type rtmp://)
odsStreamName :: Lens' OutputDestinationSettings (Maybe Text)
odsStreamName = lens _odsStreamName (\s a -> s {_odsStreamName = a})

instance FromJSON OutputDestinationSettings where
  parseJSON =
    withObject
      "OutputDestinationSettings"
      ( \x ->
          OutputDestinationSettings'
            <$> (x .:? "url")
            <*> (x .:? "username")
            <*> (x .:? "passwordParam")
            <*> (x .:? "streamName")
      )

instance Hashable OutputDestinationSettings

instance NFData OutputDestinationSettings

instance ToJSON OutputDestinationSettings where
  toJSON OutputDestinationSettings' {..} =
    object
      ( catMaybes
          [ ("url" .=) <$> _odsURL,
            ("username" .=) <$> _odsUsername,
            ("passwordParam" .=) <$> _odsPasswordParam,
            ("streamName" .=) <$> _odsStreamName
          ]
      )
