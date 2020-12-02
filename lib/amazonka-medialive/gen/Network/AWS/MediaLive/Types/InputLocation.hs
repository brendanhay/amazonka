{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputLocation where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Input Location
--
-- /See:/ 'inputLocation' smart constructor.
data InputLocation = InputLocation'
  { _ilUsername :: !(Maybe Text),
    _ilPasswordParam :: !(Maybe Text),
    _ilURI :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ilUsername' - Documentation update needed
--
-- * 'ilPasswordParam' - key used to extract the password from EC2 Parameter store
--
-- * 'ilURI' - Uniform Resource Identifier - This should be a path to a file accessible to the Live system (eg. a http:// URI) depending on the output type. For example, a RTMP destination should have a uri simliar to: "rtmp://fmsserver/live".
inputLocation ::
  -- | 'ilURI'
  Text ->
  InputLocation
inputLocation pURI_ =
  InputLocation'
    { _ilUsername = Nothing,
      _ilPasswordParam = Nothing,
      _ilURI = pURI_
    }

-- | Documentation update needed
ilUsername :: Lens' InputLocation (Maybe Text)
ilUsername = lens _ilUsername (\s a -> s {_ilUsername = a})

-- | key used to extract the password from EC2 Parameter store
ilPasswordParam :: Lens' InputLocation (Maybe Text)
ilPasswordParam = lens _ilPasswordParam (\s a -> s {_ilPasswordParam = a})

-- | Uniform Resource Identifier - This should be a path to a file accessible to the Live system (eg. a http:// URI) depending on the output type. For example, a RTMP destination should have a uri simliar to: "rtmp://fmsserver/live".
ilURI :: Lens' InputLocation Text
ilURI = lens _ilURI (\s a -> s {_ilURI = a})

instance FromJSON InputLocation where
  parseJSON =
    withObject
      "InputLocation"
      ( \x ->
          InputLocation'
            <$> (x .:? "username") <*> (x .:? "passwordParam") <*> (x .: "uri")
      )

instance Hashable InputLocation

instance NFData InputLocation

instance ToJSON InputLocation where
  toJSON InputLocation' {..} =
    object
      ( catMaybes
          [ ("username" .=) <$> _ilUsername,
            ("passwordParam" .=) <$> _ilPasswordParam,
            Just ("uri" .= _ilURI)
          ]
      )
