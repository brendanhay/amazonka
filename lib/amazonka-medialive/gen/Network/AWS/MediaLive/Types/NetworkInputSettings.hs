{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.NetworkInputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.NetworkInputSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.HlsInputSettings
import Network.AWS.MediaLive.Types.NetworkInputServerValidation
import Network.AWS.Prelude

-- | Network source to transcode. Must be accessible to the Elemental Live node that is running the live event through a network connection.
--
-- /See:/ 'networkInputSettings' smart constructor.
data NetworkInputSettings = NetworkInputSettings'
  { _nisHlsInputSettings ::
      !(Maybe HlsInputSettings),
    _nisServerValidation ::
      !(Maybe NetworkInputServerValidation)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NetworkInputSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nisHlsInputSettings' - Specifies HLS input settings when the uri is for a HLS manifest.
--
-- * 'nisServerValidation' - Check HTTPS server certificates. When set to checkCryptographyOnly, cryptography in the certificate will be checked, but not the server's name. Certain subdomains (notably S3 buckets that use dots in the bucket name) do not strictly match the corresponding certificate's wildcard pattern and would otherwise cause the event to error. This setting is ignored for protocols that do not use https.
networkInputSettings ::
  NetworkInputSettings
networkInputSettings =
  NetworkInputSettings'
    { _nisHlsInputSettings = Nothing,
      _nisServerValidation = Nothing
    }

-- | Specifies HLS input settings when the uri is for a HLS manifest.
nisHlsInputSettings :: Lens' NetworkInputSettings (Maybe HlsInputSettings)
nisHlsInputSettings = lens _nisHlsInputSettings (\s a -> s {_nisHlsInputSettings = a})

-- | Check HTTPS server certificates. When set to checkCryptographyOnly, cryptography in the certificate will be checked, but not the server's name. Certain subdomains (notably S3 buckets that use dots in the bucket name) do not strictly match the corresponding certificate's wildcard pattern and would otherwise cause the event to error. This setting is ignored for protocols that do not use https.
nisServerValidation :: Lens' NetworkInputSettings (Maybe NetworkInputServerValidation)
nisServerValidation = lens _nisServerValidation (\s a -> s {_nisServerValidation = a})

instance FromJSON NetworkInputSettings where
  parseJSON =
    withObject
      "NetworkInputSettings"
      ( \x ->
          NetworkInputSettings'
            <$> (x .:? "hlsInputSettings") <*> (x .:? "serverValidation")
      )

instance Hashable NetworkInputSettings

instance NFData NetworkInputSettings

instance ToJSON NetworkInputSettings where
  toJSON NetworkInputSettings' {..} =
    object
      ( catMaybes
          [ ("hlsInputSettings" .=) <$> _nisHlsInputSettings,
            ("serverValidation" .=) <$> _nisServerValidation
          ]
      )
