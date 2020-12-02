{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.SigningProfileParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.SigningProfileParameter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the code-signing profile.
--
--
--
-- /See:/ 'signingProfileParameter' smart constructor.
data SigningProfileParameter = SigningProfileParameter'
  { _sppPlatform ::
      !(Maybe Text),
    _sppCertificateARN :: !(Maybe Text),
    _sppCertificatePathOnDevice ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SigningProfileParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sppPlatform' - The hardware platform of your device.
--
-- * 'sppCertificateARN' - Certificate ARN.
--
-- * 'sppCertificatePathOnDevice' - The location of the code-signing certificate on your device.
signingProfileParameter ::
  SigningProfileParameter
signingProfileParameter =
  SigningProfileParameter'
    { _sppPlatform = Nothing,
      _sppCertificateARN = Nothing,
      _sppCertificatePathOnDevice = Nothing
    }

-- | The hardware platform of your device.
sppPlatform :: Lens' SigningProfileParameter (Maybe Text)
sppPlatform = lens _sppPlatform (\s a -> s {_sppPlatform = a})

-- | Certificate ARN.
sppCertificateARN :: Lens' SigningProfileParameter (Maybe Text)
sppCertificateARN = lens _sppCertificateARN (\s a -> s {_sppCertificateARN = a})

-- | The location of the code-signing certificate on your device.
sppCertificatePathOnDevice :: Lens' SigningProfileParameter (Maybe Text)
sppCertificatePathOnDevice = lens _sppCertificatePathOnDevice (\s a -> s {_sppCertificatePathOnDevice = a})

instance FromJSON SigningProfileParameter where
  parseJSON =
    withObject
      "SigningProfileParameter"
      ( \x ->
          SigningProfileParameter'
            <$> (x .:? "platform")
            <*> (x .:? "certificateArn")
            <*> (x .:? "certificatePathOnDevice")
      )

instance Hashable SigningProfileParameter

instance NFData SigningProfileParameter

instance ToJSON SigningProfileParameter where
  toJSON SigningProfileParameter' {..} =
    object
      ( catMaybes
          [ ("platform" .=) <$> _sppPlatform,
            ("certificateArn" .=) <$> _sppCertificateARN,
            ("certificatePathOnDevice" .=) <$> _sppCertificatePathOnDevice
          ]
      )
