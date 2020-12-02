{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.SpekeKeyProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.SpekeKeyProvider where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A configuration for accessing an external Secure Packager and Encoder Key Exchange (SPEKE) service that will provide encryption keys.
--
-- /See:/ 'spekeKeyProvider' smart constructor.
data SpekeKeyProvider = SpekeKeyProvider'
  { _skpCertificateARN ::
      !(Maybe Text),
    _skpResourceId :: !Text,
    _skpSystemIds :: ![Text],
    _skpURL :: !Text,
    _skpRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SpekeKeyProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'skpCertificateARN' - An Amazon Resource Name (ARN) of a Certificate Manager certificate that MediaPackage will use for enforcing secure end-to-end data transfer with the key provider service.
--
-- * 'skpResourceId' - The resource ID to include in key requests.
--
-- * 'skpSystemIds' - The system IDs to include in key requests.
--
-- * 'skpURL' - The URL of the external key provider service.
--
-- * 'skpRoleARN' - An Amazon Resource Name (ARN) of an IAM role that AWS Elemental MediaPackage will assume when accessing the key provider service.
spekeKeyProvider ::
  -- | 'skpResourceId'
  Text ->
  -- | 'skpURL'
  Text ->
  -- | 'skpRoleARN'
  Text ->
  SpekeKeyProvider
spekeKeyProvider pResourceId_ pURL_ pRoleARN_ =
  SpekeKeyProvider'
    { _skpCertificateARN = Nothing,
      _skpResourceId = pResourceId_,
      _skpSystemIds = mempty,
      _skpURL = pURL_,
      _skpRoleARN = pRoleARN_
    }

-- | An Amazon Resource Name (ARN) of a Certificate Manager certificate that MediaPackage will use for enforcing secure end-to-end data transfer with the key provider service.
skpCertificateARN :: Lens' SpekeKeyProvider (Maybe Text)
skpCertificateARN = lens _skpCertificateARN (\s a -> s {_skpCertificateARN = a})

-- | The resource ID to include in key requests.
skpResourceId :: Lens' SpekeKeyProvider Text
skpResourceId = lens _skpResourceId (\s a -> s {_skpResourceId = a})

-- | The system IDs to include in key requests.
skpSystemIds :: Lens' SpekeKeyProvider [Text]
skpSystemIds = lens _skpSystemIds (\s a -> s {_skpSystemIds = a}) . _Coerce

-- | The URL of the external key provider service.
skpURL :: Lens' SpekeKeyProvider Text
skpURL = lens _skpURL (\s a -> s {_skpURL = a})

-- | An Amazon Resource Name (ARN) of an IAM role that AWS Elemental MediaPackage will assume when accessing the key provider service.
skpRoleARN :: Lens' SpekeKeyProvider Text
skpRoleARN = lens _skpRoleARN (\s a -> s {_skpRoleARN = a})

instance FromJSON SpekeKeyProvider where
  parseJSON =
    withObject
      "SpekeKeyProvider"
      ( \x ->
          SpekeKeyProvider'
            <$> (x .:? "certificateArn")
            <*> (x .: "resourceId")
            <*> (x .:? "systemIds" .!= mempty)
            <*> (x .: "url")
            <*> (x .: "roleArn")
      )

instance Hashable SpekeKeyProvider

instance NFData SpekeKeyProvider

instance ToJSON SpekeKeyProvider where
  toJSON SpekeKeyProvider' {..} =
    object
      ( catMaybes
          [ ("certificateArn" .=) <$> _skpCertificateARN,
            Just ("resourceId" .= _skpResourceId),
            Just ("systemIds" .= _skpSystemIds),
            Just ("url" .= _skpURL),
            Just ("roleArn" .= _skpRoleARN)
          ]
      )
