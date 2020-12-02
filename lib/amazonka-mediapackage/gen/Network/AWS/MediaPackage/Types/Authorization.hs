{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.Authorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.Authorization where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | CDN Authorization credentials
--
-- /See:/ 'authorization' smart constructor.
data Authorization = Authorization'
  { _aSecretsRoleARN :: !Text,
    _aCdnIdentifierSecret :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Authorization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aSecretsRoleARN' - The Amazon Resource Name (ARN) for the IAM role that allows MediaPackage to communicate with AWS Secrets Manager.
--
-- * 'aCdnIdentifierSecret' - The Amazon Resource Name (ARN) for the secret in Secrets Manager that your Content Distribution Network (CDN) uses for authorization to access your endpoint.
authorization ::
  -- | 'aSecretsRoleARN'
  Text ->
  -- | 'aCdnIdentifierSecret'
  Text ->
  Authorization
authorization pSecretsRoleARN_ pCdnIdentifierSecret_ =
  Authorization'
    { _aSecretsRoleARN = pSecretsRoleARN_,
      _aCdnIdentifierSecret = pCdnIdentifierSecret_
    }

-- | The Amazon Resource Name (ARN) for the IAM role that allows MediaPackage to communicate with AWS Secrets Manager.
aSecretsRoleARN :: Lens' Authorization Text
aSecretsRoleARN = lens _aSecretsRoleARN (\s a -> s {_aSecretsRoleARN = a})

-- | The Amazon Resource Name (ARN) for the secret in Secrets Manager that your Content Distribution Network (CDN) uses for authorization to access your endpoint.
aCdnIdentifierSecret :: Lens' Authorization Text
aCdnIdentifierSecret = lens _aCdnIdentifierSecret (\s a -> s {_aCdnIdentifierSecret = a})

instance FromJSON Authorization where
  parseJSON =
    withObject
      "Authorization"
      ( \x ->
          Authorization'
            <$> (x .: "secretsRoleArn") <*> (x .: "cdnIdentifierSecret")
      )

instance Hashable Authorization

instance NFData Authorization

instance ToJSON Authorization where
  toJSON Authorization' {..} =
    object
      ( catMaybes
          [ Just ("secretsRoleArn" .= _aSecretsRoleARN),
            Just ("cdnIdentifierSecret" .= _aCdnIdentifierSecret)
          ]
      )
