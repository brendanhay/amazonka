{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.RegistryCredential
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.RegistryCredential where

import Network.AWS.CodeBuild.Types.CredentialProviderType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about credentials that provide access to a private Docker registry. When this is set:
--
--
--     * @imagePullCredentialsType@ must be set to @SERVICE_ROLE@ .
--
--     * images cannot be curated or an Amazon ECR image.
--
--
--
-- For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-private-registry.html Private Registry with AWS Secrets Manager Sample for AWS CodeBuild> .
--
--
-- /See:/ 'registryCredential' smart constructor.
data RegistryCredential = RegistryCredential'
  { _rcCredential ::
      !Text,
    _rcCredentialProvider :: !CredentialProviderType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegistryCredential' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcCredential' - The Amazon Resource Name (ARN) or name of credentials created using AWS Secrets Manager.
--
-- * 'rcCredentialProvider' - The service that created the credentials to access a private Docker registry. The valid value, SECRETS_MANAGER, is for AWS Secrets Manager.
registryCredential ::
  -- | 'rcCredential'
  Text ->
  -- | 'rcCredentialProvider'
  CredentialProviderType ->
  RegistryCredential
registryCredential pCredential_ pCredentialProvider_ =
  RegistryCredential'
    { _rcCredential = pCredential_,
      _rcCredentialProvider = pCredentialProvider_
    }

-- | The Amazon Resource Name (ARN) or name of credentials created using AWS Secrets Manager.
rcCredential :: Lens' RegistryCredential Text
rcCredential = lens _rcCredential (\s a -> s {_rcCredential = a})

-- | The service that created the credentials to access a private Docker registry. The valid value, SECRETS_MANAGER, is for AWS Secrets Manager.
rcCredentialProvider :: Lens' RegistryCredential CredentialProviderType
rcCredentialProvider = lens _rcCredentialProvider (\s a -> s {_rcCredentialProvider = a})

instance FromJSON RegistryCredential where
  parseJSON =
    withObject
      "RegistryCredential"
      ( \x ->
          RegistryCredential'
            <$> (x .: "credential") <*> (x .: "credentialProvider")
      )

instance Hashable RegistryCredential

instance NFData RegistryCredential

instance ToJSON RegistryCredential where
  toJSON RegistryCredential' {..} =
    object
      ( catMaybes
          [ Just ("credential" .= _rcCredential),
            Just ("credentialProvider" .= _rcCredentialProvider)
          ]
      )
