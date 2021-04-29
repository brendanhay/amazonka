{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.RegistryCredential
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.RegistryCredential where

import Network.AWS.CodeBuild.Types.CredentialProviderType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about credentials that provide access to a private Docker
-- registry. When this is set:
--
-- -   @imagePullCredentialsType@ must be set to @SERVICE_ROLE@.
--
-- -   images cannot be curated or an Amazon ECR image.
--
-- For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-private-registry.html Private Registry with AWS Secrets Manager Sample for AWS CodeBuild>.
--
-- /See:/ 'newRegistryCredential' smart constructor.
data RegistryCredential = RegistryCredential'
  { -- | The Amazon Resource Name (ARN) or name of credentials created using AWS
    -- Secrets Manager.
    --
    -- The @credential@ can use the name of the credentials only if they exist
    -- in your current AWS Region.
    credential :: Prelude.Text,
    -- | The service that created the credentials to access a private Docker
    -- registry. The valid value, SECRETS_MANAGER, is for AWS Secrets Manager.
    credentialProvider :: CredentialProviderType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegistryCredential' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'credential', 'registryCredential_credential' - The Amazon Resource Name (ARN) or name of credentials created using AWS
-- Secrets Manager.
--
-- The @credential@ can use the name of the credentials only if they exist
-- in your current AWS Region.
--
-- 'credentialProvider', 'registryCredential_credentialProvider' - The service that created the credentials to access a private Docker
-- registry. The valid value, SECRETS_MANAGER, is for AWS Secrets Manager.
newRegistryCredential ::
  -- | 'credential'
  Prelude.Text ->
  -- | 'credentialProvider'
  CredentialProviderType ->
  RegistryCredential
newRegistryCredential
  pCredential_
  pCredentialProvider_ =
    RegistryCredential'
      { credential = pCredential_,
        credentialProvider = pCredentialProvider_
      }

-- | The Amazon Resource Name (ARN) or name of credentials created using AWS
-- Secrets Manager.
--
-- The @credential@ can use the name of the credentials only if they exist
-- in your current AWS Region.
registryCredential_credential :: Lens.Lens' RegistryCredential Prelude.Text
registryCredential_credential = Lens.lens (\RegistryCredential' {credential} -> credential) (\s@RegistryCredential' {} a -> s {credential = a} :: RegistryCredential)

-- | The service that created the credentials to access a private Docker
-- registry. The valid value, SECRETS_MANAGER, is for AWS Secrets Manager.
registryCredential_credentialProvider :: Lens.Lens' RegistryCredential CredentialProviderType
registryCredential_credentialProvider = Lens.lens (\RegistryCredential' {credentialProvider} -> credentialProvider) (\s@RegistryCredential' {} a -> s {credentialProvider = a} :: RegistryCredential)

instance Prelude.FromJSON RegistryCredential where
  parseJSON =
    Prelude.withObject
      "RegistryCredential"
      ( \x ->
          RegistryCredential'
            Prelude.<$> (x Prelude..: "credential")
            Prelude.<*> (x Prelude..: "credentialProvider")
      )

instance Prelude.Hashable RegistryCredential

instance Prelude.NFData RegistryCredential

instance Prelude.ToJSON RegistryCredential where
  toJSON RegistryCredential' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("credential" Prelude..= credential),
            Prelude.Just
              ( "credentialProvider"
                  Prelude..= credentialProvider
              )
          ]
      )
