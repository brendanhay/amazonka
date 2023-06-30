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
-- Module      : Amazonka.SecurityHub.Types.AwsCodeBuildProjectEnvironmentRegistryCredential
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCodeBuildProjectEnvironmentRegistryCredential where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The credentials for access to a private registry.
--
-- /See:/ 'newAwsCodeBuildProjectEnvironmentRegistryCredential' smart constructor.
data AwsCodeBuildProjectEnvironmentRegistryCredential = AwsCodeBuildProjectEnvironmentRegistryCredential'
  { -- | The ARN or name of credentials created using Secrets Manager.
    --
    -- The credential can use the name of the credentials only if they exist in
    -- your current Amazon Web Services Region.
    credential :: Prelude.Maybe Prelude.Text,
    -- | The service that created the credentials to access a private Docker
    -- registry.
    --
    -- The valid value,@ SECRETS_MANAGER@, is for Secrets Manager.
    credentialProvider :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCodeBuildProjectEnvironmentRegistryCredential' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'credential', 'awsCodeBuildProjectEnvironmentRegistryCredential_credential' - The ARN or name of credentials created using Secrets Manager.
--
-- The credential can use the name of the credentials only if they exist in
-- your current Amazon Web Services Region.
--
-- 'credentialProvider', 'awsCodeBuildProjectEnvironmentRegistryCredential_credentialProvider' - The service that created the credentials to access a private Docker
-- registry.
--
-- The valid value,@ SECRETS_MANAGER@, is for Secrets Manager.
newAwsCodeBuildProjectEnvironmentRegistryCredential ::
  AwsCodeBuildProjectEnvironmentRegistryCredential
newAwsCodeBuildProjectEnvironmentRegistryCredential =
  AwsCodeBuildProjectEnvironmentRegistryCredential'
    { credential =
        Prelude.Nothing,
      credentialProvider =
        Prelude.Nothing
    }

-- | The ARN or name of credentials created using Secrets Manager.
--
-- The credential can use the name of the credentials only if they exist in
-- your current Amazon Web Services Region.
awsCodeBuildProjectEnvironmentRegistryCredential_credential :: Lens.Lens' AwsCodeBuildProjectEnvironmentRegistryCredential (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectEnvironmentRegistryCredential_credential = Lens.lens (\AwsCodeBuildProjectEnvironmentRegistryCredential' {credential} -> credential) (\s@AwsCodeBuildProjectEnvironmentRegistryCredential' {} a -> s {credential = a} :: AwsCodeBuildProjectEnvironmentRegistryCredential)

-- | The service that created the credentials to access a private Docker
-- registry.
--
-- The valid value,@ SECRETS_MANAGER@, is for Secrets Manager.
awsCodeBuildProjectEnvironmentRegistryCredential_credentialProvider :: Lens.Lens' AwsCodeBuildProjectEnvironmentRegistryCredential (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectEnvironmentRegistryCredential_credentialProvider = Lens.lens (\AwsCodeBuildProjectEnvironmentRegistryCredential' {credentialProvider} -> credentialProvider) (\s@AwsCodeBuildProjectEnvironmentRegistryCredential' {} a -> s {credentialProvider = a} :: AwsCodeBuildProjectEnvironmentRegistryCredential)

instance
  Data.FromJSON
    AwsCodeBuildProjectEnvironmentRegistryCredential
  where
  parseJSON =
    Data.withObject
      "AwsCodeBuildProjectEnvironmentRegistryCredential"
      ( \x ->
          AwsCodeBuildProjectEnvironmentRegistryCredential'
            Prelude.<$> (x Data..:? "Credential")
            Prelude.<*> (x Data..:? "CredentialProvider")
      )

instance
  Prelude.Hashable
    AwsCodeBuildProjectEnvironmentRegistryCredential
  where
  hashWithSalt
    _salt
    AwsCodeBuildProjectEnvironmentRegistryCredential' {..} =
      _salt
        `Prelude.hashWithSalt` credential
        `Prelude.hashWithSalt` credentialProvider

instance
  Prelude.NFData
    AwsCodeBuildProjectEnvironmentRegistryCredential
  where
  rnf
    AwsCodeBuildProjectEnvironmentRegistryCredential' {..} =
      Prelude.rnf credential
        `Prelude.seq` Prelude.rnf credentialProvider

instance
  Data.ToJSON
    AwsCodeBuildProjectEnvironmentRegistryCredential
  where
  toJSON
    AwsCodeBuildProjectEnvironmentRegistryCredential' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Credential" Data..=) Prelude.<$> credential,
              ("CredentialProvider" Data..=)
                Prelude.<$> credentialProvider
            ]
        )
