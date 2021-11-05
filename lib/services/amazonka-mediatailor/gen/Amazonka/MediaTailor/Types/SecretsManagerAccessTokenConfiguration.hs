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
-- Module      : Amazonka.MediaTailor.Types.SecretsManagerAccessTokenConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.SecretsManagerAccessTokenConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | AWS Secrets Manager access token configuration parameters. For
-- information about Secrets Manager access token authentication, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/channel-assembly-access-configuration-access-token.html Working with AWS Secrets Manager access token authentication>.
--
-- /See:/ 'newSecretsManagerAccessTokenConfiguration' smart constructor.
data SecretsManagerAccessTokenConfiguration = SecretsManagerAccessTokenConfiguration'
  { -- | The name of the HTTP header used to supply the access token in requests
    -- to the source location.
    headerName :: Prelude.Maybe Prelude.Text,
    -- | The AWS Secrets Manager
    -- <https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_CreateSecret.html#SecretsManager-CreateSecret-request-SecretString.html SecretString>
    -- key associated with the access token. MediaTailor uses the key to look
    -- up SecretString key and value pair containing the access token.
    secretStringKey :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Secrets Manager secret that
    -- contains the access token.
    secretArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecretsManagerAccessTokenConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'headerName', 'secretsManagerAccessTokenConfiguration_headerName' - The name of the HTTP header used to supply the access token in requests
-- to the source location.
--
-- 'secretStringKey', 'secretsManagerAccessTokenConfiguration_secretStringKey' - The AWS Secrets Manager
-- <https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_CreateSecret.html#SecretsManager-CreateSecret-request-SecretString.html SecretString>
-- key associated with the access token. MediaTailor uses the key to look
-- up SecretString key and value pair containing the access token.
--
-- 'secretArn', 'secretsManagerAccessTokenConfiguration_secretArn' - The Amazon Resource Name (ARN) of the AWS Secrets Manager secret that
-- contains the access token.
newSecretsManagerAccessTokenConfiguration ::
  SecretsManagerAccessTokenConfiguration
newSecretsManagerAccessTokenConfiguration =
  SecretsManagerAccessTokenConfiguration'
    { headerName =
        Prelude.Nothing,
      secretStringKey = Prelude.Nothing,
      secretArn = Prelude.Nothing
    }

-- | The name of the HTTP header used to supply the access token in requests
-- to the source location.
secretsManagerAccessTokenConfiguration_headerName :: Lens.Lens' SecretsManagerAccessTokenConfiguration (Prelude.Maybe Prelude.Text)
secretsManagerAccessTokenConfiguration_headerName = Lens.lens (\SecretsManagerAccessTokenConfiguration' {headerName} -> headerName) (\s@SecretsManagerAccessTokenConfiguration' {} a -> s {headerName = a} :: SecretsManagerAccessTokenConfiguration)

-- | The AWS Secrets Manager
-- <https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_CreateSecret.html#SecretsManager-CreateSecret-request-SecretString.html SecretString>
-- key associated with the access token. MediaTailor uses the key to look
-- up SecretString key and value pair containing the access token.
secretsManagerAccessTokenConfiguration_secretStringKey :: Lens.Lens' SecretsManagerAccessTokenConfiguration (Prelude.Maybe Prelude.Text)
secretsManagerAccessTokenConfiguration_secretStringKey = Lens.lens (\SecretsManagerAccessTokenConfiguration' {secretStringKey} -> secretStringKey) (\s@SecretsManagerAccessTokenConfiguration' {} a -> s {secretStringKey = a} :: SecretsManagerAccessTokenConfiguration)

-- | The Amazon Resource Name (ARN) of the AWS Secrets Manager secret that
-- contains the access token.
secretsManagerAccessTokenConfiguration_secretArn :: Lens.Lens' SecretsManagerAccessTokenConfiguration (Prelude.Maybe Prelude.Text)
secretsManagerAccessTokenConfiguration_secretArn = Lens.lens (\SecretsManagerAccessTokenConfiguration' {secretArn} -> secretArn) (\s@SecretsManagerAccessTokenConfiguration' {} a -> s {secretArn = a} :: SecretsManagerAccessTokenConfiguration)

instance
  Core.FromJSON
    SecretsManagerAccessTokenConfiguration
  where
  parseJSON =
    Core.withObject
      "SecretsManagerAccessTokenConfiguration"
      ( \x ->
          SecretsManagerAccessTokenConfiguration'
            Prelude.<$> (x Core..:? "HeaderName")
            Prelude.<*> (x Core..:? "SecretStringKey")
            Prelude.<*> (x Core..:? "SecretArn")
      )

instance
  Prelude.Hashable
    SecretsManagerAccessTokenConfiguration

instance
  Prelude.NFData
    SecretsManagerAccessTokenConfiguration

instance
  Core.ToJSON
    SecretsManagerAccessTokenConfiguration
  where
  toJSON SecretsManagerAccessTokenConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("HeaderName" Core..=) Prelude.<$> headerName,
            ("SecretStringKey" Core..=)
              Prelude.<$> secretStringKey,
            ("SecretArn" Core..=) Prelude.<$> secretArn
          ]
      )
