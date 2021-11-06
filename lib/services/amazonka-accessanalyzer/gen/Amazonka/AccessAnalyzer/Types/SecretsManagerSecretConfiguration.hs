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
-- Module      : Amazonka.AccessAnalyzer.Types.SecretsManagerSecretConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.SecretsManagerSecretConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration for a Secrets Manager secret. For more information,
-- see
-- <https://docs.aws.amazon.com/secretsmanager/latest/apireference/API_CreateSecret.html CreateSecret>.
--
-- You can propose a configuration for a new secret or an existing secret
-- that you own by specifying the secret policy and optional KMS encryption
-- key. If the configuration is for an existing secret and you do not
-- specify the secret policy, the access preview uses the existing policy
-- for the secret. If the access preview is for a new resource and you do
-- not specify the policy, the access preview assumes a secret without a
-- policy. To propose deletion of an existing policy, you can specify an
-- empty string. If the proposed configuration is for a new secret and you
-- do not specify the KMS key ID, the access preview uses the default CMK
-- of the Amazon Web Services account. If you specify an empty string for
-- the KMS key ID, the access preview uses the default CMK of the Amazon
-- Web Services account. For more information about secret policy limits,
-- see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_limits.html Quotas for Secrets Manager.>.
--
-- /See:/ 'newSecretsManagerSecretConfiguration' smart constructor.
data SecretsManagerSecretConfiguration = SecretsManagerSecretConfiguration'
  { -- | The proposed ARN, key ID, or alias of the KMS customer master key (CMK).
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The proposed resource policy defining who can access or manage the
    -- secret.
    secretPolicy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecretsManagerSecretConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'secretsManagerSecretConfiguration_kmsKeyId' - The proposed ARN, key ID, or alias of the KMS customer master key (CMK).
--
-- 'secretPolicy', 'secretsManagerSecretConfiguration_secretPolicy' - The proposed resource policy defining who can access or manage the
-- secret.
newSecretsManagerSecretConfiguration ::
  SecretsManagerSecretConfiguration
newSecretsManagerSecretConfiguration =
  SecretsManagerSecretConfiguration'
    { kmsKeyId =
        Prelude.Nothing,
      secretPolicy = Prelude.Nothing
    }

-- | The proposed ARN, key ID, or alias of the KMS customer master key (CMK).
secretsManagerSecretConfiguration_kmsKeyId :: Lens.Lens' SecretsManagerSecretConfiguration (Prelude.Maybe Prelude.Text)
secretsManagerSecretConfiguration_kmsKeyId = Lens.lens (\SecretsManagerSecretConfiguration' {kmsKeyId} -> kmsKeyId) (\s@SecretsManagerSecretConfiguration' {} a -> s {kmsKeyId = a} :: SecretsManagerSecretConfiguration)

-- | The proposed resource policy defining who can access or manage the
-- secret.
secretsManagerSecretConfiguration_secretPolicy :: Lens.Lens' SecretsManagerSecretConfiguration (Prelude.Maybe Prelude.Text)
secretsManagerSecretConfiguration_secretPolicy = Lens.lens (\SecretsManagerSecretConfiguration' {secretPolicy} -> secretPolicy) (\s@SecretsManagerSecretConfiguration' {} a -> s {secretPolicy = a} :: SecretsManagerSecretConfiguration)

instance
  Core.FromJSON
    SecretsManagerSecretConfiguration
  where
  parseJSON =
    Core.withObject
      "SecretsManagerSecretConfiguration"
      ( \x ->
          SecretsManagerSecretConfiguration'
            Prelude.<$> (x Core..:? "kmsKeyId")
            Prelude.<*> (x Core..:? "secretPolicy")
      )

instance
  Prelude.Hashable
    SecretsManagerSecretConfiguration

instance
  Prelude.NFData
    SecretsManagerSecretConfiguration

instance
  Core.ToJSON
    SecretsManagerSecretConfiguration
  where
  toJSON SecretsManagerSecretConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("kmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("secretPolicy" Core..=) Prelude.<$> secretPolicy
          ]
      )
