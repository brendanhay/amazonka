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
-- Module      : Network.AWS.SecurityHub.Types.AwsSecretsManagerSecretDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsSecretsManagerSecretDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecurityHub.Types.AwsSecretsManagerSecretRotationRules

-- | Details about an Secrets Manager secret.
--
-- /See:/ 'newAwsSecretsManagerSecretDetails' smart constructor.
data AwsSecretsManagerSecretDetails = AwsSecretsManagerSecretDetails'
  { -- | Defines the rotation schedule for the secret.
    rotationRules :: Prelude.Maybe AwsSecretsManagerSecretRotationRules,
    -- | Whether rotation is enabled.
    rotationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The ARN, Key ID, or alias of the KMS key used to encrypt the
    -- @SecretString@ or @SecretBinary@ values for versions of this secret.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the secret.
    name :: Prelude.Maybe Prelude.Text,
    -- | Whether the secret is deleted.
    deleted :: Prelude.Maybe Prelude.Bool,
    -- | The user-provided description of the secret.
    description :: Prelude.Maybe Prelude.Text,
    -- | Whether the rotation occurred within the specified rotation frequency.
    rotationOccurredWithinFrequency :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the Lambda function that rotates the secret.
    rotationLambdaArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsSecretsManagerSecretDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rotationRules', 'awsSecretsManagerSecretDetails_rotationRules' - Defines the rotation schedule for the secret.
--
-- 'rotationEnabled', 'awsSecretsManagerSecretDetails_rotationEnabled' - Whether rotation is enabled.
--
-- 'kmsKeyId', 'awsSecretsManagerSecretDetails_kmsKeyId' - The ARN, Key ID, or alias of the KMS key used to encrypt the
-- @SecretString@ or @SecretBinary@ values for versions of this secret.
--
-- 'name', 'awsSecretsManagerSecretDetails_name' - The name of the secret.
--
-- 'deleted', 'awsSecretsManagerSecretDetails_deleted' - Whether the secret is deleted.
--
-- 'description', 'awsSecretsManagerSecretDetails_description' - The user-provided description of the secret.
--
-- 'rotationOccurredWithinFrequency', 'awsSecretsManagerSecretDetails_rotationOccurredWithinFrequency' - Whether the rotation occurred within the specified rotation frequency.
--
-- 'rotationLambdaArn', 'awsSecretsManagerSecretDetails_rotationLambdaArn' - The ARN of the Lambda function that rotates the secret.
newAwsSecretsManagerSecretDetails ::
  AwsSecretsManagerSecretDetails
newAwsSecretsManagerSecretDetails =
  AwsSecretsManagerSecretDetails'
    { rotationRules =
        Prelude.Nothing,
      rotationEnabled = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      name = Prelude.Nothing,
      deleted = Prelude.Nothing,
      description = Prelude.Nothing,
      rotationOccurredWithinFrequency =
        Prelude.Nothing,
      rotationLambdaArn = Prelude.Nothing
    }

-- | Defines the rotation schedule for the secret.
awsSecretsManagerSecretDetails_rotationRules :: Lens.Lens' AwsSecretsManagerSecretDetails (Prelude.Maybe AwsSecretsManagerSecretRotationRules)
awsSecretsManagerSecretDetails_rotationRules = Lens.lens (\AwsSecretsManagerSecretDetails' {rotationRules} -> rotationRules) (\s@AwsSecretsManagerSecretDetails' {} a -> s {rotationRules = a} :: AwsSecretsManagerSecretDetails)

-- | Whether rotation is enabled.
awsSecretsManagerSecretDetails_rotationEnabled :: Lens.Lens' AwsSecretsManagerSecretDetails (Prelude.Maybe Prelude.Bool)
awsSecretsManagerSecretDetails_rotationEnabled = Lens.lens (\AwsSecretsManagerSecretDetails' {rotationEnabled} -> rotationEnabled) (\s@AwsSecretsManagerSecretDetails' {} a -> s {rotationEnabled = a} :: AwsSecretsManagerSecretDetails)

-- | The ARN, Key ID, or alias of the KMS key used to encrypt the
-- @SecretString@ or @SecretBinary@ values for versions of this secret.
awsSecretsManagerSecretDetails_kmsKeyId :: Lens.Lens' AwsSecretsManagerSecretDetails (Prelude.Maybe Prelude.Text)
awsSecretsManagerSecretDetails_kmsKeyId = Lens.lens (\AwsSecretsManagerSecretDetails' {kmsKeyId} -> kmsKeyId) (\s@AwsSecretsManagerSecretDetails' {} a -> s {kmsKeyId = a} :: AwsSecretsManagerSecretDetails)

-- | The name of the secret.
awsSecretsManagerSecretDetails_name :: Lens.Lens' AwsSecretsManagerSecretDetails (Prelude.Maybe Prelude.Text)
awsSecretsManagerSecretDetails_name = Lens.lens (\AwsSecretsManagerSecretDetails' {name} -> name) (\s@AwsSecretsManagerSecretDetails' {} a -> s {name = a} :: AwsSecretsManagerSecretDetails)

-- | Whether the secret is deleted.
awsSecretsManagerSecretDetails_deleted :: Lens.Lens' AwsSecretsManagerSecretDetails (Prelude.Maybe Prelude.Bool)
awsSecretsManagerSecretDetails_deleted = Lens.lens (\AwsSecretsManagerSecretDetails' {deleted} -> deleted) (\s@AwsSecretsManagerSecretDetails' {} a -> s {deleted = a} :: AwsSecretsManagerSecretDetails)

-- | The user-provided description of the secret.
awsSecretsManagerSecretDetails_description :: Lens.Lens' AwsSecretsManagerSecretDetails (Prelude.Maybe Prelude.Text)
awsSecretsManagerSecretDetails_description = Lens.lens (\AwsSecretsManagerSecretDetails' {description} -> description) (\s@AwsSecretsManagerSecretDetails' {} a -> s {description = a} :: AwsSecretsManagerSecretDetails)

-- | Whether the rotation occurred within the specified rotation frequency.
awsSecretsManagerSecretDetails_rotationOccurredWithinFrequency :: Lens.Lens' AwsSecretsManagerSecretDetails (Prelude.Maybe Prelude.Bool)
awsSecretsManagerSecretDetails_rotationOccurredWithinFrequency = Lens.lens (\AwsSecretsManagerSecretDetails' {rotationOccurredWithinFrequency} -> rotationOccurredWithinFrequency) (\s@AwsSecretsManagerSecretDetails' {} a -> s {rotationOccurredWithinFrequency = a} :: AwsSecretsManagerSecretDetails)

-- | The ARN of the Lambda function that rotates the secret.
awsSecretsManagerSecretDetails_rotationLambdaArn :: Lens.Lens' AwsSecretsManagerSecretDetails (Prelude.Maybe Prelude.Text)
awsSecretsManagerSecretDetails_rotationLambdaArn = Lens.lens (\AwsSecretsManagerSecretDetails' {rotationLambdaArn} -> rotationLambdaArn) (\s@AwsSecretsManagerSecretDetails' {} a -> s {rotationLambdaArn = a} :: AwsSecretsManagerSecretDetails)

instance Core.FromJSON AwsSecretsManagerSecretDetails where
  parseJSON =
    Core.withObject
      "AwsSecretsManagerSecretDetails"
      ( \x ->
          AwsSecretsManagerSecretDetails'
            Prelude.<$> (x Core..:? "RotationRules")
            Prelude.<*> (x Core..:? "RotationEnabled")
            Prelude.<*> (x Core..:? "KmsKeyId")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Deleted")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "RotationOccurredWithinFrequency")
            Prelude.<*> (x Core..:? "RotationLambdaArn")
      )

instance
  Prelude.Hashable
    AwsSecretsManagerSecretDetails

instance
  Prelude.NFData
    AwsSecretsManagerSecretDetails

instance Core.ToJSON AwsSecretsManagerSecretDetails where
  toJSON AwsSecretsManagerSecretDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RotationRules" Core..=) Prelude.<$> rotationRules,
            ("RotationEnabled" Core..=)
              Prelude.<$> rotationEnabled,
            ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("Name" Core..=) Prelude.<$> name,
            ("Deleted" Core..=) Prelude.<$> deleted,
            ("Description" Core..=) Prelude.<$> description,
            ("RotationOccurredWithinFrequency" Core..=)
              Prelude.<$> rotationOccurredWithinFrequency,
            ("RotationLambdaArn" Core..=)
              Prelude.<$> rotationLambdaArn
          ]
      )
