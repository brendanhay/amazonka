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
-- Module      : Network.AWS.SecurityHub.Types.AwsSecretsManagerSecretRotationRules
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsSecretsManagerSecretRotationRules where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Defines the rotation schedule for the secret.
--
-- /See:/ 'newAwsSecretsManagerSecretRotationRules' smart constructor.
data AwsSecretsManagerSecretRotationRules = AwsSecretsManagerSecretRotationRules'
  { -- | The number of days after the previous rotation to rotate the secret.
    automaticallyAfterDays :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsSecretsManagerSecretRotationRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automaticallyAfterDays', 'awsSecretsManagerSecretRotationRules_automaticallyAfterDays' - The number of days after the previous rotation to rotate the secret.
newAwsSecretsManagerSecretRotationRules ::
  AwsSecretsManagerSecretRotationRules
newAwsSecretsManagerSecretRotationRules =
  AwsSecretsManagerSecretRotationRules'
    { automaticallyAfterDays =
        Prelude.Nothing
    }

-- | The number of days after the previous rotation to rotate the secret.
awsSecretsManagerSecretRotationRules_automaticallyAfterDays :: Lens.Lens' AwsSecretsManagerSecretRotationRules (Prelude.Maybe Prelude.Int)
awsSecretsManagerSecretRotationRules_automaticallyAfterDays = Lens.lens (\AwsSecretsManagerSecretRotationRules' {automaticallyAfterDays} -> automaticallyAfterDays) (\s@AwsSecretsManagerSecretRotationRules' {} a -> s {automaticallyAfterDays = a} :: AwsSecretsManagerSecretRotationRules)

instance
  Core.FromJSON
    AwsSecretsManagerSecretRotationRules
  where
  parseJSON =
    Core.withObject
      "AwsSecretsManagerSecretRotationRules"
      ( \x ->
          AwsSecretsManagerSecretRotationRules'
            Prelude.<$> (x Core..:? "AutomaticallyAfterDays")
      )

instance
  Prelude.Hashable
    AwsSecretsManagerSecretRotationRules

instance
  Prelude.NFData
    AwsSecretsManagerSecretRotationRules

instance
  Core.ToJSON
    AwsSecretsManagerSecretRotationRules
  where
  toJSON AwsSecretsManagerSecretRotationRules' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AutomaticallyAfterDays" Core..=)
              Prelude.<$> automaticallyAfterDays
          ]
      )
