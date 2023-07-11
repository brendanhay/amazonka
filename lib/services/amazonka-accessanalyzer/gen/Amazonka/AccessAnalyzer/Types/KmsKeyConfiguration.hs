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
-- Module      : Amazonka.AccessAnalyzer.Types.KmsKeyConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.KmsKeyConfiguration where

import Amazonka.AccessAnalyzer.Types.KmsGrantConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Proposed access control configuration for a KMS key. You can propose a
-- configuration for a new KMS key or an existing KMS key that you own by
-- specifying the key policy and KMS grant configuration. If the
-- configuration is for an existing key and you do not specify the key
-- policy, the access preview uses the existing policy for the key. If the
-- access preview is for a new resource and you do not specify the key
-- policy, then the access preview uses the default key policy. The
-- proposed key policy cannot be an empty string. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default Default key policy>.
-- For more information about key policy limits, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/resource-limits.html Resource quotas>.
--
-- /See:/ 'newKmsKeyConfiguration' smart constructor.
data KmsKeyConfiguration = KmsKeyConfiguration'
  { -- | A list of proposed grant configurations for the KMS key. If the proposed
    -- grant configuration is for an existing key, the access preview uses the
    -- proposed list of grant configurations in place of the existing grants.
    -- Otherwise, the access preview uses the existing grants for the key.
    grants :: Prelude.Maybe [KmsGrantConfiguration],
    -- | Resource policy configuration for the KMS key. The only valid value for
    -- the name of the key policy is @default@. For more information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default Default key policy>.
    keyPolicies :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KmsKeyConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grants', 'kmsKeyConfiguration_grants' - A list of proposed grant configurations for the KMS key. If the proposed
-- grant configuration is for an existing key, the access preview uses the
-- proposed list of grant configurations in place of the existing grants.
-- Otherwise, the access preview uses the existing grants for the key.
--
-- 'keyPolicies', 'kmsKeyConfiguration_keyPolicies' - Resource policy configuration for the KMS key. The only valid value for
-- the name of the key policy is @default@. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default Default key policy>.
newKmsKeyConfiguration ::
  KmsKeyConfiguration
newKmsKeyConfiguration =
  KmsKeyConfiguration'
    { grants = Prelude.Nothing,
      keyPolicies = Prelude.Nothing
    }

-- | A list of proposed grant configurations for the KMS key. If the proposed
-- grant configuration is for an existing key, the access preview uses the
-- proposed list of grant configurations in place of the existing grants.
-- Otherwise, the access preview uses the existing grants for the key.
kmsKeyConfiguration_grants :: Lens.Lens' KmsKeyConfiguration (Prelude.Maybe [KmsGrantConfiguration])
kmsKeyConfiguration_grants = Lens.lens (\KmsKeyConfiguration' {grants} -> grants) (\s@KmsKeyConfiguration' {} a -> s {grants = a} :: KmsKeyConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Resource policy configuration for the KMS key. The only valid value for
-- the name of the key policy is @default@. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html#key-policy-default Default key policy>.
kmsKeyConfiguration_keyPolicies :: Lens.Lens' KmsKeyConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
kmsKeyConfiguration_keyPolicies = Lens.lens (\KmsKeyConfiguration' {keyPolicies} -> keyPolicies) (\s@KmsKeyConfiguration' {} a -> s {keyPolicies = a} :: KmsKeyConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON KmsKeyConfiguration where
  parseJSON =
    Data.withObject
      "KmsKeyConfiguration"
      ( \x ->
          KmsKeyConfiguration'
            Prelude.<$> (x Data..:? "grants" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "keyPolicies" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable KmsKeyConfiguration where
  hashWithSalt _salt KmsKeyConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` grants
      `Prelude.hashWithSalt` keyPolicies

instance Prelude.NFData KmsKeyConfiguration where
  rnf KmsKeyConfiguration' {..} =
    Prelude.rnf grants
      `Prelude.seq` Prelude.rnf keyPolicies

instance Data.ToJSON KmsKeyConfiguration where
  toJSON KmsKeyConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("grants" Data..=) Prelude.<$> grants,
            ("keyPolicies" Data..=) Prelude.<$> keyPolicies
          ]
      )
