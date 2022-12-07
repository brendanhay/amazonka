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
-- Module      : Amazonka.Glacier.Types.VaultAccessPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.VaultAccessPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the vault access policy.
--
-- /See:/ 'newVaultAccessPolicy' smart constructor.
data VaultAccessPolicy = VaultAccessPolicy'
  { -- | The vault access policy.
    policy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VaultAccessPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'vaultAccessPolicy_policy' - The vault access policy.
newVaultAccessPolicy ::
  VaultAccessPolicy
newVaultAccessPolicy =
  VaultAccessPolicy' {policy = Prelude.Nothing}

-- | The vault access policy.
vaultAccessPolicy_policy :: Lens.Lens' VaultAccessPolicy (Prelude.Maybe Prelude.Text)
vaultAccessPolicy_policy = Lens.lens (\VaultAccessPolicy' {policy} -> policy) (\s@VaultAccessPolicy' {} a -> s {policy = a} :: VaultAccessPolicy)

instance Data.FromJSON VaultAccessPolicy where
  parseJSON =
    Data.withObject
      "VaultAccessPolicy"
      ( \x ->
          VaultAccessPolicy' Prelude.<$> (x Data..:? "Policy")
      )

instance Prelude.Hashable VaultAccessPolicy where
  hashWithSalt _salt VaultAccessPolicy' {..} =
    _salt `Prelude.hashWithSalt` policy

instance Prelude.NFData VaultAccessPolicy where
  rnf VaultAccessPolicy' {..} = Prelude.rnf policy

instance Data.ToJSON VaultAccessPolicy where
  toJSON VaultAccessPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Policy" Data..=) Prelude.<$> policy]
      )
