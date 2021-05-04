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
-- Module      : Network.AWS.Glacier.Types.VaultAccessPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.VaultAccessPolicy where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the vault access policy.
--
-- /See:/ 'newVaultAccessPolicy' smart constructor.
data VaultAccessPolicy = VaultAccessPolicy'
  { -- | The vault access policy.
    policy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON VaultAccessPolicy where
  parseJSON =
    Prelude.withObject
      "VaultAccessPolicy"
      ( \x ->
          VaultAccessPolicy'
            Prelude.<$> (x Prelude..:? "Policy")
      )

instance Prelude.Hashable VaultAccessPolicy

instance Prelude.NFData VaultAccessPolicy

instance Prelude.ToJSON VaultAccessPolicy where
  toJSON VaultAccessPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Policy" Prelude..=) Prelude.<$> policy]
      )
