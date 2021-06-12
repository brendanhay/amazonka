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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the vault access policy.
--
-- /See:/ 'newVaultAccessPolicy' smart constructor.
data VaultAccessPolicy = VaultAccessPolicy'
  { -- | The vault access policy.
    policy :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  VaultAccessPolicy' {policy = Core.Nothing}

-- | The vault access policy.
vaultAccessPolicy_policy :: Lens.Lens' VaultAccessPolicy (Core.Maybe Core.Text)
vaultAccessPolicy_policy = Lens.lens (\VaultAccessPolicy' {policy} -> policy) (\s@VaultAccessPolicy' {} a -> s {policy = a} :: VaultAccessPolicy)

instance Core.FromJSON VaultAccessPolicy where
  parseJSON =
    Core.withObject
      "VaultAccessPolicy"
      ( \x ->
          VaultAccessPolicy' Core.<$> (x Core..:? "Policy")
      )

instance Core.Hashable VaultAccessPolicy

instance Core.NFData VaultAccessPolicy

instance Core.ToJSON VaultAccessPolicy where
  toJSON VaultAccessPolicy' {..} =
    Core.object
      (Core.catMaybes [("Policy" Core..=) Core.<$> policy])
