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
-- Module      : Network.AWS.Glacier.Types.VaultLockPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.VaultLockPolicy where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the vault lock policy.
--
-- /See:/ 'newVaultLockPolicy' smart constructor.
data VaultLockPolicy = VaultLockPolicy'
  { -- | The vault lock policy.
    policy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'VaultLockPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'vaultLockPolicy_policy' - The vault lock policy.
newVaultLockPolicy ::
  VaultLockPolicy
newVaultLockPolicy =
  VaultLockPolicy' {policy = Prelude.Nothing}

-- | The vault lock policy.
vaultLockPolicy_policy :: Lens.Lens' VaultLockPolicy (Prelude.Maybe Prelude.Text)
vaultLockPolicy_policy = Lens.lens (\VaultLockPolicy' {policy} -> policy) (\s@VaultLockPolicy' {} a -> s {policy = a} :: VaultLockPolicy)

instance Prelude.Hashable VaultLockPolicy

instance Prelude.NFData VaultLockPolicy

instance Prelude.ToJSON VaultLockPolicy where
  toJSON VaultLockPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Policy" Prelude..=) Prelude.<$> policy]
      )
