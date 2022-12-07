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
-- Module      : Amazonka.NetworkManager.Types.CoreNetworkPolicyVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.CoreNetworkPolicyVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.ChangeSetState
import Amazonka.NetworkManager.Types.CoreNetworkPolicyAlias
import qualified Amazonka.Prelude as Prelude

-- | Describes a core network policy version.
--
-- /See:/ 'newCoreNetworkPolicyVersion' smart constructor.
data CoreNetworkPolicyVersion = CoreNetworkPolicyVersion'
  { -- | Whether a core network policy is the current policy or the most recently
    -- submitted policy.
    alias :: Prelude.Maybe CoreNetworkPolicyAlias,
    -- | The ID of a core network.
    coreNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The status of the policy version change set.
    changeSetState :: Prelude.Maybe ChangeSetState,
    -- | The ID of the policy version.
    policyVersionId :: Prelude.Maybe Prelude.Int,
    -- | The description of a core network policy version.
    description :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when a core network policy version was created.
    createdAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoreNetworkPolicyVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'coreNetworkPolicyVersion_alias' - Whether a core network policy is the current policy or the most recently
-- submitted policy.
--
-- 'coreNetworkId', 'coreNetworkPolicyVersion_coreNetworkId' - The ID of a core network.
--
-- 'changeSetState', 'coreNetworkPolicyVersion_changeSetState' - The status of the policy version change set.
--
-- 'policyVersionId', 'coreNetworkPolicyVersion_policyVersionId' - The ID of the policy version.
--
-- 'description', 'coreNetworkPolicyVersion_description' - The description of a core network policy version.
--
-- 'createdAt', 'coreNetworkPolicyVersion_createdAt' - The timestamp when a core network policy version was created.
newCoreNetworkPolicyVersion ::
  CoreNetworkPolicyVersion
newCoreNetworkPolicyVersion =
  CoreNetworkPolicyVersion'
    { alias = Prelude.Nothing,
      coreNetworkId = Prelude.Nothing,
      changeSetState = Prelude.Nothing,
      policyVersionId = Prelude.Nothing,
      description = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | Whether a core network policy is the current policy or the most recently
-- submitted policy.
coreNetworkPolicyVersion_alias :: Lens.Lens' CoreNetworkPolicyVersion (Prelude.Maybe CoreNetworkPolicyAlias)
coreNetworkPolicyVersion_alias = Lens.lens (\CoreNetworkPolicyVersion' {alias} -> alias) (\s@CoreNetworkPolicyVersion' {} a -> s {alias = a} :: CoreNetworkPolicyVersion)

-- | The ID of a core network.
coreNetworkPolicyVersion_coreNetworkId :: Lens.Lens' CoreNetworkPolicyVersion (Prelude.Maybe Prelude.Text)
coreNetworkPolicyVersion_coreNetworkId = Lens.lens (\CoreNetworkPolicyVersion' {coreNetworkId} -> coreNetworkId) (\s@CoreNetworkPolicyVersion' {} a -> s {coreNetworkId = a} :: CoreNetworkPolicyVersion)

-- | The status of the policy version change set.
coreNetworkPolicyVersion_changeSetState :: Lens.Lens' CoreNetworkPolicyVersion (Prelude.Maybe ChangeSetState)
coreNetworkPolicyVersion_changeSetState = Lens.lens (\CoreNetworkPolicyVersion' {changeSetState} -> changeSetState) (\s@CoreNetworkPolicyVersion' {} a -> s {changeSetState = a} :: CoreNetworkPolicyVersion)

-- | The ID of the policy version.
coreNetworkPolicyVersion_policyVersionId :: Lens.Lens' CoreNetworkPolicyVersion (Prelude.Maybe Prelude.Int)
coreNetworkPolicyVersion_policyVersionId = Lens.lens (\CoreNetworkPolicyVersion' {policyVersionId} -> policyVersionId) (\s@CoreNetworkPolicyVersion' {} a -> s {policyVersionId = a} :: CoreNetworkPolicyVersion)

-- | The description of a core network policy version.
coreNetworkPolicyVersion_description :: Lens.Lens' CoreNetworkPolicyVersion (Prelude.Maybe Prelude.Text)
coreNetworkPolicyVersion_description = Lens.lens (\CoreNetworkPolicyVersion' {description} -> description) (\s@CoreNetworkPolicyVersion' {} a -> s {description = a} :: CoreNetworkPolicyVersion)

-- | The timestamp when a core network policy version was created.
coreNetworkPolicyVersion_createdAt :: Lens.Lens' CoreNetworkPolicyVersion (Prelude.Maybe Prelude.UTCTime)
coreNetworkPolicyVersion_createdAt = Lens.lens (\CoreNetworkPolicyVersion' {createdAt} -> createdAt) (\s@CoreNetworkPolicyVersion' {} a -> s {createdAt = a} :: CoreNetworkPolicyVersion) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON CoreNetworkPolicyVersion where
  parseJSON =
    Data.withObject
      "CoreNetworkPolicyVersion"
      ( \x ->
          CoreNetworkPolicyVersion'
            Prelude.<$> (x Data..:? "Alias")
            Prelude.<*> (x Data..:? "CoreNetworkId")
            Prelude.<*> (x Data..:? "ChangeSetState")
            Prelude.<*> (x Data..:? "PolicyVersionId")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "CreatedAt")
      )

instance Prelude.Hashable CoreNetworkPolicyVersion where
  hashWithSalt _salt CoreNetworkPolicyVersion' {..} =
    _salt `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` changeSetState
      `Prelude.hashWithSalt` policyVersionId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData CoreNetworkPolicyVersion where
  rnf CoreNetworkPolicyVersion' {..} =
    Prelude.rnf alias
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf changeSetState
      `Prelude.seq` Prelude.rnf policyVersionId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf createdAt
