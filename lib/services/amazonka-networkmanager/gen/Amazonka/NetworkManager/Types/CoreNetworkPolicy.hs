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
-- Module      : Amazonka.NetworkManager.Types.CoreNetworkPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.CoreNetworkPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.ChangeSetState
import Amazonka.NetworkManager.Types.CoreNetworkPolicyAlias
import Amazonka.NetworkManager.Types.CoreNetworkPolicyError
import qualified Amazonka.Prelude as Prelude

-- | Describes a core network policy. You can have only one LIVE Core Policy.
--
-- /See:/ 'newCoreNetworkPolicy' smart constructor.
data CoreNetworkPolicy = CoreNetworkPolicy'
  { -- | Whether a core network policy is the current LIVE policy or the most
    -- recently submitted policy.
    alias :: Prelude.Maybe CoreNetworkPolicyAlias,
    -- | The state of a core network policy.
    changeSetState :: Prelude.Maybe ChangeSetState,
    -- | The ID of a core network.
    coreNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when a core network policy was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The description of a core network policy.
    description :: Prelude.Maybe Prelude.Text,
    -- | Describes a core network policy.
    policyDocument :: Prelude.Maybe Prelude.Text,
    -- | Describes any errors in a core network policy.
    policyErrors :: Prelude.Maybe [CoreNetworkPolicyError],
    -- | The ID of the policy version.
    policyVersionId :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoreNetworkPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'coreNetworkPolicy_alias' - Whether a core network policy is the current LIVE policy or the most
-- recently submitted policy.
--
-- 'changeSetState', 'coreNetworkPolicy_changeSetState' - The state of a core network policy.
--
-- 'coreNetworkId', 'coreNetworkPolicy_coreNetworkId' - The ID of a core network.
--
-- 'createdAt', 'coreNetworkPolicy_createdAt' - The timestamp when a core network policy was created.
--
-- 'description', 'coreNetworkPolicy_description' - The description of a core network policy.
--
-- 'policyDocument', 'coreNetworkPolicy_policyDocument' - Describes a core network policy.
--
-- 'policyErrors', 'coreNetworkPolicy_policyErrors' - Describes any errors in a core network policy.
--
-- 'policyVersionId', 'coreNetworkPolicy_policyVersionId' - The ID of the policy version.
newCoreNetworkPolicy ::
  CoreNetworkPolicy
newCoreNetworkPolicy =
  CoreNetworkPolicy'
    { alias = Prelude.Nothing,
      changeSetState = Prelude.Nothing,
      coreNetworkId = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      description = Prelude.Nothing,
      policyDocument = Prelude.Nothing,
      policyErrors = Prelude.Nothing,
      policyVersionId = Prelude.Nothing
    }

-- | Whether a core network policy is the current LIVE policy or the most
-- recently submitted policy.
coreNetworkPolicy_alias :: Lens.Lens' CoreNetworkPolicy (Prelude.Maybe CoreNetworkPolicyAlias)
coreNetworkPolicy_alias = Lens.lens (\CoreNetworkPolicy' {alias} -> alias) (\s@CoreNetworkPolicy' {} a -> s {alias = a} :: CoreNetworkPolicy)

-- | The state of a core network policy.
coreNetworkPolicy_changeSetState :: Lens.Lens' CoreNetworkPolicy (Prelude.Maybe ChangeSetState)
coreNetworkPolicy_changeSetState = Lens.lens (\CoreNetworkPolicy' {changeSetState} -> changeSetState) (\s@CoreNetworkPolicy' {} a -> s {changeSetState = a} :: CoreNetworkPolicy)

-- | The ID of a core network.
coreNetworkPolicy_coreNetworkId :: Lens.Lens' CoreNetworkPolicy (Prelude.Maybe Prelude.Text)
coreNetworkPolicy_coreNetworkId = Lens.lens (\CoreNetworkPolicy' {coreNetworkId} -> coreNetworkId) (\s@CoreNetworkPolicy' {} a -> s {coreNetworkId = a} :: CoreNetworkPolicy)

-- | The timestamp when a core network policy was created.
coreNetworkPolicy_createdAt :: Lens.Lens' CoreNetworkPolicy (Prelude.Maybe Prelude.UTCTime)
coreNetworkPolicy_createdAt = Lens.lens (\CoreNetworkPolicy' {createdAt} -> createdAt) (\s@CoreNetworkPolicy' {} a -> s {createdAt = a} :: CoreNetworkPolicy) Prelude.. Lens.mapping Data._Time

-- | The description of a core network policy.
coreNetworkPolicy_description :: Lens.Lens' CoreNetworkPolicy (Prelude.Maybe Prelude.Text)
coreNetworkPolicy_description = Lens.lens (\CoreNetworkPolicy' {description} -> description) (\s@CoreNetworkPolicy' {} a -> s {description = a} :: CoreNetworkPolicy)

-- | Describes a core network policy.
coreNetworkPolicy_policyDocument :: Lens.Lens' CoreNetworkPolicy (Prelude.Maybe Prelude.Text)
coreNetworkPolicy_policyDocument = Lens.lens (\CoreNetworkPolicy' {policyDocument} -> policyDocument) (\s@CoreNetworkPolicy' {} a -> s {policyDocument = a} :: CoreNetworkPolicy)

-- | Describes any errors in a core network policy.
coreNetworkPolicy_policyErrors :: Lens.Lens' CoreNetworkPolicy (Prelude.Maybe [CoreNetworkPolicyError])
coreNetworkPolicy_policyErrors = Lens.lens (\CoreNetworkPolicy' {policyErrors} -> policyErrors) (\s@CoreNetworkPolicy' {} a -> s {policyErrors = a} :: CoreNetworkPolicy) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the policy version.
coreNetworkPolicy_policyVersionId :: Lens.Lens' CoreNetworkPolicy (Prelude.Maybe Prelude.Int)
coreNetworkPolicy_policyVersionId = Lens.lens (\CoreNetworkPolicy' {policyVersionId} -> policyVersionId) (\s@CoreNetworkPolicy' {} a -> s {policyVersionId = a} :: CoreNetworkPolicy)

instance Data.FromJSON CoreNetworkPolicy where
  parseJSON =
    Data.withObject
      "CoreNetworkPolicy"
      ( \x ->
          CoreNetworkPolicy'
            Prelude.<$> (x Data..:? "Alias")
            Prelude.<*> (x Data..:? "ChangeSetState")
            Prelude.<*> (x Data..:? "CoreNetworkId")
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "PolicyDocument")
            Prelude.<*> (x Data..:? "PolicyErrors" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PolicyVersionId")
      )

instance Prelude.Hashable CoreNetworkPolicy where
  hashWithSalt _salt CoreNetworkPolicy' {..} =
    _salt `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` changeSetState
      `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` policyDocument
      `Prelude.hashWithSalt` policyErrors
      `Prelude.hashWithSalt` policyVersionId

instance Prelude.NFData CoreNetworkPolicy where
  rnf CoreNetworkPolicy' {..} =
    Prelude.rnf alias
      `Prelude.seq` Prelude.rnf changeSetState
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf policyDocument
      `Prelude.seq` Prelude.rnf policyErrors
      `Prelude.seq` Prelude.rnf policyVersionId
