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
-- Module      : Amazonka.CloudWatchLogs.Types.AccountPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchLogs.Types.AccountPolicy where

import Amazonka.CloudWatchLogs.Types.PolicyType
import Amazonka.CloudWatchLogs.Types.Scope
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains information about one CloudWatch Logs account
-- policy.
--
-- /See:/ 'newAccountPolicy' smart constructor.
data AccountPolicy = AccountPolicy'
  { -- | The Amazon Web Services account ID that the policy applies to.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that this policy was most recently updated.
    lastUpdatedTime :: Prelude.Maybe Prelude.Natural,
    -- | The policy document for this account policy.
    --
    -- The JSON specified in @policyDocument@ can be up to 30,720 characters.
    policyDocument :: Prelude.Maybe Prelude.Text,
    -- | The name of the account policy.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | The type of policy for this account policy.
    policyType :: Prelude.Maybe PolicyType,
    -- | The scope of the account policy.
    scope :: Prelude.Maybe Scope
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'accountPolicy_accountId' - The Amazon Web Services account ID that the policy applies to.
--
-- 'lastUpdatedTime', 'accountPolicy_lastUpdatedTime' - The date and time that this policy was most recently updated.
--
-- 'policyDocument', 'accountPolicy_policyDocument' - The policy document for this account policy.
--
-- The JSON specified in @policyDocument@ can be up to 30,720 characters.
--
-- 'policyName', 'accountPolicy_policyName' - The name of the account policy.
--
-- 'policyType', 'accountPolicy_policyType' - The type of policy for this account policy.
--
-- 'scope', 'accountPolicy_scope' - The scope of the account policy.
newAccountPolicy ::
  AccountPolicy
newAccountPolicy =
  AccountPolicy'
    { accountId = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      policyDocument = Prelude.Nothing,
      policyName = Prelude.Nothing,
      policyType = Prelude.Nothing,
      scope = Prelude.Nothing
    }

-- | The Amazon Web Services account ID that the policy applies to.
accountPolicy_accountId :: Lens.Lens' AccountPolicy (Prelude.Maybe Prelude.Text)
accountPolicy_accountId = Lens.lens (\AccountPolicy' {accountId} -> accountId) (\s@AccountPolicy' {} a -> s {accountId = a} :: AccountPolicy)

-- | The date and time that this policy was most recently updated.
accountPolicy_lastUpdatedTime :: Lens.Lens' AccountPolicy (Prelude.Maybe Prelude.Natural)
accountPolicy_lastUpdatedTime = Lens.lens (\AccountPolicy' {lastUpdatedTime} -> lastUpdatedTime) (\s@AccountPolicy' {} a -> s {lastUpdatedTime = a} :: AccountPolicy)

-- | The policy document for this account policy.
--
-- The JSON specified in @policyDocument@ can be up to 30,720 characters.
accountPolicy_policyDocument :: Lens.Lens' AccountPolicy (Prelude.Maybe Prelude.Text)
accountPolicy_policyDocument = Lens.lens (\AccountPolicy' {policyDocument} -> policyDocument) (\s@AccountPolicy' {} a -> s {policyDocument = a} :: AccountPolicy)

-- | The name of the account policy.
accountPolicy_policyName :: Lens.Lens' AccountPolicy (Prelude.Maybe Prelude.Text)
accountPolicy_policyName = Lens.lens (\AccountPolicy' {policyName} -> policyName) (\s@AccountPolicy' {} a -> s {policyName = a} :: AccountPolicy)

-- | The type of policy for this account policy.
accountPolicy_policyType :: Lens.Lens' AccountPolicy (Prelude.Maybe PolicyType)
accountPolicy_policyType = Lens.lens (\AccountPolicy' {policyType} -> policyType) (\s@AccountPolicy' {} a -> s {policyType = a} :: AccountPolicy)

-- | The scope of the account policy.
accountPolicy_scope :: Lens.Lens' AccountPolicy (Prelude.Maybe Scope)
accountPolicy_scope = Lens.lens (\AccountPolicy' {scope} -> scope) (\s@AccountPolicy' {} a -> s {scope = a} :: AccountPolicy)

instance Data.FromJSON AccountPolicy where
  parseJSON =
    Data.withObject
      "AccountPolicy"
      ( \x ->
          AccountPolicy'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "lastUpdatedTime")
            Prelude.<*> (x Data..:? "policyDocument")
            Prelude.<*> (x Data..:? "policyName")
            Prelude.<*> (x Data..:? "policyType")
            Prelude.<*> (x Data..:? "scope")
      )

instance Prelude.Hashable AccountPolicy where
  hashWithSalt _salt AccountPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` policyDocument
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policyType
      `Prelude.hashWithSalt` scope

instance Prelude.NFData AccountPolicy where
  rnf AccountPolicy' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf policyDocument
      `Prelude.seq` Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf policyType
      `Prelude.seq` Prelude.rnf scope
