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
-- Module      : Amazonka.CloudWatch.Types.ManagedRuleState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.ManagedRuleState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The status of a managed Contributor Insights rule.
--
-- /See:/ 'newManagedRuleState' smart constructor.
data ManagedRuleState = ManagedRuleState'
  { -- | The name of the Contributor Insights rule that contains data for the
    -- specified Amazon Web Services resource.
    ruleName :: Prelude.Text,
    -- | Indicates whether the rule is enabled or disabled.
    state :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManagedRuleState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleName', 'managedRuleState_ruleName' - The name of the Contributor Insights rule that contains data for the
-- specified Amazon Web Services resource.
--
-- 'state', 'managedRuleState_state' - Indicates whether the rule is enabled or disabled.
newManagedRuleState ::
  -- | 'ruleName'
  Prelude.Text ->
  -- | 'state'
  Prelude.Text ->
  ManagedRuleState
newManagedRuleState pRuleName_ pState_ =
  ManagedRuleState'
    { ruleName = pRuleName_,
      state = pState_
    }

-- | The name of the Contributor Insights rule that contains data for the
-- specified Amazon Web Services resource.
managedRuleState_ruleName :: Lens.Lens' ManagedRuleState Prelude.Text
managedRuleState_ruleName = Lens.lens (\ManagedRuleState' {ruleName} -> ruleName) (\s@ManagedRuleState' {} a -> s {ruleName = a} :: ManagedRuleState)

-- | Indicates whether the rule is enabled or disabled.
managedRuleState_state :: Lens.Lens' ManagedRuleState Prelude.Text
managedRuleState_state = Lens.lens (\ManagedRuleState' {state} -> state) (\s@ManagedRuleState' {} a -> s {state = a} :: ManagedRuleState)

instance Data.FromXML ManagedRuleState where
  parseXML x =
    ManagedRuleState'
      Prelude.<$> (x Data..@ "RuleName")
      Prelude.<*> (x Data..@ "State")

instance Prelude.Hashable ManagedRuleState where
  hashWithSalt _salt ManagedRuleState' {..} =
    _salt
      `Prelude.hashWithSalt` ruleName
      `Prelude.hashWithSalt` state

instance Prelude.NFData ManagedRuleState where
  rnf ManagedRuleState' {..} =
    Prelude.rnf ruleName `Prelude.seq`
      Prelude.rnf state
