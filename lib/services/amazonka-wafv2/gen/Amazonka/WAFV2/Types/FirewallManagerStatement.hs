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
-- Module      : Amazonka.WAFV2.Types.FirewallManagerStatement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.FirewallManagerStatement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.ManagedRuleGroupStatement
import Amazonka.WAFV2.Types.RuleGroupReferenceStatement

-- | The processing guidance for an Firewall Manager rule. This is like a
-- regular rule Statement, but it can only contain a single rule group
-- reference.
--
-- /See:/ 'newFirewallManagerStatement' smart constructor.
data FirewallManagerStatement = FirewallManagerStatement'
  { -- | A statement used by Firewall Manager to run the rules that are defined
    -- in a managed rule group. This is managed by Firewall Manager for an
    -- Firewall Manager WAF policy.
    managedRuleGroupStatement :: Prelude.Maybe ManagedRuleGroupStatement,
    -- | A statement used by Firewall Manager to run the rules that are defined
    -- in a rule group. This is managed by Firewall Manager for an Firewall
    -- Manager WAF policy.
    ruleGroupReferenceStatement :: Prelude.Maybe RuleGroupReferenceStatement
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FirewallManagerStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'managedRuleGroupStatement', 'firewallManagerStatement_managedRuleGroupStatement' - A statement used by Firewall Manager to run the rules that are defined
-- in a managed rule group. This is managed by Firewall Manager for an
-- Firewall Manager WAF policy.
--
-- 'ruleGroupReferenceStatement', 'firewallManagerStatement_ruleGroupReferenceStatement' - A statement used by Firewall Manager to run the rules that are defined
-- in a rule group. This is managed by Firewall Manager for an Firewall
-- Manager WAF policy.
newFirewallManagerStatement ::
  FirewallManagerStatement
newFirewallManagerStatement =
  FirewallManagerStatement'
    { managedRuleGroupStatement =
        Prelude.Nothing,
      ruleGroupReferenceStatement = Prelude.Nothing
    }

-- | A statement used by Firewall Manager to run the rules that are defined
-- in a managed rule group. This is managed by Firewall Manager for an
-- Firewall Manager WAF policy.
firewallManagerStatement_managedRuleGroupStatement :: Lens.Lens' FirewallManagerStatement (Prelude.Maybe ManagedRuleGroupStatement)
firewallManagerStatement_managedRuleGroupStatement = Lens.lens (\FirewallManagerStatement' {managedRuleGroupStatement} -> managedRuleGroupStatement) (\s@FirewallManagerStatement' {} a -> s {managedRuleGroupStatement = a} :: FirewallManagerStatement)

-- | A statement used by Firewall Manager to run the rules that are defined
-- in a rule group. This is managed by Firewall Manager for an Firewall
-- Manager WAF policy.
firewallManagerStatement_ruleGroupReferenceStatement :: Lens.Lens' FirewallManagerStatement (Prelude.Maybe RuleGroupReferenceStatement)
firewallManagerStatement_ruleGroupReferenceStatement = Lens.lens (\FirewallManagerStatement' {ruleGroupReferenceStatement} -> ruleGroupReferenceStatement) (\s@FirewallManagerStatement' {} a -> s {ruleGroupReferenceStatement = a} :: FirewallManagerStatement)

instance Data.FromJSON FirewallManagerStatement where
  parseJSON =
    Data.withObject
      "FirewallManagerStatement"
      ( \x ->
          FirewallManagerStatement'
            Prelude.<$> (x Data..:? "ManagedRuleGroupStatement")
            Prelude.<*> (x Data..:? "RuleGroupReferenceStatement")
      )

instance Prelude.Hashable FirewallManagerStatement where
  hashWithSalt _salt FirewallManagerStatement' {..} =
    _salt
      `Prelude.hashWithSalt` managedRuleGroupStatement
      `Prelude.hashWithSalt` ruleGroupReferenceStatement

instance Prelude.NFData FirewallManagerStatement where
  rnf FirewallManagerStatement' {..} =
    Prelude.rnf managedRuleGroupStatement
      `Prelude.seq` Prelude.rnf ruleGroupReferenceStatement
