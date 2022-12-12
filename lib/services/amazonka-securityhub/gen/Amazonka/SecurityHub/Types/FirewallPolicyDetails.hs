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
-- Module      : Amazonka.SecurityHub.Types.FirewallPolicyDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.FirewallPolicyDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.FirewallPolicyStatefulRuleGroupReferencesDetails
import Amazonka.SecurityHub.Types.FirewallPolicyStatelessCustomActionsDetails
import Amazonka.SecurityHub.Types.FirewallPolicyStatelessRuleGroupReferencesDetails

-- | Defines the behavior of the firewall.
--
-- /See:/ 'newFirewallPolicyDetails' smart constructor.
data FirewallPolicyDetails = FirewallPolicyDetails'
  { -- | The stateful rule groups that are used in the firewall policy.
    statefulRuleGroupReferences :: Prelude.Maybe [FirewallPolicyStatefulRuleGroupReferencesDetails],
    -- | The custom action definitions that are available to use in the firewall
    -- policy\'s @StatelessDefaultActions@ setting.
    statelessCustomActions :: Prelude.Maybe [FirewallPolicyStatelessCustomActionsDetails],
    -- | The actions to take on a packet if it doesn\'t match any of the
    -- stateless rules in the policy.
    --
    -- You must specify a standard action (@aws:pass@, @aws:drop@,
    -- @aws:forward_to_sfe@), and can optionally include a custom action from
    -- @StatelessCustomActions@.
    statelessDefaultActions :: Prelude.Maybe [Prelude.Text],
    -- | The actions to take on a fragmented UDP packet if it doesn\'t match any
    -- of the stateless rules in the policy.
    --
    -- You must specify a standard action (@aws:pass@, @aws:drop@,
    -- @aws:forward_to_sfe@), and can optionally include a custom action from
    -- @StatelessCustomActions@.
    statelessFragmentDefaultActions :: Prelude.Maybe [Prelude.Text],
    -- | The stateless rule groups that are used in the firewall policy.
    statelessRuleGroupReferences :: Prelude.Maybe [FirewallPolicyStatelessRuleGroupReferencesDetails]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FirewallPolicyDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statefulRuleGroupReferences', 'firewallPolicyDetails_statefulRuleGroupReferences' - The stateful rule groups that are used in the firewall policy.
--
-- 'statelessCustomActions', 'firewallPolicyDetails_statelessCustomActions' - The custom action definitions that are available to use in the firewall
-- policy\'s @StatelessDefaultActions@ setting.
--
-- 'statelessDefaultActions', 'firewallPolicyDetails_statelessDefaultActions' - The actions to take on a packet if it doesn\'t match any of the
-- stateless rules in the policy.
--
-- You must specify a standard action (@aws:pass@, @aws:drop@,
-- @aws:forward_to_sfe@), and can optionally include a custom action from
-- @StatelessCustomActions@.
--
-- 'statelessFragmentDefaultActions', 'firewallPolicyDetails_statelessFragmentDefaultActions' - The actions to take on a fragmented UDP packet if it doesn\'t match any
-- of the stateless rules in the policy.
--
-- You must specify a standard action (@aws:pass@, @aws:drop@,
-- @aws:forward_to_sfe@), and can optionally include a custom action from
-- @StatelessCustomActions@.
--
-- 'statelessRuleGroupReferences', 'firewallPolicyDetails_statelessRuleGroupReferences' - The stateless rule groups that are used in the firewall policy.
newFirewallPolicyDetails ::
  FirewallPolicyDetails
newFirewallPolicyDetails =
  FirewallPolicyDetails'
    { statefulRuleGroupReferences =
        Prelude.Nothing,
      statelessCustomActions = Prelude.Nothing,
      statelessDefaultActions = Prelude.Nothing,
      statelessFragmentDefaultActions = Prelude.Nothing,
      statelessRuleGroupReferences = Prelude.Nothing
    }

-- | The stateful rule groups that are used in the firewall policy.
firewallPolicyDetails_statefulRuleGroupReferences :: Lens.Lens' FirewallPolicyDetails (Prelude.Maybe [FirewallPolicyStatefulRuleGroupReferencesDetails])
firewallPolicyDetails_statefulRuleGroupReferences = Lens.lens (\FirewallPolicyDetails' {statefulRuleGroupReferences} -> statefulRuleGroupReferences) (\s@FirewallPolicyDetails' {} a -> s {statefulRuleGroupReferences = a} :: FirewallPolicyDetails) Prelude.. Lens.mapping Lens.coerced

-- | The custom action definitions that are available to use in the firewall
-- policy\'s @StatelessDefaultActions@ setting.
firewallPolicyDetails_statelessCustomActions :: Lens.Lens' FirewallPolicyDetails (Prelude.Maybe [FirewallPolicyStatelessCustomActionsDetails])
firewallPolicyDetails_statelessCustomActions = Lens.lens (\FirewallPolicyDetails' {statelessCustomActions} -> statelessCustomActions) (\s@FirewallPolicyDetails' {} a -> s {statelessCustomActions = a} :: FirewallPolicyDetails) Prelude.. Lens.mapping Lens.coerced

-- | The actions to take on a packet if it doesn\'t match any of the
-- stateless rules in the policy.
--
-- You must specify a standard action (@aws:pass@, @aws:drop@,
-- @aws:forward_to_sfe@), and can optionally include a custom action from
-- @StatelessCustomActions@.
firewallPolicyDetails_statelessDefaultActions :: Lens.Lens' FirewallPolicyDetails (Prelude.Maybe [Prelude.Text])
firewallPolicyDetails_statelessDefaultActions = Lens.lens (\FirewallPolicyDetails' {statelessDefaultActions} -> statelessDefaultActions) (\s@FirewallPolicyDetails' {} a -> s {statelessDefaultActions = a} :: FirewallPolicyDetails) Prelude.. Lens.mapping Lens.coerced

-- | The actions to take on a fragmented UDP packet if it doesn\'t match any
-- of the stateless rules in the policy.
--
-- You must specify a standard action (@aws:pass@, @aws:drop@,
-- @aws:forward_to_sfe@), and can optionally include a custom action from
-- @StatelessCustomActions@.
firewallPolicyDetails_statelessFragmentDefaultActions :: Lens.Lens' FirewallPolicyDetails (Prelude.Maybe [Prelude.Text])
firewallPolicyDetails_statelessFragmentDefaultActions = Lens.lens (\FirewallPolicyDetails' {statelessFragmentDefaultActions} -> statelessFragmentDefaultActions) (\s@FirewallPolicyDetails' {} a -> s {statelessFragmentDefaultActions = a} :: FirewallPolicyDetails) Prelude.. Lens.mapping Lens.coerced

-- | The stateless rule groups that are used in the firewall policy.
firewallPolicyDetails_statelessRuleGroupReferences :: Lens.Lens' FirewallPolicyDetails (Prelude.Maybe [FirewallPolicyStatelessRuleGroupReferencesDetails])
firewallPolicyDetails_statelessRuleGroupReferences = Lens.lens (\FirewallPolicyDetails' {statelessRuleGroupReferences} -> statelessRuleGroupReferences) (\s@FirewallPolicyDetails' {} a -> s {statelessRuleGroupReferences = a} :: FirewallPolicyDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON FirewallPolicyDetails where
  parseJSON =
    Data.withObject
      "FirewallPolicyDetails"
      ( \x ->
          FirewallPolicyDetails'
            Prelude.<$> ( x Data..:? "StatefulRuleGroupReferences"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "StatelessCustomActions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "StatelessDefaultActions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "StatelessFragmentDefaultActions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "StatelessRuleGroupReferences"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable FirewallPolicyDetails where
  hashWithSalt _salt FirewallPolicyDetails' {..} =
    _salt
      `Prelude.hashWithSalt` statefulRuleGroupReferences
      `Prelude.hashWithSalt` statelessCustomActions
      `Prelude.hashWithSalt` statelessDefaultActions
      `Prelude.hashWithSalt` statelessFragmentDefaultActions
      `Prelude.hashWithSalt` statelessRuleGroupReferences

instance Prelude.NFData FirewallPolicyDetails where
  rnf FirewallPolicyDetails' {..} =
    Prelude.rnf statefulRuleGroupReferences
      `Prelude.seq` Prelude.rnf statelessCustomActions
      `Prelude.seq` Prelude.rnf statelessDefaultActions
      `Prelude.seq` Prelude.rnf statelessFragmentDefaultActions
      `Prelude.seq` Prelude.rnf statelessRuleGroupReferences

instance Data.ToJSON FirewallPolicyDetails where
  toJSON FirewallPolicyDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StatefulRuleGroupReferences" Data..=)
              Prelude.<$> statefulRuleGroupReferences,
            ("StatelessCustomActions" Data..=)
              Prelude.<$> statelessCustomActions,
            ("StatelessDefaultActions" Data..=)
              Prelude.<$> statelessDefaultActions,
            ("StatelessFragmentDefaultActions" Data..=)
              Prelude.<$> statelessFragmentDefaultActions,
            ("StatelessRuleGroupReferences" Data..=)
              Prelude.<$> statelessRuleGroupReferences
          ]
      )
