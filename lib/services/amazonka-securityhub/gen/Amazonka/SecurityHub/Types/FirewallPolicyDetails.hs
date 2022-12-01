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
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.FirewallPolicyStatefulRuleGroupReferencesDetails
import Amazonka.SecurityHub.Types.FirewallPolicyStatelessCustomActionsDetails
import Amazonka.SecurityHub.Types.FirewallPolicyStatelessRuleGroupReferencesDetails

-- | Defines the behavior of the firewall.
--
-- /See:/ 'newFirewallPolicyDetails' smart constructor.
data FirewallPolicyDetails = FirewallPolicyDetails'
  { -- | The custom action definitions that are available to use in the firewall
    -- policy\'s @StatelessDefaultActions@ setting.
    statelessCustomActions :: Prelude.Maybe [FirewallPolicyStatelessCustomActionsDetails],
    -- | The stateful rule groups that are used in the firewall policy.
    statefulRuleGroupReferences :: Prelude.Maybe [FirewallPolicyStatefulRuleGroupReferencesDetails],
    -- | The stateless rule groups that are used in the firewall policy.
    statelessRuleGroupReferences :: Prelude.Maybe [FirewallPolicyStatelessRuleGroupReferencesDetails],
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
    statelessFragmentDefaultActions :: Prelude.Maybe [Prelude.Text]
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
-- 'statelessCustomActions', 'firewallPolicyDetails_statelessCustomActions' - The custom action definitions that are available to use in the firewall
-- policy\'s @StatelessDefaultActions@ setting.
--
-- 'statefulRuleGroupReferences', 'firewallPolicyDetails_statefulRuleGroupReferences' - The stateful rule groups that are used in the firewall policy.
--
-- 'statelessRuleGroupReferences', 'firewallPolicyDetails_statelessRuleGroupReferences' - The stateless rule groups that are used in the firewall policy.
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
newFirewallPolicyDetails ::
  FirewallPolicyDetails
newFirewallPolicyDetails =
  FirewallPolicyDetails'
    { statelessCustomActions =
        Prelude.Nothing,
      statefulRuleGroupReferences = Prelude.Nothing,
      statelessRuleGroupReferences = Prelude.Nothing,
      statelessDefaultActions = Prelude.Nothing,
      statelessFragmentDefaultActions = Prelude.Nothing
    }

-- | The custom action definitions that are available to use in the firewall
-- policy\'s @StatelessDefaultActions@ setting.
firewallPolicyDetails_statelessCustomActions :: Lens.Lens' FirewallPolicyDetails (Prelude.Maybe [FirewallPolicyStatelessCustomActionsDetails])
firewallPolicyDetails_statelessCustomActions = Lens.lens (\FirewallPolicyDetails' {statelessCustomActions} -> statelessCustomActions) (\s@FirewallPolicyDetails' {} a -> s {statelessCustomActions = a} :: FirewallPolicyDetails) Prelude.. Lens.mapping Lens.coerced

-- | The stateful rule groups that are used in the firewall policy.
firewallPolicyDetails_statefulRuleGroupReferences :: Lens.Lens' FirewallPolicyDetails (Prelude.Maybe [FirewallPolicyStatefulRuleGroupReferencesDetails])
firewallPolicyDetails_statefulRuleGroupReferences = Lens.lens (\FirewallPolicyDetails' {statefulRuleGroupReferences} -> statefulRuleGroupReferences) (\s@FirewallPolicyDetails' {} a -> s {statefulRuleGroupReferences = a} :: FirewallPolicyDetails) Prelude.. Lens.mapping Lens.coerced

-- | The stateless rule groups that are used in the firewall policy.
firewallPolicyDetails_statelessRuleGroupReferences :: Lens.Lens' FirewallPolicyDetails (Prelude.Maybe [FirewallPolicyStatelessRuleGroupReferencesDetails])
firewallPolicyDetails_statelessRuleGroupReferences = Lens.lens (\FirewallPolicyDetails' {statelessRuleGroupReferences} -> statelessRuleGroupReferences) (\s@FirewallPolicyDetails' {} a -> s {statelessRuleGroupReferences = a} :: FirewallPolicyDetails) Prelude.. Lens.mapping Lens.coerced

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

instance Core.FromJSON FirewallPolicyDetails where
  parseJSON =
    Core.withObject
      "FirewallPolicyDetails"
      ( \x ->
          FirewallPolicyDetails'
            Prelude.<$> ( x Core..:? "StatelessCustomActions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "StatefulRuleGroupReferences"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "StatelessRuleGroupReferences"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "StatelessDefaultActions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "StatelessFragmentDefaultActions"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable FirewallPolicyDetails where
  hashWithSalt _salt FirewallPolicyDetails' {..} =
    _salt `Prelude.hashWithSalt` statelessCustomActions
      `Prelude.hashWithSalt` statefulRuleGroupReferences
      `Prelude.hashWithSalt` statelessRuleGroupReferences
      `Prelude.hashWithSalt` statelessDefaultActions
      `Prelude.hashWithSalt` statelessFragmentDefaultActions

instance Prelude.NFData FirewallPolicyDetails where
  rnf FirewallPolicyDetails' {..} =
    Prelude.rnf statelessCustomActions
      `Prelude.seq` Prelude.rnf statefulRuleGroupReferences
      `Prelude.seq` Prelude.rnf statelessRuleGroupReferences
      `Prelude.seq` Prelude.rnf statelessDefaultActions
      `Prelude.seq` Prelude.rnf statelessFragmentDefaultActions

instance Core.ToJSON FirewallPolicyDetails where
  toJSON FirewallPolicyDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StatelessCustomActions" Core..=)
              Prelude.<$> statelessCustomActions,
            ("StatefulRuleGroupReferences" Core..=)
              Prelude.<$> statefulRuleGroupReferences,
            ("StatelessRuleGroupReferences" Core..=)
              Prelude.<$> statelessRuleGroupReferences,
            ("StatelessDefaultActions" Core..=)
              Prelude.<$> statelessDefaultActions,
            ("StatelessFragmentDefaultActions" Core..=)
              Prelude.<$> statelessFragmentDefaultActions
          ]
      )
