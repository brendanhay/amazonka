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
-- Module      : Network.AWS.FMS.Types.NetworkFirewallPolicyDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.NetworkFirewallPolicyDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types.StatefulRuleGroup
import Network.AWS.FMS.Types.StatelessRuleGroup
import qualified Network.AWS.Lens as Lens

-- | The definition of the AWS Network Firewall firewall policy.
--
-- /See:/ 'newNetworkFirewallPolicyDescription' smart constructor.
data NetworkFirewallPolicyDescription = NetworkFirewallPolicyDescription'
  { -- | The stateless rule groups that are used in the Network Firewall firewall
    -- policy.
    statelessRuleGroups :: Core.Maybe [StatelessRuleGroup],
    -- | The actions to take on packet fragments that don\'t match any of the
    -- stateless rule groups.
    statelessFragmentDefaultActions :: Core.Maybe [Core.Text],
    -- | The actions to take on packets that don\'t match any of the stateless
    -- rule groups.
    statelessDefaultActions :: Core.Maybe [Core.Text],
    -- | The stateful rule groups that are used in the Network Firewall firewall
    -- policy.
    statefulRuleGroups :: Core.Maybe [StatefulRuleGroup],
    -- | Names of custom actions that are available for use in the stateless
    -- default actions settings.
    statelessCustomActions :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NetworkFirewallPolicyDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statelessRuleGroups', 'networkFirewallPolicyDescription_statelessRuleGroups' - The stateless rule groups that are used in the Network Firewall firewall
-- policy.
--
-- 'statelessFragmentDefaultActions', 'networkFirewallPolicyDescription_statelessFragmentDefaultActions' - The actions to take on packet fragments that don\'t match any of the
-- stateless rule groups.
--
-- 'statelessDefaultActions', 'networkFirewallPolicyDescription_statelessDefaultActions' - The actions to take on packets that don\'t match any of the stateless
-- rule groups.
--
-- 'statefulRuleGroups', 'networkFirewallPolicyDescription_statefulRuleGroups' - The stateful rule groups that are used in the Network Firewall firewall
-- policy.
--
-- 'statelessCustomActions', 'networkFirewallPolicyDescription_statelessCustomActions' - Names of custom actions that are available for use in the stateless
-- default actions settings.
newNetworkFirewallPolicyDescription ::
  NetworkFirewallPolicyDescription
newNetworkFirewallPolicyDescription =
  NetworkFirewallPolicyDescription'
    { statelessRuleGroups =
        Core.Nothing,
      statelessFragmentDefaultActions =
        Core.Nothing,
      statelessDefaultActions = Core.Nothing,
      statefulRuleGroups = Core.Nothing,
      statelessCustomActions = Core.Nothing
    }

-- | The stateless rule groups that are used in the Network Firewall firewall
-- policy.
networkFirewallPolicyDescription_statelessRuleGroups :: Lens.Lens' NetworkFirewallPolicyDescription (Core.Maybe [StatelessRuleGroup])
networkFirewallPolicyDescription_statelessRuleGroups = Lens.lens (\NetworkFirewallPolicyDescription' {statelessRuleGroups} -> statelessRuleGroups) (\s@NetworkFirewallPolicyDescription' {} a -> s {statelessRuleGroups = a} :: NetworkFirewallPolicyDescription) Core.. Lens.mapping Lens._Coerce

-- | The actions to take on packet fragments that don\'t match any of the
-- stateless rule groups.
networkFirewallPolicyDescription_statelessFragmentDefaultActions :: Lens.Lens' NetworkFirewallPolicyDescription (Core.Maybe [Core.Text])
networkFirewallPolicyDescription_statelessFragmentDefaultActions = Lens.lens (\NetworkFirewallPolicyDescription' {statelessFragmentDefaultActions} -> statelessFragmentDefaultActions) (\s@NetworkFirewallPolicyDescription' {} a -> s {statelessFragmentDefaultActions = a} :: NetworkFirewallPolicyDescription) Core.. Lens.mapping Lens._Coerce

-- | The actions to take on packets that don\'t match any of the stateless
-- rule groups.
networkFirewallPolicyDescription_statelessDefaultActions :: Lens.Lens' NetworkFirewallPolicyDescription (Core.Maybe [Core.Text])
networkFirewallPolicyDescription_statelessDefaultActions = Lens.lens (\NetworkFirewallPolicyDescription' {statelessDefaultActions} -> statelessDefaultActions) (\s@NetworkFirewallPolicyDescription' {} a -> s {statelessDefaultActions = a} :: NetworkFirewallPolicyDescription) Core.. Lens.mapping Lens._Coerce

-- | The stateful rule groups that are used in the Network Firewall firewall
-- policy.
networkFirewallPolicyDescription_statefulRuleGroups :: Lens.Lens' NetworkFirewallPolicyDescription (Core.Maybe [StatefulRuleGroup])
networkFirewallPolicyDescription_statefulRuleGroups = Lens.lens (\NetworkFirewallPolicyDescription' {statefulRuleGroups} -> statefulRuleGroups) (\s@NetworkFirewallPolicyDescription' {} a -> s {statefulRuleGroups = a} :: NetworkFirewallPolicyDescription) Core.. Lens.mapping Lens._Coerce

-- | Names of custom actions that are available for use in the stateless
-- default actions settings.
networkFirewallPolicyDescription_statelessCustomActions :: Lens.Lens' NetworkFirewallPolicyDescription (Core.Maybe [Core.Text])
networkFirewallPolicyDescription_statelessCustomActions = Lens.lens (\NetworkFirewallPolicyDescription' {statelessCustomActions} -> statelessCustomActions) (\s@NetworkFirewallPolicyDescription' {} a -> s {statelessCustomActions = a} :: NetworkFirewallPolicyDescription) Core.. Lens.mapping Lens._Coerce

instance
  Core.FromJSON
    NetworkFirewallPolicyDescription
  where
  parseJSON =
    Core.withObject
      "NetworkFirewallPolicyDescription"
      ( \x ->
          NetworkFirewallPolicyDescription'
            Core.<$> ( x Core..:? "StatelessRuleGroups"
                         Core..!= Core.mempty
                     )
            Core.<*> ( x Core..:? "StatelessFragmentDefaultActions"
                         Core..!= Core.mempty
                     )
            Core.<*> ( x Core..:? "StatelessDefaultActions"
                         Core..!= Core.mempty
                     )
            Core.<*> ( x Core..:? "StatefulRuleGroups"
                         Core..!= Core.mempty
                     )
            Core.<*> ( x Core..:? "StatelessCustomActions"
                         Core..!= Core.mempty
                     )
      )

instance
  Core.Hashable
    NetworkFirewallPolicyDescription

instance Core.NFData NetworkFirewallPolicyDescription
