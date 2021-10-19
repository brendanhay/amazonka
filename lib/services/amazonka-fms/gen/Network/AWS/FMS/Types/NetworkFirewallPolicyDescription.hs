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
import qualified Network.AWS.Prelude as Prelude

-- | The definition of the Network Firewall firewall policy.
--
-- /See:/ 'newNetworkFirewallPolicyDescription' smart constructor.
data NetworkFirewallPolicyDescription = NetworkFirewallPolicyDescription'
  { -- | The stateful rule groups that are used in the Network Firewall firewall
    -- policy.
    statefulRuleGroups :: Prelude.Maybe [StatefulRuleGroup],
    -- | The stateless rule groups that are used in the Network Firewall firewall
    -- policy.
    statelessRuleGroups :: Prelude.Maybe [StatelessRuleGroup],
    -- | The actions to take on packet fragments that don\'t match any of the
    -- stateless rule groups.
    statelessFragmentDefaultActions :: Prelude.Maybe [Prelude.Text],
    -- | Names of custom actions that are available for use in the stateless
    -- default actions settings.
    statelessCustomActions :: Prelude.Maybe [Prelude.Text],
    -- | The actions to take on packets that don\'t match any of the stateless
    -- rule groups.
    statelessDefaultActions :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkFirewallPolicyDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statefulRuleGroups', 'networkFirewallPolicyDescription_statefulRuleGroups' - The stateful rule groups that are used in the Network Firewall firewall
-- policy.
--
-- 'statelessRuleGroups', 'networkFirewallPolicyDescription_statelessRuleGroups' - The stateless rule groups that are used in the Network Firewall firewall
-- policy.
--
-- 'statelessFragmentDefaultActions', 'networkFirewallPolicyDescription_statelessFragmentDefaultActions' - The actions to take on packet fragments that don\'t match any of the
-- stateless rule groups.
--
-- 'statelessCustomActions', 'networkFirewallPolicyDescription_statelessCustomActions' - Names of custom actions that are available for use in the stateless
-- default actions settings.
--
-- 'statelessDefaultActions', 'networkFirewallPolicyDescription_statelessDefaultActions' - The actions to take on packets that don\'t match any of the stateless
-- rule groups.
newNetworkFirewallPolicyDescription ::
  NetworkFirewallPolicyDescription
newNetworkFirewallPolicyDescription =
  NetworkFirewallPolicyDescription'
    { statefulRuleGroups =
        Prelude.Nothing,
      statelessRuleGroups = Prelude.Nothing,
      statelessFragmentDefaultActions =
        Prelude.Nothing,
      statelessCustomActions = Prelude.Nothing,
      statelessDefaultActions = Prelude.Nothing
    }

-- | The stateful rule groups that are used in the Network Firewall firewall
-- policy.
networkFirewallPolicyDescription_statefulRuleGroups :: Lens.Lens' NetworkFirewallPolicyDescription (Prelude.Maybe [StatefulRuleGroup])
networkFirewallPolicyDescription_statefulRuleGroups = Lens.lens (\NetworkFirewallPolicyDescription' {statefulRuleGroups} -> statefulRuleGroups) (\s@NetworkFirewallPolicyDescription' {} a -> s {statefulRuleGroups = a} :: NetworkFirewallPolicyDescription) Prelude.. Lens.mapping Lens.coerced

-- | The stateless rule groups that are used in the Network Firewall firewall
-- policy.
networkFirewallPolicyDescription_statelessRuleGroups :: Lens.Lens' NetworkFirewallPolicyDescription (Prelude.Maybe [StatelessRuleGroup])
networkFirewallPolicyDescription_statelessRuleGroups = Lens.lens (\NetworkFirewallPolicyDescription' {statelessRuleGroups} -> statelessRuleGroups) (\s@NetworkFirewallPolicyDescription' {} a -> s {statelessRuleGroups = a} :: NetworkFirewallPolicyDescription) Prelude.. Lens.mapping Lens.coerced

-- | The actions to take on packet fragments that don\'t match any of the
-- stateless rule groups.
networkFirewallPolicyDescription_statelessFragmentDefaultActions :: Lens.Lens' NetworkFirewallPolicyDescription (Prelude.Maybe [Prelude.Text])
networkFirewallPolicyDescription_statelessFragmentDefaultActions = Lens.lens (\NetworkFirewallPolicyDescription' {statelessFragmentDefaultActions} -> statelessFragmentDefaultActions) (\s@NetworkFirewallPolicyDescription' {} a -> s {statelessFragmentDefaultActions = a} :: NetworkFirewallPolicyDescription) Prelude.. Lens.mapping Lens.coerced

-- | Names of custom actions that are available for use in the stateless
-- default actions settings.
networkFirewallPolicyDescription_statelessCustomActions :: Lens.Lens' NetworkFirewallPolicyDescription (Prelude.Maybe [Prelude.Text])
networkFirewallPolicyDescription_statelessCustomActions = Lens.lens (\NetworkFirewallPolicyDescription' {statelessCustomActions} -> statelessCustomActions) (\s@NetworkFirewallPolicyDescription' {} a -> s {statelessCustomActions = a} :: NetworkFirewallPolicyDescription) Prelude.. Lens.mapping Lens.coerced

-- | The actions to take on packets that don\'t match any of the stateless
-- rule groups.
networkFirewallPolicyDescription_statelessDefaultActions :: Lens.Lens' NetworkFirewallPolicyDescription (Prelude.Maybe [Prelude.Text])
networkFirewallPolicyDescription_statelessDefaultActions = Lens.lens (\NetworkFirewallPolicyDescription' {statelessDefaultActions} -> statelessDefaultActions) (\s@NetworkFirewallPolicyDescription' {} a -> s {statelessDefaultActions = a} :: NetworkFirewallPolicyDescription) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromJSON
    NetworkFirewallPolicyDescription
  where
  parseJSON =
    Core.withObject
      "NetworkFirewallPolicyDescription"
      ( \x ->
          NetworkFirewallPolicyDescription'
            Prelude.<$> ( x Core..:? "StatefulRuleGroups"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "StatelessRuleGroups"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "StatelessFragmentDefaultActions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "StatelessCustomActions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "StatelessDefaultActions"
                            Core..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    NetworkFirewallPolicyDescription

instance
  Prelude.NFData
    NetworkFirewallPolicyDescription
