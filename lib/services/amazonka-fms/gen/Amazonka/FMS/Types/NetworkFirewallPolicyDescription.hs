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
-- Module      : Amazonka.FMS.Types.NetworkFirewallPolicyDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.NetworkFirewallPolicyDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.StatefulEngineOptions
import Amazonka.FMS.Types.StatefulRuleGroup
import Amazonka.FMS.Types.StatelessRuleGroup
import qualified Amazonka.Prelude as Prelude

-- | The definition of the Network Firewall firewall policy.
--
-- /See:/ 'newNetworkFirewallPolicyDescription' smart constructor.
data NetworkFirewallPolicyDescription = NetworkFirewallPolicyDescription'
  { -- | Additional options governing how Network Firewall handles stateful
    -- rules. The stateful rule groups that you use in your policy must have
    -- stateful rule options settings that are compatible with these settings.
    statefulEngineOptions :: Prelude.Maybe StatefulEngineOptions,
    -- | Names of custom actions that are available for use in the stateless
    -- default actions settings.
    statelessCustomActions :: Prelude.Maybe [Prelude.Text],
    -- | The default actions to take on a packet that doesn\'t match any stateful
    -- rules. The stateful default action is optional, and is only valid when
    -- using the strict rule order.
    --
    -- Valid values of the stateful default action:
    --
    -- -   aws:drop_strict
    --
    -- -   aws:drop_established
    --
    -- -   aws:alert_strict
    --
    -- -   aws:alert_established
    statefulDefaultActions :: Prelude.Maybe [Prelude.Text],
    -- | The actions to take on packets that don\'t match any of the stateless
    -- rule groups.
    statelessDefaultActions :: Prelude.Maybe [Prelude.Text],
    -- | The stateless rule groups that are used in the Network Firewall firewall
    -- policy.
    statelessRuleGroups :: Prelude.Maybe [StatelessRuleGroup],
    -- | The actions to take on packet fragments that don\'t match any of the
    -- stateless rule groups.
    statelessFragmentDefaultActions :: Prelude.Maybe [Prelude.Text],
    -- | The stateful rule groups that are used in the Network Firewall firewall
    -- policy.
    statefulRuleGroups :: Prelude.Maybe [StatefulRuleGroup]
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
-- 'statefulEngineOptions', 'networkFirewallPolicyDescription_statefulEngineOptions' - Additional options governing how Network Firewall handles stateful
-- rules. The stateful rule groups that you use in your policy must have
-- stateful rule options settings that are compatible with these settings.
--
-- 'statelessCustomActions', 'networkFirewallPolicyDescription_statelessCustomActions' - Names of custom actions that are available for use in the stateless
-- default actions settings.
--
-- 'statefulDefaultActions', 'networkFirewallPolicyDescription_statefulDefaultActions' - The default actions to take on a packet that doesn\'t match any stateful
-- rules. The stateful default action is optional, and is only valid when
-- using the strict rule order.
--
-- Valid values of the stateful default action:
--
-- -   aws:drop_strict
--
-- -   aws:drop_established
--
-- -   aws:alert_strict
--
-- -   aws:alert_established
--
-- 'statelessDefaultActions', 'networkFirewallPolicyDescription_statelessDefaultActions' - The actions to take on packets that don\'t match any of the stateless
-- rule groups.
--
-- 'statelessRuleGroups', 'networkFirewallPolicyDescription_statelessRuleGroups' - The stateless rule groups that are used in the Network Firewall firewall
-- policy.
--
-- 'statelessFragmentDefaultActions', 'networkFirewallPolicyDescription_statelessFragmentDefaultActions' - The actions to take on packet fragments that don\'t match any of the
-- stateless rule groups.
--
-- 'statefulRuleGroups', 'networkFirewallPolicyDescription_statefulRuleGroups' - The stateful rule groups that are used in the Network Firewall firewall
-- policy.
newNetworkFirewallPolicyDescription ::
  NetworkFirewallPolicyDescription
newNetworkFirewallPolicyDescription =
  NetworkFirewallPolicyDescription'
    { statefulEngineOptions =
        Prelude.Nothing,
      statelessCustomActions = Prelude.Nothing,
      statefulDefaultActions = Prelude.Nothing,
      statelessDefaultActions = Prelude.Nothing,
      statelessRuleGroups = Prelude.Nothing,
      statelessFragmentDefaultActions =
        Prelude.Nothing,
      statefulRuleGroups = Prelude.Nothing
    }

-- | Additional options governing how Network Firewall handles stateful
-- rules. The stateful rule groups that you use in your policy must have
-- stateful rule options settings that are compatible with these settings.
networkFirewallPolicyDescription_statefulEngineOptions :: Lens.Lens' NetworkFirewallPolicyDescription (Prelude.Maybe StatefulEngineOptions)
networkFirewallPolicyDescription_statefulEngineOptions = Lens.lens (\NetworkFirewallPolicyDescription' {statefulEngineOptions} -> statefulEngineOptions) (\s@NetworkFirewallPolicyDescription' {} a -> s {statefulEngineOptions = a} :: NetworkFirewallPolicyDescription)

-- | Names of custom actions that are available for use in the stateless
-- default actions settings.
networkFirewallPolicyDescription_statelessCustomActions :: Lens.Lens' NetworkFirewallPolicyDescription (Prelude.Maybe [Prelude.Text])
networkFirewallPolicyDescription_statelessCustomActions = Lens.lens (\NetworkFirewallPolicyDescription' {statelessCustomActions} -> statelessCustomActions) (\s@NetworkFirewallPolicyDescription' {} a -> s {statelessCustomActions = a} :: NetworkFirewallPolicyDescription) Prelude.. Lens.mapping Lens.coerced

-- | The default actions to take on a packet that doesn\'t match any stateful
-- rules. The stateful default action is optional, and is only valid when
-- using the strict rule order.
--
-- Valid values of the stateful default action:
--
-- -   aws:drop_strict
--
-- -   aws:drop_established
--
-- -   aws:alert_strict
--
-- -   aws:alert_established
networkFirewallPolicyDescription_statefulDefaultActions :: Lens.Lens' NetworkFirewallPolicyDescription (Prelude.Maybe [Prelude.Text])
networkFirewallPolicyDescription_statefulDefaultActions = Lens.lens (\NetworkFirewallPolicyDescription' {statefulDefaultActions} -> statefulDefaultActions) (\s@NetworkFirewallPolicyDescription' {} a -> s {statefulDefaultActions = a} :: NetworkFirewallPolicyDescription) Prelude.. Lens.mapping Lens.coerced

-- | The actions to take on packets that don\'t match any of the stateless
-- rule groups.
networkFirewallPolicyDescription_statelessDefaultActions :: Lens.Lens' NetworkFirewallPolicyDescription (Prelude.Maybe [Prelude.Text])
networkFirewallPolicyDescription_statelessDefaultActions = Lens.lens (\NetworkFirewallPolicyDescription' {statelessDefaultActions} -> statelessDefaultActions) (\s@NetworkFirewallPolicyDescription' {} a -> s {statelessDefaultActions = a} :: NetworkFirewallPolicyDescription) Prelude.. Lens.mapping Lens.coerced

-- | The stateless rule groups that are used in the Network Firewall firewall
-- policy.
networkFirewallPolicyDescription_statelessRuleGroups :: Lens.Lens' NetworkFirewallPolicyDescription (Prelude.Maybe [StatelessRuleGroup])
networkFirewallPolicyDescription_statelessRuleGroups = Lens.lens (\NetworkFirewallPolicyDescription' {statelessRuleGroups} -> statelessRuleGroups) (\s@NetworkFirewallPolicyDescription' {} a -> s {statelessRuleGroups = a} :: NetworkFirewallPolicyDescription) Prelude.. Lens.mapping Lens.coerced

-- | The actions to take on packet fragments that don\'t match any of the
-- stateless rule groups.
networkFirewallPolicyDescription_statelessFragmentDefaultActions :: Lens.Lens' NetworkFirewallPolicyDescription (Prelude.Maybe [Prelude.Text])
networkFirewallPolicyDescription_statelessFragmentDefaultActions = Lens.lens (\NetworkFirewallPolicyDescription' {statelessFragmentDefaultActions} -> statelessFragmentDefaultActions) (\s@NetworkFirewallPolicyDescription' {} a -> s {statelessFragmentDefaultActions = a} :: NetworkFirewallPolicyDescription) Prelude.. Lens.mapping Lens.coerced

-- | The stateful rule groups that are used in the Network Firewall firewall
-- policy.
networkFirewallPolicyDescription_statefulRuleGroups :: Lens.Lens' NetworkFirewallPolicyDescription (Prelude.Maybe [StatefulRuleGroup])
networkFirewallPolicyDescription_statefulRuleGroups = Lens.lens (\NetworkFirewallPolicyDescription' {statefulRuleGroups} -> statefulRuleGroups) (\s@NetworkFirewallPolicyDescription' {} a -> s {statefulRuleGroups = a} :: NetworkFirewallPolicyDescription) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    NetworkFirewallPolicyDescription
  where
  parseJSON =
    Data.withObject
      "NetworkFirewallPolicyDescription"
      ( \x ->
          NetworkFirewallPolicyDescription'
            Prelude.<$> (x Data..:? "StatefulEngineOptions")
            Prelude.<*> ( x Data..:? "StatelessCustomActions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "StatefulDefaultActions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "StatelessDefaultActions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "StatelessRuleGroups"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "StatelessFragmentDefaultActions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "StatefulRuleGroups"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    NetworkFirewallPolicyDescription
  where
  hashWithSalt
    _salt
    NetworkFirewallPolicyDescription' {..} =
      _salt `Prelude.hashWithSalt` statefulEngineOptions
        `Prelude.hashWithSalt` statelessCustomActions
        `Prelude.hashWithSalt` statefulDefaultActions
        `Prelude.hashWithSalt` statelessDefaultActions
        `Prelude.hashWithSalt` statelessRuleGroups
        `Prelude.hashWithSalt` statelessFragmentDefaultActions
        `Prelude.hashWithSalt` statefulRuleGroups

instance
  Prelude.NFData
    NetworkFirewallPolicyDescription
  where
  rnf NetworkFirewallPolicyDescription' {..} =
    Prelude.rnf statefulEngineOptions
      `Prelude.seq` Prelude.rnf statelessCustomActions
      `Prelude.seq` Prelude.rnf statefulDefaultActions
      `Prelude.seq` Prelude.rnf statelessDefaultActions
      `Prelude.seq` Prelude.rnf statelessRuleGroups
      `Prelude.seq` Prelude.rnf statelessFragmentDefaultActions
      `Prelude.seq` Prelude.rnf statefulRuleGroups
