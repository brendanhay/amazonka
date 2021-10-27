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
-- Module      : Network.AWS.NetworkFirewall.Types.StatefulRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.NetworkFirewall.Types.StatefulRule where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.NetworkFirewall.Types.Header
import Network.AWS.NetworkFirewall.Types.RuleOption
import Network.AWS.NetworkFirewall.Types.StatefulAction
import qualified Network.AWS.Prelude as Prelude

-- | A single Suricata rules specification, for use in a stateful rule group.
-- Use this option to specify a simple Suricata rule with protocol, source
-- and destination, ports, direction, and rule options. For information
-- about the Suricata @Rules@ format, see
-- <https://suricata.readthedocs.io/en/suricata-5.0.0/rules/intro.html# Rules Format>.
--
-- /See:/ 'newStatefulRule' smart constructor.
data StatefulRule = StatefulRule'
  { -- | Defines what Network Firewall should do with the packets in a traffic
    -- flow when the flow matches the stateful rule criteria. For all actions,
    -- Network Firewall performs the specified action and discontinues stateful
    -- inspection of the traffic flow.
    --
    -- The actions for a stateful rule are defined as follows:
    --
    -- -   __PASS__ - Permits the packets to go to the intended destination.
    --
    -- -   __DROP__ - Blocks the packets from going to the intended destination
    --     and sends an alert log message, if alert logging is configured in
    --     the Firewall LoggingConfiguration.
    --
    -- -   __ALERT__ - Permits the packets to go to the intended destination
    --     and sends an alert log message, if alert logging is configured in
    --     the Firewall LoggingConfiguration.
    --
    --     You can use this action to test a rule that you intend to use to
    --     drop traffic. You can enable the rule with @ALERT@ action, verify in
    --     the logs that the rule is filtering as you want, then change the
    --     action to @DROP@.
    action :: StatefulAction,
    -- | The stateful inspection criteria for this rule, used to inspect traffic
    -- flows.
    header :: Header,
    -- | Additional options for the rule. These are the Suricata @RuleOptions@
    -- settings.
    ruleOptions :: [RuleOption]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StatefulRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'statefulRule_action' - Defines what Network Firewall should do with the packets in a traffic
-- flow when the flow matches the stateful rule criteria. For all actions,
-- Network Firewall performs the specified action and discontinues stateful
-- inspection of the traffic flow.
--
-- The actions for a stateful rule are defined as follows:
--
-- -   __PASS__ - Permits the packets to go to the intended destination.
--
-- -   __DROP__ - Blocks the packets from going to the intended destination
--     and sends an alert log message, if alert logging is configured in
--     the Firewall LoggingConfiguration.
--
-- -   __ALERT__ - Permits the packets to go to the intended destination
--     and sends an alert log message, if alert logging is configured in
--     the Firewall LoggingConfiguration.
--
--     You can use this action to test a rule that you intend to use to
--     drop traffic. You can enable the rule with @ALERT@ action, verify in
--     the logs that the rule is filtering as you want, then change the
--     action to @DROP@.
--
-- 'header', 'statefulRule_header' - The stateful inspection criteria for this rule, used to inspect traffic
-- flows.
--
-- 'ruleOptions', 'statefulRule_ruleOptions' - Additional options for the rule. These are the Suricata @RuleOptions@
-- settings.
newStatefulRule ::
  -- | 'action'
  StatefulAction ->
  -- | 'header'
  Header ->
  StatefulRule
newStatefulRule pAction_ pHeader_ =
  StatefulRule'
    { action = pAction_,
      header = pHeader_,
      ruleOptions = Prelude.mempty
    }

-- | Defines what Network Firewall should do with the packets in a traffic
-- flow when the flow matches the stateful rule criteria. For all actions,
-- Network Firewall performs the specified action and discontinues stateful
-- inspection of the traffic flow.
--
-- The actions for a stateful rule are defined as follows:
--
-- -   __PASS__ - Permits the packets to go to the intended destination.
--
-- -   __DROP__ - Blocks the packets from going to the intended destination
--     and sends an alert log message, if alert logging is configured in
--     the Firewall LoggingConfiguration.
--
-- -   __ALERT__ - Permits the packets to go to the intended destination
--     and sends an alert log message, if alert logging is configured in
--     the Firewall LoggingConfiguration.
--
--     You can use this action to test a rule that you intend to use to
--     drop traffic. You can enable the rule with @ALERT@ action, verify in
--     the logs that the rule is filtering as you want, then change the
--     action to @DROP@.
statefulRule_action :: Lens.Lens' StatefulRule StatefulAction
statefulRule_action = Lens.lens (\StatefulRule' {action} -> action) (\s@StatefulRule' {} a -> s {action = a} :: StatefulRule)

-- | The stateful inspection criteria for this rule, used to inspect traffic
-- flows.
statefulRule_header :: Lens.Lens' StatefulRule Header
statefulRule_header = Lens.lens (\StatefulRule' {header} -> header) (\s@StatefulRule' {} a -> s {header = a} :: StatefulRule)

-- | Additional options for the rule. These are the Suricata @RuleOptions@
-- settings.
statefulRule_ruleOptions :: Lens.Lens' StatefulRule [RuleOption]
statefulRule_ruleOptions = Lens.lens (\StatefulRule' {ruleOptions} -> ruleOptions) (\s@StatefulRule' {} a -> s {ruleOptions = a} :: StatefulRule) Prelude.. Lens.coerced

instance Core.FromJSON StatefulRule where
  parseJSON =
    Core.withObject
      "StatefulRule"
      ( \x ->
          StatefulRule'
            Prelude.<$> (x Core..: "Action")
            Prelude.<*> (x Core..: "Header")
            Prelude.<*> (x Core..:? "RuleOptions" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable StatefulRule

instance Prelude.NFData StatefulRule

instance Core.ToJSON StatefulRule where
  toJSON StatefulRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Action" Core..= action),
            Prelude.Just ("Header" Core..= header),
            Prelude.Just ("RuleOptions" Core..= ruleOptions)
          ]
      )
