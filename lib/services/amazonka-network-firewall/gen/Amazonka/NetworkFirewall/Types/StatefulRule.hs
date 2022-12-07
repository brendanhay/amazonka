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
-- Module      : Amazonka.NetworkFirewall.Types.StatefulRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.StatefulRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.Header
import Amazonka.NetworkFirewall.Types.RuleOption
import Amazonka.NetworkFirewall.Types.StatefulAction
import qualified Amazonka.Prelude as Prelude

-- | A single Suricata rules specification, for use in a stateful rule group.
-- Use this option to specify a simple Suricata rule with protocol, source
-- and destination, ports, direction, and rule options. For information
-- about the Suricata @Rules@ format, see
-- <https://suricata.readthedocs.io/rules/intro.html# Rules Format>.
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

instance Data.FromJSON StatefulRule where
  parseJSON =
    Data.withObject
      "StatefulRule"
      ( \x ->
          StatefulRule'
            Prelude.<$> (x Data..: "Action")
            Prelude.<*> (x Data..: "Header")
            Prelude.<*> (x Data..:? "RuleOptions" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable StatefulRule where
  hashWithSalt _salt StatefulRule' {..} =
    _salt `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` header
      `Prelude.hashWithSalt` ruleOptions

instance Prelude.NFData StatefulRule where
  rnf StatefulRule' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf header
      `Prelude.seq` Prelude.rnf ruleOptions

instance Data.ToJSON StatefulRule where
  toJSON StatefulRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Action" Data..= action),
            Prelude.Just ("Header" Data..= header),
            Prelude.Just ("RuleOptions" Data..= ruleOptions)
          ]
      )
