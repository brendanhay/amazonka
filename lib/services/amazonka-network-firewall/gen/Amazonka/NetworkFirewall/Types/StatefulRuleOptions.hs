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
-- Module      : Amazonka.NetworkFirewall.Types.StatefulRuleOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.StatefulRuleOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.RuleOrder
import qualified Amazonka.Prelude as Prelude

-- | Additional options governing how Network Firewall handles the rule
-- group. You can only use these for stateful rule groups.
--
-- /See:/ 'newStatefulRuleOptions' smart constructor.
data StatefulRuleOptions = StatefulRuleOptions'
  { -- | Indicates how to manage the order of the rule evaluation for the rule
    -- group. @DEFAULT_ACTION_ORDER@ is the default behavior. Stateful rules
    -- are provided to the rule engine as Suricata compatible strings, and
    -- Suricata evaluates them based on certain settings. For more information,
    -- see
    -- <https://docs.aws.amazon.com/network-firewall/latest/developerguide/suricata-rule-evaluation-order.html Evaluation order for stateful rules>
    -- in the /Network Firewall Developer Guide/.
    ruleOrder :: Prelude.Maybe RuleOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StatefulRuleOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleOrder', 'statefulRuleOptions_ruleOrder' - Indicates how to manage the order of the rule evaluation for the rule
-- group. @DEFAULT_ACTION_ORDER@ is the default behavior. Stateful rules
-- are provided to the rule engine as Suricata compatible strings, and
-- Suricata evaluates them based on certain settings. For more information,
-- see
-- <https://docs.aws.amazon.com/network-firewall/latest/developerguide/suricata-rule-evaluation-order.html Evaluation order for stateful rules>
-- in the /Network Firewall Developer Guide/.
newStatefulRuleOptions ::
  StatefulRuleOptions
newStatefulRuleOptions =
  StatefulRuleOptions' {ruleOrder = Prelude.Nothing}

-- | Indicates how to manage the order of the rule evaluation for the rule
-- group. @DEFAULT_ACTION_ORDER@ is the default behavior. Stateful rules
-- are provided to the rule engine as Suricata compatible strings, and
-- Suricata evaluates them based on certain settings. For more information,
-- see
-- <https://docs.aws.amazon.com/network-firewall/latest/developerguide/suricata-rule-evaluation-order.html Evaluation order for stateful rules>
-- in the /Network Firewall Developer Guide/.
statefulRuleOptions_ruleOrder :: Lens.Lens' StatefulRuleOptions (Prelude.Maybe RuleOrder)
statefulRuleOptions_ruleOrder = Lens.lens (\StatefulRuleOptions' {ruleOrder} -> ruleOrder) (\s@StatefulRuleOptions' {} a -> s {ruleOrder = a} :: StatefulRuleOptions)

instance Data.FromJSON StatefulRuleOptions where
  parseJSON =
    Data.withObject
      "StatefulRuleOptions"
      ( \x ->
          StatefulRuleOptions'
            Prelude.<$> (x Data..:? "RuleOrder")
      )

instance Prelude.Hashable StatefulRuleOptions where
  hashWithSalt _salt StatefulRuleOptions' {..} =
    _salt `Prelude.hashWithSalt` ruleOrder

instance Prelude.NFData StatefulRuleOptions where
  rnf StatefulRuleOptions' {..} = Prelude.rnf ruleOrder

instance Data.ToJSON StatefulRuleOptions where
  toJSON StatefulRuleOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [("RuleOrder" Data..=) Prelude.<$> ruleOrder]
      )
