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
-- Module      : Amazonka.FMS.Types.StatefulEngineOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.StatefulEngineOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.RuleOrder
import qualified Amazonka.Prelude as Prelude

-- | Configuration settings for the handling of the stateful rule groups in a
-- Network Firewall firewall policy.
--
-- /See:/ 'newStatefulEngineOptions' smart constructor.
data StatefulEngineOptions = StatefulEngineOptions'
  { -- | Indicates how to manage the order of stateful rule evaluation for the
    -- policy. @DEFAULT_ACTION_ORDER@ is the default behavior. Stateful rules
    -- are provided to the rule engine as Suricata compatible strings, and
    -- Suricata evaluates them based on certain settings. For more information,
    -- see
    -- <https://docs.aws.amazon.com/network-firewall/latest/developerguide/suricata-rule-evaluation-order.html Evaluation order for stateful rules>
    -- in the /Network Firewall Developer Guide/.
    ruleOrder :: Prelude.Maybe RuleOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StatefulEngineOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleOrder', 'statefulEngineOptions_ruleOrder' - Indicates how to manage the order of stateful rule evaluation for the
-- policy. @DEFAULT_ACTION_ORDER@ is the default behavior. Stateful rules
-- are provided to the rule engine as Suricata compatible strings, and
-- Suricata evaluates them based on certain settings. For more information,
-- see
-- <https://docs.aws.amazon.com/network-firewall/latest/developerguide/suricata-rule-evaluation-order.html Evaluation order for stateful rules>
-- in the /Network Firewall Developer Guide/.
newStatefulEngineOptions ::
  StatefulEngineOptions
newStatefulEngineOptions =
  StatefulEngineOptions' {ruleOrder = Prelude.Nothing}

-- | Indicates how to manage the order of stateful rule evaluation for the
-- policy. @DEFAULT_ACTION_ORDER@ is the default behavior. Stateful rules
-- are provided to the rule engine as Suricata compatible strings, and
-- Suricata evaluates them based on certain settings. For more information,
-- see
-- <https://docs.aws.amazon.com/network-firewall/latest/developerguide/suricata-rule-evaluation-order.html Evaluation order for stateful rules>
-- in the /Network Firewall Developer Guide/.
statefulEngineOptions_ruleOrder :: Lens.Lens' StatefulEngineOptions (Prelude.Maybe RuleOrder)
statefulEngineOptions_ruleOrder = Lens.lens (\StatefulEngineOptions' {ruleOrder} -> ruleOrder) (\s@StatefulEngineOptions' {} a -> s {ruleOrder = a} :: StatefulEngineOptions)

instance Data.FromJSON StatefulEngineOptions where
  parseJSON =
    Data.withObject
      "StatefulEngineOptions"
      ( \x ->
          StatefulEngineOptions'
            Prelude.<$> (x Data..:? "RuleOrder")
      )

instance Prelude.Hashable StatefulEngineOptions where
  hashWithSalt _salt StatefulEngineOptions' {..} =
    _salt `Prelude.hashWithSalt` ruleOrder

instance Prelude.NFData StatefulEngineOptions where
  rnf StatefulEngineOptions' {..} =
    Prelude.rnf ruleOrder
