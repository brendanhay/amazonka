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
-- Module      : Amazonka.NetworkFirewall.Types.StatefulEngineOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.StatefulEngineOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.NetworkFirewall.Types.RuleOrder
import qualified Amazonka.Prelude as Prelude

-- | Configuration settings for the handling of the stateful rule groups in a
-- firewall policy.
--
-- /See:/ 'newStatefulEngineOptions' smart constructor.
data StatefulEngineOptions = StatefulEngineOptions'
  { -- | Indicates how to manage the order of stateful rule evaluation for the
    -- policy. By default, Network Firewall leaves the rule evaluation order up
    -- to the Suricata rule processing engine. If you set this to
    -- @STRICT_ORDER@, your rules are evaluated in the exact order that you
    -- provide them in the policy. With strict ordering, the rule groups are
    -- evaluated by order of priority, starting from the lowest number, and the
    -- rules in each rule group are processed in the order that they\'re
    -- defined.
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
-- policy. By default, Network Firewall leaves the rule evaluation order up
-- to the Suricata rule processing engine. If you set this to
-- @STRICT_ORDER@, your rules are evaluated in the exact order that you
-- provide them in the policy. With strict ordering, the rule groups are
-- evaluated by order of priority, starting from the lowest number, and the
-- rules in each rule group are processed in the order that they\'re
-- defined.
newStatefulEngineOptions ::
  StatefulEngineOptions
newStatefulEngineOptions =
  StatefulEngineOptions' {ruleOrder = Prelude.Nothing}

-- | Indicates how to manage the order of stateful rule evaluation for the
-- policy. By default, Network Firewall leaves the rule evaluation order up
-- to the Suricata rule processing engine. If you set this to
-- @STRICT_ORDER@, your rules are evaluated in the exact order that you
-- provide them in the policy. With strict ordering, the rule groups are
-- evaluated by order of priority, starting from the lowest number, and the
-- rules in each rule group are processed in the order that they\'re
-- defined.
statefulEngineOptions_ruleOrder :: Lens.Lens' StatefulEngineOptions (Prelude.Maybe RuleOrder)
statefulEngineOptions_ruleOrder = Lens.lens (\StatefulEngineOptions' {ruleOrder} -> ruleOrder) (\s@StatefulEngineOptions' {} a -> s {ruleOrder = a} :: StatefulEngineOptions)

instance Core.FromJSON StatefulEngineOptions where
  parseJSON =
    Core.withObject
      "StatefulEngineOptions"
      ( \x ->
          StatefulEngineOptions'
            Prelude.<$> (x Core..:? "RuleOrder")
      )

instance Prelude.Hashable StatefulEngineOptions where
  hashWithSalt salt' StatefulEngineOptions' {..} =
    salt' `Prelude.hashWithSalt` ruleOrder

instance Prelude.NFData StatefulEngineOptions where
  rnf StatefulEngineOptions' {..} =
    Prelude.rnf ruleOrder

instance Core.ToJSON StatefulEngineOptions where
  toJSON StatefulEngineOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [("RuleOrder" Core..=) Prelude.<$> ruleOrder]
      )
