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
-- Module      : Amazonka.Route53RecoveryControlConfig.Types.Rule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryControlConfig.Types.Rule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53RecoveryControlConfig.Types.AssertionRule
import Amazonka.Route53RecoveryControlConfig.Types.GatingRule

-- | A safety rule. A safety rule can be an assertion rule or a gating rule.
--
-- /See:/ 'newRule' smart constructor.
data Rule = Rule'
  { -- | An assertion rule enforces that, when a routing control state is
    -- changed, the criteria set by the rule configuration is met. Otherwise,
    -- the change to the routing control is not accepted.
    assertion :: Prelude.Maybe AssertionRule,
    -- | A gating rule verifies that a set of gating controls evaluates as true,
    -- based on a rule configuration that you specify. If the gating rule
    -- evaluates to true, Amazon Route 53 Application Recovery Controller
    -- allows a set of routing control state changes to run and complete
    -- against the set of target controls.
    gating :: Prelude.Maybe GatingRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Rule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assertion', 'rule_assertion' - An assertion rule enforces that, when a routing control state is
-- changed, the criteria set by the rule configuration is met. Otherwise,
-- the change to the routing control is not accepted.
--
-- 'gating', 'rule_gating' - A gating rule verifies that a set of gating controls evaluates as true,
-- based on a rule configuration that you specify. If the gating rule
-- evaluates to true, Amazon Route 53 Application Recovery Controller
-- allows a set of routing control state changes to run and complete
-- against the set of target controls.
newRule ::
  Rule
newRule =
  Rule'
    { assertion = Prelude.Nothing,
      gating = Prelude.Nothing
    }

-- | An assertion rule enforces that, when a routing control state is
-- changed, the criteria set by the rule configuration is met. Otherwise,
-- the change to the routing control is not accepted.
rule_assertion :: Lens.Lens' Rule (Prelude.Maybe AssertionRule)
rule_assertion = Lens.lens (\Rule' {assertion} -> assertion) (\s@Rule' {} a -> s {assertion = a} :: Rule)

-- | A gating rule verifies that a set of gating controls evaluates as true,
-- based on a rule configuration that you specify. If the gating rule
-- evaluates to true, Amazon Route 53 Application Recovery Controller
-- allows a set of routing control state changes to run and complete
-- against the set of target controls.
rule_gating :: Lens.Lens' Rule (Prelude.Maybe GatingRule)
rule_gating = Lens.lens (\Rule' {gating} -> gating) (\s@Rule' {} a -> s {gating = a} :: Rule)

instance Core.FromJSON Rule where
  parseJSON =
    Core.withObject
      "Rule"
      ( \x ->
          Rule'
            Prelude.<$> (x Core..:? "ASSERTION")
            Prelude.<*> (x Core..:? "GATING")
      )

instance Prelude.Hashable Rule where
  hashWithSalt _salt Rule' {..} =
    _salt `Prelude.hashWithSalt` assertion
      `Prelude.hashWithSalt` gating

instance Prelude.NFData Rule where
  rnf Rule' {..} =
    Prelude.rnf assertion
      `Prelude.seq` Prelude.rnf gating
