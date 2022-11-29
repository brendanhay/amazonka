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
-- Module      : Amazonka.ELBV2.Types.RulePriorityPair
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.RulePriorityPair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the priorities for the rules for a listener.
--
-- /See:/ 'newRulePriorityPair' smart constructor.
data RulePriorityPair = RulePriorityPair'
  { -- | The Amazon Resource Name (ARN) of the rule.
    ruleArn :: Prelude.Maybe Prelude.Text,
    -- | The rule priority.
    priority :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RulePriorityPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleArn', 'rulePriorityPair_ruleArn' - The Amazon Resource Name (ARN) of the rule.
--
-- 'priority', 'rulePriorityPair_priority' - The rule priority.
newRulePriorityPair ::
  RulePriorityPair
newRulePriorityPair =
  RulePriorityPair'
    { ruleArn = Prelude.Nothing,
      priority = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the rule.
rulePriorityPair_ruleArn :: Lens.Lens' RulePriorityPair (Prelude.Maybe Prelude.Text)
rulePriorityPair_ruleArn = Lens.lens (\RulePriorityPair' {ruleArn} -> ruleArn) (\s@RulePriorityPair' {} a -> s {ruleArn = a} :: RulePriorityPair)

-- | The rule priority.
rulePriorityPair_priority :: Lens.Lens' RulePriorityPair (Prelude.Maybe Prelude.Natural)
rulePriorityPair_priority = Lens.lens (\RulePriorityPair' {priority} -> priority) (\s@RulePriorityPair' {} a -> s {priority = a} :: RulePriorityPair)

instance Prelude.Hashable RulePriorityPair where
  hashWithSalt _salt RulePriorityPair' {..} =
    _salt `Prelude.hashWithSalt` ruleArn
      `Prelude.hashWithSalt` priority

instance Prelude.NFData RulePriorityPair where
  rnf RulePriorityPair' {..} =
    Prelude.rnf ruleArn
      `Prelude.seq` Prelude.rnf priority

instance Core.ToQuery RulePriorityPair where
  toQuery RulePriorityPair' {..} =
    Prelude.mconcat
      [ "RuleArn" Core.=: ruleArn,
        "Priority" Core.=: priority
      ]
