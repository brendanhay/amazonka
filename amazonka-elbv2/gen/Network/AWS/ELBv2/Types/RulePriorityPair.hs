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
-- Module      : Network.AWS.ELBv2.Types.RulePriorityPair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.RulePriorityPair where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the priorities for the rules for a listener.
--
-- /See:/ 'newRulePriorityPair' smart constructor.
data RulePriorityPair = RulePriorityPair'
  { -- | The Amazon Resource Name (ARN) of the rule.
    ruleArn :: Core.Maybe Core.Text,
    -- | The rule priority.
    priority :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { ruleArn = Core.Nothing,
      priority = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the rule.
rulePriorityPair_ruleArn :: Lens.Lens' RulePriorityPair (Core.Maybe Core.Text)
rulePriorityPair_ruleArn = Lens.lens (\RulePriorityPair' {ruleArn} -> ruleArn) (\s@RulePriorityPair' {} a -> s {ruleArn = a} :: RulePriorityPair)

-- | The rule priority.
rulePriorityPair_priority :: Lens.Lens' RulePriorityPair (Core.Maybe Core.Natural)
rulePriorityPair_priority = Lens.lens (\RulePriorityPair' {priority} -> priority) (\s@RulePriorityPair' {} a -> s {priority = a} :: RulePriorityPair)

instance Core.Hashable RulePriorityPair

instance Core.NFData RulePriorityPair

instance Core.ToQuery RulePriorityPair where
  toQuery RulePriorityPair' {..} =
    Core.mconcat
      [ "RuleArn" Core.=: ruleArn,
        "Priority" Core.=: priority
      ]
