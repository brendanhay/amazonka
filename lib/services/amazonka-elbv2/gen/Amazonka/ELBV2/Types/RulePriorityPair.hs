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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.RulePriorityPair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the priorities for the rules for a listener.
--
-- /See:/ 'newRulePriorityPair' smart constructor.
data RulePriorityPair = RulePriorityPair'
  { -- | The rule priority.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the rule.
    ruleArn :: Prelude.Maybe Prelude.Text
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
-- 'priority', 'rulePriorityPair_priority' - The rule priority.
--
-- 'ruleArn', 'rulePriorityPair_ruleArn' - The Amazon Resource Name (ARN) of the rule.
newRulePriorityPair ::
  RulePriorityPair
newRulePriorityPair =
  RulePriorityPair'
    { priority = Prelude.Nothing,
      ruleArn = Prelude.Nothing
    }

-- | The rule priority.
rulePriorityPair_priority :: Lens.Lens' RulePriorityPair (Prelude.Maybe Prelude.Natural)
rulePriorityPair_priority = Lens.lens (\RulePriorityPair' {priority} -> priority) (\s@RulePriorityPair' {} a -> s {priority = a} :: RulePriorityPair)

-- | The Amazon Resource Name (ARN) of the rule.
rulePriorityPair_ruleArn :: Lens.Lens' RulePriorityPair (Prelude.Maybe Prelude.Text)
rulePriorityPair_ruleArn = Lens.lens (\RulePriorityPair' {ruleArn} -> ruleArn) (\s@RulePriorityPair' {} a -> s {ruleArn = a} :: RulePriorityPair)

instance Prelude.Hashable RulePriorityPair where
  hashWithSalt _salt RulePriorityPair' {..} =
    _salt
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` ruleArn

instance Prelude.NFData RulePriorityPair where
  rnf RulePriorityPair' {..} =
    Prelude.rnf priority
      `Prelude.seq` Prelude.rnf ruleArn

instance Data.ToQuery RulePriorityPair where
  toQuery RulePriorityPair' {..} =
    Prelude.mconcat
      [ "Priority" Data.=: priority,
        "RuleArn" Data.=: ruleArn
      ]
