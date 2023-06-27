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
-- Module      : Amazonka.VPCLattice.Types.RuleUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.RuleUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VPCLattice.Types.RuleAction
import Amazonka.VPCLattice.Types.RuleMatch

-- | Represents an object when updating a rule.
--
-- /See:/ 'newRuleUpdate' smart constructor.
data RuleUpdate = RuleUpdate'
  { -- | The rule action.
    action :: Prelude.Maybe RuleAction,
    -- | The rule match.
    match :: Prelude.Maybe RuleMatch,
    -- | The rule priority. A listener can\'t have multiple rules with the same
    -- priority.
    priority :: Prelude.Maybe Prelude.Natural,
    -- | The ID or Amazon Resource Name (ARN) of the rule.
    ruleIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'ruleUpdate_action' - The rule action.
--
-- 'match', 'ruleUpdate_match' - The rule match.
--
-- 'priority', 'ruleUpdate_priority' - The rule priority. A listener can\'t have multiple rules with the same
-- priority.
--
-- 'ruleIdentifier', 'ruleUpdate_ruleIdentifier' - The ID or Amazon Resource Name (ARN) of the rule.
newRuleUpdate ::
  -- | 'ruleIdentifier'
  Prelude.Text ->
  RuleUpdate
newRuleUpdate pRuleIdentifier_ =
  RuleUpdate'
    { action = Prelude.Nothing,
      match = Prelude.Nothing,
      priority = Prelude.Nothing,
      ruleIdentifier = pRuleIdentifier_
    }

-- | The rule action.
ruleUpdate_action :: Lens.Lens' RuleUpdate (Prelude.Maybe RuleAction)
ruleUpdate_action = Lens.lens (\RuleUpdate' {action} -> action) (\s@RuleUpdate' {} a -> s {action = a} :: RuleUpdate)

-- | The rule match.
ruleUpdate_match :: Lens.Lens' RuleUpdate (Prelude.Maybe RuleMatch)
ruleUpdate_match = Lens.lens (\RuleUpdate' {match} -> match) (\s@RuleUpdate' {} a -> s {match = a} :: RuleUpdate)

-- | The rule priority. A listener can\'t have multiple rules with the same
-- priority.
ruleUpdate_priority :: Lens.Lens' RuleUpdate (Prelude.Maybe Prelude.Natural)
ruleUpdate_priority = Lens.lens (\RuleUpdate' {priority} -> priority) (\s@RuleUpdate' {} a -> s {priority = a} :: RuleUpdate)

-- | The ID or Amazon Resource Name (ARN) of the rule.
ruleUpdate_ruleIdentifier :: Lens.Lens' RuleUpdate Prelude.Text
ruleUpdate_ruleIdentifier = Lens.lens (\RuleUpdate' {ruleIdentifier} -> ruleIdentifier) (\s@RuleUpdate' {} a -> s {ruleIdentifier = a} :: RuleUpdate)

instance Prelude.Hashable RuleUpdate where
  hashWithSalt _salt RuleUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` match
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` ruleIdentifier

instance Prelude.NFData RuleUpdate where
  rnf RuleUpdate' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf match
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf ruleIdentifier

instance Data.ToJSON RuleUpdate where
  toJSON RuleUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("action" Data..=) Prelude.<$> action,
            ("match" Data..=) Prelude.<$> match,
            ("priority" Data..=) Prelude.<$> priority,
            Prelude.Just
              ("ruleIdentifier" Data..= ruleIdentifier)
          ]
      )
