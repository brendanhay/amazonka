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
-- Module      : Amazonka.VPCLattice.Types.RuleAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.RuleAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VPCLattice.Types.FixedResponseAction
import Amazonka.VPCLattice.Types.ForwardAction

-- | Describes the action for a rule. Each rule must include exactly one of
-- the following types of actions: @forward @or @fixed-response@, and it
-- must be the last action to be performed.
--
-- /See:/ 'newRuleAction' smart constructor.
data RuleAction = RuleAction'
  { -- | Describes the rule action that returns a custom HTTP response.
    fixedResponse :: Prelude.Maybe FixedResponseAction,
    -- | The forward action. Traffic that matches the rule is forwarded to the
    -- specified target groups.
    forward :: Prelude.Maybe ForwardAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fixedResponse', 'ruleAction_fixedResponse' - Describes the rule action that returns a custom HTTP response.
--
-- 'forward', 'ruleAction_forward' - The forward action. Traffic that matches the rule is forwarded to the
-- specified target groups.
newRuleAction ::
  RuleAction
newRuleAction =
  RuleAction'
    { fixedResponse = Prelude.Nothing,
      forward = Prelude.Nothing
    }

-- | Describes the rule action that returns a custom HTTP response.
ruleAction_fixedResponse :: Lens.Lens' RuleAction (Prelude.Maybe FixedResponseAction)
ruleAction_fixedResponse = Lens.lens (\RuleAction' {fixedResponse} -> fixedResponse) (\s@RuleAction' {} a -> s {fixedResponse = a} :: RuleAction)

-- | The forward action. Traffic that matches the rule is forwarded to the
-- specified target groups.
ruleAction_forward :: Lens.Lens' RuleAction (Prelude.Maybe ForwardAction)
ruleAction_forward = Lens.lens (\RuleAction' {forward} -> forward) (\s@RuleAction' {} a -> s {forward = a} :: RuleAction)

instance Data.FromJSON RuleAction where
  parseJSON =
    Data.withObject
      "RuleAction"
      ( \x ->
          RuleAction'
            Prelude.<$> (x Data..:? "fixedResponse")
            Prelude.<*> (x Data..:? "forward")
      )

instance Prelude.Hashable RuleAction where
  hashWithSalt _salt RuleAction' {..} =
    _salt
      `Prelude.hashWithSalt` fixedResponse
      `Prelude.hashWithSalt` forward

instance Prelude.NFData RuleAction where
  rnf RuleAction' {..} =
    Prelude.rnf fixedResponse
      `Prelude.seq` Prelude.rnf forward

instance Data.ToJSON RuleAction where
  toJSON RuleAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("fixedResponse" Data..=) Prelude.<$> fixedResponse,
            ("forward" Data..=) Prelude.<$> forward
          ]
      )
