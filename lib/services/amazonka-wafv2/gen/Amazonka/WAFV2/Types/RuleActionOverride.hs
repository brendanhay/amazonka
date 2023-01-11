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
-- Module      : Amazonka.WAFV2.Types.RuleActionOverride
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.RuleActionOverride where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.RuleAction

-- | Action setting to use in the place of a rule action that is configured
-- inside the rule group. You specify one override for each rule whose
-- action you want to change.
--
-- You can use overrides for testing, for example you can override all of
-- rule actions to @Count@ and then monitor the resulting count metrics to
-- understand how the rule group would handle your web traffic. You can
-- also permanently override some or all actions, to modify how the rule
-- group manages your web traffic.
--
-- /See:/ 'newRuleActionOverride' smart constructor.
data RuleActionOverride = RuleActionOverride'
  { -- | The name of the rule to override.
    name :: Prelude.Text,
    -- | The override action to use, in place of the configured action of the
    -- rule in the rule group.
    actionToUse :: RuleAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleActionOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'ruleActionOverride_name' - The name of the rule to override.
--
-- 'actionToUse', 'ruleActionOverride_actionToUse' - The override action to use, in place of the configured action of the
-- rule in the rule group.
newRuleActionOverride ::
  -- | 'name'
  Prelude.Text ->
  -- | 'actionToUse'
  RuleAction ->
  RuleActionOverride
newRuleActionOverride pName_ pActionToUse_ =
  RuleActionOverride'
    { name = pName_,
      actionToUse = pActionToUse_
    }

-- | The name of the rule to override.
ruleActionOverride_name :: Lens.Lens' RuleActionOverride Prelude.Text
ruleActionOverride_name = Lens.lens (\RuleActionOverride' {name} -> name) (\s@RuleActionOverride' {} a -> s {name = a} :: RuleActionOverride)

-- | The override action to use, in place of the configured action of the
-- rule in the rule group.
ruleActionOverride_actionToUse :: Lens.Lens' RuleActionOverride RuleAction
ruleActionOverride_actionToUse = Lens.lens (\RuleActionOverride' {actionToUse} -> actionToUse) (\s@RuleActionOverride' {} a -> s {actionToUse = a} :: RuleActionOverride)

instance Data.FromJSON RuleActionOverride where
  parseJSON =
    Data.withObject
      "RuleActionOverride"
      ( \x ->
          RuleActionOverride'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "ActionToUse")
      )

instance Prelude.Hashable RuleActionOverride where
  hashWithSalt _salt RuleActionOverride' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` actionToUse

instance Prelude.NFData RuleActionOverride where
  rnf RuleActionOverride' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf actionToUse

instance Data.ToJSON RuleActionOverride where
  toJSON RuleActionOverride' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("ActionToUse" Data..= actionToUse)
          ]
      )
