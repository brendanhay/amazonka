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
-- Module      : Amazonka.WAFV2.Types.RuleGroupReferenceStatement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.RuleGroupReferenceStatement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.ExcludedRule
import Amazonka.WAFV2.Types.RuleActionOverride

-- | A rule statement used to run the rules that are defined in a RuleGroup.
-- To use this, create a rule group with your rules, then provide the ARN
-- of the rule group in this statement.
--
-- You cannot nest a @RuleGroupReferenceStatement@, for example for use
-- inside a @NotStatement@ or @OrStatement@. You can only use a rule group
-- reference statement at the top level inside a web ACL.
--
-- /See:/ 'newRuleGroupReferenceStatement' smart constructor.
data RuleGroupReferenceStatement = RuleGroupReferenceStatement'
  { -- | Rules in the referenced rule group whose actions are set to @Count@.
    --
    -- Instead of this option, use @RuleActionOverrides@. It accepts any valid
    -- action setting, including @Count@.
    excludedRules :: Prelude.Maybe [ExcludedRule],
    -- | Action settings to use in the place of the rule actions that are
    -- configured inside the rule group. You specify one override for each rule
    -- whose action you want to change.
    --
    -- You can use overrides for testing, for example you can override all of
    -- rule actions to @Count@ and then monitor the resulting count metrics to
    -- understand how the rule group would handle your web traffic. You can
    -- also permanently override some or all actions, to modify how the rule
    -- group manages your web traffic.
    ruleActionOverrides :: Prelude.Maybe (Prelude.NonEmpty RuleActionOverride),
    -- | The Amazon Resource Name (ARN) of the entity.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupReferenceStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excludedRules', 'ruleGroupReferenceStatement_excludedRules' - Rules in the referenced rule group whose actions are set to @Count@.
--
-- Instead of this option, use @RuleActionOverrides@. It accepts any valid
-- action setting, including @Count@.
--
-- 'ruleActionOverrides', 'ruleGroupReferenceStatement_ruleActionOverrides' - Action settings to use in the place of the rule actions that are
-- configured inside the rule group. You specify one override for each rule
-- whose action you want to change.
--
-- You can use overrides for testing, for example you can override all of
-- rule actions to @Count@ and then monitor the resulting count metrics to
-- understand how the rule group would handle your web traffic. You can
-- also permanently override some or all actions, to modify how the rule
-- group manages your web traffic.
--
-- 'arn', 'ruleGroupReferenceStatement_arn' - The Amazon Resource Name (ARN) of the entity.
newRuleGroupReferenceStatement ::
  -- | 'arn'
  Prelude.Text ->
  RuleGroupReferenceStatement
newRuleGroupReferenceStatement pARN_ =
  RuleGroupReferenceStatement'
    { excludedRules =
        Prelude.Nothing,
      ruleActionOverrides = Prelude.Nothing,
      arn = pARN_
    }

-- | Rules in the referenced rule group whose actions are set to @Count@.
--
-- Instead of this option, use @RuleActionOverrides@. It accepts any valid
-- action setting, including @Count@.
ruleGroupReferenceStatement_excludedRules :: Lens.Lens' RuleGroupReferenceStatement (Prelude.Maybe [ExcludedRule])
ruleGroupReferenceStatement_excludedRules = Lens.lens (\RuleGroupReferenceStatement' {excludedRules} -> excludedRules) (\s@RuleGroupReferenceStatement' {} a -> s {excludedRules = a} :: RuleGroupReferenceStatement) Prelude.. Lens.mapping Lens.coerced

-- | Action settings to use in the place of the rule actions that are
-- configured inside the rule group. You specify one override for each rule
-- whose action you want to change.
--
-- You can use overrides for testing, for example you can override all of
-- rule actions to @Count@ and then monitor the resulting count metrics to
-- understand how the rule group would handle your web traffic. You can
-- also permanently override some or all actions, to modify how the rule
-- group manages your web traffic.
ruleGroupReferenceStatement_ruleActionOverrides :: Lens.Lens' RuleGroupReferenceStatement (Prelude.Maybe (Prelude.NonEmpty RuleActionOverride))
ruleGroupReferenceStatement_ruleActionOverrides = Lens.lens (\RuleGroupReferenceStatement' {ruleActionOverrides} -> ruleActionOverrides) (\s@RuleGroupReferenceStatement' {} a -> s {ruleActionOverrides = a} :: RuleGroupReferenceStatement) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the entity.
ruleGroupReferenceStatement_arn :: Lens.Lens' RuleGroupReferenceStatement Prelude.Text
ruleGroupReferenceStatement_arn = Lens.lens (\RuleGroupReferenceStatement' {arn} -> arn) (\s@RuleGroupReferenceStatement' {} a -> s {arn = a} :: RuleGroupReferenceStatement)

instance Data.FromJSON RuleGroupReferenceStatement where
  parseJSON =
    Data.withObject
      "RuleGroupReferenceStatement"
      ( \x ->
          RuleGroupReferenceStatement'
            Prelude.<$> (x Data..:? "ExcludedRules" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "RuleActionOverrides")
            Prelude.<*> (x Data..: "ARN")
      )

instance Prelude.Hashable RuleGroupReferenceStatement where
  hashWithSalt _salt RuleGroupReferenceStatement' {..} =
    _salt `Prelude.hashWithSalt` excludedRules
      `Prelude.hashWithSalt` ruleActionOverrides
      `Prelude.hashWithSalt` arn

instance Prelude.NFData RuleGroupReferenceStatement where
  rnf RuleGroupReferenceStatement' {..} =
    Prelude.rnf excludedRules
      `Prelude.seq` Prelude.rnf ruleActionOverrides
      `Prelude.seq` Prelude.rnf arn

instance Data.ToJSON RuleGroupReferenceStatement where
  toJSON RuleGroupReferenceStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExcludedRules" Data..=) Prelude.<$> excludedRules,
            ("RuleActionOverrides" Data..=)
              Prelude.<$> ruleActionOverrides,
            Prelude.Just ("ARN" Data..= arn)
          ]
      )
