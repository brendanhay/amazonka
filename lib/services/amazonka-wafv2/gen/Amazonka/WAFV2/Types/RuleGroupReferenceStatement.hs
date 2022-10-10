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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.ExcludedRule

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
  { -- | The rules in the referenced rule group whose actions are set to @Count@.
    -- When you exclude a rule, WAF evaluates it exactly as it would if the
    -- rule action setting were @Count@. This is a useful option for testing
    -- the rules in a rule group without modifying how they handle your web
    -- traffic.
    excludedRules :: Prelude.Maybe [ExcludedRule],
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
-- 'excludedRules', 'ruleGroupReferenceStatement_excludedRules' - The rules in the referenced rule group whose actions are set to @Count@.
-- When you exclude a rule, WAF evaluates it exactly as it would if the
-- rule action setting were @Count@. This is a useful option for testing
-- the rules in a rule group without modifying how they handle your web
-- traffic.
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
      arn = pARN_
    }

-- | The rules in the referenced rule group whose actions are set to @Count@.
-- When you exclude a rule, WAF evaluates it exactly as it would if the
-- rule action setting were @Count@. This is a useful option for testing
-- the rules in a rule group without modifying how they handle your web
-- traffic.
ruleGroupReferenceStatement_excludedRules :: Lens.Lens' RuleGroupReferenceStatement (Prelude.Maybe [ExcludedRule])
ruleGroupReferenceStatement_excludedRules = Lens.lens (\RuleGroupReferenceStatement' {excludedRules} -> excludedRules) (\s@RuleGroupReferenceStatement' {} a -> s {excludedRules = a} :: RuleGroupReferenceStatement) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the entity.
ruleGroupReferenceStatement_arn :: Lens.Lens' RuleGroupReferenceStatement Prelude.Text
ruleGroupReferenceStatement_arn = Lens.lens (\RuleGroupReferenceStatement' {arn} -> arn) (\s@RuleGroupReferenceStatement' {} a -> s {arn = a} :: RuleGroupReferenceStatement)

instance Core.FromJSON RuleGroupReferenceStatement where
  parseJSON =
    Core.withObject
      "RuleGroupReferenceStatement"
      ( \x ->
          RuleGroupReferenceStatement'
            Prelude.<$> (x Core..:? "ExcludedRules" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "ARN")
      )

instance Prelude.Hashable RuleGroupReferenceStatement where
  hashWithSalt _salt RuleGroupReferenceStatement' {..} =
    _salt `Prelude.hashWithSalt` excludedRules
      `Prelude.hashWithSalt` arn

instance Prelude.NFData RuleGroupReferenceStatement where
  rnf RuleGroupReferenceStatement' {..} =
    Prelude.rnf excludedRules
      `Prelude.seq` Prelude.rnf arn

instance Core.ToJSON RuleGroupReferenceStatement where
  toJSON RuleGroupReferenceStatement' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ExcludedRules" Core..=) Prelude.<$> excludedRules,
            Prelude.Just ("ARN" Core..= arn)
          ]
      )
