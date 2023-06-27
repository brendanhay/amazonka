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
-- Module      : Amazonka.SecurityHub.Types.UpdateAutomationRulesRequestItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.UpdateAutomationRulesRequestItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AutomationRulesAction
import Amazonka.SecurityHub.Types.AutomationRulesFindingFilters
import Amazonka.SecurityHub.Types.RuleStatus

-- | Specifies the parameters to update in an existing automation rule.
--
-- /See:/ 'newUpdateAutomationRulesRequestItem' smart constructor.
data UpdateAutomationRulesRequestItem = UpdateAutomationRulesRequestItem'
  { -- | One or more actions to update finding fields if a finding matches the
    -- conditions specified in @Criteria@.
    actions :: Prelude.Maybe (Prelude.NonEmpty AutomationRulesAction),
    -- | A set of ASFF finding field attributes and corresponding expected values
    -- that Security Hub uses to filter findings. If a finding matches the
    -- conditions specified in this parameter, Security Hub applies the rule
    -- action to the finding.
    criteria :: Prelude.Maybe AutomationRulesFindingFilters,
    -- | A description of the rule.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether a rule is the last to be applied with respect to a
    -- finding that matches the rule criteria. This is useful when a finding
    -- matches the criteria for multiple rules, and each rule has different
    -- actions. If the value of this field is set to @true@ for a rule,
    -- Security Hub applies the rule action to a finding that matches the rule
    -- criteria and won\'t evaluate other rules for the finding.  The default
    -- value of this field is @false@.
    isTerminal :: Prelude.Maybe Prelude.Bool,
    -- | The name of the rule.
    ruleName :: Prelude.Maybe Prelude.Text,
    -- | An integer ranging from 1 to 1000 that represents the order in which the
    -- rule action is applied to findings. Security Hub applies rules with
    -- lower values for this parameter first.
    ruleOrder :: Prelude.Maybe Prelude.Natural,
    -- | Whether the rule is active after it is created. If this parameter is
    -- equal to @ENABLED@, Security Hub will apply the rule to findings and
    -- finding updates after the rule is created. To change the value of this
    -- parameter after creating a rule, use @BatchUpdateAutomationRules@.
    ruleStatus :: Prelude.Maybe RuleStatus,
    -- | The Amazon Resource Name (ARN) for the rule.
    ruleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAutomationRulesRequestItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'updateAutomationRulesRequestItem_actions' - One or more actions to update finding fields if a finding matches the
-- conditions specified in @Criteria@.
--
-- 'criteria', 'updateAutomationRulesRequestItem_criteria' - A set of ASFF finding field attributes and corresponding expected values
-- that Security Hub uses to filter findings. If a finding matches the
-- conditions specified in this parameter, Security Hub applies the rule
-- action to the finding.
--
-- 'description', 'updateAutomationRulesRequestItem_description' - A description of the rule.
--
-- 'isTerminal', 'updateAutomationRulesRequestItem_isTerminal' - Specifies whether a rule is the last to be applied with respect to a
-- finding that matches the rule criteria. This is useful when a finding
-- matches the criteria for multiple rules, and each rule has different
-- actions. If the value of this field is set to @true@ for a rule,
-- Security Hub applies the rule action to a finding that matches the rule
-- criteria and won\'t evaluate other rules for the finding.  The default
-- value of this field is @false@.
--
-- 'ruleName', 'updateAutomationRulesRequestItem_ruleName' - The name of the rule.
--
-- 'ruleOrder', 'updateAutomationRulesRequestItem_ruleOrder' - An integer ranging from 1 to 1000 that represents the order in which the
-- rule action is applied to findings. Security Hub applies rules with
-- lower values for this parameter first.
--
-- 'ruleStatus', 'updateAutomationRulesRequestItem_ruleStatus' - Whether the rule is active after it is created. If this parameter is
-- equal to @ENABLED@, Security Hub will apply the rule to findings and
-- finding updates after the rule is created. To change the value of this
-- parameter after creating a rule, use @BatchUpdateAutomationRules@.
--
-- 'ruleArn', 'updateAutomationRulesRequestItem_ruleArn' - The Amazon Resource Name (ARN) for the rule.
newUpdateAutomationRulesRequestItem ::
  -- | 'ruleArn'
  Prelude.Text ->
  UpdateAutomationRulesRequestItem
newUpdateAutomationRulesRequestItem pRuleArn_ =
  UpdateAutomationRulesRequestItem'
    { actions =
        Prelude.Nothing,
      criteria = Prelude.Nothing,
      description = Prelude.Nothing,
      isTerminal = Prelude.Nothing,
      ruleName = Prelude.Nothing,
      ruleOrder = Prelude.Nothing,
      ruleStatus = Prelude.Nothing,
      ruleArn = pRuleArn_
    }

-- | One or more actions to update finding fields if a finding matches the
-- conditions specified in @Criteria@.
updateAutomationRulesRequestItem_actions :: Lens.Lens' UpdateAutomationRulesRequestItem (Prelude.Maybe (Prelude.NonEmpty AutomationRulesAction))
updateAutomationRulesRequestItem_actions = Lens.lens (\UpdateAutomationRulesRequestItem' {actions} -> actions) (\s@UpdateAutomationRulesRequestItem' {} a -> s {actions = a} :: UpdateAutomationRulesRequestItem) Prelude.. Lens.mapping Lens.coerced

-- | A set of ASFF finding field attributes and corresponding expected values
-- that Security Hub uses to filter findings. If a finding matches the
-- conditions specified in this parameter, Security Hub applies the rule
-- action to the finding.
updateAutomationRulesRequestItem_criteria :: Lens.Lens' UpdateAutomationRulesRequestItem (Prelude.Maybe AutomationRulesFindingFilters)
updateAutomationRulesRequestItem_criteria = Lens.lens (\UpdateAutomationRulesRequestItem' {criteria} -> criteria) (\s@UpdateAutomationRulesRequestItem' {} a -> s {criteria = a} :: UpdateAutomationRulesRequestItem)

-- | A description of the rule.
updateAutomationRulesRequestItem_description :: Lens.Lens' UpdateAutomationRulesRequestItem (Prelude.Maybe Prelude.Text)
updateAutomationRulesRequestItem_description = Lens.lens (\UpdateAutomationRulesRequestItem' {description} -> description) (\s@UpdateAutomationRulesRequestItem' {} a -> s {description = a} :: UpdateAutomationRulesRequestItem)

-- | Specifies whether a rule is the last to be applied with respect to a
-- finding that matches the rule criteria. This is useful when a finding
-- matches the criteria for multiple rules, and each rule has different
-- actions. If the value of this field is set to @true@ for a rule,
-- Security Hub applies the rule action to a finding that matches the rule
-- criteria and won\'t evaluate other rules for the finding.  The default
-- value of this field is @false@.
updateAutomationRulesRequestItem_isTerminal :: Lens.Lens' UpdateAutomationRulesRequestItem (Prelude.Maybe Prelude.Bool)
updateAutomationRulesRequestItem_isTerminal = Lens.lens (\UpdateAutomationRulesRequestItem' {isTerminal} -> isTerminal) (\s@UpdateAutomationRulesRequestItem' {} a -> s {isTerminal = a} :: UpdateAutomationRulesRequestItem)

-- | The name of the rule.
updateAutomationRulesRequestItem_ruleName :: Lens.Lens' UpdateAutomationRulesRequestItem (Prelude.Maybe Prelude.Text)
updateAutomationRulesRequestItem_ruleName = Lens.lens (\UpdateAutomationRulesRequestItem' {ruleName} -> ruleName) (\s@UpdateAutomationRulesRequestItem' {} a -> s {ruleName = a} :: UpdateAutomationRulesRequestItem)

-- | An integer ranging from 1 to 1000 that represents the order in which the
-- rule action is applied to findings. Security Hub applies rules with
-- lower values for this parameter first.
updateAutomationRulesRequestItem_ruleOrder :: Lens.Lens' UpdateAutomationRulesRequestItem (Prelude.Maybe Prelude.Natural)
updateAutomationRulesRequestItem_ruleOrder = Lens.lens (\UpdateAutomationRulesRequestItem' {ruleOrder} -> ruleOrder) (\s@UpdateAutomationRulesRequestItem' {} a -> s {ruleOrder = a} :: UpdateAutomationRulesRequestItem)

-- | Whether the rule is active after it is created. If this parameter is
-- equal to @ENABLED@, Security Hub will apply the rule to findings and
-- finding updates after the rule is created. To change the value of this
-- parameter after creating a rule, use @BatchUpdateAutomationRules@.
updateAutomationRulesRequestItem_ruleStatus :: Lens.Lens' UpdateAutomationRulesRequestItem (Prelude.Maybe RuleStatus)
updateAutomationRulesRequestItem_ruleStatus = Lens.lens (\UpdateAutomationRulesRequestItem' {ruleStatus} -> ruleStatus) (\s@UpdateAutomationRulesRequestItem' {} a -> s {ruleStatus = a} :: UpdateAutomationRulesRequestItem)

-- | The Amazon Resource Name (ARN) for the rule.
updateAutomationRulesRequestItem_ruleArn :: Lens.Lens' UpdateAutomationRulesRequestItem Prelude.Text
updateAutomationRulesRequestItem_ruleArn = Lens.lens (\UpdateAutomationRulesRequestItem' {ruleArn} -> ruleArn) (\s@UpdateAutomationRulesRequestItem' {} a -> s {ruleArn = a} :: UpdateAutomationRulesRequestItem)

instance
  Prelude.Hashable
    UpdateAutomationRulesRequestItem
  where
  hashWithSalt
    _salt
    UpdateAutomationRulesRequestItem' {..} =
      _salt
        `Prelude.hashWithSalt` actions
        `Prelude.hashWithSalt` criteria
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` isTerminal
        `Prelude.hashWithSalt` ruleName
        `Prelude.hashWithSalt` ruleOrder
        `Prelude.hashWithSalt` ruleStatus
        `Prelude.hashWithSalt` ruleArn

instance
  Prelude.NFData
    UpdateAutomationRulesRequestItem
  where
  rnf UpdateAutomationRulesRequestItem' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf criteria
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf isTerminal
      `Prelude.seq` Prelude.rnf ruleName
      `Prelude.seq` Prelude.rnf ruleOrder
      `Prelude.seq` Prelude.rnf ruleStatus
      `Prelude.seq` Prelude.rnf ruleArn

instance Data.ToJSON UpdateAutomationRulesRequestItem where
  toJSON UpdateAutomationRulesRequestItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Actions" Data..=) Prelude.<$> actions,
            ("Criteria" Data..=) Prelude.<$> criteria,
            ("Description" Data..=) Prelude.<$> description,
            ("IsTerminal" Data..=) Prelude.<$> isTerminal,
            ("RuleName" Data..=) Prelude.<$> ruleName,
            ("RuleOrder" Data..=) Prelude.<$> ruleOrder,
            ("RuleStatus" Data..=) Prelude.<$> ruleStatus,
            Prelude.Just ("RuleArn" Data..= ruleArn)
          ]
      )
