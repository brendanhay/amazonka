{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecurityHub.CreateAutomationRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an automation rule based on input parameters.
module Amazonka.SecurityHub.CreateAutomationRule
  ( -- * Creating a Request
    CreateAutomationRule (..),
    newCreateAutomationRule,

    -- * Request Lenses
    createAutomationRule_isTerminal,
    createAutomationRule_ruleStatus,
    createAutomationRule_tags,
    createAutomationRule_ruleOrder,
    createAutomationRule_ruleName,
    createAutomationRule_description,
    createAutomationRule_criteria,
    createAutomationRule_actions,

    -- * Destructuring the Response
    CreateAutomationRuleResponse (..),
    newCreateAutomationRuleResponse,

    -- * Response Lenses
    createAutomationRuleResponse_ruleArn,
    createAutomationRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newCreateAutomationRule' smart constructor.
data CreateAutomationRule = CreateAutomationRule'
  { -- | Specifies whether a rule is the last to be applied with respect to a
    -- finding that matches the rule criteria. This is useful when a finding
    -- matches the criteria for multiple rules, and each rule has different
    -- actions. If the value of this field is set to @true@ for a rule,
    -- Security Hub applies the rule action to a finding that matches the rule
    -- criteria and won\'t evaluate other rules for the finding. The default
    -- value of this field is @false@.
    isTerminal :: Prelude.Maybe Prelude.Bool,
    -- | Whether the rule is active after it is created. If this parameter is
    -- equal to @Enabled@, Security Hub will apply the rule to findings and
    -- finding updates after the rule is created. To change the value of this
    -- parameter after creating a rule, use @BatchUpdateAutomationRules@.
    ruleStatus :: Prelude.Maybe RuleStatus,
    -- | User-defined tags that help you label the purpose of a rule.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An integer ranging from 1 to 1000 that represents the order in which the
    -- rule action is applied to findings. Security Hub applies rules with
    -- lower values for this parameter first.
    ruleOrder :: Prelude.Natural,
    -- | The name of the rule.
    ruleName :: Prelude.Text,
    -- | A description of the rule.
    description :: Prelude.Text,
    -- | A set of ASFF finding field attributes and corresponding expected values
    -- that Security Hub uses to filter findings. If a finding matches the
    -- conditions specified in this parameter, Security Hub applies the rule
    -- action to the finding.
    criteria :: AutomationRulesFindingFilters,
    -- | One or more actions to update finding fields if a finding matches the
    -- conditions specified in @Criteria@.
    actions :: Prelude.NonEmpty AutomationRulesAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAutomationRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTerminal', 'createAutomationRule_isTerminal' - Specifies whether a rule is the last to be applied with respect to a
-- finding that matches the rule criteria. This is useful when a finding
-- matches the criteria for multiple rules, and each rule has different
-- actions. If the value of this field is set to @true@ for a rule,
-- Security Hub applies the rule action to a finding that matches the rule
-- criteria and won\'t evaluate other rules for the finding. The default
-- value of this field is @false@.
--
-- 'ruleStatus', 'createAutomationRule_ruleStatus' - Whether the rule is active after it is created. If this parameter is
-- equal to @Enabled@, Security Hub will apply the rule to findings and
-- finding updates after the rule is created. To change the value of this
-- parameter after creating a rule, use @BatchUpdateAutomationRules@.
--
-- 'tags', 'createAutomationRule_tags' - User-defined tags that help you label the purpose of a rule.
--
-- 'ruleOrder', 'createAutomationRule_ruleOrder' - An integer ranging from 1 to 1000 that represents the order in which the
-- rule action is applied to findings. Security Hub applies rules with
-- lower values for this parameter first.
--
-- 'ruleName', 'createAutomationRule_ruleName' - The name of the rule.
--
-- 'description', 'createAutomationRule_description' - A description of the rule.
--
-- 'criteria', 'createAutomationRule_criteria' - A set of ASFF finding field attributes and corresponding expected values
-- that Security Hub uses to filter findings. If a finding matches the
-- conditions specified in this parameter, Security Hub applies the rule
-- action to the finding.
--
-- 'actions', 'createAutomationRule_actions' - One or more actions to update finding fields if a finding matches the
-- conditions specified in @Criteria@.
newCreateAutomationRule ::
  -- | 'ruleOrder'
  Prelude.Natural ->
  -- | 'ruleName'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'criteria'
  AutomationRulesFindingFilters ->
  -- | 'actions'
  Prelude.NonEmpty AutomationRulesAction ->
  CreateAutomationRule
newCreateAutomationRule
  pRuleOrder_
  pRuleName_
  pDescription_
  pCriteria_
  pActions_ =
    CreateAutomationRule'
      { isTerminal = Prelude.Nothing,
        ruleStatus = Prelude.Nothing,
        tags = Prelude.Nothing,
        ruleOrder = pRuleOrder_,
        ruleName = pRuleName_,
        description = pDescription_,
        criteria = pCriteria_,
        actions = Lens.coerced Lens.# pActions_
      }

-- | Specifies whether a rule is the last to be applied with respect to a
-- finding that matches the rule criteria. This is useful when a finding
-- matches the criteria for multiple rules, and each rule has different
-- actions. If the value of this field is set to @true@ for a rule,
-- Security Hub applies the rule action to a finding that matches the rule
-- criteria and won\'t evaluate other rules for the finding. The default
-- value of this field is @false@.
createAutomationRule_isTerminal :: Lens.Lens' CreateAutomationRule (Prelude.Maybe Prelude.Bool)
createAutomationRule_isTerminal = Lens.lens (\CreateAutomationRule' {isTerminal} -> isTerminal) (\s@CreateAutomationRule' {} a -> s {isTerminal = a} :: CreateAutomationRule)

-- | Whether the rule is active after it is created. If this parameter is
-- equal to @Enabled@, Security Hub will apply the rule to findings and
-- finding updates after the rule is created. To change the value of this
-- parameter after creating a rule, use @BatchUpdateAutomationRules@.
createAutomationRule_ruleStatus :: Lens.Lens' CreateAutomationRule (Prelude.Maybe RuleStatus)
createAutomationRule_ruleStatus = Lens.lens (\CreateAutomationRule' {ruleStatus} -> ruleStatus) (\s@CreateAutomationRule' {} a -> s {ruleStatus = a} :: CreateAutomationRule)

-- | User-defined tags that help you label the purpose of a rule.
createAutomationRule_tags :: Lens.Lens' CreateAutomationRule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createAutomationRule_tags = Lens.lens (\CreateAutomationRule' {tags} -> tags) (\s@CreateAutomationRule' {} a -> s {tags = a} :: CreateAutomationRule) Prelude.. Lens.mapping Lens.coerced

-- | An integer ranging from 1 to 1000 that represents the order in which the
-- rule action is applied to findings. Security Hub applies rules with
-- lower values for this parameter first.
createAutomationRule_ruleOrder :: Lens.Lens' CreateAutomationRule Prelude.Natural
createAutomationRule_ruleOrder = Lens.lens (\CreateAutomationRule' {ruleOrder} -> ruleOrder) (\s@CreateAutomationRule' {} a -> s {ruleOrder = a} :: CreateAutomationRule)

-- | The name of the rule.
createAutomationRule_ruleName :: Lens.Lens' CreateAutomationRule Prelude.Text
createAutomationRule_ruleName = Lens.lens (\CreateAutomationRule' {ruleName} -> ruleName) (\s@CreateAutomationRule' {} a -> s {ruleName = a} :: CreateAutomationRule)

-- | A description of the rule.
createAutomationRule_description :: Lens.Lens' CreateAutomationRule Prelude.Text
createAutomationRule_description = Lens.lens (\CreateAutomationRule' {description} -> description) (\s@CreateAutomationRule' {} a -> s {description = a} :: CreateAutomationRule)

-- | A set of ASFF finding field attributes and corresponding expected values
-- that Security Hub uses to filter findings. If a finding matches the
-- conditions specified in this parameter, Security Hub applies the rule
-- action to the finding.
createAutomationRule_criteria :: Lens.Lens' CreateAutomationRule AutomationRulesFindingFilters
createAutomationRule_criteria = Lens.lens (\CreateAutomationRule' {criteria} -> criteria) (\s@CreateAutomationRule' {} a -> s {criteria = a} :: CreateAutomationRule)

-- | One or more actions to update finding fields if a finding matches the
-- conditions specified in @Criteria@.
createAutomationRule_actions :: Lens.Lens' CreateAutomationRule (Prelude.NonEmpty AutomationRulesAction)
createAutomationRule_actions = Lens.lens (\CreateAutomationRule' {actions} -> actions) (\s@CreateAutomationRule' {} a -> s {actions = a} :: CreateAutomationRule) Prelude.. Lens.coerced

instance Core.AWSRequest CreateAutomationRule where
  type
    AWSResponse CreateAutomationRule =
      CreateAutomationRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAutomationRuleResponse'
            Prelude.<$> (x Data..?> "RuleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAutomationRule where
  hashWithSalt _salt CreateAutomationRule' {..} =
    _salt
      `Prelude.hashWithSalt` isTerminal
      `Prelude.hashWithSalt` ruleStatus
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` ruleOrder
      `Prelude.hashWithSalt` ruleName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` criteria
      `Prelude.hashWithSalt` actions

instance Prelude.NFData CreateAutomationRule where
  rnf CreateAutomationRule' {..} =
    Prelude.rnf isTerminal
      `Prelude.seq` Prelude.rnf ruleStatus
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf ruleOrder
      `Prelude.seq` Prelude.rnf ruleName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf criteria
      `Prelude.seq` Prelude.rnf actions

instance Data.ToHeaders CreateAutomationRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAutomationRule where
  toJSON CreateAutomationRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IsTerminal" Data..=) Prelude.<$> isTerminal,
            ("RuleStatus" Data..=) Prelude.<$> ruleStatus,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("RuleOrder" Data..= ruleOrder),
            Prelude.Just ("RuleName" Data..= ruleName),
            Prelude.Just ("Description" Data..= description),
            Prelude.Just ("Criteria" Data..= criteria),
            Prelude.Just ("Actions" Data..= actions)
          ]
      )

instance Data.ToPath CreateAutomationRule where
  toPath = Prelude.const "/automationrules/create"

instance Data.ToQuery CreateAutomationRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAutomationRuleResponse' smart constructor.
data CreateAutomationRuleResponse = CreateAutomationRuleResponse'
  { -- | The Amazon Resource Name (ARN) of the automation rule that you created.
    ruleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAutomationRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleArn', 'createAutomationRuleResponse_ruleArn' - The Amazon Resource Name (ARN) of the automation rule that you created.
--
-- 'httpStatus', 'createAutomationRuleResponse_httpStatus' - The response's http status code.
newCreateAutomationRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAutomationRuleResponse
newCreateAutomationRuleResponse pHttpStatus_ =
  CreateAutomationRuleResponse'
    { ruleArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the automation rule that you created.
createAutomationRuleResponse_ruleArn :: Lens.Lens' CreateAutomationRuleResponse (Prelude.Maybe Prelude.Text)
createAutomationRuleResponse_ruleArn = Lens.lens (\CreateAutomationRuleResponse' {ruleArn} -> ruleArn) (\s@CreateAutomationRuleResponse' {} a -> s {ruleArn = a} :: CreateAutomationRuleResponse)

-- | The response's http status code.
createAutomationRuleResponse_httpStatus :: Lens.Lens' CreateAutomationRuleResponse Prelude.Int
createAutomationRuleResponse_httpStatus = Lens.lens (\CreateAutomationRuleResponse' {httpStatus} -> httpStatus) (\s@CreateAutomationRuleResponse' {} a -> s {httpStatus = a} :: CreateAutomationRuleResponse)

instance Prelude.NFData CreateAutomationRuleResponse where
  rnf CreateAutomationRuleResponse' {..} =
    Prelude.rnf ruleArn
      `Prelude.seq` Prelude.rnf httpStatus
