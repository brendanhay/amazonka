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
-- Module      : Amazonka.SecurityHub.Types.AutomationRulesConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AutomationRulesConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AutomationRulesAction
import Amazonka.SecurityHub.Types.AutomationRulesFindingFilters
import Amazonka.SecurityHub.Types.RuleStatus

-- | Defines the configuration of an automation rule.
--
-- /See:/ 'newAutomationRulesConfig' smart constructor.
data AutomationRulesConfig = AutomationRulesConfig'
  { -- | One or more actions to update finding fields if a finding matches the
    -- defined criteria of the rule.
    actions :: Prelude.Maybe (Prelude.NonEmpty AutomationRulesAction),
    -- | A timestamp that indicates when the rule was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The principal that created a rule.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | A set of
    -- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-findings-format.html Amazon Web Services Security Finding Format>
    -- finding field attributes and corresponding expected values that Security
    -- Hub uses to filter findings. If a finding matches the conditions
    -- specified in this parameter, Security Hub applies the rule action to the
    -- finding.
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
    -- | The Amazon Resource Name (ARN) of a rule.
    ruleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the rule.
    ruleName :: Prelude.Maybe Prelude.Text,
    -- | An integer ranging from 1 to 1000 that represents the order in which the
    -- rule action is applied to findings. Security Hub applies rules with
    -- lower values for this parameter first.
    ruleOrder :: Prelude.Maybe Prelude.Natural,
    -- | Whether the rule is active after it is created. If this parameter is
    -- equal to @>ENABLED@, Security Hub will apply the rule to findings and
    -- finding updates after the rule is created.
    ruleStatus :: Prelude.Maybe RuleStatus,
    -- | A timestamp that indicates when the rule was most recently updated.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    updatedAt :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutomationRulesConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'automationRulesConfig_actions' - One or more actions to update finding fields if a finding matches the
-- defined criteria of the rule.
--
-- 'createdAt', 'automationRulesConfig_createdAt' - A timestamp that indicates when the rule was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'createdBy', 'automationRulesConfig_createdBy' - The principal that created a rule.
--
-- 'criteria', 'automationRulesConfig_criteria' - A set of
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-findings-format.html Amazon Web Services Security Finding Format>
-- finding field attributes and corresponding expected values that Security
-- Hub uses to filter findings. If a finding matches the conditions
-- specified in this parameter, Security Hub applies the rule action to the
-- finding.
--
-- 'description', 'automationRulesConfig_description' - A description of the rule.
--
-- 'isTerminal', 'automationRulesConfig_isTerminal' - Specifies whether a rule is the last to be applied with respect to a
-- finding that matches the rule criteria. This is useful when a finding
-- matches the criteria for multiple rules, and each rule has different
-- actions. If the value of this field is set to @true@ for a rule,
-- Security Hub applies the rule action to a finding that matches the rule
-- criteria and won\'t evaluate other rules for the finding.  The default
-- value of this field is @false@.
--
-- 'ruleArn', 'automationRulesConfig_ruleArn' - The Amazon Resource Name (ARN) of a rule.
--
-- 'ruleName', 'automationRulesConfig_ruleName' - The name of the rule.
--
-- 'ruleOrder', 'automationRulesConfig_ruleOrder' - An integer ranging from 1 to 1000 that represents the order in which the
-- rule action is applied to findings. Security Hub applies rules with
-- lower values for this parameter first.
--
-- 'ruleStatus', 'automationRulesConfig_ruleStatus' - Whether the rule is active after it is created. If this parameter is
-- equal to @>ENABLED@, Security Hub will apply the rule to findings and
-- finding updates after the rule is created.
--
-- 'updatedAt', 'automationRulesConfig_updatedAt' - A timestamp that indicates when the rule was most recently updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
newAutomationRulesConfig ::
  AutomationRulesConfig
newAutomationRulesConfig =
  AutomationRulesConfig'
    { actions = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      criteria = Prelude.Nothing,
      description = Prelude.Nothing,
      isTerminal = Prelude.Nothing,
      ruleArn = Prelude.Nothing,
      ruleName = Prelude.Nothing,
      ruleOrder = Prelude.Nothing,
      ruleStatus = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | One or more actions to update finding fields if a finding matches the
-- defined criteria of the rule.
automationRulesConfig_actions :: Lens.Lens' AutomationRulesConfig (Prelude.Maybe (Prelude.NonEmpty AutomationRulesAction))
automationRulesConfig_actions = Lens.lens (\AutomationRulesConfig' {actions} -> actions) (\s@AutomationRulesConfig' {} a -> s {actions = a} :: AutomationRulesConfig) Prelude.. Lens.mapping Lens.coerced

-- | A timestamp that indicates when the rule was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
automationRulesConfig_createdAt :: Lens.Lens' AutomationRulesConfig (Prelude.Maybe Prelude.UTCTime)
automationRulesConfig_createdAt = Lens.lens (\AutomationRulesConfig' {createdAt} -> createdAt) (\s@AutomationRulesConfig' {} a -> s {createdAt = a} :: AutomationRulesConfig) Prelude.. Lens.mapping Data._Time

-- | The principal that created a rule.
automationRulesConfig_createdBy :: Lens.Lens' AutomationRulesConfig (Prelude.Maybe Prelude.Text)
automationRulesConfig_createdBy = Lens.lens (\AutomationRulesConfig' {createdBy} -> createdBy) (\s@AutomationRulesConfig' {} a -> s {createdBy = a} :: AutomationRulesConfig)

-- | A set of
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-findings-format.html Amazon Web Services Security Finding Format>
-- finding field attributes and corresponding expected values that Security
-- Hub uses to filter findings. If a finding matches the conditions
-- specified in this parameter, Security Hub applies the rule action to the
-- finding.
automationRulesConfig_criteria :: Lens.Lens' AutomationRulesConfig (Prelude.Maybe AutomationRulesFindingFilters)
automationRulesConfig_criteria = Lens.lens (\AutomationRulesConfig' {criteria} -> criteria) (\s@AutomationRulesConfig' {} a -> s {criteria = a} :: AutomationRulesConfig)

-- | A description of the rule.
automationRulesConfig_description :: Lens.Lens' AutomationRulesConfig (Prelude.Maybe Prelude.Text)
automationRulesConfig_description = Lens.lens (\AutomationRulesConfig' {description} -> description) (\s@AutomationRulesConfig' {} a -> s {description = a} :: AutomationRulesConfig)

-- | Specifies whether a rule is the last to be applied with respect to a
-- finding that matches the rule criteria. This is useful when a finding
-- matches the criteria for multiple rules, and each rule has different
-- actions. If the value of this field is set to @true@ for a rule,
-- Security Hub applies the rule action to a finding that matches the rule
-- criteria and won\'t evaluate other rules for the finding.  The default
-- value of this field is @false@.
automationRulesConfig_isTerminal :: Lens.Lens' AutomationRulesConfig (Prelude.Maybe Prelude.Bool)
automationRulesConfig_isTerminal = Lens.lens (\AutomationRulesConfig' {isTerminal} -> isTerminal) (\s@AutomationRulesConfig' {} a -> s {isTerminal = a} :: AutomationRulesConfig)

-- | The Amazon Resource Name (ARN) of a rule.
automationRulesConfig_ruleArn :: Lens.Lens' AutomationRulesConfig (Prelude.Maybe Prelude.Text)
automationRulesConfig_ruleArn = Lens.lens (\AutomationRulesConfig' {ruleArn} -> ruleArn) (\s@AutomationRulesConfig' {} a -> s {ruleArn = a} :: AutomationRulesConfig)

-- | The name of the rule.
automationRulesConfig_ruleName :: Lens.Lens' AutomationRulesConfig (Prelude.Maybe Prelude.Text)
automationRulesConfig_ruleName = Lens.lens (\AutomationRulesConfig' {ruleName} -> ruleName) (\s@AutomationRulesConfig' {} a -> s {ruleName = a} :: AutomationRulesConfig)

-- | An integer ranging from 1 to 1000 that represents the order in which the
-- rule action is applied to findings. Security Hub applies rules with
-- lower values for this parameter first.
automationRulesConfig_ruleOrder :: Lens.Lens' AutomationRulesConfig (Prelude.Maybe Prelude.Natural)
automationRulesConfig_ruleOrder = Lens.lens (\AutomationRulesConfig' {ruleOrder} -> ruleOrder) (\s@AutomationRulesConfig' {} a -> s {ruleOrder = a} :: AutomationRulesConfig)

-- | Whether the rule is active after it is created. If this parameter is
-- equal to @>ENABLED@, Security Hub will apply the rule to findings and
-- finding updates after the rule is created.
automationRulesConfig_ruleStatus :: Lens.Lens' AutomationRulesConfig (Prelude.Maybe RuleStatus)
automationRulesConfig_ruleStatus = Lens.lens (\AutomationRulesConfig' {ruleStatus} -> ruleStatus) (\s@AutomationRulesConfig' {} a -> s {ruleStatus = a} :: AutomationRulesConfig)

-- | A timestamp that indicates when the rule was most recently updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
automationRulesConfig_updatedAt :: Lens.Lens' AutomationRulesConfig (Prelude.Maybe Prelude.UTCTime)
automationRulesConfig_updatedAt = Lens.lens (\AutomationRulesConfig' {updatedAt} -> updatedAt) (\s@AutomationRulesConfig' {} a -> s {updatedAt = a} :: AutomationRulesConfig) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON AutomationRulesConfig where
  parseJSON =
    Data.withObject
      "AutomationRulesConfig"
      ( \x ->
          AutomationRulesConfig'
            Prelude.<$> (x Data..:? "Actions")
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "CreatedBy")
            Prelude.<*> (x Data..:? "Criteria")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "IsTerminal")
            Prelude.<*> (x Data..:? "RuleArn")
            Prelude.<*> (x Data..:? "RuleName")
            Prelude.<*> (x Data..:? "RuleOrder")
            Prelude.<*> (x Data..:? "RuleStatus")
            Prelude.<*> (x Data..:? "UpdatedAt")
      )

instance Prelude.Hashable AutomationRulesConfig where
  hashWithSalt _salt AutomationRulesConfig' {..} =
    _salt
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` criteria
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` isTerminal
      `Prelude.hashWithSalt` ruleArn
      `Prelude.hashWithSalt` ruleName
      `Prelude.hashWithSalt` ruleOrder
      `Prelude.hashWithSalt` ruleStatus
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData AutomationRulesConfig where
  rnf AutomationRulesConfig' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf criteria
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf isTerminal
      `Prelude.seq` Prelude.rnf ruleArn
      `Prelude.seq` Prelude.rnf ruleName
      `Prelude.seq` Prelude.rnf ruleOrder
      `Prelude.seq` Prelude.rnf ruleStatus
      `Prelude.seq` Prelude.rnf updatedAt
