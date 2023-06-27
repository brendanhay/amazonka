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
-- Module      : Amazonka.SecurityHub.Types.AutomationRulesMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AutomationRulesMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.RuleStatus

-- | Metadata for automation rules in the calling account. The response
-- includes rules with a @RuleStatus@ of @ENABLED@ and @DISABLED@.
--
-- /See:/ 'newAutomationRulesMetadata' smart constructor.
data AutomationRulesMetadata = AutomationRulesMetadata'
  { -- | A timestamp that indicates when the rule was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The principal that created a rule.
    createdBy :: Prelude.Maybe Prelude.Text,
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
    -- | The Amazon Resource Name (ARN) for the rule.
    ruleArn :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'AutomationRulesMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'automationRulesMetadata_createdAt' - A timestamp that indicates when the rule was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'createdBy', 'automationRulesMetadata_createdBy' - The principal that created a rule.
--
-- 'description', 'automationRulesMetadata_description' - A description of the rule.
--
-- 'isTerminal', 'automationRulesMetadata_isTerminal' - Specifies whether a rule is the last to be applied with respect to a
-- finding that matches the rule criteria. This is useful when a finding
-- matches the criteria for multiple rules, and each rule has different
-- actions. If the value of this field is set to @true@ for a rule,
-- Security Hub applies the rule action to a finding that matches the rule
-- criteria and won\'t evaluate other rules for the finding.  The default
-- value of this field is @false@.
--
-- 'ruleArn', 'automationRulesMetadata_ruleArn' - The Amazon Resource Name (ARN) for the rule.
--
-- 'ruleName', 'automationRulesMetadata_ruleName' - The name of the rule.
--
-- 'ruleOrder', 'automationRulesMetadata_ruleOrder' - An integer ranging from 1 to 1000 that represents the order in which the
-- rule action is applied to findings. Security Hub applies rules with
-- lower values for this parameter first.
--
-- 'ruleStatus', 'automationRulesMetadata_ruleStatus' - Whether the rule is active after it is created. If this parameter is
-- equal to @ENABLED@, Security Hub will apply the rule to findings and
-- finding updates after the rule is created. To change the value of this
-- parameter after creating a rule, use @BatchUpdateAutomationRules@.
--
-- 'updatedAt', 'automationRulesMetadata_updatedAt' - A timestamp that indicates when the rule was most recently updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
newAutomationRulesMetadata ::
  AutomationRulesMetadata
newAutomationRulesMetadata =
  AutomationRulesMetadata'
    { createdAt =
        Prelude.Nothing,
      createdBy = Prelude.Nothing,
      description = Prelude.Nothing,
      isTerminal = Prelude.Nothing,
      ruleArn = Prelude.Nothing,
      ruleName = Prelude.Nothing,
      ruleOrder = Prelude.Nothing,
      ruleStatus = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | A timestamp that indicates when the rule was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
automationRulesMetadata_createdAt :: Lens.Lens' AutomationRulesMetadata (Prelude.Maybe Prelude.UTCTime)
automationRulesMetadata_createdAt = Lens.lens (\AutomationRulesMetadata' {createdAt} -> createdAt) (\s@AutomationRulesMetadata' {} a -> s {createdAt = a} :: AutomationRulesMetadata) Prelude.. Lens.mapping Data._Time

-- | The principal that created a rule.
automationRulesMetadata_createdBy :: Lens.Lens' AutomationRulesMetadata (Prelude.Maybe Prelude.Text)
automationRulesMetadata_createdBy = Lens.lens (\AutomationRulesMetadata' {createdBy} -> createdBy) (\s@AutomationRulesMetadata' {} a -> s {createdBy = a} :: AutomationRulesMetadata)

-- | A description of the rule.
automationRulesMetadata_description :: Lens.Lens' AutomationRulesMetadata (Prelude.Maybe Prelude.Text)
automationRulesMetadata_description = Lens.lens (\AutomationRulesMetadata' {description} -> description) (\s@AutomationRulesMetadata' {} a -> s {description = a} :: AutomationRulesMetadata)

-- | Specifies whether a rule is the last to be applied with respect to a
-- finding that matches the rule criteria. This is useful when a finding
-- matches the criteria for multiple rules, and each rule has different
-- actions. If the value of this field is set to @true@ for a rule,
-- Security Hub applies the rule action to a finding that matches the rule
-- criteria and won\'t evaluate other rules for the finding.  The default
-- value of this field is @false@.
automationRulesMetadata_isTerminal :: Lens.Lens' AutomationRulesMetadata (Prelude.Maybe Prelude.Bool)
automationRulesMetadata_isTerminal = Lens.lens (\AutomationRulesMetadata' {isTerminal} -> isTerminal) (\s@AutomationRulesMetadata' {} a -> s {isTerminal = a} :: AutomationRulesMetadata)

-- | The Amazon Resource Name (ARN) for the rule.
automationRulesMetadata_ruleArn :: Lens.Lens' AutomationRulesMetadata (Prelude.Maybe Prelude.Text)
automationRulesMetadata_ruleArn = Lens.lens (\AutomationRulesMetadata' {ruleArn} -> ruleArn) (\s@AutomationRulesMetadata' {} a -> s {ruleArn = a} :: AutomationRulesMetadata)

-- | The name of the rule.
automationRulesMetadata_ruleName :: Lens.Lens' AutomationRulesMetadata (Prelude.Maybe Prelude.Text)
automationRulesMetadata_ruleName = Lens.lens (\AutomationRulesMetadata' {ruleName} -> ruleName) (\s@AutomationRulesMetadata' {} a -> s {ruleName = a} :: AutomationRulesMetadata)

-- | An integer ranging from 1 to 1000 that represents the order in which the
-- rule action is applied to findings. Security Hub applies rules with
-- lower values for this parameter first.
automationRulesMetadata_ruleOrder :: Lens.Lens' AutomationRulesMetadata (Prelude.Maybe Prelude.Natural)
automationRulesMetadata_ruleOrder = Lens.lens (\AutomationRulesMetadata' {ruleOrder} -> ruleOrder) (\s@AutomationRulesMetadata' {} a -> s {ruleOrder = a} :: AutomationRulesMetadata)

-- | Whether the rule is active after it is created. If this parameter is
-- equal to @ENABLED@, Security Hub will apply the rule to findings and
-- finding updates after the rule is created. To change the value of this
-- parameter after creating a rule, use @BatchUpdateAutomationRules@.
automationRulesMetadata_ruleStatus :: Lens.Lens' AutomationRulesMetadata (Prelude.Maybe RuleStatus)
automationRulesMetadata_ruleStatus = Lens.lens (\AutomationRulesMetadata' {ruleStatus} -> ruleStatus) (\s@AutomationRulesMetadata' {} a -> s {ruleStatus = a} :: AutomationRulesMetadata)

-- | A timestamp that indicates when the rule was most recently updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
automationRulesMetadata_updatedAt :: Lens.Lens' AutomationRulesMetadata (Prelude.Maybe Prelude.UTCTime)
automationRulesMetadata_updatedAt = Lens.lens (\AutomationRulesMetadata' {updatedAt} -> updatedAt) (\s@AutomationRulesMetadata' {} a -> s {updatedAt = a} :: AutomationRulesMetadata) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON AutomationRulesMetadata where
  parseJSON =
    Data.withObject
      "AutomationRulesMetadata"
      ( \x ->
          AutomationRulesMetadata'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "CreatedBy")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "IsTerminal")
            Prelude.<*> (x Data..:? "RuleArn")
            Prelude.<*> (x Data..:? "RuleName")
            Prelude.<*> (x Data..:? "RuleOrder")
            Prelude.<*> (x Data..:? "RuleStatus")
            Prelude.<*> (x Data..:? "UpdatedAt")
      )

instance Prelude.Hashable AutomationRulesMetadata where
  hashWithSalt _salt AutomationRulesMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` isTerminal
      `Prelude.hashWithSalt` ruleArn
      `Prelude.hashWithSalt` ruleName
      `Prelude.hashWithSalt` ruleOrder
      `Prelude.hashWithSalt` ruleStatus
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData AutomationRulesMetadata where
  rnf AutomationRulesMetadata' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf isTerminal
      `Prelude.seq` Prelude.rnf ruleArn
      `Prelude.seq` Prelude.rnf ruleName
      `Prelude.seq` Prelude.rnf ruleOrder
      `Prelude.seq` Prelude.rnf ruleStatus
      `Prelude.seq` Prelude.rnf updatedAt
