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
-- Module      : Network.AWS.Config.Types.ConfigRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigRule where

import Network.AWS.Config.Types.ConfigRuleState
import Network.AWS.Config.Types.MaximumExecutionFrequency
import Network.AWS.Config.Types.Scope
import Network.AWS.Config.Types.Source
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An Config rule represents an Lambda function that you create for a
-- custom rule or a predefined function for an Config managed rule. The
-- function evaluates configuration items to assess whether your Amazon Web
-- Services resources comply with your desired configurations. This
-- function can run when Config detects a configuration change to an Amazon
-- Web Services resource and at a periodic frequency that you choose (for
-- example, every 24 hours).
--
-- You can use the Amazon Web Services CLI and Amazon Web Services SDKs if
-- you want to create a rule that triggers evaluations for your resources
-- when Config delivers the configuration snapshot. For more information,
-- see ConfigSnapshotDeliveryProperties.
--
-- For more information about developing and using Config rules, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config.html Evaluating Amazon Web Services resource Configurations with Config>
-- in the /Config Developer Guide/.
--
-- /See:/ 'newConfigRule' smart constructor.
data ConfigRule = ConfigRule'
  { -- | The Amazon Resource Name (ARN) of the Config rule.
    configRuleArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Config rule.
    configRuleId :: Prelude.Maybe Prelude.Text,
    -- | The maximum frequency with which Config runs evaluations for a rule. You
    -- can specify a value for @MaximumExecutionFrequency@ when:
    --
    -- -   You are using an Config managed rule that is triggered at a periodic
    --     frequency.
    --
    -- -   Your custom rule is triggered when Config delivers the configuration
    --     snapshot. For more information, see
    --     ConfigSnapshotDeliveryProperties.
    --
    -- By default, rules with a periodic trigger are evaluated every 24 hours.
    -- To change the frequency, specify a valid value for the
    -- @MaximumExecutionFrequency@ parameter.
    maximumExecutionFrequency :: Prelude.Maybe MaximumExecutionFrequency,
    -- | The name that you assign to the Config rule. The name is required if you
    -- are adding a new rule.
    configRuleName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the Config rule is active or is currently being
    -- deleted by Config. It can also indicate the evaluation status for the
    -- Config rule.
    --
    -- Config sets the state of the rule to @EVALUATING@ temporarily after you
    -- use the @StartConfigRulesEvaluation@ request to evaluate your resources
    -- against the Config rule.
    --
    -- Config sets the state of the rule to @DELETING_RESULTS@ temporarily
    -- after you use the @DeleteEvaluationResults@ request to delete the
    -- current evaluation results for the Config rule.
    --
    -- Config temporarily sets the state of a rule to @DELETING@ after you use
    -- the @DeleteConfigRule@ request to delete the rule. After Config deletes
    -- the rule, the rule and all of its evaluations are erased and are no
    -- longer available.
    configRuleState :: Prelude.Maybe ConfigRuleState,
    -- | Defines which resources can trigger an evaluation for the rule. The
    -- scope can include one or more resource types, a combination of one
    -- resource type and one resource ID, or a combination of a tag key and
    -- value. Specify a scope to constrain the resources that can trigger an
    -- evaluation for the rule. If you do not specify a scope, evaluations are
    -- triggered when any resource in the recording group changes.
    --
    -- The scope can be empty.
    scope :: Prelude.Maybe Scope,
    -- | A string, in JSON format, that is passed to the Config rule Lambda
    -- function.
    inputParameters :: Prelude.Maybe Prelude.Text,
    -- | The description that you provide for the Config rule.
    description :: Prelude.Maybe Prelude.Text,
    -- | Service principal name of the service that created the rule.
    --
    -- The field is populated only if the service linked rule is created by a
    -- service. The field is empty if you create your own rule.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | Provides the rule owner (Amazon Web Services or customer), the rule
    -- identifier, and the notifications that cause the function to evaluate
    -- your Amazon Web Services resources.
    source :: Source
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configRuleArn', 'configRule_configRuleArn' - The Amazon Resource Name (ARN) of the Config rule.
--
-- 'configRuleId', 'configRule_configRuleId' - The ID of the Config rule.
--
-- 'maximumExecutionFrequency', 'configRule_maximumExecutionFrequency' - The maximum frequency with which Config runs evaluations for a rule. You
-- can specify a value for @MaximumExecutionFrequency@ when:
--
-- -   You are using an Config managed rule that is triggered at a periodic
--     frequency.
--
-- -   Your custom rule is triggered when Config delivers the configuration
--     snapshot. For more information, see
--     ConfigSnapshotDeliveryProperties.
--
-- By default, rules with a periodic trigger are evaluated every 24 hours.
-- To change the frequency, specify a valid value for the
-- @MaximumExecutionFrequency@ parameter.
--
-- 'configRuleName', 'configRule_configRuleName' - The name that you assign to the Config rule. The name is required if you
-- are adding a new rule.
--
-- 'configRuleState', 'configRule_configRuleState' - Indicates whether the Config rule is active or is currently being
-- deleted by Config. It can also indicate the evaluation status for the
-- Config rule.
--
-- Config sets the state of the rule to @EVALUATING@ temporarily after you
-- use the @StartConfigRulesEvaluation@ request to evaluate your resources
-- against the Config rule.
--
-- Config sets the state of the rule to @DELETING_RESULTS@ temporarily
-- after you use the @DeleteEvaluationResults@ request to delete the
-- current evaluation results for the Config rule.
--
-- Config temporarily sets the state of a rule to @DELETING@ after you use
-- the @DeleteConfigRule@ request to delete the rule. After Config deletes
-- the rule, the rule and all of its evaluations are erased and are no
-- longer available.
--
-- 'scope', 'configRule_scope' - Defines which resources can trigger an evaluation for the rule. The
-- scope can include one or more resource types, a combination of one
-- resource type and one resource ID, or a combination of a tag key and
-- value. Specify a scope to constrain the resources that can trigger an
-- evaluation for the rule. If you do not specify a scope, evaluations are
-- triggered when any resource in the recording group changes.
--
-- The scope can be empty.
--
-- 'inputParameters', 'configRule_inputParameters' - A string, in JSON format, that is passed to the Config rule Lambda
-- function.
--
-- 'description', 'configRule_description' - The description that you provide for the Config rule.
--
-- 'createdBy', 'configRule_createdBy' - Service principal name of the service that created the rule.
--
-- The field is populated only if the service linked rule is created by a
-- service. The field is empty if you create your own rule.
--
-- 'source', 'configRule_source' - Provides the rule owner (Amazon Web Services or customer), the rule
-- identifier, and the notifications that cause the function to evaluate
-- your Amazon Web Services resources.
newConfigRule ::
  -- | 'source'
  Source ->
  ConfigRule
newConfigRule pSource_ =
  ConfigRule'
    { configRuleArn = Prelude.Nothing,
      configRuleId = Prelude.Nothing,
      maximumExecutionFrequency = Prelude.Nothing,
      configRuleName = Prelude.Nothing,
      configRuleState = Prelude.Nothing,
      scope = Prelude.Nothing,
      inputParameters = Prelude.Nothing,
      description = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      source = pSource_
    }

-- | The Amazon Resource Name (ARN) of the Config rule.
configRule_configRuleArn :: Lens.Lens' ConfigRule (Prelude.Maybe Prelude.Text)
configRule_configRuleArn = Lens.lens (\ConfigRule' {configRuleArn} -> configRuleArn) (\s@ConfigRule' {} a -> s {configRuleArn = a} :: ConfigRule)

-- | The ID of the Config rule.
configRule_configRuleId :: Lens.Lens' ConfigRule (Prelude.Maybe Prelude.Text)
configRule_configRuleId = Lens.lens (\ConfigRule' {configRuleId} -> configRuleId) (\s@ConfigRule' {} a -> s {configRuleId = a} :: ConfigRule)

-- | The maximum frequency with which Config runs evaluations for a rule. You
-- can specify a value for @MaximumExecutionFrequency@ when:
--
-- -   You are using an Config managed rule that is triggered at a periodic
--     frequency.
--
-- -   Your custom rule is triggered when Config delivers the configuration
--     snapshot. For more information, see
--     ConfigSnapshotDeliveryProperties.
--
-- By default, rules with a periodic trigger are evaluated every 24 hours.
-- To change the frequency, specify a valid value for the
-- @MaximumExecutionFrequency@ parameter.
configRule_maximumExecutionFrequency :: Lens.Lens' ConfigRule (Prelude.Maybe MaximumExecutionFrequency)
configRule_maximumExecutionFrequency = Lens.lens (\ConfigRule' {maximumExecutionFrequency} -> maximumExecutionFrequency) (\s@ConfigRule' {} a -> s {maximumExecutionFrequency = a} :: ConfigRule)

-- | The name that you assign to the Config rule. The name is required if you
-- are adding a new rule.
configRule_configRuleName :: Lens.Lens' ConfigRule (Prelude.Maybe Prelude.Text)
configRule_configRuleName = Lens.lens (\ConfigRule' {configRuleName} -> configRuleName) (\s@ConfigRule' {} a -> s {configRuleName = a} :: ConfigRule)

-- | Indicates whether the Config rule is active or is currently being
-- deleted by Config. It can also indicate the evaluation status for the
-- Config rule.
--
-- Config sets the state of the rule to @EVALUATING@ temporarily after you
-- use the @StartConfigRulesEvaluation@ request to evaluate your resources
-- against the Config rule.
--
-- Config sets the state of the rule to @DELETING_RESULTS@ temporarily
-- after you use the @DeleteEvaluationResults@ request to delete the
-- current evaluation results for the Config rule.
--
-- Config temporarily sets the state of a rule to @DELETING@ after you use
-- the @DeleteConfigRule@ request to delete the rule. After Config deletes
-- the rule, the rule and all of its evaluations are erased and are no
-- longer available.
configRule_configRuleState :: Lens.Lens' ConfigRule (Prelude.Maybe ConfigRuleState)
configRule_configRuleState = Lens.lens (\ConfigRule' {configRuleState} -> configRuleState) (\s@ConfigRule' {} a -> s {configRuleState = a} :: ConfigRule)

-- | Defines which resources can trigger an evaluation for the rule. The
-- scope can include one or more resource types, a combination of one
-- resource type and one resource ID, or a combination of a tag key and
-- value. Specify a scope to constrain the resources that can trigger an
-- evaluation for the rule. If you do not specify a scope, evaluations are
-- triggered when any resource in the recording group changes.
--
-- The scope can be empty.
configRule_scope :: Lens.Lens' ConfigRule (Prelude.Maybe Scope)
configRule_scope = Lens.lens (\ConfigRule' {scope} -> scope) (\s@ConfigRule' {} a -> s {scope = a} :: ConfigRule)

-- | A string, in JSON format, that is passed to the Config rule Lambda
-- function.
configRule_inputParameters :: Lens.Lens' ConfigRule (Prelude.Maybe Prelude.Text)
configRule_inputParameters = Lens.lens (\ConfigRule' {inputParameters} -> inputParameters) (\s@ConfigRule' {} a -> s {inputParameters = a} :: ConfigRule)

-- | The description that you provide for the Config rule.
configRule_description :: Lens.Lens' ConfigRule (Prelude.Maybe Prelude.Text)
configRule_description = Lens.lens (\ConfigRule' {description} -> description) (\s@ConfigRule' {} a -> s {description = a} :: ConfigRule)

-- | Service principal name of the service that created the rule.
--
-- The field is populated only if the service linked rule is created by a
-- service. The field is empty if you create your own rule.
configRule_createdBy :: Lens.Lens' ConfigRule (Prelude.Maybe Prelude.Text)
configRule_createdBy = Lens.lens (\ConfigRule' {createdBy} -> createdBy) (\s@ConfigRule' {} a -> s {createdBy = a} :: ConfigRule)

-- | Provides the rule owner (Amazon Web Services or customer), the rule
-- identifier, and the notifications that cause the function to evaluate
-- your Amazon Web Services resources.
configRule_source :: Lens.Lens' ConfigRule Source
configRule_source = Lens.lens (\ConfigRule' {source} -> source) (\s@ConfigRule' {} a -> s {source = a} :: ConfigRule)

instance Core.FromJSON ConfigRule where
  parseJSON =
    Core.withObject
      "ConfigRule"
      ( \x ->
          ConfigRule'
            Prelude.<$> (x Core..:? "ConfigRuleArn")
            Prelude.<*> (x Core..:? "ConfigRuleId")
            Prelude.<*> (x Core..:? "MaximumExecutionFrequency")
            Prelude.<*> (x Core..:? "ConfigRuleName")
            Prelude.<*> (x Core..:? "ConfigRuleState")
            Prelude.<*> (x Core..:? "Scope")
            Prelude.<*> (x Core..:? "InputParameters")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "CreatedBy")
            Prelude.<*> (x Core..: "Source")
      )

instance Prelude.Hashable ConfigRule

instance Prelude.NFData ConfigRule

instance Core.ToJSON ConfigRule where
  toJSON ConfigRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ConfigRuleArn" Core..=) Prelude.<$> configRuleArn,
            ("ConfigRuleId" Core..=) Prelude.<$> configRuleId,
            ("MaximumExecutionFrequency" Core..=)
              Prelude.<$> maximumExecutionFrequency,
            ("ConfigRuleName" Core..=)
              Prelude.<$> configRuleName,
            ("ConfigRuleState" Core..=)
              Prelude.<$> configRuleState,
            ("Scope" Core..=) Prelude.<$> scope,
            ("InputParameters" Core..=)
              Prelude.<$> inputParameters,
            ("Description" Core..=) Prelude.<$> description,
            ("CreatedBy" Core..=) Prelude.<$> createdBy,
            Prelude.Just ("Source" Core..= source)
          ]
      )
