{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigRule
  ( ConfigRule (..),

    -- * Smart constructor
    mkConfigRule,

    -- * Lenses
    crInputParameters,
    crConfigRuleName,
    crCreatedBy,
    crMaximumExecutionFrequency,
    crConfigRuleId,
    crScope,
    crConfigRuleState,
    crDescription,
    crConfigRuleARN,
    crSource,
  )
where

import Network.AWS.Config.Types.ConfigRuleState
import Network.AWS.Config.Types.MaximumExecutionFrequency
import Network.AWS.Config.Types.Scope
import Network.AWS.Config.Types.Source
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An AWS Config rule represents an AWS Lambda function that you create for a custom rule or a predefined function for an AWS managed rule. The function evaluates configuration items to assess whether your AWS resources comply with your desired configurations. This function can run when AWS Config detects a configuration change to an AWS resource and at a periodic frequency that you choose (for example, every 24 hours).
--
-- For more information about developing and using AWS Config rules, see <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config.html Evaluating AWS Resource Configurations with AWS Config> in the /AWS Config Developer Guide/ .
--
-- /See:/ 'mkConfigRule' smart constructor.
data ConfigRule = ConfigRule'
  { inputParameters ::
      Lude.Maybe Lude.Text,
    configRuleName :: Lude.Maybe Lude.Text,
    createdBy :: Lude.Maybe Lude.Text,
    maximumExecutionFrequency :: Lude.Maybe MaximumExecutionFrequency,
    configRuleId :: Lude.Maybe Lude.Text,
    scope :: Lude.Maybe Scope,
    configRuleState :: Lude.Maybe ConfigRuleState,
    description :: Lude.Maybe Lude.Text,
    configRuleARN :: Lude.Maybe Lude.Text,
    source :: Source
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfigRule' with the minimum fields required to make a request.
--
-- * 'configRuleARN' - The Amazon Resource Name (ARN) of the AWS Config rule.
-- * 'configRuleId' - The ID of the AWS Config rule.
-- * 'configRuleName' - The name that you assign to the AWS Config rule. The name is required if you are adding a new rule.
-- * 'configRuleState' - Indicates whether the AWS Config rule is active or is currently being deleted by AWS Config. It can also indicate the evaluation status for the AWS Config rule.
--
-- AWS Config sets the state of the rule to @EVALUATING@ temporarily after you use the @StartConfigRulesEvaluation@ request to evaluate your resources against the AWS Config rule.
-- AWS Config sets the state of the rule to @DELETING_RESULTS@ temporarily after you use the @DeleteEvaluationResults@ request to delete the current evaluation results for the AWS Config rule.
-- AWS Config temporarily sets the state of a rule to @DELETING@ after you use the @DeleteConfigRule@ request to delete the rule. After AWS Config deletes the rule, the rule and all of its evaluations are erased and are no longer available.
-- * 'createdBy' - Service principal name of the service that created the rule.
-- * 'description' - The description that you provide for the AWS Config rule.
-- * 'inputParameters' - A string, in JSON format, that is passed to the AWS Config rule Lambda function.
-- * 'maximumExecutionFrequency' - The maximum frequency with which AWS Config runs evaluations for a rule. You can specify a value for @MaximumExecutionFrequency@ when:
--
--
--     * You are using an AWS managed rule that is triggered at a periodic frequency.
--
--
--     * Your custom rule is triggered when AWS Config delivers the configuration snapshot. For more information, see 'ConfigSnapshotDeliveryProperties' .
--
--
-- * 'scope' - Defines which resources can trigger an evaluation for the rule. The scope can include one or more resource types, a combination of one resource type and one resource ID, or a combination of a tag key and value. Specify a scope to constrain the resources that can trigger an evaluation for the rule. If you do not specify a scope, evaluations are triggered when any resource in the recording group changes.
-- * 'source' - Provides the rule owner (AWS or customer), the rule identifier, and the notifications that cause the function to evaluate your AWS resources.
mkConfigRule ::
  -- | 'source'
  Source ->
  ConfigRule
mkConfigRule pSource_ =
  ConfigRule'
    { inputParameters = Lude.Nothing,
      configRuleName = Lude.Nothing,
      createdBy = Lude.Nothing,
      maximumExecutionFrequency = Lude.Nothing,
      configRuleId = Lude.Nothing,
      scope = Lude.Nothing,
      configRuleState = Lude.Nothing,
      description = Lude.Nothing,
      configRuleARN = Lude.Nothing,
      source = pSource_
    }

-- | A string, in JSON format, that is passed to the AWS Config rule Lambda function.
--
-- /Note:/ Consider using 'inputParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crInputParameters :: Lens.Lens' ConfigRule (Lude.Maybe Lude.Text)
crInputParameters = Lens.lens (inputParameters :: ConfigRule -> Lude.Maybe Lude.Text) (\s a -> s {inputParameters = a} :: ConfigRule)
{-# DEPRECATED crInputParameters "Use generic-lens or generic-optics with 'inputParameters' instead." #-}

-- | The name that you assign to the AWS Config rule. The name is required if you are adding a new rule.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crConfigRuleName :: Lens.Lens' ConfigRule (Lude.Maybe Lude.Text)
crConfigRuleName = Lens.lens (configRuleName :: ConfigRule -> Lude.Maybe Lude.Text) (\s a -> s {configRuleName = a} :: ConfigRule)
{-# DEPRECATED crConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

-- | Service principal name of the service that created the rule.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crCreatedBy :: Lens.Lens' ConfigRule (Lude.Maybe Lude.Text)
crCreatedBy = Lens.lens (createdBy :: ConfigRule -> Lude.Maybe Lude.Text) (\s a -> s {createdBy = a} :: ConfigRule)
{-# DEPRECATED crCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | The maximum frequency with which AWS Config runs evaluations for a rule. You can specify a value for @MaximumExecutionFrequency@ when:
--
--
--     * You are using an AWS managed rule that is triggered at a periodic frequency.
--
--
--     * Your custom rule is triggered when AWS Config delivers the configuration snapshot. For more information, see 'ConfigSnapshotDeliveryProperties' .
--
--
--
-- /Note:/ Consider using 'maximumExecutionFrequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crMaximumExecutionFrequency :: Lens.Lens' ConfigRule (Lude.Maybe MaximumExecutionFrequency)
crMaximumExecutionFrequency = Lens.lens (maximumExecutionFrequency :: ConfigRule -> Lude.Maybe MaximumExecutionFrequency) (\s a -> s {maximumExecutionFrequency = a} :: ConfigRule)
{-# DEPRECATED crMaximumExecutionFrequency "Use generic-lens or generic-optics with 'maximumExecutionFrequency' instead." #-}

-- | The ID of the AWS Config rule.
--
-- /Note:/ Consider using 'configRuleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crConfigRuleId :: Lens.Lens' ConfigRule (Lude.Maybe Lude.Text)
crConfigRuleId = Lens.lens (configRuleId :: ConfigRule -> Lude.Maybe Lude.Text) (\s a -> s {configRuleId = a} :: ConfigRule)
{-# DEPRECATED crConfigRuleId "Use generic-lens or generic-optics with 'configRuleId' instead." #-}

-- | Defines which resources can trigger an evaluation for the rule. The scope can include one or more resource types, a combination of one resource type and one resource ID, or a combination of a tag key and value. Specify a scope to constrain the resources that can trigger an evaluation for the rule. If you do not specify a scope, evaluations are triggered when any resource in the recording group changes.
--
-- /Note:/ Consider using 'scope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crScope :: Lens.Lens' ConfigRule (Lude.Maybe Scope)
crScope = Lens.lens (scope :: ConfigRule -> Lude.Maybe Scope) (\s a -> s {scope = a} :: ConfigRule)
{-# DEPRECATED crScope "Use generic-lens or generic-optics with 'scope' instead." #-}

-- | Indicates whether the AWS Config rule is active or is currently being deleted by AWS Config. It can also indicate the evaluation status for the AWS Config rule.
--
-- AWS Config sets the state of the rule to @EVALUATING@ temporarily after you use the @StartConfigRulesEvaluation@ request to evaluate your resources against the AWS Config rule.
-- AWS Config sets the state of the rule to @DELETING_RESULTS@ temporarily after you use the @DeleteEvaluationResults@ request to delete the current evaluation results for the AWS Config rule.
-- AWS Config temporarily sets the state of a rule to @DELETING@ after you use the @DeleteConfigRule@ request to delete the rule. After AWS Config deletes the rule, the rule and all of its evaluations are erased and are no longer available.
--
-- /Note:/ Consider using 'configRuleState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crConfigRuleState :: Lens.Lens' ConfigRule (Lude.Maybe ConfigRuleState)
crConfigRuleState = Lens.lens (configRuleState :: ConfigRule -> Lude.Maybe ConfigRuleState) (\s a -> s {configRuleState = a} :: ConfigRule)
{-# DEPRECATED crConfigRuleState "Use generic-lens or generic-optics with 'configRuleState' instead." #-}

-- | The description that you provide for the AWS Config rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDescription :: Lens.Lens' ConfigRule (Lude.Maybe Lude.Text)
crDescription = Lens.lens (description :: ConfigRule -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ConfigRule)
{-# DEPRECATED crDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Config rule.
--
-- /Note:/ Consider using 'configRuleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crConfigRuleARN :: Lens.Lens' ConfigRule (Lude.Maybe Lude.Text)
crConfigRuleARN = Lens.lens (configRuleARN :: ConfigRule -> Lude.Maybe Lude.Text) (\s a -> s {configRuleARN = a} :: ConfigRule)
{-# DEPRECATED crConfigRuleARN "Use generic-lens or generic-optics with 'configRuleARN' instead." #-}

-- | Provides the rule owner (AWS or customer), the rule identifier, and the notifications that cause the function to evaluate your AWS resources.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crSource :: Lens.Lens' ConfigRule Source
crSource = Lens.lens (source :: ConfigRule -> Source) (\s a -> s {source = a} :: ConfigRule)
{-# DEPRECATED crSource "Use generic-lens or generic-optics with 'source' instead." #-}

instance Lude.FromJSON ConfigRule where
  parseJSON =
    Lude.withObject
      "ConfigRule"
      ( \x ->
          ConfigRule'
            Lude.<$> (x Lude..:? "InputParameters")
            Lude.<*> (x Lude..:? "ConfigRuleName")
            Lude.<*> (x Lude..:? "CreatedBy")
            Lude.<*> (x Lude..:? "MaximumExecutionFrequency")
            Lude.<*> (x Lude..:? "ConfigRuleId")
            Lude.<*> (x Lude..:? "Scope")
            Lude.<*> (x Lude..:? "ConfigRuleState")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "ConfigRuleArn")
            Lude.<*> (x Lude..: "Source")
      )

instance Lude.ToJSON ConfigRule where
  toJSON ConfigRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InputParameters" Lude..=) Lude.<$> inputParameters,
            ("ConfigRuleName" Lude..=) Lude.<$> configRuleName,
            ("CreatedBy" Lude..=) Lude.<$> createdBy,
            ("MaximumExecutionFrequency" Lude..=)
              Lude.<$> maximumExecutionFrequency,
            ("ConfigRuleId" Lude..=) Lude.<$> configRuleId,
            ("Scope" Lude..=) Lude.<$> scope,
            ("ConfigRuleState" Lude..=) Lude.<$> configRuleState,
            ("Description" Lude..=) Lude.<$> description,
            ("ConfigRuleArn" Lude..=) Lude.<$> configRuleARN,
            Lude.Just ("Source" Lude..= source)
          ]
      )
