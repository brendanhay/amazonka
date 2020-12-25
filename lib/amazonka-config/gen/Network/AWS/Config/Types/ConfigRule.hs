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
    crSource,
    crConfigRuleArn,
    crConfigRuleId,
    crConfigRuleName,
    crConfigRuleState,
    crCreatedBy,
    crDescription,
    crInputParameters,
    crMaximumExecutionFrequency,
    crScope,
  )
where

import qualified Network.AWS.Config.Types.ConfigRuleName as Types
import qualified Network.AWS.Config.Types.ConfigRuleState as Types
import qualified Network.AWS.Config.Types.EmptiableStringWithCharLimit256 as Types
import qualified Network.AWS.Config.Types.MaximumExecutionFrequency as Types
import qualified Network.AWS.Config.Types.Scope as Types
import qualified Network.AWS.Config.Types.Source as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit1024 as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit256 as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit64 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An AWS Config rule represents an AWS Lambda function that you create for a custom rule or a predefined function for an AWS managed rule. The function evaluates configuration items to assess whether your AWS resources comply with your desired configurations. This function can run when AWS Config detects a configuration change to an AWS resource and at a periodic frequency that you choose (for example, every 24 hours).
--
-- For more information about developing and using AWS Config rules, see <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config.html Evaluating AWS Resource Configurations with AWS Config> in the /AWS Config Developer Guide/ .
--
-- /See:/ 'mkConfigRule' smart constructor.
data ConfigRule = ConfigRule'
  { -- | Provides the rule owner (AWS or customer), the rule identifier, and the notifications that cause the function to evaluate your AWS resources.
    source :: Types.Source,
    -- | The Amazon Resource Name (ARN) of the AWS Config rule.
    configRuleArn :: Core.Maybe Types.StringWithCharLimit256,
    -- | The ID of the AWS Config rule.
    configRuleId :: Core.Maybe Types.StringWithCharLimit64,
    -- | The name that you assign to the AWS Config rule. The name is required if you are adding a new rule.
    configRuleName :: Core.Maybe Types.ConfigRuleName,
    -- | Indicates whether the AWS Config rule is active or is currently being deleted by AWS Config. It can also indicate the evaluation status for the AWS Config rule.
    --
    -- AWS Config sets the state of the rule to @EVALUATING@ temporarily after you use the @StartConfigRulesEvaluation@ request to evaluate your resources against the AWS Config rule.
    -- AWS Config sets the state of the rule to @DELETING_RESULTS@ temporarily after you use the @DeleteEvaluationResults@ request to delete the current evaluation results for the AWS Config rule.
    -- AWS Config temporarily sets the state of a rule to @DELETING@ after you use the @DeleteConfigRule@ request to delete the rule. After AWS Config deletes the rule, the rule and all of its evaluations are erased and are no longer available.
    configRuleState :: Core.Maybe Types.ConfigRuleState,
    -- | Service principal name of the service that created the rule.
    createdBy :: Core.Maybe Types.StringWithCharLimit256,
    -- | The description that you provide for the AWS Config rule.
    description :: Core.Maybe Types.EmptiableStringWithCharLimit256,
    -- | A string, in JSON format, that is passed to the AWS Config rule Lambda function.
    inputParameters :: Core.Maybe Types.StringWithCharLimit1024,
    -- | The maximum frequency with which AWS Config runs evaluations for a rule. You can specify a value for @MaximumExecutionFrequency@ when:
    --
    --
    --     * You are using an AWS managed rule that is triggered at a periodic frequency.
    --
    --
    --     * Your custom rule is triggered when AWS Config delivers the configuration snapshot. For more information, see 'ConfigSnapshotDeliveryProperties' .
    maximumExecutionFrequency :: Core.Maybe Types.MaximumExecutionFrequency,
    -- | Defines which resources can trigger an evaluation for the rule. The scope can include one or more resource types, a combination of one resource type and one resource ID, or a combination of a tag key and value. Specify a scope to constrain the resources that can trigger an evaluation for the rule. If you do not specify a scope, evaluations are triggered when any resource in the recording group changes.
    scope :: Core.Maybe Types.Scope
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConfigRule' value with any optional fields omitted.
mkConfigRule ::
  -- | 'source'
  Types.Source ->
  ConfigRule
mkConfigRule source =
  ConfigRule'
    { source,
      configRuleArn = Core.Nothing,
      configRuleId = Core.Nothing,
      configRuleName = Core.Nothing,
      configRuleState = Core.Nothing,
      createdBy = Core.Nothing,
      description = Core.Nothing,
      inputParameters = Core.Nothing,
      maximumExecutionFrequency = Core.Nothing,
      scope = Core.Nothing
    }

-- | Provides the rule owner (AWS or customer), the rule identifier, and the notifications that cause the function to evaluate your AWS resources.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crSource :: Lens.Lens' ConfigRule Types.Source
crSource = Lens.field @"source"
{-# DEPRECATED crSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Config rule.
--
-- /Note:/ Consider using 'configRuleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crConfigRuleArn :: Lens.Lens' ConfigRule (Core.Maybe Types.StringWithCharLimit256)
crConfigRuleArn = Lens.field @"configRuleArn"
{-# DEPRECATED crConfigRuleArn "Use generic-lens or generic-optics with 'configRuleArn' instead." #-}

-- | The ID of the AWS Config rule.
--
-- /Note:/ Consider using 'configRuleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crConfigRuleId :: Lens.Lens' ConfigRule (Core.Maybe Types.StringWithCharLimit64)
crConfigRuleId = Lens.field @"configRuleId"
{-# DEPRECATED crConfigRuleId "Use generic-lens or generic-optics with 'configRuleId' instead." #-}

-- | The name that you assign to the AWS Config rule. The name is required if you are adding a new rule.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crConfigRuleName :: Lens.Lens' ConfigRule (Core.Maybe Types.ConfigRuleName)
crConfigRuleName = Lens.field @"configRuleName"
{-# DEPRECATED crConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

-- | Indicates whether the AWS Config rule is active or is currently being deleted by AWS Config. It can also indicate the evaluation status for the AWS Config rule.
--
-- AWS Config sets the state of the rule to @EVALUATING@ temporarily after you use the @StartConfigRulesEvaluation@ request to evaluate your resources against the AWS Config rule.
-- AWS Config sets the state of the rule to @DELETING_RESULTS@ temporarily after you use the @DeleteEvaluationResults@ request to delete the current evaluation results for the AWS Config rule.
-- AWS Config temporarily sets the state of a rule to @DELETING@ after you use the @DeleteConfigRule@ request to delete the rule. After AWS Config deletes the rule, the rule and all of its evaluations are erased and are no longer available.
--
-- /Note:/ Consider using 'configRuleState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crConfigRuleState :: Lens.Lens' ConfigRule (Core.Maybe Types.ConfigRuleState)
crConfigRuleState = Lens.field @"configRuleState"
{-# DEPRECATED crConfigRuleState "Use generic-lens or generic-optics with 'configRuleState' instead." #-}

-- | Service principal name of the service that created the rule.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crCreatedBy :: Lens.Lens' ConfigRule (Core.Maybe Types.StringWithCharLimit256)
crCreatedBy = Lens.field @"createdBy"
{-# DEPRECATED crCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | The description that you provide for the AWS Config rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDescription :: Lens.Lens' ConfigRule (Core.Maybe Types.EmptiableStringWithCharLimit256)
crDescription = Lens.field @"description"
{-# DEPRECATED crDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A string, in JSON format, that is passed to the AWS Config rule Lambda function.
--
-- /Note:/ Consider using 'inputParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crInputParameters :: Lens.Lens' ConfigRule (Core.Maybe Types.StringWithCharLimit1024)
crInputParameters = Lens.field @"inputParameters"
{-# DEPRECATED crInputParameters "Use generic-lens or generic-optics with 'inputParameters' instead." #-}

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
crMaximumExecutionFrequency :: Lens.Lens' ConfigRule (Core.Maybe Types.MaximumExecutionFrequency)
crMaximumExecutionFrequency = Lens.field @"maximumExecutionFrequency"
{-# DEPRECATED crMaximumExecutionFrequency "Use generic-lens or generic-optics with 'maximumExecutionFrequency' instead." #-}

-- | Defines which resources can trigger an evaluation for the rule. The scope can include one or more resource types, a combination of one resource type and one resource ID, or a combination of a tag key and value. Specify a scope to constrain the resources that can trigger an evaluation for the rule. If you do not specify a scope, evaluations are triggered when any resource in the recording group changes.
--
-- /Note:/ Consider using 'scope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crScope :: Lens.Lens' ConfigRule (Core.Maybe Types.Scope)
crScope = Lens.field @"scope"
{-# DEPRECATED crScope "Use generic-lens or generic-optics with 'scope' instead." #-}

instance Core.FromJSON ConfigRule where
  toJSON ConfigRule {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Source" Core..= source),
            ("ConfigRuleArn" Core..=) Core.<$> configRuleArn,
            ("ConfigRuleId" Core..=) Core.<$> configRuleId,
            ("ConfigRuleName" Core..=) Core.<$> configRuleName,
            ("ConfigRuleState" Core..=) Core.<$> configRuleState,
            ("CreatedBy" Core..=) Core.<$> createdBy,
            ("Description" Core..=) Core.<$> description,
            ("InputParameters" Core..=) Core.<$> inputParameters,
            ("MaximumExecutionFrequency" Core..=)
              Core.<$> maximumExecutionFrequency,
            ("Scope" Core..=) Core.<$> scope
          ]
      )

instance Core.FromJSON ConfigRule where
  parseJSON =
    Core.withObject "ConfigRule" Core.$
      \x ->
        ConfigRule'
          Core.<$> (x Core..: "Source")
          Core.<*> (x Core..:? "ConfigRuleArn")
          Core.<*> (x Core..:? "ConfigRuleId")
          Core.<*> (x Core..:? "ConfigRuleName")
          Core.<*> (x Core..:? "ConfigRuleState")
          Core.<*> (x Core..:? "CreatedBy")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "InputParameters")
          Core.<*> (x Core..:? "MaximumExecutionFrequency")
          Core.<*> (x Core..:? "Scope")
