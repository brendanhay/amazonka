{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationCustomRuleMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.OrganizationCustomRuleMetadata
  ( OrganizationCustomRuleMetadata (..)
  -- * Smart constructor
  , mkOrganizationCustomRuleMetadata
  -- * Lenses
  , ocrmLambdaFunctionArn
  , ocrmOrganizationConfigRuleTriggerTypes
  , ocrmDescription
  , ocrmInputParameters
  , ocrmMaximumExecutionFrequency
  , ocrmResourceIdScope
  , ocrmResourceTypesScope
  , ocrmTagKeyScope
  , ocrmTagValueScope
  ) where

import qualified Network.AWS.Config.Types.Description as Types
import qualified Network.AWS.Config.Types.MaximumExecutionFrequency as Types
import qualified Network.AWS.Config.Types.OrganizationConfigRuleTriggerType as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit2048 as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit256 as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit768 as Types
import qualified Network.AWS.Config.Types.TagKeyScope as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that specifies organization custom rule metadata such as resource type, resource ID of AWS resource, Lamdba function ARN, and organization trigger types that trigger AWS Config to evaluate your AWS resources against a rule. It also provides the frequency with which you want AWS Config to run evaluations for the rule if the trigger type is periodic.
--
-- /See:/ 'mkOrganizationCustomRuleMetadata' smart constructor.
data OrganizationCustomRuleMetadata = OrganizationCustomRuleMetadata'
  { lambdaFunctionArn :: Types.StringWithCharLimit256
    -- ^ The lambda function ARN.
  , organizationConfigRuleTriggerTypes :: [Types.OrganizationConfigRuleTriggerType]
    -- ^ The type of notification that triggers AWS Config to run an evaluation for a rule. You can specify the following notification types:
--
--
--     * @ConfigurationItemChangeNotification@ - Triggers an evaluation when AWS Config delivers a configuration item as a result of a resource change.
--
--
--     * @OversizedConfigurationItemChangeNotification@ - Triggers an evaluation when AWS Config delivers an oversized configuration item. AWS Config may generate this notification type when a resource changes and the notification exceeds the maximum size allowed by Amazon SNS.
--
--
--     * @ScheduledNotification@ - Triggers a periodic evaluation at the frequency specified for @MaximumExecutionFrequency@ .
--
--
  , description :: Core.Maybe Types.Description
    -- ^ The description that you provide for organization config rule.
  , inputParameters :: Core.Maybe Types.StringWithCharLimit2048
    -- ^ A string, in JSON format, that is passed to organization config rule Lambda function.
  , maximumExecutionFrequency :: Core.Maybe Types.MaximumExecutionFrequency
    -- ^ The maximum frequency with which AWS Config runs evaluations for a rule. Your custom rule is triggered when AWS Config delivers the configuration snapshot. For more information, see 'ConfigSnapshotDeliveryProperties' .
  , resourceIdScope :: Core.Maybe Types.StringWithCharLimit768
    -- ^ The ID of the AWS resource that was evaluated.
  , resourceTypesScope :: Core.Maybe [Types.StringWithCharLimit256]
    -- ^ The type of the AWS resource that was evaluated.
  , tagKeyScope :: Core.Maybe Types.TagKeyScope
    -- ^ One part of a key-value pair that make up a tag. A key is a general label that acts like a category for more specific tag values. 
  , tagValueScope :: Core.Maybe Types.StringWithCharLimit256
    -- ^ The optional part of a key-value pair that make up a tag. A value acts as a descriptor within a tag category (key). 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OrganizationCustomRuleMetadata' value with any optional fields omitted.
mkOrganizationCustomRuleMetadata
    :: Types.StringWithCharLimit256 -- ^ 'lambdaFunctionArn'
    -> OrganizationCustomRuleMetadata
mkOrganizationCustomRuleMetadata lambdaFunctionArn
  = OrganizationCustomRuleMetadata'{lambdaFunctionArn,
                                    organizationConfigRuleTriggerTypes = Core.mempty,
                                    description = Core.Nothing, inputParameters = Core.Nothing,
                                    maximumExecutionFrequency = Core.Nothing,
                                    resourceIdScope = Core.Nothing,
                                    resourceTypesScope = Core.Nothing, tagKeyScope = Core.Nothing,
                                    tagValueScope = Core.Nothing}

-- | The lambda function ARN.
--
-- /Note:/ Consider using 'lambdaFunctionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrmLambdaFunctionArn :: Lens.Lens' OrganizationCustomRuleMetadata Types.StringWithCharLimit256
ocrmLambdaFunctionArn = Lens.field @"lambdaFunctionArn"
{-# INLINEABLE ocrmLambdaFunctionArn #-}
{-# DEPRECATED lambdaFunctionArn "Use generic-lens or generic-optics with 'lambdaFunctionArn' instead"  #-}

-- | The type of notification that triggers AWS Config to run an evaluation for a rule. You can specify the following notification types:
--
--
--     * @ConfigurationItemChangeNotification@ - Triggers an evaluation when AWS Config delivers a configuration item as a result of a resource change.
--
--
--     * @OversizedConfigurationItemChangeNotification@ - Triggers an evaluation when AWS Config delivers an oversized configuration item. AWS Config may generate this notification type when a resource changes and the notification exceeds the maximum size allowed by Amazon SNS.
--
--
--     * @ScheduledNotification@ - Triggers a periodic evaluation at the frequency specified for @MaximumExecutionFrequency@ .
--
--
--
-- /Note:/ Consider using 'organizationConfigRuleTriggerTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrmOrganizationConfigRuleTriggerTypes :: Lens.Lens' OrganizationCustomRuleMetadata [Types.OrganizationConfigRuleTriggerType]
ocrmOrganizationConfigRuleTriggerTypes = Lens.field @"organizationConfigRuleTriggerTypes"
{-# INLINEABLE ocrmOrganizationConfigRuleTriggerTypes #-}
{-# DEPRECATED organizationConfigRuleTriggerTypes "Use generic-lens or generic-optics with 'organizationConfigRuleTriggerTypes' instead"  #-}

-- | The description that you provide for organization config rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrmDescription :: Lens.Lens' OrganizationCustomRuleMetadata (Core.Maybe Types.Description)
ocrmDescription = Lens.field @"description"
{-# INLINEABLE ocrmDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A string, in JSON format, that is passed to organization config rule Lambda function.
--
-- /Note:/ Consider using 'inputParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrmInputParameters :: Lens.Lens' OrganizationCustomRuleMetadata (Core.Maybe Types.StringWithCharLimit2048)
ocrmInputParameters = Lens.field @"inputParameters"
{-# INLINEABLE ocrmInputParameters #-}
{-# DEPRECATED inputParameters "Use generic-lens or generic-optics with 'inputParameters' instead"  #-}

-- | The maximum frequency with which AWS Config runs evaluations for a rule. Your custom rule is triggered when AWS Config delivers the configuration snapshot. For more information, see 'ConfigSnapshotDeliveryProperties' .
--
-- /Note:/ Consider using 'maximumExecutionFrequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrmMaximumExecutionFrequency :: Lens.Lens' OrganizationCustomRuleMetadata (Core.Maybe Types.MaximumExecutionFrequency)
ocrmMaximumExecutionFrequency = Lens.field @"maximumExecutionFrequency"
{-# INLINEABLE ocrmMaximumExecutionFrequency #-}
{-# DEPRECATED maximumExecutionFrequency "Use generic-lens or generic-optics with 'maximumExecutionFrequency' instead"  #-}

-- | The ID of the AWS resource that was evaluated.
--
-- /Note:/ Consider using 'resourceIdScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrmResourceIdScope :: Lens.Lens' OrganizationCustomRuleMetadata (Core.Maybe Types.StringWithCharLimit768)
ocrmResourceIdScope = Lens.field @"resourceIdScope"
{-# INLINEABLE ocrmResourceIdScope #-}
{-# DEPRECATED resourceIdScope "Use generic-lens or generic-optics with 'resourceIdScope' instead"  #-}

-- | The type of the AWS resource that was evaluated.
--
-- /Note:/ Consider using 'resourceTypesScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrmResourceTypesScope :: Lens.Lens' OrganizationCustomRuleMetadata (Core.Maybe [Types.StringWithCharLimit256])
ocrmResourceTypesScope = Lens.field @"resourceTypesScope"
{-# INLINEABLE ocrmResourceTypesScope #-}
{-# DEPRECATED resourceTypesScope "Use generic-lens or generic-optics with 'resourceTypesScope' instead"  #-}

-- | One part of a key-value pair that make up a tag. A key is a general label that acts like a category for more specific tag values. 
--
-- /Note:/ Consider using 'tagKeyScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrmTagKeyScope :: Lens.Lens' OrganizationCustomRuleMetadata (Core.Maybe Types.TagKeyScope)
ocrmTagKeyScope = Lens.field @"tagKeyScope"
{-# INLINEABLE ocrmTagKeyScope #-}
{-# DEPRECATED tagKeyScope "Use generic-lens or generic-optics with 'tagKeyScope' instead"  #-}

-- | The optional part of a key-value pair that make up a tag. A value acts as a descriptor within a tag category (key). 
--
-- /Note:/ Consider using 'tagValueScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrmTagValueScope :: Lens.Lens' OrganizationCustomRuleMetadata (Core.Maybe Types.StringWithCharLimit256)
ocrmTagValueScope = Lens.field @"tagValueScope"
{-# INLINEABLE ocrmTagValueScope #-}
{-# DEPRECATED tagValueScope "Use generic-lens or generic-optics with 'tagValueScope' instead"  #-}

instance Core.FromJSON OrganizationCustomRuleMetadata where
        toJSON OrganizationCustomRuleMetadata{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("LambdaFunctionArn" Core..= lambdaFunctionArn),
                  Core.Just
                    ("OrganizationConfigRuleTriggerTypes" Core..=
                       organizationConfigRuleTriggerTypes),
                  ("Description" Core..=) Core.<$> description,
                  ("InputParameters" Core..=) Core.<$> inputParameters,
                  ("MaximumExecutionFrequency" Core..=) Core.<$>
                    maximumExecutionFrequency,
                  ("ResourceIdScope" Core..=) Core.<$> resourceIdScope,
                  ("ResourceTypesScope" Core..=) Core.<$> resourceTypesScope,
                  ("TagKeyScope" Core..=) Core.<$> tagKeyScope,
                  ("TagValueScope" Core..=) Core.<$> tagValueScope])

instance Core.FromJSON OrganizationCustomRuleMetadata where
        parseJSON
          = Core.withObject "OrganizationCustomRuleMetadata" Core.$
              \ x ->
                OrganizationCustomRuleMetadata' Core.<$>
                  (x Core..: "LambdaFunctionArn") Core.<*>
                    x Core..:? "OrganizationConfigRuleTriggerTypes" Core..!=
                      Core.mempty
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "InputParameters"
                    Core.<*> x Core..:? "MaximumExecutionFrequency"
                    Core.<*> x Core..:? "ResourceIdScope"
                    Core.<*> x Core..:? "ResourceTypesScope"
                    Core.<*> x Core..:? "TagKeyScope"
                    Core.<*> x Core..:? "TagValueScope"
