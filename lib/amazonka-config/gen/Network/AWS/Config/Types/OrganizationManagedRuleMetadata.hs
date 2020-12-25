{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationManagedRuleMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationManagedRuleMetadata
  ( OrganizationManagedRuleMetadata (..),

    -- * Smart constructor
    mkOrganizationManagedRuleMetadata,

    -- * Lenses
    omrmRuleIdentifier,
    omrmDescription,
    omrmInputParameters,
    omrmMaximumExecutionFrequency,
    omrmResourceIdScope,
    omrmResourceTypesScope,
    omrmTagKeyScope,
    omrmTagValueScope,
  )
where

import qualified Network.AWS.Config.Types.Description as Types
import qualified Network.AWS.Config.Types.MaximumExecutionFrequency as Types
import qualified Network.AWS.Config.Types.ResourceIdScope as Types
import qualified Network.AWS.Config.Types.RuleIdentifier as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit2048 as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit256 as Types
import qualified Network.AWS.Config.Types.TagKeyScope as Types
import qualified Network.AWS.Config.Types.TagValueScope as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that specifies organization managed rule metadata such as resource type and ID of AWS resource along with the rule identifier. It also provides the frequency with which you want AWS Config to run evaluations for the rule if the trigger type is periodic.
--
-- /See:/ 'mkOrganizationManagedRuleMetadata' smart constructor.
data OrganizationManagedRuleMetadata = OrganizationManagedRuleMetadata'
  { -- | For organization config managed rules, a predefined identifier from a list. For example, @IAM_PASSWORD_POLICY@ is a managed rule. To reference a managed rule, see <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html Using AWS Managed Config Rules> .
    ruleIdentifier :: Types.RuleIdentifier,
    -- | The description that you provide for organization config rule.
    description :: Core.Maybe Types.Description,
    -- | A string, in JSON format, that is passed to organization config rule Lambda function.
    inputParameters :: Core.Maybe Types.StringWithCharLimit2048,
    -- | The maximum frequency with which AWS Config runs evaluations for a rule. You are using an AWS managed rule that is triggered at a periodic frequency.
    maximumExecutionFrequency :: Core.Maybe Types.MaximumExecutionFrequency,
    -- | The ID of the AWS resource that was evaluated.
    resourceIdScope :: Core.Maybe Types.ResourceIdScope,
    -- | The type of the AWS resource that was evaluated.
    resourceTypesScope :: Core.Maybe [Types.StringWithCharLimit256],
    -- | One part of a key-value pair that make up a tag. A key is a general label that acts like a category for more specific tag values.
    tagKeyScope :: Core.Maybe Types.TagKeyScope,
    -- | The optional part of a key-value pair that make up a tag. A value acts as a descriptor within a tag category (key).
    tagValueScope :: Core.Maybe Types.TagValueScope
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OrganizationManagedRuleMetadata' value with any optional fields omitted.
mkOrganizationManagedRuleMetadata ::
  -- | 'ruleIdentifier'
  Types.RuleIdentifier ->
  OrganizationManagedRuleMetadata
mkOrganizationManagedRuleMetadata ruleIdentifier =
  OrganizationManagedRuleMetadata'
    { ruleIdentifier,
      description = Core.Nothing,
      inputParameters = Core.Nothing,
      maximumExecutionFrequency = Core.Nothing,
      resourceIdScope = Core.Nothing,
      resourceTypesScope = Core.Nothing,
      tagKeyScope = Core.Nothing,
      tagValueScope = Core.Nothing
    }

-- | For organization config managed rules, a predefined identifier from a list. For example, @IAM_PASSWORD_POLICY@ is a managed rule. To reference a managed rule, see <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html Using AWS Managed Config Rules> .
--
-- /Note:/ Consider using 'ruleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
omrmRuleIdentifier :: Lens.Lens' OrganizationManagedRuleMetadata Types.RuleIdentifier
omrmRuleIdentifier = Lens.field @"ruleIdentifier"
{-# DEPRECATED omrmRuleIdentifier "Use generic-lens or generic-optics with 'ruleIdentifier' instead." #-}

-- | The description that you provide for organization config rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
omrmDescription :: Lens.Lens' OrganizationManagedRuleMetadata (Core.Maybe Types.Description)
omrmDescription = Lens.field @"description"
{-# DEPRECATED omrmDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A string, in JSON format, that is passed to organization config rule Lambda function.
--
-- /Note:/ Consider using 'inputParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
omrmInputParameters :: Lens.Lens' OrganizationManagedRuleMetadata (Core.Maybe Types.StringWithCharLimit2048)
omrmInputParameters = Lens.field @"inputParameters"
{-# DEPRECATED omrmInputParameters "Use generic-lens or generic-optics with 'inputParameters' instead." #-}

-- | The maximum frequency with which AWS Config runs evaluations for a rule. You are using an AWS managed rule that is triggered at a periodic frequency.
--
-- /Note:/ Consider using 'maximumExecutionFrequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
omrmMaximumExecutionFrequency :: Lens.Lens' OrganizationManagedRuleMetadata (Core.Maybe Types.MaximumExecutionFrequency)
omrmMaximumExecutionFrequency = Lens.field @"maximumExecutionFrequency"
{-# DEPRECATED omrmMaximumExecutionFrequency "Use generic-lens or generic-optics with 'maximumExecutionFrequency' instead." #-}

-- | The ID of the AWS resource that was evaluated.
--
-- /Note:/ Consider using 'resourceIdScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
omrmResourceIdScope :: Lens.Lens' OrganizationManagedRuleMetadata (Core.Maybe Types.ResourceIdScope)
omrmResourceIdScope = Lens.field @"resourceIdScope"
{-# DEPRECATED omrmResourceIdScope "Use generic-lens or generic-optics with 'resourceIdScope' instead." #-}

-- | The type of the AWS resource that was evaluated.
--
-- /Note:/ Consider using 'resourceTypesScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
omrmResourceTypesScope :: Lens.Lens' OrganizationManagedRuleMetadata (Core.Maybe [Types.StringWithCharLimit256])
omrmResourceTypesScope = Lens.field @"resourceTypesScope"
{-# DEPRECATED omrmResourceTypesScope "Use generic-lens or generic-optics with 'resourceTypesScope' instead." #-}

-- | One part of a key-value pair that make up a tag. A key is a general label that acts like a category for more specific tag values.
--
-- /Note:/ Consider using 'tagKeyScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
omrmTagKeyScope :: Lens.Lens' OrganizationManagedRuleMetadata (Core.Maybe Types.TagKeyScope)
omrmTagKeyScope = Lens.field @"tagKeyScope"
{-# DEPRECATED omrmTagKeyScope "Use generic-lens or generic-optics with 'tagKeyScope' instead." #-}

-- | The optional part of a key-value pair that make up a tag. A value acts as a descriptor within a tag category (key).
--
-- /Note:/ Consider using 'tagValueScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
omrmTagValueScope :: Lens.Lens' OrganizationManagedRuleMetadata (Core.Maybe Types.TagValueScope)
omrmTagValueScope = Lens.field @"tagValueScope"
{-# DEPRECATED omrmTagValueScope "Use generic-lens or generic-optics with 'tagValueScope' instead." #-}

instance Core.FromJSON OrganizationManagedRuleMetadata where
  toJSON OrganizationManagedRuleMetadata {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("RuleIdentifier" Core..= ruleIdentifier),
            ("Description" Core..=) Core.<$> description,
            ("InputParameters" Core..=) Core.<$> inputParameters,
            ("MaximumExecutionFrequency" Core..=)
              Core.<$> maximumExecutionFrequency,
            ("ResourceIdScope" Core..=) Core.<$> resourceIdScope,
            ("ResourceTypesScope" Core..=) Core.<$> resourceTypesScope,
            ("TagKeyScope" Core..=) Core.<$> tagKeyScope,
            ("TagValueScope" Core..=) Core.<$> tagValueScope
          ]
      )

instance Core.FromJSON OrganizationManagedRuleMetadata where
  parseJSON =
    Core.withObject "OrganizationManagedRuleMetadata" Core.$
      \x ->
        OrganizationManagedRuleMetadata'
          Core.<$> (x Core..: "RuleIdentifier")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "InputParameters")
          Core.<*> (x Core..:? "MaximumExecutionFrequency")
          Core.<*> (x Core..:? "ResourceIdScope")
          Core.<*> (x Core..:? "ResourceTypesScope")
          Core.<*> (x Core..:? "TagKeyScope")
          Core.<*> (x Core..:? "TagValueScope")
