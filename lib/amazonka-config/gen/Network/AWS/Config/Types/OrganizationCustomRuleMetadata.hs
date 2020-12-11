-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationCustomRuleMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationCustomRuleMetadata
  ( OrganizationCustomRuleMetadata (..),

    -- * Smart constructor
    mkOrganizationCustomRuleMetadata,

    -- * Lenses
    ocrmInputParameters,
    ocrmResourceIdScope,
    ocrmTagValueScope,
    ocrmMaximumExecutionFrequency,
    ocrmTagKeyScope,
    ocrmResourceTypesScope,
    ocrmDescription,
    ocrmLambdaFunctionARN,
    ocrmOrganizationConfigRuleTriggerTypes,
  )
where

import Network.AWS.Config.Types.MaximumExecutionFrequency
import Network.AWS.Config.Types.OrganizationConfigRuleTriggerType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that specifies organization custom rule metadata such as resource type, resource ID of AWS resource, Lamdba function ARN, and organization trigger types that trigger AWS Config to evaluate your AWS resources against a rule. It also provides the frequency with which you want AWS Config to run evaluations for the rule if the trigger type is periodic.
--
-- /See:/ 'mkOrganizationCustomRuleMetadata' smart constructor.
data OrganizationCustomRuleMetadata = OrganizationCustomRuleMetadata'
  { inputParameters ::
      Lude.Maybe Lude.Text,
    resourceIdScope ::
      Lude.Maybe Lude.Text,
    tagValueScope ::
      Lude.Maybe Lude.Text,
    maximumExecutionFrequency ::
      Lude.Maybe
        MaximumExecutionFrequency,
    tagKeyScope ::
      Lude.Maybe Lude.Text,
    resourceTypesScope ::
      Lude.Maybe [Lude.Text],
    description ::
      Lude.Maybe Lude.Text,
    lambdaFunctionARN ::
      Lude.Text,
    organizationConfigRuleTriggerTypes ::
      [OrganizationConfigRuleTriggerType]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OrganizationCustomRuleMetadata' with the minimum fields required to make a request.
--
-- * 'description' - The description that you provide for organization config rule.
-- * 'inputParameters' - A string, in JSON format, that is passed to organization config rule Lambda function.
-- * 'lambdaFunctionARN' - The lambda function ARN.
-- * 'maximumExecutionFrequency' - The maximum frequency with which AWS Config runs evaluations for a rule. Your custom rule is triggered when AWS Config delivers the configuration snapshot. For more information, see 'ConfigSnapshotDeliveryProperties' .
-- * 'organizationConfigRuleTriggerTypes' - The type of notification that triggers AWS Config to run an evaluation for a rule. You can specify the following notification types:
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
-- * 'resourceIdScope' - The ID of the AWS resource that was evaluated.
-- * 'resourceTypesScope' - The type of the AWS resource that was evaluated.
-- * 'tagKeyScope' - One part of a key-value pair that make up a tag. A key is a general label that acts like a category for more specific tag values.
-- * 'tagValueScope' - The optional part of a key-value pair that make up a tag. A value acts as a descriptor within a tag category (key).
mkOrganizationCustomRuleMetadata ::
  -- | 'lambdaFunctionARN'
  Lude.Text ->
  OrganizationCustomRuleMetadata
mkOrganizationCustomRuleMetadata pLambdaFunctionARN_ =
  OrganizationCustomRuleMetadata'
    { inputParameters = Lude.Nothing,
      resourceIdScope = Lude.Nothing,
      tagValueScope = Lude.Nothing,
      maximumExecutionFrequency = Lude.Nothing,
      tagKeyScope = Lude.Nothing,
      resourceTypesScope = Lude.Nothing,
      description = Lude.Nothing,
      lambdaFunctionARN = pLambdaFunctionARN_,
      organizationConfigRuleTriggerTypes = Lude.mempty
    }

-- | A string, in JSON format, that is passed to organization config rule Lambda function.
--
-- /Note:/ Consider using 'inputParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrmInputParameters :: Lens.Lens' OrganizationCustomRuleMetadata (Lude.Maybe Lude.Text)
ocrmInputParameters = Lens.lens (inputParameters :: OrganizationCustomRuleMetadata -> Lude.Maybe Lude.Text) (\s a -> s {inputParameters = a} :: OrganizationCustomRuleMetadata)
{-# DEPRECATED ocrmInputParameters "Use generic-lens or generic-optics with 'inputParameters' instead." #-}

-- | The ID of the AWS resource that was evaluated.
--
-- /Note:/ Consider using 'resourceIdScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrmResourceIdScope :: Lens.Lens' OrganizationCustomRuleMetadata (Lude.Maybe Lude.Text)
ocrmResourceIdScope = Lens.lens (resourceIdScope :: OrganizationCustomRuleMetadata -> Lude.Maybe Lude.Text) (\s a -> s {resourceIdScope = a} :: OrganizationCustomRuleMetadata)
{-# DEPRECATED ocrmResourceIdScope "Use generic-lens or generic-optics with 'resourceIdScope' instead." #-}

-- | The optional part of a key-value pair that make up a tag. A value acts as a descriptor within a tag category (key).
--
-- /Note:/ Consider using 'tagValueScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrmTagValueScope :: Lens.Lens' OrganizationCustomRuleMetadata (Lude.Maybe Lude.Text)
ocrmTagValueScope = Lens.lens (tagValueScope :: OrganizationCustomRuleMetadata -> Lude.Maybe Lude.Text) (\s a -> s {tagValueScope = a} :: OrganizationCustomRuleMetadata)
{-# DEPRECATED ocrmTagValueScope "Use generic-lens or generic-optics with 'tagValueScope' instead." #-}

-- | The maximum frequency with which AWS Config runs evaluations for a rule. Your custom rule is triggered when AWS Config delivers the configuration snapshot. For more information, see 'ConfigSnapshotDeliveryProperties' .
--
-- /Note:/ Consider using 'maximumExecutionFrequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrmMaximumExecutionFrequency :: Lens.Lens' OrganizationCustomRuleMetadata (Lude.Maybe MaximumExecutionFrequency)
ocrmMaximumExecutionFrequency = Lens.lens (maximumExecutionFrequency :: OrganizationCustomRuleMetadata -> Lude.Maybe MaximumExecutionFrequency) (\s a -> s {maximumExecutionFrequency = a} :: OrganizationCustomRuleMetadata)
{-# DEPRECATED ocrmMaximumExecutionFrequency "Use generic-lens or generic-optics with 'maximumExecutionFrequency' instead." #-}

-- | One part of a key-value pair that make up a tag. A key is a general label that acts like a category for more specific tag values.
--
-- /Note:/ Consider using 'tagKeyScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrmTagKeyScope :: Lens.Lens' OrganizationCustomRuleMetadata (Lude.Maybe Lude.Text)
ocrmTagKeyScope = Lens.lens (tagKeyScope :: OrganizationCustomRuleMetadata -> Lude.Maybe Lude.Text) (\s a -> s {tagKeyScope = a} :: OrganizationCustomRuleMetadata)
{-# DEPRECATED ocrmTagKeyScope "Use generic-lens or generic-optics with 'tagKeyScope' instead." #-}

-- | The type of the AWS resource that was evaluated.
--
-- /Note:/ Consider using 'resourceTypesScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrmResourceTypesScope :: Lens.Lens' OrganizationCustomRuleMetadata (Lude.Maybe [Lude.Text])
ocrmResourceTypesScope = Lens.lens (resourceTypesScope :: OrganizationCustomRuleMetadata -> Lude.Maybe [Lude.Text]) (\s a -> s {resourceTypesScope = a} :: OrganizationCustomRuleMetadata)
{-# DEPRECATED ocrmResourceTypesScope "Use generic-lens or generic-optics with 'resourceTypesScope' instead." #-}

-- | The description that you provide for organization config rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrmDescription :: Lens.Lens' OrganizationCustomRuleMetadata (Lude.Maybe Lude.Text)
ocrmDescription = Lens.lens (description :: OrganizationCustomRuleMetadata -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: OrganizationCustomRuleMetadata)
{-# DEPRECATED ocrmDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The lambda function ARN.
--
-- /Note:/ Consider using 'lambdaFunctionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrmLambdaFunctionARN :: Lens.Lens' OrganizationCustomRuleMetadata Lude.Text
ocrmLambdaFunctionARN = Lens.lens (lambdaFunctionARN :: OrganizationCustomRuleMetadata -> Lude.Text) (\s a -> s {lambdaFunctionARN = a} :: OrganizationCustomRuleMetadata)
{-# DEPRECATED ocrmLambdaFunctionARN "Use generic-lens or generic-optics with 'lambdaFunctionARN' instead." #-}

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
ocrmOrganizationConfigRuleTriggerTypes :: Lens.Lens' OrganizationCustomRuleMetadata [OrganizationConfigRuleTriggerType]
ocrmOrganizationConfigRuleTriggerTypes = Lens.lens (organizationConfigRuleTriggerTypes :: OrganizationCustomRuleMetadata -> [OrganizationConfigRuleTriggerType]) (\s a -> s {organizationConfigRuleTriggerTypes = a} :: OrganizationCustomRuleMetadata)
{-# DEPRECATED ocrmOrganizationConfigRuleTriggerTypes "Use generic-lens or generic-optics with 'organizationConfigRuleTriggerTypes' instead." #-}

instance Lude.FromJSON OrganizationCustomRuleMetadata where
  parseJSON =
    Lude.withObject
      "OrganizationCustomRuleMetadata"
      ( \x ->
          OrganizationCustomRuleMetadata'
            Lude.<$> (x Lude..:? "InputParameters")
            Lude.<*> (x Lude..:? "ResourceIdScope")
            Lude.<*> (x Lude..:? "TagValueScope")
            Lude.<*> (x Lude..:? "MaximumExecutionFrequency")
            Lude.<*> (x Lude..:? "TagKeyScope")
            Lude.<*> (x Lude..:? "ResourceTypesScope" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..: "LambdaFunctionArn")
            Lude.<*> ( x Lude..:? "OrganizationConfigRuleTriggerTypes"
                         Lude..!= Lude.mempty
                     )
      )

instance Lude.ToJSON OrganizationCustomRuleMetadata where
  toJSON OrganizationCustomRuleMetadata' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InputParameters" Lude..=) Lude.<$> inputParameters,
            ("ResourceIdScope" Lude..=) Lude.<$> resourceIdScope,
            ("TagValueScope" Lude..=) Lude.<$> tagValueScope,
            ("MaximumExecutionFrequency" Lude..=)
              Lude.<$> maximumExecutionFrequency,
            ("TagKeyScope" Lude..=) Lude.<$> tagKeyScope,
            ("ResourceTypesScope" Lude..=) Lude.<$> resourceTypesScope,
            ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("LambdaFunctionArn" Lude..= lambdaFunctionARN),
            Lude.Just
              ( "OrganizationConfigRuleTriggerTypes"
                  Lude..= organizationConfigRuleTriggerTypes
              )
          ]
      )
