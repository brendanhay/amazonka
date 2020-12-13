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
    omrmInputParameters,
    omrmResourceIdScope,
    omrmTagValueScope,
    omrmMaximumExecutionFrequency,
    omrmTagKeyScope,
    omrmRuleIdentifier,
    omrmResourceTypesScope,
    omrmDescription,
  )
where

import Network.AWS.Config.Types.MaximumExecutionFrequency
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that specifies organization managed rule metadata such as resource type and ID of AWS resource along with the rule identifier. It also provides the frequency with which you want AWS Config to run evaluations for the rule if the trigger type is periodic.
--
-- /See:/ 'mkOrganizationManagedRuleMetadata' smart constructor.
data OrganizationManagedRuleMetadata = OrganizationManagedRuleMetadata'
  { -- | A string, in JSON format, that is passed to organization config rule Lambda function.
    inputParameters :: Lude.Maybe Lude.Text,
    -- | The ID of the AWS resource that was evaluated.
    resourceIdScope :: Lude.Maybe Lude.Text,
    -- | The optional part of a key-value pair that make up a tag. A value acts as a descriptor within a tag category (key).
    tagValueScope :: Lude.Maybe Lude.Text,
    -- | The maximum frequency with which AWS Config runs evaluations for a rule. You are using an AWS managed rule that is triggered at a periodic frequency.
    maximumExecutionFrequency :: Lude.Maybe MaximumExecutionFrequency,
    -- | One part of a key-value pair that make up a tag. A key is a general label that acts like a category for more specific tag values.
    tagKeyScope :: Lude.Maybe Lude.Text,
    -- | For organization config managed rules, a predefined identifier from a list. For example, @IAM_PASSWORD_POLICY@ is a managed rule. To reference a managed rule, see <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html Using AWS Managed Config Rules> .
    ruleIdentifier :: Lude.Text,
    -- | The type of the AWS resource that was evaluated.
    resourceTypesScope :: Lude.Maybe [Lude.Text],
    -- | The description that you provide for organization config rule.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OrganizationManagedRuleMetadata' with the minimum fields required to make a request.
--
-- * 'inputParameters' - A string, in JSON format, that is passed to organization config rule Lambda function.
-- * 'resourceIdScope' - The ID of the AWS resource that was evaluated.
-- * 'tagValueScope' - The optional part of a key-value pair that make up a tag. A value acts as a descriptor within a tag category (key).
-- * 'maximumExecutionFrequency' - The maximum frequency with which AWS Config runs evaluations for a rule. You are using an AWS managed rule that is triggered at a periodic frequency.
-- * 'tagKeyScope' - One part of a key-value pair that make up a tag. A key is a general label that acts like a category for more specific tag values.
-- * 'ruleIdentifier' - For organization config managed rules, a predefined identifier from a list. For example, @IAM_PASSWORD_POLICY@ is a managed rule. To reference a managed rule, see <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html Using AWS Managed Config Rules> .
-- * 'resourceTypesScope' - The type of the AWS resource that was evaluated.
-- * 'description' - The description that you provide for organization config rule.
mkOrganizationManagedRuleMetadata ::
  -- | 'ruleIdentifier'
  Lude.Text ->
  OrganizationManagedRuleMetadata
mkOrganizationManagedRuleMetadata pRuleIdentifier_ =
  OrganizationManagedRuleMetadata'
    { inputParameters = Lude.Nothing,
      resourceIdScope = Lude.Nothing,
      tagValueScope = Lude.Nothing,
      maximumExecutionFrequency = Lude.Nothing,
      tagKeyScope = Lude.Nothing,
      ruleIdentifier = pRuleIdentifier_,
      resourceTypesScope = Lude.Nothing,
      description = Lude.Nothing
    }

-- | A string, in JSON format, that is passed to organization config rule Lambda function.
--
-- /Note:/ Consider using 'inputParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
omrmInputParameters :: Lens.Lens' OrganizationManagedRuleMetadata (Lude.Maybe Lude.Text)
omrmInputParameters = Lens.lens (inputParameters :: OrganizationManagedRuleMetadata -> Lude.Maybe Lude.Text) (\s a -> s {inputParameters = a} :: OrganizationManagedRuleMetadata)
{-# DEPRECATED omrmInputParameters "Use generic-lens or generic-optics with 'inputParameters' instead." #-}

-- | The ID of the AWS resource that was evaluated.
--
-- /Note:/ Consider using 'resourceIdScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
omrmResourceIdScope :: Lens.Lens' OrganizationManagedRuleMetadata (Lude.Maybe Lude.Text)
omrmResourceIdScope = Lens.lens (resourceIdScope :: OrganizationManagedRuleMetadata -> Lude.Maybe Lude.Text) (\s a -> s {resourceIdScope = a} :: OrganizationManagedRuleMetadata)
{-# DEPRECATED omrmResourceIdScope "Use generic-lens or generic-optics with 'resourceIdScope' instead." #-}

-- | The optional part of a key-value pair that make up a tag. A value acts as a descriptor within a tag category (key).
--
-- /Note:/ Consider using 'tagValueScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
omrmTagValueScope :: Lens.Lens' OrganizationManagedRuleMetadata (Lude.Maybe Lude.Text)
omrmTagValueScope = Lens.lens (tagValueScope :: OrganizationManagedRuleMetadata -> Lude.Maybe Lude.Text) (\s a -> s {tagValueScope = a} :: OrganizationManagedRuleMetadata)
{-# DEPRECATED omrmTagValueScope "Use generic-lens or generic-optics with 'tagValueScope' instead." #-}

-- | The maximum frequency with which AWS Config runs evaluations for a rule. You are using an AWS managed rule that is triggered at a periodic frequency.
--
-- /Note:/ Consider using 'maximumExecutionFrequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
omrmMaximumExecutionFrequency :: Lens.Lens' OrganizationManagedRuleMetadata (Lude.Maybe MaximumExecutionFrequency)
omrmMaximumExecutionFrequency = Lens.lens (maximumExecutionFrequency :: OrganizationManagedRuleMetadata -> Lude.Maybe MaximumExecutionFrequency) (\s a -> s {maximumExecutionFrequency = a} :: OrganizationManagedRuleMetadata)
{-# DEPRECATED omrmMaximumExecutionFrequency "Use generic-lens or generic-optics with 'maximumExecutionFrequency' instead." #-}

-- | One part of a key-value pair that make up a tag. A key is a general label that acts like a category for more specific tag values.
--
-- /Note:/ Consider using 'tagKeyScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
omrmTagKeyScope :: Lens.Lens' OrganizationManagedRuleMetadata (Lude.Maybe Lude.Text)
omrmTagKeyScope = Lens.lens (tagKeyScope :: OrganizationManagedRuleMetadata -> Lude.Maybe Lude.Text) (\s a -> s {tagKeyScope = a} :: OrganizationManagedRuleMetadata)
{-# DEPRECATED omrmTagKeyScope "Use generic-lens or generic-optics with 'tagKeyScope' instead." #-}

-- | For organization config managed rules, a predefined identifier from a list. For example, @IAM_PASSWORD_POLICY@ is a managed rule. To reference a managed rule, see <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html Using AWS Managed Config Rules> .
--
-- /Note:/ Consider using 'ruleIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
omrmRuleIdentifier :: Lens.Lens' OrganizationManagedRuleMetadata Lude.Text
omrmRuleIdentifier = Lens.lens (ruleIdentifier :: OrganizationManagedRuleMetadata -> Lude.Text) (\s a -> s {ruleIdentifier = a} :: OrganizationManagedRuleMetadata)
{-# DEPRECATED omrmRuleIdentifier "Use generic-lens or generic-optics with 'ruleIdentifier' instead." #-}

-- | The type of the AWS resource that was evaluated.
--
-- /Note:/ Consider using 'resourceTypesScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
omrmResourceTypesScope :: Lens.Lens' OrganizationManagedRuleMetadata (Lude.Maybe [Lude.Text])
omrmResourceTypesScope = Lens.lens (resourceTypesScope :: OrganizationManagedRuleMetadata -> Lude.Maybe [Lude.Text]) (\s a -> s {resourceTypesScope = a} :: OrganizationManagedRuleMetadata)
{-# DEPRECATED omrmResourceTypesScope "Use generic-lens or generic-optics with 'resourceTypesScope' instead." #-}

-- | The description that you provide for organization config rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
omrmDescription :: Lens.Lens' OrganizationManagedRuleMetadata (Lude.Maybe Lude.Text)
omrmDescription = Lens.lens (description :: OrganizationManagedRuleMetadata -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: OrganizationManagedRuleMetadata)
{-# DEPRECATED omrmDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON OrganizationManagedRuleMetadata where
  parseJSON =
    Lude.withObject
      "OrganizationManagedRuleMetadata"
      ( \x ->
          OrganizationManagedRuleMetadata'
            Lude.<$> (x Lude..:? "InputParameters")
            Lude.<*> (x Lude..:? "ResourceIdScope")
            Lude.<*> (x Lude..:? "TagValueScope")
            Lude.<*> (x Lude..:? "MaximumExecutionFrequency")
            Lude.<*> (x Lude..:? "TagKeyScope")
            Lude.<*> (x Lude..: "RuleIdentifier")
            Lude.<*> (x Lude..:? "ResourceTypesScope" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Description")
      )

instance Lude.ToJSON OrganizationManagedRuleMetadata where
  toJSON OrganizationManagedRuleMetadata' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InputParameters" Lude..=) Lude.<$> inputParameters,
            ("ResourceIdScope" Lude..=) Lude.<$> resourceIdScope,
            ("TagValueScope" Lude..=) Lude.<$> tagValueScope,
            ("MaximumExecutionFrequency" Lude..=)
              Lude.<$> maximumExecutionFrequency,
            ("TagKeyScope" Lude..=) Lude.<$> tagKeyScope,
            Lude.Just ("RuleIdentifier" Lude..= ruleIdentifier),
            ("ResourceTypesScope" Lude..=) Lude.<$> resourceTypesScope,
            ("Description" Lude..=) Lude.<$> description
          ]
      )
