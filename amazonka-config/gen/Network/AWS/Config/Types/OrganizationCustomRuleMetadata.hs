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
-- Module      : Network.AWS.Config.Types.OrganizationCustomRuleMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationCustomRuleMetadata where

import Network.AWS.Config.Types.MaximumExecutionFrequency
import Network.AWS.Config.Types.OrganizationConfigRuleTriggerType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object that specifies organization custom rule metadata such as
-- resource type, resource ID of AWS resource, Lamdba function ARN, and
-- organization trigger types that trigger AWS Config to evaluate your AWS
-- resources against a rule. It also provides the frequency with which you
-- want AWS Config to run evaluations for the rule if the trigger type is
-- periodic.
--
-- /See:/ 'newOrganizationCustomRuleMetadata' smart constructor.
data OrganizationCustomRuleMetadata = OrganizationCustomRuleMetadata'
  { -- | One part of a key-value pair that make up a tag. A key is a general
    -- label that acts like a category for more specific tag values.
    tagKeyScope :: Core.Maybe Core.Text,
    -- | The maximum frequency with which AWS Config runs evaluations for a rule.
    -- Your custom rule is triggered when AWS Config delivers the configuration
    -- snapshot. For more information, see ConfigSnapshotDeliveryProperties.
    --
    -- By default, rules with a periodic trigger are evaluated every 24 hours.
    -- To change the frequency, specify a valid value for the
    -- @MaximumExecutionFrequency@ parameter.
    maximumExecutionFrequency :: Core.Maybe MaximumExecutionFrequency,
    -- | The ID of the AWS resource that was evaluated.
    resourceIdScope :: Core.Maybe Core.Text,
    -- | A string, in JSON format, that is passed to organization config rule
    -- Lambda function.
    inputParameters :: Core.Maybe Core.Text,
    -- | The description that you provide for organization config rule.
    description :: Core.Maybe Core.Text,
    -- | The type of the AWS resource that was evaluated.
    resourceTypesScope :: Core.Maybe [Core.Text],
    -- | The optional part of a key-value pair that make up a tag. A value acts
    -- as a descriptor within a tag category (key).
    tagValueScope :: Core.Maybe Core.Text,
    -- | The lambda function ARN.
    lambdaFunctionArn :: Core.Text,
    -- | The type of notification that triggers AWS Config to run an evaluation
    -- for a rule. You can specify the following notification types:
    --
    -- -   @ConfigurationItemChangeNotification@ - Triggers an evaluation when
    --     AWS Config delivers a configuration item as a result of a resource
    --     change.
    --
    -- -   @OversizedConfigurationItemChangeNotification@ - Triggers an
    --     evaluation when AWS Config delivers an oversized configuration item.
    --     AWS Config may generate this notification type when a resource
    --     changes and the notification exceeds the maximum size allowed by
    --     Amazon SNS.
    --
    -- -   @ScheduledNotification@ - Triggers a periodic evaluation at the
    --     frequency specified for @MaximumExecutionFrequency@.
    organizationConfigRuleTriggerTypes :: [OrganizationConfigRuleTriggerType]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OrganizationCustomRuleMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagKeyScope', 'organizationCustomRuleMetadata_tagKeyScope' - One part of a key-value pair that make up a tag. A key is a general
-- label that acts like a category for more specific tag values.
--
-- 'maximumExecutionFrequency', 'organizationCustomRuleMetadata_maximumExecutionFrequency' - The maximum frequency with which AWS Config runs evaluations for a rule.
-- Your custom rule is triggered when AWS Config delivers the configuration
-- snapshot. For more information, see ConfigSnapshotDeliveryProperties.
--
-- By default, rules with a periodic trigger are evaluated every 24 hours.
-- To change the frequency, specify a valid value for the
-- @MaximumExecutionFrequency@ parameter.
--
-- 'resourceIdScope', 'organizationCustomRuleMetadata_resourceIdScope' - The ID of the AWS resource that was evaluated.
--
-- 'inputParameters', 'organizationCustomRuleMetadata_inputParameters' - A string, in JSON format, that is passed to organization config rule
-- Lambda function.
--
-- 'description', 'organizationCustomRuleMetadata_description' - The description that you provide for organization config rule.
--
-- 'resourceTypesScope', 'organizationCustomRuleMetadata_resourceTypesScope' - The type of the AWS resource that was evaluated.
--
-- 'tagValueScope', 'organizationCustomRuleMetadata_tagValueScope' - The optional part of a key-value pair that make up a tag. A value acts
-- as a descriptor within a tag category (key).
--
-- 'lambdaFunctionArn', 'organizationCustomRuleMetadata_lambdaFunctionArn' - The lambda function ARN.
--
-- 'organizationConfigRuleTriggerTypes', 'organizationCustomRuleMetadata_organizationConfigRuleTriggerTypes' - The type of notification that triggers AWS Config to run an evaluation
-- for a rule. You can specify the following notification types:
--
-- -   @ConfigurationItemChangeNotification@ - Triggers an evaluation when
--     AWS Config delivers a configuration item as a result of a resource
--     change.
--
-- -   @OversizedConfigurationItemChangeNotification@ - Triggers an
--     evaluation when AWS Config delivers an oversized configuration item.
--     AWS Config may generate this notification type when a resource
--     changes and the notification exceeds the maximum size allowed by
--     Amazon SNS.
--
-- -   @ScheduledNotification@ - Triggers a periodic evaluation at the
--     frequency specified for @MaximumExecutionFrequency@.
newOrganizationCustomRuleMetadata ::
  -- | 'lambdaFunctionArn'
  Core.Text ->
  OrganizationCustomRuleMetadata
newOrganizationCustomRuleMetadata pLambdaFunctionArn_ =
  OrganizationCustomRuleMetadata'
    { tagKeyScope =
        Core.Nothing,
      maximumExecutionFrequency = Core.Nothing,
      resourceIdScope = Core.Nothing,
      inputParameters = Core.Nothing,
      description = Core.Nothing,
      resourceTypesScope = Core.Nothing,
      tagValueScope = Core.Nothing,
      lambdaFunctionArn = pLambdaFunctionArn_,
      organizationConfigRuleTriggerTypes =
        Core.mempty
    }

-- | One part of a key-value pair that make up a tag. A key is a general
-- label that acts like a category for more specific tag values.
organizationCustomRuleMetadata_tagKeyScope :: Lens.Lens' OrganizationCustomRuleMetadata (Core.Maybe Core.Text)
organizationCustomRuleMetadata_tagKeyScope = Lens.lens (\OrganizationCustomRuleMetadata' {tagKeyScope} -> tagKeyScope) (\s@OrganizationCustomRuleMetadata' {} a -> s {tagKeyScope = a} :: OrganizationCustomRuleMetadata)

-- | The maximum frequency with which AWS Config runs evaluations for a rule.
-- Your custom rule is triggered when AWS Config delivers the configuration
-- snapshot. For more information, see ConfigSnapshotDeliveryProperties.
--
-- By default, rules with a periodic trigger are evaluated every 24 hours.
-- To change the frequency, specify a valid value for the
-- @MaximumExecutionFrequency@ parameter.
organizationCustomRuleMetadata_maximumExecutionFrequency :: Lens.Lens' OrganizationCustomRuleMetadata (Core.Maybe MaximumExecutionFrequency)
organizationCustomRuleMetadata_maximumExecutionFrequency = Lens.lens (\OrganizationCustomRuleMetadata' {maximumExecutionFrequency} -> maximumExecutionFrequency) (\s@OrganizationCustomRuleMetadata' {} a -> s {maximumExecutionFrequency = a} :: OrganizationCustomRuleMetadata)

-- | The ID of the AWS resource that was evaluated.
organizationCustomRuleMetadata_resourceIdScope :: Lens.Lens' OrganizationCustomRuleMetadata (Core.Maybe Core.Text)
organizationCustomRuleMetadata_resourceIdScope = Lens.lens (\OrganizationCustomRuleMetadata' {resourceIdScope} -> resourceIdScope) (\s@OrganizationCustomRuleMetadata' {} a -> s {resourceIdScope = a} :: OrganizationCustomRuleMetadata)

-- | A string, in JSON format, that is passed to organization config rule
-- Lambda function.
organizationCustomRuleMetadata_inputParameters :: Lens.Lens' OrganizationCustomRuleMetadata (Core.Maybe Core.Text)
organizationCustomRuleMetadata_inputParameters = Lens.lens (\OrganizationCustomRuleMetadata' {inputParameters} -> inputParameters) (\s@OrganizationCustomRuleMetadata' {} a -> s {inputParameters = a} :: OrganizationCustomRuleMetadata)

-- | The description that you provide for organization config rule.
organizationCustomRuleMetadata_description :: Lens.Lens' OrganizationCustomRuleMetadata (Core.Maybe Core.Text)
organizationCustomRuleMetadata_description = Lens.lens (\OrganizationCustomRuleMetadata' {description} -> description) (\s@OrganizationCustomRuleMetadata' {} a -> s {description = a} :: OrganizationCustomRuleMetadata)

-- | The type of the AWS resource that was evaluated.
organizationCustomRuleMetadata_resourceTypesScope :: Lens.Lens' OrganizationCustomRuleMetadata (Core.Maybe [Core.Text])
organizationCustomRuleMetadata_resourceTypesScope = Lens.lens (\OrganizationCustomRuleMetadata' {resourceTypesScope} -> resourceTypesScope) (\s@OrganizationCustomRuleMetadata' {} a -> s {resourceTypesScope = a} :: OrganizationCustomRuleMetadata) Core.. Lens.mapping Lens._Coerce

-- | The optional part of a key-value pair that make up a tag. A value acts
-- as a descriptor within a tag category (key).
organizationCustomRuleMetadata_tagValueScope :: Lens.Lens' OrganizationCustomRuleMetadata (Core.Maybe Core.Text)
organizationCustomRuleMetadata_tagValueScope = Lens.lens (\OrganizationCustomRuleMetadata' {tagValueScope} -> tagValueScope) (\s@OrganizationCustomRuleMetadata' {} a -> s {tagValueScope = a} :: OrganizationCustomRuleMetadata)

-- | The lambda function ARN.
organizationCustomRuleMetadata_lambdaFunctionArn :: Lens.Lens' OrganizationCustomRuleMetadata Core.Text
organizationCustomRuleMetadata_lambdaFunctionArn = Lens.lens (\OrganizationCustomRuleMetadata' {lambdaFunctionArn} -> lambdaFunctionArn) (\s@OrganizationCustomRuleMetadata' {} a -> s {lambdaFunctionArn = a} :: OrganizationCustomRuleMetadata)

-- | The type of notification that triggers AWS Config to run an evaluation
-- for a rule. You can specify the following notification types:
--
-- -   @ConfigurationItemChangeNotification@ - Triggers an evaluation when
--     AWS Config delivers a configuration item as a result of a resource
--     change.
--
-- -   @OversizedConfigurationItemChangeNotification@ - Triggers an
--     evaluation when AWS Config delivers an oversized configuration item.
--     AWS Config may generate this notification type when a resource
--     changes and the notification exceeds the maximum size allowed by
--     Amazon SNS.
--
-- -   @ScheduledNotification@ - Triggers a periodic evaluation at the
--     frequency specified for @MaximumExecutionFrequency@.
organizationCustomRuleMetadata_organizationConfigRuleTriggerTypes :: Lens.Lens' OrganizationCustomRuleMetadata [OrganizationConfigRuleTriggerType]
organizationCustomRuleMetadata_organizationConfigRuleTriggerTypes = Lens.lens (\OrganizationCustomRuleMetadata' {organizationConfigRuleTriggerTypes} -> organizationConfigRuleTriggerTypes) (\s@OrganizationCustomRuleMetadata' {} a -> s {organizationConfigRuleTriggerTypes = a} :: OrganizationCustomRuleMetadata) Core.. Lens._Coerce

instance Core.FromJSON OrganizationCustomRuleMetadata where
  parseJSON =
    Core.withObject
      "OrganizationCustomRuleMetadata"
      ( \x ->
          OrganizationCustomRuleMetadata'
            Core.<$> (x Core..:? "TagKeyScope")
            Core.<*> (x Core..:? "MaximumExecutionFrequency")
            Core.<*> (x Core..:? "ResourceIdScope")
            Core.<*> (x Core..:? "InputParameters")
            Core.<*> (x Core..:? "Description")
            Core.<*> ( x Core..:? "ResourceTypesScope"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "TagValueScope")
            Core.<*> (x Core..: "LambdaFunctionArn")
            Core.<*> ( x Core..:? "OrganizationConfigRuleTriggerTypes"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable OrganizationCustomRuleMetadata

instance Core.NFData OrganizationCustomRuleMetadata

instance Core.ToJSON OrganizationCustomRuleMetadata where
  toJSON OrganizationCustomRuleMetadata' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TagKeyScope" Core..=) Core.<$> tagKeyScope,
            ("MaximumExecutionFrequency" Core..=)
              Core.<$> maximumExecutionFrequency,
            ("ResourceIdScope" Core..=) Core.<$> resourceIdScope,
            ("InputParameters" Core..=) Core.<$> inputParameters,
            ("Description" Core..=) Core.<$> description,
            ("ResourceTypesScope" Core..=)
              Core.<$> resourceTypesScope,
            ("TagValueScope" Core..=) Core.<$> tagValueScope,
            Core.Just
              ("LambdaFunctionArn" Core..= lambdaFunctionArn),
            Core.Just
              ( "OrganizationConfigRuleTriggerTypes"
                  Core..= organizationConfigRuleTriggerTypes
              )
          ]
      )
