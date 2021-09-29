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
import qualified Network.AWS.Prelude as Prelude

-- | An object that specifies organization custom rule metadata such as
-- resource type, resource ID of Amazon Web Services resource, Lambda
-- function ARN, and organization trigger types that trigger Config to
-- evaluate your Amazon Web Services resources against a rule. It also
-- provides the frequency with which you want Config to run evaluations for
-- the rule if the trigger type is periodic.
--
-- /See:/ 'newOrganizationCustomRuleMetadata' smart constructor.
data OrganizationCustomRuleMetadata = OrganizationCustomRuleMetadata'
  { -- | One part of a key-value pair that make up a tag. A key is a general
    -- label that acts like a category for more specific tag values.
    tagKeyScope :: Prelude.Maybe Prelude.Text,
    -- | The maximum frequency with which Config runs evaluations for a rule.
    -- Your custom rule is triggered when Config delivers the configuration
    -- snapshot. For more information, see ConfigSnapshotDeliveryProperties.
    --
    -- By default, rules with a periodic trigger are evaluated every 24 hours.
    -- To change the frequency, specify a valid value for the
    -- @MaximumExecutionFrequency@ parameter.
    maximumExecutionFrequency :: Prelude.Maybe MaximumExecutionFrequency,
    -- | The ID of the Amazon Web Services resource that was evaluated.
    resourceIdScope :: Prelude.Maybe Prelude.Text,
    -- | A string, in JSON format, that is passed to organization config rule
    -- Lambda function.
    inputParameters :: Prelude.Maybe Prelude.Text,
    -- | The description that you provide for organization config rule.
    description :: Prelude.Maybe Prelude.Text,
    -- | The type of the Amazon Web Services resource that was evaluated.
    resourceTypesScope :: Prelude.Maybe [Prelude.Text],
    -- | The optional part of a key-value pair that make up a tag. A value acts
    -- as a descriptor within a tag category (key).
    tagValueScope :: Prelude.Maybe Prelude.Text,
    -- | The lambda function ARN.
    lambdaFunctionArn :: Prelude.Text,
    -- | The type of notification that triggers Config to run an evaluation for a
    -- rule. You can specify the following notification types:
    --
    -- -   @ConfigurationItemChangeNotification@ - Triggers an evaluation when
    --     Config delivers a configuration item as a result of a resource
    --     change.
    --
    -- -   @OversizedConfigurationItemChangeNotification@ - Triggers an
    --     evaluation when Config delivers an oversized configuration item.
    --     Config may generate this notification type when a resource changes
    --     and the notification exceeds the maximum size allowed by Amazon SNS.
    --
    -- -   @ScheduledNotification@ - Triggers a periodic evaluation at the
    --     frequency specified for @MaximumExecutionFrequency@.
    organizationConfigRuleTriggerTypes :: [OrganizationConfigRuleTriggerType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'maximumExecutionFrequency', 'organizationCustomRuleMetadata_maximumExecutionFrequency' - The maximum frequency with which Config runs evaluations for a rule.
-- Your custom rule is triggered when Config delivers the configuration
-- snapshot. For more information, see ConfigSnapshotDeliveryProperties.
--
-- By default, rules with a periodic trigger are evaluated every 24 hours.
-- To change the frequency, specify a valid value for the
-- @MaximumExecutionFrequency@ parameter.
--
-- 'resourceIdScope', 'organizationCustomRuleMetadata_resourceIdScope' - The ID of the Amazon Web Services resource that was evaluated.
--
-- 'inputParameters', 'organizationCustomRuleMetadata_inputParameters' - A string, in JSON format, that is passed to organization config rule
-- Lambda function.
--
-- 'description', 'organizationCustomRuleMetadata_description' - The description that you provide for organization config rule.
--
-- 'resourceTypesScope', 'organizationCustomRuleMetadata_resourceTypesScope' - The type of the Amazon Web Services resource that was evaluated.
--
-- 'tagValueScope', 'organizationCustomRuleMetadata_tagValueScope' - The optional part of a key-value pair that make up a tag. A value acts
-- as a descriptor within a tag category (key).
--
-- 'lambdaFunctionArn', 'organizationCustomRuleMetadata_lambdaFunctionArn' - The lambda function ARN.
--
-- 'organizationConfigRuleTriggerTypes', 'organizationCustomRuleMetadata_organizationConfigRuleTriggerTypes' - The type of notification that triggers Config to run an evaluation for a
-- rule. You can specify the following notification types:
--
-- -   @ConfigurationItemChangeNotification@ - Triggers an evaluation when
--     Config delivers a configuration item as a result of a resource
--     change.
--
-- -   @OversizedConfigurationItemChangeNotification@ - Triggers an
--     evaluation when Config delivers an oversized configuration item.
--     Config may generate this notification type when a resource changes
--     and the notification exceeds the maximum size allowed by Amazon SNS.
--
-- -   @ScheduledNotification@ - Triggers a periodic evaluation at the
--     frequency specified for @MaximumExecutionFrequency@.
newOrganizationCustomRuleMetadata ::
  -- | 'lambdaFunctionArn'
  Prelude.Text ->
  OrganizationCustomRuleMetadata
newOrganizationCustomRuleMetadata pLambdaFunctionArn_ =
  OrganizationCustomRuleMetadata'
    { tagKeyScope =
        Prelude.Nothing,
      maximumExecutionFrequency = Prelude.Nothing,
      resourceIdScope = Prelude.Nothing,
      inputParameters = Prelude.Nothing,
      description = Prelude.Nothing,
      resourceTypesScope = Prelude.Nothing,
      tagValueScope = Prelude.Nothing,
      lambdaFunctionArn = pLambdaFunctionArn_,
      organizationConfigRuleTriggerTypes =
        Prelude.mempty
    }

-- | One part of a key-value pair that make up a tag. A key is a general
-- label that acts like a category for more specific tag values.
organizationCustomRuleMetadata_tagKeyScope :: Lens.Lens' OrganizationCustomRuleMetadata (Prelude.Maybe Prelude.Text)
organizationCustomRuleMetadata_tagKeyScope = Lens.lens (\OrganizationCustomRuleMetadata' {tagKeyScope} -> tagKeyScope) (\s@OrganizationCustomRuleMetadata' {} a -> s {tagKeyScope = a} :: OrganizationCustomRuleMetadata)

-- | The maximum frequency with which Config runs evaluations for a rule.
-- Your custom rule is triggered when Config delivers the configuration
-- snapshot. For more information, see ConfigSnapshotDeliveryProperties.
--
-- By default, rules with a periodic trigger are evaluated every 24 hours.
-- To change the frequency, specify a valid value for the
-- @MaximumExecutionFrequency@ parameter.
organizationCustomRuleMetadata_maximumExecutionFrequency :: Lens.Lens' OrganizationCustomRuleMetadata (Prelude.Maybe MaximumExecutionFrequency)
organizationCustomRuleMetadata_maximumExecutionFrequency = Lens.lens (\OrganizationCustomRuleMetadata' {maximumExecutionFrequency} -> maximumExecutionFrequency) (\s@OrganizationCustomRuleMetadata' {} a -> s {maximumExecutionFrequency = a} :: OrganizationCustomRuleMetadata)

-- | The ID of the Amazon Web Services resource that was evaluated.
organizationCustomRuleMetadata_resourceIdScope :: Lens.Lens' OrganizationCustomRuleMetadata (Prelude.Maybe Prelude.Text)
organizationCustomRuleMetadata_resourceIdScope = Lens.lens (\OrganizationCustomRuleMetadata' {resourceIdScope} -> resourceIdScope) (\s@OrganizationCustomRuleMetadata' {} a -> s {resourceIdScope = a} :: OrganizationCustomRuleMetadata)

-- | A string, in JSON format, that is passed to organization config rule
-- Lambda function.
organizationCustomRuleMetadata_inputParameters :: Lens.Lens' OrganizationCustomRuleMetadata (Prelude.Maybe Prelude.Text)
organizationCustomRuleMetadata_inputParameters = Lens.lens (\OrganizationCustomRuleMetadata' {inputParameters} -> inputParameters) (\s@OrganizationCustomRuleMetadata' {} a -> s {inputParameters = a} :: OrganizationCustomRuleMetadata)

-- | The description that you provide for organization config rule.
organizationCustomRuleMetadata_description :: Lens.Lens' OrganizationCustomRuleMetadata (Prelude.Maybe Prelude.Text)
organizationCustomRuleMetadata_description = Lens.lens (\OrganizationCustomRuleMetadata' {description} -> description) (\s@OrganizationCustomRuleMetadata' {} a -> s {description = a} :: OrganizationCustomRuleMetadata)

-- | The type of the Amazon Web Services resource that was evaluated.
organizationCustomRuleMetadata_resourceTypesScope :: Lens.Lens' OrganizationCustomRuleMetadata (Prelude.Maybe [Prelude.Text])
organizationCustomRuleMetadata_resourceTypesScope = Lens.lens (\OrganizationCustomRuleMetadata' {resourceTypesScope} -> resourceTypesScope) (\s@OrganizationCustomRuleMetadata' {} a -> s {resourceTypesScope = a} :: OrganizationCustomRuleMetadata) Prelude.. Lens.mapping Lens._Coerce

-- | The optional part of a key-value pair that make up a tag. A value acts
-- as a descriptor within a tag category (key).
organizationCustomRuleMetadata_tagValueScope :: Lens.Lens' OrganizationCustomRuleMetadata (Prelude.Maybe Prelude.Text)
organizationCustomRuleMetadata_tagValueScope = Lens.lens (\OrganizationCustomRuleMetadata' {tagValueScope} -> tagValueScope) (\s@OrganizationCustomRuleMetadata' {} a -> s {tagValueScope = a} :: OrganizationCustomRuleMetadata)

-- | The lambda function ARN.
organizationCustomRuleMetadata_lambdaFunctionArn :: Lens.Lens' OrganizationCustomRuleMetadata Prelude.Text
organizationCustomRuleMetadata_lambdaFunctionArn = Lens.lens (\OrganizationCustomRuleMetadata' {lambdaFunctionArn} -> lambdaFunctionArn) (\s@OrganizationCustomRuleMetadata' {} a -> s {lambdaFunctionArn = a} :: OrganizationCustomRuleMetadata)

-- | The type of notification that triggers Config to run an evaluation for a
-- rule. You can specify the following notification types:
--
-- -   @ConfigurationItemChangeNotification@ - Triggers an evaluation when
--     Config delivers a configuration item as a result of a resource
--     change.
--
-- -   @OversizedConfigurationItemChangeNotification@ - Triggers an
--     evaluation when Config delivers an oversized configuration item.
--     Config may generate this notification type when a resource changes
--     and the notification exceeds the maximum size allowed by Amazon SNS.
--
-- -   @ScheduledNotification@ - Triggers a periodic evaluation at the
--     frequency specified for @MaximumExecutionFrequency@.
organizationCustomRuleMetadata_organizationConfigRuleTriggerTypes :: Lens.Lens' OrganizationCustomRuleMetadata [OrganizationConfigRuleTriggerType]
organizationCustomRuleMetadata_organizationConfigRuleTriggerTypes = Lens.lens (\OrganizationCustomRuleMetadata' {organizationConfigRuleTriggerTypes} -> organizationConfigRuleTriggerTypes) (\s@OrganizationCustomRuleMetadata' {} a -> s {organizationConfigRuleTriggerTypes = a} :: OrganizationCustomRuleMetadata) Prelude.. Lens._Coerce

instance Core.FromJSON OrganizationCustomRuleMetadata where
  parseJSON =
    Core.withObject
      "OrganizationCustomRuleMetadata"
      ( \x ->
          OrganizationCustomRuleMetadata'
            Prelude.<$> (x Core..:? "TagKeyScope")
            Prelude.<*> (x Core..:? "MaximumExecutionFrequency")
            Prelude.<*> (x Core..:? "ResourceIdScope")
            Prelude.<*> (x Core..:? "InputParameters")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> ( x Core..:? "ResourceTypesScope"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "TagValueScope")
            Prelude.<*> (x Core..: "LambdaFunctionArn")
            Prelude.<*> ( x Core..:? "OrganizationConfigRuleTriggerTypes"
                            Core..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    OrganizationCustomRuleMetadata

instance
  Prelude.NFData
    OrganizationCustomRuleMetadata

instance Core.ToJSON OrganizationCustomRuleMetadata where
  toJSON OrganizationCustomRuleMetadata' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TagKeyScope" Core..=) Prelude.<$> tagKeyScope,
            ("MaximumExecutionFrequency" Core..=)
              Prelude.<$> maximumExecutionFrequency,
            ("ResourceIdScope" Core..=)
              Prelude.<$> resourceIdScope,
            ("InputParameters" Core..=)
              Prelude.<$> inputParameters,
            ("Description" Core..=) Prelude.<$> description,
            ("ResourceTypesScope" Core..=)
              Prelude.<$> resourceTypesScope,
            ("TagValueScope" Core..=) Prelude.<$> tagValueScope,
            Prelude.Just
              ("LambdaFunctionArn" Core..= lambdaFunctionArn),
            Prelude.Just
              ( "OrganizationConfigRuleTriggerTypes"
                  Core..= organizationConfigRuleTriggerTypes
              )
          ]
      )
