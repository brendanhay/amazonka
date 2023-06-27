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
-- Module      : Amazonka.Config.Types.OrganizationCustomRuleMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.OrganizationCustomRuleMetadata where

import Amazonka.Config.Types.MaximumExecutionFrequency
import Amazonka.Config.Types.OrganizationConfigRuleTriggerType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | organization custom rule metadata such as resource type, resource ID of
-- Amazon Web Services resource, Lambda function ARN, and organization
-- trigger types that trigger Config to evaluate your Amazon Web Services
-- resources against a rule. It also provides the frequency with which you
-- want Config to run evaluations for the rule if the trigger type is
-- periodic.
--
-- /See:/ 'newOrganizationCustomRuleMetadata' smart constructor.
data OrganizationCustomRuleMetadata = OrganizationCustomRuleMetadata'
  { -- | The description that you provide for your organization Config rule.
    description :: Prelude.Maybe Prelude.Text,
    -- | A string, in JSON format, that is passed to your organization Config
    -- rule Lambda function.
    inputParameters :: Prelude.Maybe Prelude.Text,
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
    -- | The type of the Amazon Web Services resource that was evaluated.
    resourceTypesScope :: Prelude.Maybe [Prelude.Text],
    -- | One part of a key-value pair that make up a tag. A key is a general
    -- label that acts like a category for more specific tag values.
    tagKeyScope :: Prelude.Maybe Prelude.Text,
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
-- 'description', 'organizationCustomRuleMetadata_description' - The description that you provide for your organization Config rule.
--
-- 'inputParameters', 'organizationCustomRuleMetadata_inputParameters' - A string, in JSON format, that is passed to your organization Config
-- rule Lambda function.
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
-- 'resourceTypesScope', 'organizationCustomRuleMetadata_resourceTypesScope' - The type of the Amazon Web Services resource that was evaluated.
--
-- 'tagKeyScope', 'organizationCustomRuleMetadata_tagKeyScope' - One part of a key-value pair that make up a tag. A key is a general
-- label that acts like a category for more specific tag values.
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
    { description =
        Prelude.Nothing,
      inputParameters = Prelude.Nothing,
      maximumExecutionFrequency = Prelude.Nothing,
      resourceIdScope = Prelude.Nothing,
      resourceTypesScope = Prelude.Nothing,
      tagKeyScope = Prelude.Nothing,
      tagValueScope = Prelude.Nothing,
      lambdaFunctionArn = pLambdaFunctionArn_,
      organizationConfigRuleTriggerTypes =
        Prelude.mempty
    }

-- | The description that you provide for your organization Config rule.
organizationCustomRuleMetadata_description :: Lens.Lens' OrganizationCustomRuleMetadata (Prelude.Maybe Prelude.Text)
organizationCustomRuleMetadata_description = Lens.lens (\OrganizationCustomRuleMetadata' {description} -> description) (\s@OrganizationCustomRuleMetadata' {} a -> s {description = a} :: OrganizationCustomRuleMetadata)

-- | A string, in JSON format, that is passed to your organization Config
-- rule Lambda function.
organizationCustomRuleMetadata_inputParameters :: Lens.Lens' OrganizationCustomRuleMetadata (Prelude.Maybe Prelude.Text)
organizationCustomRuleMetadata_inputParameters = Lens.lens (\OrganizationCustomRuleMetadata' {inputParameters} -> inputParameters) (\s@OrganizationCustomRuleMetadata' {} a -> s {inputParameters = a} :: OrganizationCustomRuleMetadata)

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

-- | The type of the Amazon Web Services resource that was evaluated.
organizationCustomRuleMetadata_resourceTypesScope :: Lens.Lens' OrganizationCustomRuleMetadata (Prelude.Maybe [Prelude.Text])
organizationCustomRuleMetadata_resourceTypesScope = Lens.lens (\OrganizationCustomRuleMetadata' {resourceTypesScope} -> resourceTypesScope) (\s@OrganizationCustomRuleMetadata' {} a -> s {resourceTypesScope = a} :: OrganizationCustomRuleMetadata) Prelude.. Lens.mapping Lens.coerced

-- | One part of a key-value pair that make up a tag. A key is a general
-- label that acts like a category for more specific tag values.
organizationCustomRuleMetadata_tagKeyScope :: Lens.Lens' OrganizationCustomRuleMetadata (Prelude.Maybe Prelude.Text)
organizationCustomRuleMetadata_tagKeyScope = Lens.lens (\OrganizationCustomRuleMetadata' {tagKeyScope} -> tagKeyScope) (\s@OrganizationCustomRuleMetadata' {} a -> s {tagKeyScope = a} :: OrganizationCustomRuleMetadata)

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
organizationCustomRuleMetadata_organizationConfigRuleTriggerTypes = Lens.lens (\OrganizationCustomRuleMetadata' {organizationConfigRuleTriggerTypes} -> organizationConfigRuleTriggerTypes) (\s@OrganizationCustomRuleMetadata' {} a -> s {organizationConfigRuleTriggerTypes = a} :: OrganizationCustomRuleMetadata) Prelude.. Lens.coerced

instance Data.FromJSON OrganizationCustomRuleMetadata where
  parseJSON =
    Data.withObject
      "OrganizationCustomRuleMetadata"
      ( \x ->
          OrganizationCustomRuleMetadata'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "InputParameters")
            Prelude.<*> (x Data..:? "MaximumExecutionFrequency")
            Prelude.<*> (x Data..:? "ResourceIdScope")
            Prelude.<*> ( x
                            Data..:? "ResourceTypesScope"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "TagKeyScope")
            Prelude.<*> (x Data..:? "TagValueScope")
            Prelude.<*> (x Data..: "LambdaFunctionArn")
            Prelude.<*> ( x
                            Data..:? "OrganizationConfigRuleTriggerTypes"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    OrganizationCustomRuleMetadata
  where
  hashWithSalt
    _salt
    OrganizationCustomRuleMetadata' {..} =
      _salt
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` inputParameters
        `Prelude.hashWithSalt` maximumExecutionFrequency
        `Prelude.hashWithSalt` resourceIdScope
        `Prelude.hashWithSalt` resourceTypesScope
        `Prelude.hashWithSalt` tagKeyScope
        `Prelude.hashWithSalt` tagValueScope
        `Prelude.hashWithSalt` lambdaFunctionArn
        `Prelude.hashWithSalt` organizationConfigRuleTriggerTypes

instance
  Prelude.NFData
    OrganizationCustomRuleMetadata
  where
  rnf OrganizationCustomRuleMetadata' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf inputParameters
      `Prelude.seq` Prelude.rnf maximumExecutionFrequency
      `Prelude.seq` Prelude.rnf resourceIdScope
      `Prelude.seq` Prelude.rnf resourceTypesScope
      `Prelude.seq` Prelude.rnf tagKeyScope
      `Prelude.seq` Prelude.rnf tagValueScope
      `Prelude.seq` Prelude.rnf lambdaFunctionArn
      `Prelude.seq` Prelude.rnf organizationConfigRuleTriggerTypes

instance Data.ToJSON OrganizationCustomRuleMetadata where
  toJSON OrganizationCustomRuleMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("InputParameters" Data..=)
              Prelude.<$> inputParameters,
            ("MaximumExecutionFrequency" Data..=)
              Prelude.<$> maximumExecutionFrequency,
            ("ResourceIdScope" Data..=)
              Prelude.<$> resourceIdScope,
            ("ResourceTypesScope" Data..=)
              Prelude.<$> resourceTypesScope,
            ("TagKeyScope" Data..=) Prelude.<$> tagKeyScope,
            ("TagValueScope" Data..=) Prelude.<$> tagValueScope,
            Prelude.Just
              ("LambdaFunctionArn" Data..= lambdaFunctionArn),
            Prelude.Just
              ( "OrganizationConfigRuleTriggerTypes"
                  Data..= organizationConfigRuleTriggerTypes
              )
          ]
      )
