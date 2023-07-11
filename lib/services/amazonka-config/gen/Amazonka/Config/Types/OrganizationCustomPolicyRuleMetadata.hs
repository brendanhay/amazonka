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
-- Module      : Amazonka.Config.Types.OrganizationCustomPolicyRuleMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.OrganizationCustomPolicyRuleMetadata where

import Amazonka.Config.Types.MaximumExecutionFrequency
import Amazonka.Config.Types.OrganizationConfigRuleTriggerTypeNoSN
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that specifies metadata for your organization\'s Config Custom
-- Policy rule. The metadata includes the runtime system in use, which
-- accounts have debug logging enabled, and other custom rule metadata,
-- such as resource type, resource ID of Amazon Web Services resource, and
-- organization trigger types that initiate Config to evaluate Amazon Web
-- Services resources against a rule.
--
-- /See:/ 'newOrganizationCustomPolicyRuleMetadata' smart constructor.
data OrganizationCustomPolicyRuleMetadata = OrganizationCustomPolicyRuleMetadata'
  { -- | A list of accounts that you can enable debug logging for your
    -- organization Config Custom Policy rule. List is null when debug logging
    -- is enabled for all accounts.
    debugLogDeliveryAccounts :: Prelude.Maybe [Prelude.Text],
    -- | The description that you provide for your organization Config Custom
    -- Policy rule.
    description :: Prelude.Maybe Prelude.Text,
    -- | A string, in JSON format, that is passed to your organization Config
    -- Custom Policy rule.
    inputParameters :: Prelude.Maybe Prelude.Text,
    -- | The maximum frequency with which Config runs evaluations for a rule.
    -- Your Config Custom Policy rule is triggered when Config delivers the
    -- configuration snapshot. For more information, see
    -- ConfigSnapshotDeliveryProperties.
    maximumExecutionFrequency :: Prelude.Maybe MaximumExecutionFrequency,
    -- | The type of notification that initiates Config to run an evaluation for
    -- a rule. For Config Custom Policy rules, Config supports change-initiated
    -- notification types:
    --
    -- -   @ConfigurationItemChangeNotification@ - Initiates an evaluation when
    --     Config delivers a configuration item as a result of a resource
    --     change.
    --
    -- -   @OversizedConfigurationItemChangeNotification@ - Initiates an
    --     evaluation when Config delivers an oversized configuration item.
    --     Config may generate this notification type when a resource changes
    --     and the notification exceeds the maximum size allowed by Amazon SNS.
    organizationConfigRuleTriggerTypes :: Prelude.Maybe [OrganizationConfigRuleTriggerTypeNoSN],
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
    -- | The runtime system for your organization Config Custom Policy rules.
    -- Guard is a policy-as-code language that allows you to write policies
    -- that are enforced by Config Custom Policy rules. For more information
    -- about Guard, see the
    -- <https://github.com/aws-cloudformation/cloudformation-guard Guard GitHub Repository>.
    policyRuntime :: Prelude.Text,
    -- | The policy definition containing the logic for your organization Config
    -- Custom Policy rule.
    policyText :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationCustomPolicyRuleMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'debugLogDeliveryAccounts', 'organizationCustomPolicyRuleMetadata_debugLogDeliveryAccounts' - A list of accounts that you can enable debug logging for your
-- organization Config Custom Policy rule. List is null when debug logging
-- is enabled for all accounts.
--
-- 'description', 'organizationCustomPolicyRuleMetadata_description' - The description that you provide for your organization Config Custom
-- Policy rule.
--
-- 'inputParameters', 'organizationCustomPolicyRuleMetadata_inputParameters' - A string, in JSON format, that is passed to your organization Config
-- Custom Policy rule.
--
-- 'maximumExecutionFrequency', 'organizationCustomPolicyRuleMetadata_maximumExecutionFrequency' - The maximum frequency with which Config runs evaluations for a rule.
-- Your Config Custom Policy rule is triggered when Config delivers the
-- configuration snapshot. For more information, see
-- ConfigSnapshotDeliveryProperties.
--
-- 'organizationConfigRuleTriggerTypes', 'organizationCustomPolicyRuleMetadata_organizationConfigRuleTriggerTypes' - The type of notification that initiates Config to run an evaluation for
-- a rule. For Config Custom Policy rules, Config supports change-initiated
-- notification types:
--
-- -   @ConfigurationItemChangeNotification@ - Initiates an evaluation when
--     Config delivers a configuration item as a result of a resource
--     change.
--
-- -   @OversizedConfigurationItemChangeNotification@ - Initiates an
--     evaluation when Config delivers an oversized configuration item.
--     Config may generate this notification type when a resource changes
--     and the notification exceeds the maximum size allowed by Amazon SNS.
--
-- 'resourceIdScope', 'organizationCustomPolicyRuleMetadata_resourceIdScope' - The ID of the Amazon Web Services resource that was evaluated.
--
-- 'resourceTypesScope', 'organizationCustomPolicyRuleMetadata_resourceTypesScope' - The type of the Amazon Web Services resource that was evaluated.
--
-- 'tagKeyScope', 'organizationCustomPolicyRuleMetadata_tagKeyScope' - One part of a key-value pair that make up a tag. A key is a general
-- label that acts like a category for more specific tag values.
--
-- 'tagValueScope', 'organizationCustomPolicyRuleMetadata_tagValueScope' - The optional part of a key-value pair that make up a tag. A value acts
-- as a descriptor within a tag category (key).
--
-- 'policyRuntime', 'organizationCustomPolicyRuleMetadata_policyRuntime' - The runtime system for your organization Config Custom Policy rules.
-- Guard is a policy-as-code language that allows you to write policies
-- that are enforced by Config Custom Policy rules. For more information
-- about Guard, see the
-- <https://github.com/aws-cloudformation/cloudformation-guard Guard GitHub Repository>.
--
-- 'policyText', 'organizationCustomPolicyRuleMetadata_policyText' - The policy definition containing the logic for your organization Config
-- Custom Policy rule.
newOrganizationCustomPolicyRuleMetadata ::
  -- | 'policyRuntime'
  Prelude.Text ->
  -- | 'policyText'
  Prelude.Text ->
  OrganizationCustomPolicyRuleMetadata
newOrganizationCustomPolicyRuleMetadata
  pPolicyRuntime_
  pPolicyText_ =
    OrganizationCustomPolicyRuleMetadata'
      { debugLogDeliveryAccounts =
          Prelude.Nothing,
        description = Prelude.Nothing,
        inputParameters = Prelude.Nothing,
        maximumExecutionFrequency =
          Prelude.Nothing,
        organizationConfigRuleTriggerTypes =
          Prelude.Nothing,
        resourceIdScope = Prelude.Nothing,
        resourceTypesScope = Prelude.Nothing,
        tagKeyScope = Prelude.Nothing,
        tagValueScope = Prelude.Nothing,
        policyRuntime = pPolicyRuntime_,
        policyText = pPolicyText_
      }

-- | A list of accounts that you can enable debug logging for your
-- organization Config Custom Policy rule. List is null when debug logging
-- is enabled for all accounts.
organizationCustomPolicyRuleMetadata_debugLogDeliveryAccounts :: Lens.Lens' OrganizationCustomPolicyRuleMetadata (Prelude.Maybe [Prelude.Text])
organizationCustomPolicyRuleMetadata_debugLogDeliveryAccounts = Lens.lens (\OrganizationCustomPolicyRuleMetadata' {debugLogDeliveryAccounts} -> debugLogDeliveryAccounts) (\s@OrganizationCustomPolicyRuleMetadata' {} a -> s {debugLogDeliveryAccounts = a} :: OrganizationCustomPolicyRuleMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The description that you provide for your organization Config Custom
-- Policy rule.
organizationCustomPolicyRuleMetadata_description :: Lens.Lens' OrganizationCustomPolicyRuleMetadata (Prelude.Maybe Prelude.Text)
organizationCustomPolicyRuleMetadata_description = Lens.lens (\OrganizationCustomPolicyRuleMetadata' {description} -> description) (\s@OrganizationCustomPolicyRuleMetadata' {} a -> s {description = a} :: OrganizationCustomPolicyRuleMetadata)

-- | A string, in JSON format, that is passed to your organization Config
-- Custom Policy rule.
organizationCustomPolicyRuleMetadata_inputParameters :: Lens.Lens' OrganizationCustomPolicyRuleMetadata (Prelude.Maybe Prelude.Text)
organizationCustomPolicyRuleMetadata_inputParameters = Lens.lens (\OrganizationCustomPolicyRuleMetadata' {inputParameters} -> inputParameters) (\s@OrganizationCustomPolicyRuleMetadata' {} a -> s {inputParameters = a} :: OrganizationCustomPolicyRuleMetadata)

-- | The maximum frequency with which Config runs evaluations for a rule.
-- Your Config Custom Policy rule is triggered when Config delivers the
-- configuration snapshot. For more information, see
-- ConfigSnapshotDeliveryProperties.
organizationCustomPolicyRuleMetadata_maximumExecutionFrequency :: Lens.Lens' OrganizationCustomPolicyRuleMetadata (Prelude.Maybe MaximumExecutionFrequency)
organizationCustomPolicyRuleMetadata_maximumExecutionFrequency = Lens.lens (\OrganizationCustomPolicyRuleMetadata' {maximumExecutionFrequency} -> maximumExecutionFrequency) (\s@OrganizationCustomPolicyRuleMetadata' {} a -> s {maximumExecutionFrequency = a} :: OrganizationCustomPolicyRuleMetadata)

-- | The type of notification that initiates Config to run an evaluation for
-- a rule. For Config Custom Policy rules, Config supports change-initiated
-- notification types:
--
-- -   @ConfigurationItemChangeNotification@ - Initiates an evaluation when
--     Config delivers a configuration item as a result of a resource
--     change.
--
-- -   @OversizedConfigurationItemChangeNotification@ - Initiates an
--     evaluation when Config delivers an oversized configuration item.
--     Config may generate this notification type when a resource changes
--     and the notification exceeds the maximum size allowed by Amazon SNS.
organizationCustomPolicyRuleMetadata_organizationConfigRuleTriggerTypes :: Lens.Lens' OrganizationCustomPolicyRuleMetadata (Prelude.Maybe [OrganizationConfigRuleTriggerTypeNoSN])
organizationCustomPolicyRuleMetadata_organizationConfigRuleTriggerTypes = Lens.lens (\OrganizationCustomPolicyRuleMetadata' {organizationConfigRuleTriggerTypes} -> organizationConfigRuleTriggerTypes) (\s@OrganizationCustomPolicyRuleMetadata' {} a -> s {organizationConfigRuleTriggerTypes = a} :: OrganizationCustomPolicyRuleMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services resource that was evaluated.
organizationCustomPolicyRuleMetadata_resourceIdScope :: Lens.Lens' OrganizationCustomPolicyRuleMetadata (Prelude.Maybe Prelude.Text)
organizationCustomPolicyRuleMetadata_resourceIdScope = Lens.lens (\OrganizationCustomPolicyRuleMetadata' {resourceIdScope} -> resourceIdScope) (\s@OrganizationCustomPolicyRuleMetadata' {} a -> s {resourceIdScope = a} :: OrganizationCustomPolicyRuleMetadata)

-- | The type of the Amazon Web Services resource that was evaluated.
organizationCustomPolicyRuleMetadata_resourceTypesScope :: Lens.Lens' OrganizationCustomPolicyRuleMetadata (Prelude.Maybe [Prelude.Text])
organizationCustomPolicyRuleMetadata_resourceTypesScope = Lens.lens (\OrganizationCustomPolicyRuleMetadata' {resourceTypesScope} -> resourceTypesScope) (\s@OrganizationCustomPolicyRuleMetadata' {} a -> s {resourceTypesScope = a} :: OrganizationCustomPolicyRuleMetadata) Prelude.. Lens.mapping Lens.coerced

-- | One part of a key-value pair that make up a tag. A key is a general
-- label that acts like a category for more specific tag values.
organizationCustomPolicyRuleMetadata_tagKeyScope :: Lens.Lens' OrganizationCustomPolicyRuleMetadata (Prelude.Maybe Prelude.Text)
organizationCustomPolicyRuleMetadata_tagKeyScope = Lens.lens (\OrganizationCustomPolicyRuleMetadata' {tagKeyScope} -> tagKeyScope) (\s@OrganizationCustomPolicyRuleMetadata' {} a -> s {tagKeyScope = a} :: OrganizationCustomPolicyRuleMetadata)

-- | The optional part of a key-value pair that make up a tag. A value acts
-- as a descriptor within a tag category (key).
organizationCustomPolicyRuleMetadata_tagValueScope :: Lens.Lens' OrganizationCustomPolicyRuleMetadata (Prelude.Maybe Prelude.Text)
organizationCustomPolicyRuleMetadata_tagValueScope = Lens.lens (\OrganizationCustomPolicyRuleMetadata' {tagValueScope} -> tagValueScope) (\s@OrganizationCustomPolicyRuleMetadata' {} a -> s {tagValueScope = a} :: OrganizationCustomPolicyRuleMetadata)

-- | The runtime system for your organization Config Custom Policy rules.
-- Guard is a policy-as-code language that allows you to write policies
-- that are enforced by Config Custom Policy rules. For more information
-- about Guard, see the
-- <https://github.com/aws-cloudformation/cloudformation-guard Guard GitHub Repository>.
organizationCustomPolicyRuleMetadata_policyRuntime :: Lens.Lens' OrganizationCustomPolicyRuleMetadata Prelude.Text
organizationCustomPolicyRuleMetadata_policyRuntime = Lens.lens (\OrganizationCustomPolicyRuleMetadata' {policyRuntime} -> policyRuntime) (\s@OrganizationCustomPolicyRuleMetadata' {} a -> s {policyRuntime = a} :: OrganizationCustomPolicyRuleMetadata)

-- | The policy definition containing the logic for your organization Config
-- Custom Policy rule.
organizationCustomPolicyRuleMetadata_policyText :: Lens.Lens' OrganizationCustomPolicyRuleMetadata Prelude.Text
organizationCustomPolicyRuleMetadata_policyText = Lens.lens (\OrganizationCustomPolicyRuleMetadata' {policyText} -> policyText) (\s@OrganizationCustomPolicyRuleMetadata' {} a -> s {policyText = a} :: OrganizationCustomPolicyRuleMetadata)

instance
  Prelude.Hashable
    OrganizationCustomPolicyRuleMetadata
  where
  hashWithSalt
    _salt
    OrganizationCustomPolicyRuleMetadata' {..} =
      _salt
        `Prelude.hashWithSalt` debugLogDeliveryAccounts
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` inputParameters
        `Prelude.hashWithSalt` maximumExecutionFrequency
        `Prelude.hashWithSalt` organizationConfigRuleTriggerTypes
        `Prelude.hashWithSalt` resourceIdScope
        `Prelude.hashWithSalt` resourceTypesScope
        `Prelude.hashWithSalt` tagKeyScope
        `Prelude.hashWithSalt` tagValueScope
        `Prelude.hashWithSalt` policyRuntime
        `Prelude.hashWithSalt` policyText

instance
  Prelude.NFData
    OrganizationCustomPolicyRuleMetadata
  where
  rnf OrganizationCustomPolicyRuleMetadata' {..} =
    Prelude.rnf debugLogDeliveryAccounts
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf inputParameters
      `Prelude.seq` Prelude.rnf maximumExecutionFrequency
      `Prelude.seq` Prelude.rnf organizationConfigRuleTriggerTypes
      `Prelude.seq` Prelude.rnf resourceIdScope
      `Prelude.seq` Prelude.rnf resourceTypesScope
      `Prelude.seq` Prelude.rnf tagKeyScope
      `Prelude.seq` Prelude.rnf tagValueScope
      `Prelude.seq` Prelude.rnf policyRuntime
      `Prelude.seq` Prelude.rnf policyText

instance
  Data.ToJSON
    OrganizationCustomPolicyRuleMetadata
  where
  toJSON OrganizationCustomPolicyRuleMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DebugLogDeliveryAccounts" Data..=)
              Prelude.<$> debugLogDeliveryAccounts,
            ("Description" Data..=) Prelude.<$> description,
            ("InputParameters" Data..=)
              Prelude.<$> inputParameters,
            ("MaximumExecutionFrequency" Data..=)
              Prelude.<$> maximumExecutionFrequency,
            ("OrganizationConfigRuleTriggerTypes" Data..=)
              Prelude.<$> organizationConfigRuleTriggerTypes,
            ("ResourceIdScope" Data..=)
              Prelude.<$> resourceIdScope,
            ("ResourceTypesScope" Data..=)
              Prelude.<$> resourceTypesScope,
            ("TagKeyScope" Data..=) Prelude.<$> tagKeyScope,
            ("TagValueScope" Data..=) Prelude.<$> tagValueScope,
            Prelude.Just ("PolicyRuntime" Data..= policyRuntime),
            Prelude.Just ("PolicyText" Data..= policyText)
          ]
      )
