{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Config.Types.OrganizationManagedRuleMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationManagedRuleMetadata where

import Network.AWS.Config.Types.MaximumExecutionFrequency
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that specifies organization managed rule metadata such as
-- resource type and ID of AWS resource along with the rule identifier. It
-- also provides the frequency with which you want AWS Config to run
-- evaluations for the rule if the trigger type is periodic.
--
-- /See:/ 'newOrganizationManagedRuleMetadata' smart constructor.
data OrganizationManagedRuleMetadata = OrganizationManagedRuleMetadata'
  { -- | One part of a key-value pair that make up a tag. A key is a general
    -- label that acts like a category for more specific tag values.
    tagKeyScope :: Prelude.Maybe Prelude.Text,
    -- | The maximum frequency with which AWS Config runs evaluations for a rule.
    -- You are using an AWS managed rule that is triggered at a periodic
    -- frequency.
    --
    -- By default, rules with a periodic trigger are evaluated every 24 hours.
    -- To change the frequency, specify a valid value for the
    -- @MaximumExecutionFrequency@ parameter.
    maximumExecutionFrequency :: Prelude.Maybe MaximumExecutionFrequency,
    -- | The ID of the AWS resource that was evaluated.
    resourceIdScope :: Prelude.Maybe Prelude.Text,
    -- | A string, in JSON format, that is passed to organization config rule
    -- Lambda function.
    inputParameters :: Prelude.Maybe Prelude.Text,
    -- | The description that you provide for organization config rule.
    description :: Prelude.Maybe Prelude.Text,
    -- | The type of the AWS resource that was evaluated.
    resourceTypesScope :: Prelude.Maybe [Prelude.Text],
    -- | The optional part of a key-value pair that make up a tag. A value acts
    -- as a descriptor within a tag category (key).
    tagValueScope :: Prelude.Maybe Prelude.Text,
    -- | For organization config managed rules, a predefined identifier from a
    -- list. For example, @IAM_PASSWORD_POLICY@ is a managed rule. To reference
    -- a managed rule, see
    -- <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html Using AWS Managed Config Rules>.
    ruleIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OrganizationManagedRuleMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagKeyScope', 'organizationManagedRuleMetadata_tagKeyScope' - One part of a key-value pair that make up a tag. A key is a general
-- label that acts like a category for more specific tag values.
--
-- 'maximumExecutionFrequency', 'organizationManagedRuleMetadata_maximumExecutionFrequency' - The maximum frequency with which AWS Config runs evaluations for a rule.
-- You are using an AWS managed rule that is triggered at a periodic
-- frequency.
--
-- By default, rules with a periodic trigger are evaluated every 24 hours.
-- To change the frequency, specify a valid value for the
-- @MaximumExecutionFrequency@ parameter.
--
-- 'resourceIdScope', 'organizationManagedRuleMetadata_resourceIdScope' - The ID of the AWS resource that was evaluated.
--
-- 'inputParameters', 'organizationManagedRuleMetadata_inputParameters' - A string, in JSON format, that is passed to organization config rule
-- Lambda function.
--
-- 'description', 'organizationManagedRuleMetadata_description' - The description that you provide for organization config rule.
--
-- 'resourceTypesScope', 'organizationManagedRuleMetadata_resourceTypesScope' - The type of the AWS resource that was evaluated.
--
-- 'tagValueScope', 'organizationManagedRuleMetadata_tagValueScope' - The optional part of a key-value pair that make up a tag. A value acts
-- as a descriptor within a tag category (key).
--
-- 'ruleIdentifier', 'organizationManagedRuleMetadata_ruleIdentifier' - For organization config managed rules, a predefined identifier from a
-- list. For example, @IAM_PASSWORD_POLICY@ is a managed rule. To reference
-- a managed rule, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html Using AWS Managed Config Rules>.
newOrganizationManagedRuleMetadata ::
  -- | 'ruleIdentifier'
  Prelude.Text ->
  OrganizationManagedRuleMetadata
newOrganizationManagedRuleMetadata pRuleIdentifier_ =
  OrganizationManagedRuleMetadata'
    { tagKeyScope =
        Prelude.Nothing,
      maximumExecutionFrequency =
        Prelude.Nothing,
      resourceIdScope = Prelude.Nothing,
      inputParameters = Prelude.Nothing,
      description = Prelude.Nothing,
      resourceTypesScope = Prelude.Nothing,
      tagValueScope = Prelude.Nothing,
      ruleIdentifier = pRuleIdentifier_
    }

-- | One part of a key-value pair that make up a tag. A key is a general
-- label that acts like a category for more specific tag values.
organizationManagedRuleMetadata_tagKeyScope :: Lens.Lens' OrganizationManagedRuleMetadata (Prelude.Maybe Prelude.Text)
organizationManagedRuleMetadata_tagKeyScope = Lens.lens (\OrganizationManagedRuleMetadata' {tagKeyScope} -> tagKeyScope) (\s@OrganizationManagedRuleMetadata' {} a -> s {tagKeyScope = a} :: OrganizationManagedRuleMetadata)

-- | The maximum frequency with which AWS Config runs evaluations for a rule.
-- You are using an AWS managed rule that is triggered at a periodic
-- frequency.
--
-- By default, rules with a periodic trigger are evaluated every 24 hours.
-- To change the frequency, specify a valid value for the
-- @MaximumExecutionFrequency@ parameter.
organizationManagedRuleMetadata_maximumExecutionFrequency :: Lens.Lens' OrganizationManagedRuleMetadata (Prelude.Maybe MaximumExecutionFrequency)
organizationManagedRuleMetadata_maximumExecutionFrequency = Lens.lens (\OrganizationManagedRuleMetadata' {maximumExecutionFrequency} -> maximumExecutionFrequency) (\s@OrganizationManagedRuleMetadata' {} a -> s {maximumExecutionFrequency = a} :: OrganizationManagedRuleMetadata)

-- | The ID of the AWS resource that was evaluated.
organizationManagedRuleMetadata_resourceIdScope :: Lens.Lens' OrganizationManagedRuleMetadata (Prelude.Maybe Prelude.Text)
organizationManagedRuleMetadata_resourceIdScope = Lens.lens (\OrganizationManagedRuleMetadata' {resourceIdScope} -> resourceIdScope) (\s@OrganizationManagedRuleMetadata' {} a -> s {resourceIdScope = a} :: OrganizationManagedRuleMetadata)

-- | A string, in JSON format, that is passed to organization config rule
-- Lambda function.
organizationManagedRuleMetadata_inputParameters :: Lens.Lens' OrganizationManagedRuleMetadata (Prelude.Maybe Prelude.Text)
organizationManagedRuleMetadata_inputParameters = Lens.lens (\OrganizationManagedRuleMetadata' {inputParameters} -> inputParameters) (\s@OrganizationManagedRuleMetadata' {} a -> s {inputParameters = a} :: OrganizationManagedRuleMetadata)

-- | The description that you provide for organization config rule.
organizationManagedRuleMetadata_description :: Lens.Lens' OrganizationManagedRuleMetadata (Prelude.Maybe Prelude.Text)
organizationManagedRuleMetadata_description = Lens.lens (\OrganizationManagedRuleMetadata' {description} -> description) (\s@OrganizationManagedRuleMetadata' {} a -> s {description = a} :: OrganizationManagedRuleMetadata)

-- | The type of the AWS resource that was evaluated.
organizationManagedRuleMetadata_resourceTypesScope :: Lens.Lens' OrganizationManagedRuleMetadata (Prelude.Maybe [Prelude.Text])
organizationManagedRuleMetadata_resourceTypesScope = Lens.lens (\OrganizationManagedRuleMetadata' {resourceTypesScope} -> resourceTypesScope) (\s@OrganizationManagedRuleMetadata' {} a -> s {resourceTypesScope = a} :: OrganizationManagedRuleMetadata) Prelude.. Lens.mapping Prelude._Coerce

-- | The optional part of a key-value pair that make up a tag. A value acts
-- as a descriptor within a tag category (key).
organizationManagedRuleMetadata_tagValueScope :: Lens.Lens' OrganizationManagedRuleMetadata (Prelude.Maybe Prelude.Text)
organizationManagedRuleMetadata_tagValueScope = Lens.lens (\OrganizationManagedRuleMetadata' {tagValueScope} -> tagValueScope) (\s@OrganizationManagedRuleMetadata' {} a -> s {tagValueScope = a} :: OrganizationManagedRuleMetadata)

-- | For organization config managed rules, a predefined identifier from a
-- list. For example, @IAM_PASSWORD_POLICY@ is a managed rule. To reference
-- a managed rule, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html Using AWS Managed Config Rules>.
organizationManagedRuleMetadata_ruleIdentifier :: Lens.Lens' OrganizationManagedRuleMetadata Prelude.Text
organizationManagedRuleMetadata_ruleIdentifier = Lens.lens (\OrganizationManagedRuleMetadata' {ruleIdentifier} -> ruleIdentifier) (\s@OrganizationManagedRuleMetadata' {} a -> s {ruleIdentifier = a} :: OrganizationManagedRuleMetadata)

instance
  Prelude.FromJSON
    OrganizationManagedRuleMetadata
  where
  parseJSON =
    Prelude.withObject
      "OrganizationManagedRuleMetadata"
      ( \x ->
          OrganizationManagedRuleMetadata'
            Prelude.<$> (x Prelude..:? "TagKeyScope")
            Prelude.<*> (x Prelude..:? "MaximumExecutionFrequency")
            Prelude.<*> (x Prelude..:? "ResourceIdScope")
            Prelude.<*> (x Prelude..:? "InputParameters")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> ( x Prelude..:? "ResourceTypesScope"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "TagValueScope")
            Prelude.<*> (x Prelude..: "RuleIdentifier")
      )

instance
  Prelude.Hashable
    OrganizationManagedRuleMetadata

instance
  Prelude.NFData
    OrganizationManagedRuleMetadata

instance
  Prelude.ToJSON
    OrganizationManagedRuleMetadata
  where
  toJSON OrganizationManagedRuleMetadata' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("TagKeyScope" Prelude..=) Prelude.<$> tagKeyScope,
            ("MaximumExecutionFrequency" Prelude..=)
              Prelude.<$> maximumExecutionFrequency,
            ("ResourceIdScope" Prelude..=)
              Prelude.<$> resourceIdScope,
            ("InputParameters" Prelude..=)
              Prelude.<$> inputParameters,
            ("Description" Prelude..=) Prelude.<$> description,
            ("ResourceTypesScope" Prelude..=)
              Prelude.<$> resourceTypesScope,
            ("TagValueScope" Prelude..=)
              Prelude.<$> tagValueScope,
            Prelude.Just
              ("RuleIdentifier" Prelude..= ruleIdentifier)
          ]
      )
