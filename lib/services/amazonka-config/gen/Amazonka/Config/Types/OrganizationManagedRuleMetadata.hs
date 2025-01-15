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
-- Module      : Amazonka.Config.Types.OrganizationManagedRuleMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.OrganizationManagedRuleMetadata where

import Amazonka.Config.Types.MaximumExecutionFrequency
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that specifies organization managed rule metadata such as
-- resource type and ID of Amazon Web Services resource along with the rule
-- identifier. It also provides the frequency with which you want Config to
-- run evaluations for the rule if the trigger type is periodic.
--
-- /See:/ 'newOrganizationManagedRuleMetadata' smart constructor.
data OrganizationManagedRuleMetadata = OrganizationManagedRuleMetadata'
  { -- | The description that you provide for your organization Config rule.
    description :: Prelude.Maybe Prelude.Text,
    -- | A string, in JSON format, that is passed to your organization Config
    -- rule Lambda function.
    inputParameters :: Prelude.Maybe Prelude.Text,
    -- | The maximum frequency with which Config runs evaluations for a rule.
    -- This is for an Config managed rule that is triggered at a periodic
    -- frequency.
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
    -- | For organization config managed rules, a predefined identifier from a
    -- list. For example, @IAM_PASSWORD_POLICY@ is a managed rule. To reference
    -- a managed rule, see
    -- <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html Using Config managed rules>.
    ruleIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationManagedRuleMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'organizationManagedRuleMetadata_description' - The description that you provide for your organization Config rule.
--
-- 'inputParameters', 'organizationManagedRuleMetadata_inputParameters' - A string, in JSON format, that is passed to your organization Config
-- rule Lambda function.
--
-- 'maximumExecutionFrequency', 'organizationManagedRuleMetadata_maximumExecutionFrequency' - The maximum frequency with which Config runs evaluations for a rule.
-- This is for an Config managed rule that is triggered at a periodic
-- frequency.
--
-- By default, rules with a periodic trigger are evaluated every 24 hours.
-- To change the frequency, specify a valid value for the
-- @MaximumExecutionFrequency@ parameter.
--
-- 'resourceIdScope', 'organizationManagedRuleMetadata_resourceIdScope' - The ID of the Amazon Web Services resource that was evaluated.
--
-- 'resourceTypesScope', 'organizationManagedRuleMetadata_resourceTypesScope' - The type of the Amazon Web Services resource that was evaluated.
--
-- 'tagKeyScope', 'organizationManagedRuleMetadata_tagKeyScope' - One part of a key-value pair that make up a tag. A key is a general
-- label that acts like a category for more specific tag values.
--
-- 'tagValueScope', 'organizationManagedRuleMetadata_tagValueScope' - The optional part of a key-value pair that make up a tag. A value acts
-- as a descriptor within a tag category (key).
--
-- 'ruleIdentifier', 'organizationManagedRuleMetadata_ruleIdentifier' - For organization config managed rules, a predefined identifier from a
-- list. For example, @IAM_PASSWORD_POLICY@ is a managed rule. To reference
-- a managed rule, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html Using Config managed rules>.
newOrganizationManagedRuleMetadata ::
  -- | 'ruleIdentifier'
  Prelude.Text ->
  OrganizationManagedRuleMetadata
newOrganizationManagedRuleMetadata pRuleIdentifier_ =
  OrganizationManagedRuleMetadata'
    { description =
        Prelude.Nothing,
      inputParameters = Prelude.Nothing,
      maximumExecutionFrequency =
        Prelude.Nothing,
      resourceIdScope = Prelude.Nothing,
      resourceTypesScope = Prelude.Nothing,
      tagKeyScope = Prelude.Nothing,
      tagValueScope = Prelude.Nothing,
      ruleIdentifier = pRuleIdentifier_
    }

-- | The description that you provide for your organization Config rule.
organizationManagedRuleMetadata_description :: Lens.Lens' OrganizationManagedRuleMetadata (Prelude.Maybe Prelude.Text)
organizationManagedRuleMetadata_description = Lens.lens (\OrganizationManagedRuleMetadata' {description} -> description) (\s@OrganizationManagedRuleMetadata' {} a -> s {description = a} :: OrganizationManagedRuleMetadata)

-- | A string, in JSON format, that is passed to your organization Config
-- rule Lambda function.
organizationManagedRuleMetadata_inputParameters :: Lens.Lens' OrganizationManagedRuleMetadata (Prelude.Maybe Prelude.Text)
organizationManagedRuleMetadata_inputParameters = Lens.lens (\OrganizationManagedRuleMetadata' {inputParameters} -> inputParameters) (\s@OrganizationManagedRuleMetadata' {} a -> s {inputParameters = a} :: OrganizationManagedRuleMetadata)

-- | The maximum frequency with which Config runs evaluations for a rule.
-- This is for an Config managed rule that is triggered at a periodic
-- frequency.
--
-- By default, rules with a periodic trigger are evaluated every 24 hours.
-- To change the frequency, specify a valid value for the
-- @MaximumExecutionFrequency@ parameter.
organizationManagedRuleMetadata_maximumExecutionFrequency :: Lens.Lens' OrganizationManagedRuleMetadata (Prelude.Maybe MaximumExecutionFrequency)
organizationManagedRuleMetadata_maximumExecutionFrequency = Lens.lens (\OrganizationManagedRuleMetadata' {maximumExecutionFrequency} -> maximumExecutionFrequency) (\s@OrganizationManagedRuleMetadata' {} a -> s {maximumExecutionFrequency = a} :: OrganizationManagedRuleMetadata)

-- | The ID of the Amazon Web Services resource that was evaluated.
organizationManagedRuleMetadata_resourceIdScope :: Lens.Lens' OrganizationManagedRuleMetadata (Prelude.Maybe Prelude.Text)
organizationManagedRuleMetadata_resourceIdScope = Lens.lens (\OrganizationManagedRuleMetadata' {resourceIdScope} -> resourceIdScope) (\s@OrganizationManagedRuleMetadata' {} a -> s {resourceIdScope = a} :: OrganizationManagedRuleMetadata)

-- | The type of the Amazon Web Services resource that was evaluated.
organizationManagedRuleMetadata_resourceTypesScope :: Lens.Lens' OrganizationManagedRuleMetadata (Prelude.Maybe [Prelude.Text])
organizationManagedRuleMetadata_resourceTypesScope = Lens.lens (\OrganizationManagedRuleMetadata' {resourceTypesScope} -> resourceTypesScope) (\s@OrganizationManagedRuleMetadata' {} a -> s {resourceTypesScope = a} :: OrganizationManagedRuleMetadata) Prelude.. Lens.mapping Lens.coerced

-- | One part of a key-value pair that make up a tag. A key is a general
-- label that acts like a category for more specific tag values.
organizationManagedRuleMetadata_tagKeyScope :: Lens.Lens' OrganizationManagedRuleMetadata (Prelude.Maybe Prelude.Text)
organizationManagedRuleMetadata_tagKeyScope = Lens.lens (\OrganizationManagedRuleMetadata' {tagKeyScope} -> tagKeyScope) (\s@OrganizationManagedRuleMetadata' {} a -> s {tagKeyScope = a} :: OrganizationManagedRuleMetadata)

-- | The optional part of a key-value pair that make up a tag. A value acts
-- as a descriptor within a tag category (key).
organizationManagedRuleMetadata_tagValueScope :: Lens.Lens' OrganizationManagedRuleMetadata (Prelude.Maybe Prelude.Text)
organizationManagedRuleMetadata_tagValueScope = Lens.lens (\OrganizationManagedRuleMetadata' {tagValueScope} -> tagValueScope) (\s@OrganizationManagedRuleMetadata' {} a -> s {tagValueScope = a} :: OrganizationManagedRuleMetadata)

-- | For organization config managed rules, a predefined identifier from a
-- list. For example, @IAM_PASSWORD_POLICY@ is a managed rule. To reference
-- a managed rule, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html Using Config managed rules>.
organizationManagedRuleMetadata_ruleIdentifier :: Lens.Lens' OrganizationManagedRuleMetadata Prelude.Text
organizationManagedRuleMetadata_ruleIdentifier = Lens.lens (\OrganizationManagedRuleMetadata' {ruleIdentifier} -> ruleIdentifier) (\s@OrganizationManagedRuleMetadata' {} a -> s {ruleIdentifier = a} :: OrganizationManagedRuleMetadata)

instance
  Data.FromJSON
    OrganizationManagedRuleMetadata
  where
  parseJSON =
    Data.withObject
      "OrganizationManagedRuleMetadata"
      ( \x ->
          OrganizationManagedRuleMetadata'
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
            Prelude.<*> (x Data..: "RuleIdentifier")
      )

instance
  Prelude.Hashable
    OrganizationManagedRuleMetadata
  where
  hashWithSalt
    _salt
    OrganizationManagedRuleMetadata' {..} =
      _salt
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` inputParameters
        `Prelude.hashWithSalt` maximumExecutionFrequency
        `Prelude.hashWithSalt` resourceIdScope
        `Prelude.hashWithSalt` resourceTypesScope
        `Prelude.hashWithSalt` tagKeyScope
        `Prelude.hashWithSalt` tagValueScope
        `Prelude.hashWithSalt` ruleIdentifier

instance
  Prelude.NFData
    OrganizationManagedRuleMetadata
  where
  rnf OrganizationManagedRuleMetadata' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf inputParameters `Prelude.seq`
        Prelude.rnf maximumExecutionFrequency `Prelude.seq`
          Prelude.rnf resourceIdScope `Prelude.seq`
            Prelude.rnf resourceTypesScope `Prelude.seq`
              Prelude.rnf tagKeyScope `Prelude.seq`
                Prelude.rnf tagValueScope `Prelude.seq`
                  Prelude.rnf ruleIdentifier

instance Data.ToJSON OrganizationManagedRuleMetadata where
  toJSON OrganizationManagedRuleMetadata' {..} =
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
              ("RuleIdentifier" Data..= ruleIdentifier)
          ]
      )
