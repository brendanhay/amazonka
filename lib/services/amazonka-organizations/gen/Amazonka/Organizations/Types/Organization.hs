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
-- Module      : Amazonka.Organizations.Types.Organization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types.Organization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Organizations.Types.OrganizationFeatureSet
import Amazonka.Organizations.Types.PolicyTypeSummary
import qualified Amazonka.Prelude as Prelude

-- | Contains details about an organization. An organization is a collection
-- of accounts that are centrally managed together using consolidated
-- billing, organized hierarchically with organizational units (OUs), and
-- controlled with policies .
--
-- /See:/ 'newOrganization' smart constructor.
data Organization = Organization'
  { -- | The Amazon Resource Name (ARN) of an organization.
    --
    -- For more information about ARNs in Organizations, see
    -- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
    -- in the /Amazon Web Services Service Authorization Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier (ID) of an organization.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for an organization
    -- ID string requires \"o-\" followed by from 10 to 32 lowercase letters or
    -- digits.
    id :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier (ID) of the management account of an organization.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID
    -- string requires exactly 12 digits.
    masterAccountId :: Prelude.Maybe Prelude.Text,
    -- | Do not use. This field is deprecated and doesn\'t provide complete
    -- information about the policies in your organization.
    --
    -- To determine the policies that are enabled and available for use in your
    -- organization, use the ListRoots operation instead.
    availablePolicyTypes :: Prelude.Maybe [PolicyTypeSummary],
    -- | Specifies the functionality that currently is available to the
    -- organization. If set to \"ALL\", then all features are enabled and
    -- policies can be applied to accounts in the organization. If set to
    -- \"CONSOLIDATED_BILLING\", then only consolidated billing functionality
    -- is available. For more information, see
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization>
    -- in the /Organizations User Guide/.
    featureSet :: Prelude.Maybe OrganizationFeatureSet,
    -- | The email address that is associated with the Amazon Web Services
    -- account that is designated as the management account for the
    -- organization.
    masterAccountEmail :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the account that is designated as the
    -- management account for the organization.
    --
    -- For more information about ARNs in Organizations, see
    -- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
    -- in the /Amazon Web Services Service Authorization Reference/.
    masterAccountArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Organization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'organization_arn' - The Amazon Resource Name (ARN) of an organization.
--
-- For more information about ARNs in Organizations, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
-- in the /Amazon Web Services Service Authorization Reference/.
--
-- 'id', 'organization_id' - The unique identifier (ID) of an organization.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an organization
-- ID string requires \"o-\" followed by from 10 to 32 lowercase letters or
-- digits.
--
-- 'masterAccountId', 'organization_masterAccountId' - The unique identifier (ID) of the management account of an organization.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID
-- string requires exactly 12 digits.
--
-- 'availablePolicyTypes', 'organization_availablePolicyTypes' - Do not use. This field is deprecated and doesn\'t provide complete
-- information about the policies in your organization.
--
-- To determine the policies that are enabled and available for use in your
-- organization, use the ListRoots operation instead.
--
-- 'featureSet', 'organization_featureSet' - Specifies the functionality that currently is available to the
-- organization. If set to \"ALL\", then all features are enabled and
-- policies can be applied to accounts in the organization. If set to
-- \"CONSOLIDATED_BILLING\", then only consolidated billing functionality
-- is available. For more information, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization>
-- in the /Organizations User Guide/.
--
-- 'masterAccountEmail', 'organization_masterAccountEmail' - The email address that is associated with the Amazon Web Services
-- account that is designated as the management account for the
-- organization.
--
-- 'masterAccountArn', 'organization_masterAccountArn' - The Amazon Resource Name (ARN) of the account that is designated as the
-- management account for the organization.
--
-- For more information about ARNs in Organizations, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
-- in the /Amazon Web Services Service Authorization Reference/.
newOrganization ::
  Organization
newOrganization =
  Organization'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      masterAccountId = Prelude.Nothing,
      availablePolicyTypes = Prelude.Nothing,
      featureSet = Prelude.Nothing,
      masterAccountEmail = Prelude.Nothing,
      masterAccountArn = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of an organization.
--
-- For more information about ARNs in Organizations, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
-- in the /Amazon Web Services Service Authorization Reference/.
organization_arn :: Lens.Lens' Organization (Prelude.Maybe Prelude.Text)
organization_arn = Lens.lens (\Organization' {arn} -> arn) (\s@Organization' {} a -> s {arn = a} :: Organization)

-- | The unique identifier (ID) of an organization.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an organization
-- ID string requires \"o-\" followed by from 10 to 32 lowercase letters or
-- digits.
organization_id :: Lens.Lens' Organization (Prelude.Maybe Prelude.Text)
organization_id = Lens.lens (\Organization' {id} -> id) (\s@Organization' {} a -> s {id = a} :: Organization)

-- | The unique identifier (ID) of the management account of an organization.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID
-- string requires exactly 12 digits.
organization_masterAccountId :: Lens.Lens' Organization (Prelude.Maybe Prelude.Text)
organization_masterAccountId = Lens.lens (\Organization' {masterAccountId} -> masterAccountId) (\s@Organization' {} a -> s {masterAccountId = a} :: Organization)

-- | Do not use. This field is deprecated and doesn\'t provide complete
-- information about the policies in your organization.
--
-- To determine the policies that are enabled and available for use in your
-- organization, use the ListRoots operation instead.
organization_availablePolicyTypes :: Lens.Lens' Organization (Prelude.Maybe [PolicyTypeSummary])
organization_availablePolicyTypes = Lens.lens (\Organization' {availablePolicyTypes} -> availablePolicyTypes) (\s@Organization' {} a -> s {availablePolicyTypes = a} :: Organization) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the functionality that currently is available to the
-- organization. If set to \"ALL\", then all features are enabled and
-- policies can be applied to accounts in the organization. If set to
-- \"CONSOLIDATED_BILLING\", then only consolidated billing functionality
-- is available. For more information, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization>
-- in the /Organizations User Guide/.
organization_featureSet :: Lens.Lens' Organization (Prelude.Maybe OrganizationFeatureSet)
organization_featureSet = Lens.lens (\Organization' {featureSet} -> featureSet) (\s@Organization' {} a -> s {featureSet = a} :: Organization)

-- | The email address that is associated with the Amazon Web Services
-- account that is designated as the management account for the
-- organization.
organization_masterAccountEmail :: Lens.Lens' Organization (Prelude.Maybe Prelude.Text)
organization_masterAccountEmail = Lens.lens (\Organization' {masterAccountEmail} -> masterAccountEmail) (\s@Organization' {} a -> s {masterAccountEmail = a} :: Organization) Prelude.. Lens.mapping Core._Sensitive

-- | The Amazon Resource Name (ARN) of the account that is designated as the
-- management account for the organization.
--
-- For more information about ARNs in Organizations, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
-- in the /Amazon Web Services Service Authorization Reference/.
organization_masterAccountArn :: Lens.Lens' Organization (Prelude.Maybe Prelude.Text)
organization_masterAccountArn = Lens.lens (\Organization' {masterAccountArn} -> masterAccountArn) (\s@Organization' {} a -> s {masterAccountArn = a} :: Organization)

instance Core.FromJSON Organization where
  parseJSON =
    Core.withObject
      "Organization"
      ( \x ->
          Organization'
            Prelude.<$> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "MasterAccountId")
            Prelude.<*> ( x Core..:? "AvailablePolicyTypes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "FeatureSet")
            Prelude.<*> (x Core..:? "MasterAccountEmail")
            Prelude.<*> (x Core..:? "MasterAccountArn")
      )

instance Prelude.Hashable Organization where
  hashWithSalt _salt Organization' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` masterAccountId
      `Prelude.hashWithSalt` availablePolicyTypes
      `Prelude.hashWithSalt` featureSet
      `Prelude.hashWithSalt` masterAccountEmail
      `Prelude.hashWithSalt` masterAccountArn

instance Prelude.NFData Organization where
  rnf Organization' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf masterAccountId
      `Prelude.seq` Prelude.rnf availablePolicyTypes
      `Prelude.seq` Prelude.rnf featureSet
      `Prelude.seq` Prelude.rnf masterAccountEmail
      `Prelude.seq` Prelude.rnf masterAccountArn
