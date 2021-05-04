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
-- Module      : Network.AWS.Organizations.Types.Organization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.Organization where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.OrganizationFeatureSet
import Network.AWS.Organizations.Types.PolicyTypeSummary
import qualified Network.AWS.Prelude as Prelude

-- | Contains details about an organization. An organization is a collection
-- of accounts that are centrally managed together using consolidated
-- billing, organized hierarchically with organizational units (OUs), and
-- controlled with policies .
--
-- /See:/ 'newOrganization' smart constructor.
data Organization = Organization'
  { -- | The email address that is associated with the AWS account that is
    -- designated as the management account for the organization.
    masterAccountEmail :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | Specifies the functionality that currently is available to the
    -- organization. If set to \"ALL\", then all features are enabled and
    -- policies can be applied to accounts in the organization. If set to
    -- \"CONSOLIDATED_BILLING\", then only consolidated billing functionality
    -- is available. For more information, see
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization>
    -- in the /AWS Organizations User Guide/.
    featureSet :: Prelude.Maybe OrganizationFeatureSet,
    -- | The Amazon Resource Name (ARN) of the account that is designated as the
    -- management account for the organization.
    --
    -- For more information about ARNs in Organizations, see
    -- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
    -- in the /AWS Service Authorization Reference/.
    masterAccountArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an organization.
    --
    -- For more information about ARNs in Organizations, see
    -- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
    -- in the /AWS Service Authorization Reference/.
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
    availablePolicyTypes :: Prelude.Maybe [PolicyTypeSummary]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Organization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'masterAccountEmail', 'organization_masterAccountEmail' - The email address that is associated with the AWS account that is
-- designated as the management account for the organization.
--
-- 'featureSet', 'organization_featureSet' - Specifies the functionality that currently is available to the
-- organization. If set to \"ALL\", then all features are enabled and
-- policies can be applied to accounts in the organization. If set to
-- \"CONSOLIDATED_BILLING\", then only consolidated billing functionality
-- is available. For more information, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization>
-- in the /AWS Organizations User Guide/.
--
-- 'masterAccountArn', 'organization_masterAccountArn' - The Amazon Resource Name (ARN) of the account that is designated as the
-- management account for the organization.
--
-- For more information about ARNs in Organizations, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
-- in the /AWS Service Authorization Reference/.
--
-- 'arn', 'organization_arn' - The Amazon Resource Name (ARN) of an organization.
--
-- For more information about ARNs in Organizations, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
-- in the /AWS Service Authorization Reference/.
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
newOrganization ::
  Organization
newOrganization =
  Organization'
    { masterAccountEmail = Prelude.Nothing,
      featureSet = Prelude.Nothing,
      masterAccountArn = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      masterAccountId = Prelude.Nothing,
      availablePolicyTypes = Prelude.Nothing
    }

-- | The email address that is associated with the AWS account that is
-- designated as the management account for the organization.
organization_masterAccountEmail :: Lens.Lens' Organization (Prelude.Maybe Prelude.Text)
organization_masterAccountEmail = Lens.lens (\Organization' {masterAccountEmail} -> masterAccountEmail) (\s@Organization' {} a -> s {masterAccountEmail = a} :: Organization) Prelude.. Lens.mapping Prelude._Sensitive

-- | Specifies the functionality that currently is available to the
-- organization. If set to \"ALL\", then all features are enabled and
-- policies can be applied to accounts in the organization. If set to
-- \"CONSOLIDATED_BILLING\", then only consolidated billing functionality
-- is available. For more information, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization>
-- in the /AWS Organizations User Guide/.
organization_featureSet :: Lens.Lens' Organization (Prelude.Maybe OrganizationFeatureSet)
organization_featureSet = Lens.lens (\Organization' {featureSet} -> featureSet) (\s@Organization' {} a -> s {featureSet = a} :: Organization)

-- | The Amazon Resource Name (ARN) of the account that is designated as the
-- management account for the organization.
--
-- For more information about ARNs in Organizations, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
-- in the /AWS Service Authorization Reference/.
organization_masterAccountArn :: Lens.Lens' Organization (Prelude.Maybe Prelude.Text)
organization_masterAccountArn = Lens.lens (\Organization' {masterAccountArn} -> masterAccountArn) (\s@Organization' {} a -> s {masterAccountArn = a} :: Organization)

-- | The Amazon Resource Name (ARN) of an organization.
--
-- For more information about ARNs in Organizations, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
-- in the /AWS Service Authorization Reference/.
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
organization_availablePolicyTypes = Lens.lens (\Organization' {availablePolicyTypes} -> availablePolicyTypes) (\s@Organization' {} a -> s {availablePolicyTypes = a} :: Organization) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON Organization where
  parseJSON =
    Prelude.withObject
      "Organization"
      ( \x ->
          Organization'
            Prelude.<$> (x Prelude..:? "MasterAccountEmail")
            Prelude.<*> (x Prelude..:? "FeatureSet")
            Prelude.<*> (x Prelude..:? "MasterAccountArn")
            Prelude.<*> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "MasterAccountId")
            Prelude.<*> ( x Prelude..:? "AvailablePolicyTypes"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Organization

instance Prelude.NFData Organization
