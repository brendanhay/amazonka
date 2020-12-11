-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.Organization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.Organization
  ( Organization (..),

    -- * Smart constructor
    mkOrganization,

    -- * Lenses
    oARN,
    oMasterAccountId,
    oMasterAccountARN,
    oMasterAccountEmail,
    oAvailablePolicyTypes,
    oId,
    oFeatureSet,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.OrganizationFeatureSet
import Network.AWS.Organizations.Types.PolicyTypeSummary
import qualified Network.AWS.Prelude as Lude

-- | Contains details about an organization. An organization is a collection of accounts that are centrally managed together using consolidated billing, organized hierarchically with organizational units (OUs), and controlled with policies .
--
-- /See:/ 'mkOrganization' smart constructor.
data Organization = Organization'
  { arn :: Lude.Maybe Lude.Text,
    masterAccountId :: Lude.Maybe Lude.Text,
    masterAccountARN :: Lude.Maybe Lude.Text,
    masterAccountEmail :: Lude.Maybe (Lude.Sensitive Lude.Text),
    availablePolicyTypes :: Lude.Maybe [PolicyTypeSummary],
    id :: Lude.Maybe Lude.Text,
    featureSet :: Lude.Maybe OrganizationFeatureSet
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Organization' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of an organization.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
-- * 'availablePolicyTypes' - /Important:/ Do not use. This field is deprecated and doesn't provide complete information about the policies in your organization.
--
-- To determine the policies that are enabled and available for use in your organization, use the 'ListRoots' operation instead.
-- * 'featureSet' - Specifies the functionality that currently is available to the organization. If set to "ALL", then all features are enabled and policies can be applied to accounts in the organization. If set to "CONSOLIDATED_BILLING", then only consolidated billing functionality is available. For more information, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization> in the /AWS Organizations User Guide/ .
-- * 'id' - The unique identifier (ID) of an organization.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an organization ID string requires "o-" followed by from 10 to 32 lowercase letters or digits.
-- * 'masterAccountARN' - The Amazon Resource Name (ARN) of the account that is designated as the management account for the organization.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
-- * 'masterAccountEmail' - The email address that is associated with the AWS account that is designated as the management account for the organization.
-- * 'masterAccountId' - The unique identifier (ID) of the management account of an organization.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
mkOrganization ::
  Organization
mkOrganization =
  Organization'
    { arn = Lude.Nothing,
      masterAccountId = Lude.Nothing,
      masterAccountARN = Lude.Nothing,
      masterAccountEmail = Lude.Nothing,
      availablePolicyTypes = Lude.Nothing,
      id = Lude.Nothing,
      featureSet = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of an organization.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oARN :: Lens.Lens' Organization (Lude.Maybe Lude.Text)
oARN = Lens.lens (arn :: Organization -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Organization)
{-# DEPRECATED oARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The unique identifier (ID) of the management account of an organization.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
--
-- /Note:/ Consider using 'masterAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oMasterAccountId :: Lens.Lens' Organization (Lude.Maybe Lude.Text)
oMasterAccountId = Lens.lens (masterAccountId :: Organization -> Lude.Maybe Lude.Text) (\s a -> s {masterAccountId = a} :: Organization)
{-# DEPRECATED oMasterAccountId "Use generic-lens or generic-optics with 'masterAccountId' instead." #-}

-- | The Amazon Resource Name (ARN) of the account that is designated as the management account for the organization.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- /Note:/ Consider using 'masterAccountARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oMasterAccountARN :: Lens.Lens' Organization (Lude.Maybe Lude.Text)
oMasterAccountARN = Lens.lens (masterAccountARN :: Organization -> Lude.Maybe Lude.Text) (\s a -> s {masterAccountARN = a} :: Organization)
{-# DEPRECATED oMasterAccountARN "Use generic-lens or generic-optics with 'masterAccountARN' instead." #-}

-- | The email address that is associated with the AWS account that is designated as the management account for the organization.
--
-- /Note:/ Consider using 'masterAccountEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oMasterAccountEmail :: Lens.Lens' Organization (Lude.Maybe (Lude.Sensitive Lude.Text))
oMasterAccountEmail = Lens.lens (masterAccountEmail :: Organization -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {masterAccountEmail = a} :: Organization)
{-# DEPRECATED oMasterAccountEmail "Use generic-lens or generic-optics with 'masterAccountEmail' instead." #-}

-- | /Important:/ Do not use. This field is deprecated and doesn't provide complete information about the policies in your organization.
--
-- To determine the policies that are enabled and available for use in your organization, use the 'ListRoots' operation instead.
--
-- /Note:/ Consider using 'availablePolicyTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oAvailablePolicyTypes :: Lens.Lens' Organization (Lude.Maybe [PolicyTypeSummary])
oAvailablePolicyTypes = Lens.lens (availablePolicyTypes :: Organization -> Lude.Maybe [PolicyTypeSummary]) (\s a -> s {availablePolicyTypes = a} :: Organization)
{-# DEPRECATED oAvailablePolicyTypes "Use generic-lens or generic-optics with 'availablePolicyTypes' instead." #-}

-- | The unique identifier (ID) of an organization.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an organization ID string requires "o-" followed by from 10 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oId :: Lens.Lens' Organization (Lude.Maybe Lude.Text)
oId = Lens.lens (id :: Organization -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Organization)
{-# DEPRECATED oId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Specifies the functionality that currently is available to the organization. If set to "ALL", then all features are enabled and policies can be applied to accounts in the organization. If set to "CONSOLIDATED_BILLING", then only consolidated billing functionality is available. For more information, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization> in the /AWS Organizations User Guide/ .
--
-- /Note:/ Consider using 'featureSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oFeatureSet :: Lens.Lens' Organization (Lude.Maybe OrganizationFeatureSet)
oFeatureSet = Lens.lens (featureSet :: Organization -> Lude.Maybe OrganizationFeatureSet) (\s a -> s {featureSet = a} :: Organization)
{-# DEPRECATED oFeatureSet "Use generic-lens or generic-optics with 'featureSet' instead." #-}

instance Lude.FromJSON Organization where
  parseJSON =
    Lude.withObject
      "Organization"
      ( \x ->
          Organization'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "MasterAccountId")
            Lude.<*> (x Lude..:? "MasterAccountArn")
            Lude.<*> (x Lude..:? "MasterAccountEmail")
            Lude.<*> (x Lude..:? "AvailablePolicyTypes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "FeatureSet")
      )
