{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    oArn,
    oAvailablePolicyTypes,
    oFeatureSet,
    oId,
    oMasterAccountArn,
    oMasterAccountEmail,
    oMasterAccountId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types.AccountArn as Types
import qualified Network.AWS.Organizations.Types.AccountId as Types
import qualified Network.AWS.Organizations.Types.Arn as Types
import qualified Network.AWS.Organizations.Types.Email as Types
import qualified Network.AWS.Organizations.Types.Id as Types
import qualified Network.AWS.Organizations.Types.OrganizationFeatureSet as Types
import qualified Network.AWS.Organizations.Types.PolicyTypeSummary as Types
import qualified Network.AWS.Prelude as Core

-- | Contains details about an organization. An organization is a collection of accounts that are centrally managed together using consolidated billing, organized hierarchically with organizational units (OUs), and controlled with policies .
--
-- /See:/ 'mkOrganization' smart constructor.
data Organization = Organization'
  { -- | The Amazon Resource Name (ARN) of an organization.
    --
    -- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
    arn :: Core.Maybe Types.Arn,
    -- | /Important:/ Do not use. This field is deprecated and doesn't provide complete information about the policies in your organization.
    --
    -- To determine the policies that are enabled and available for use in your organization, use the 'ListRoots' operation instead.
    availablePolicyTypes :: Core.Maybe [Types.PolicyTypeSummary],
    -- | Specifies the functionality that currently is available to the organization. If set to "ALL", then all features are enabled and policies can be applied to accounts in the organization. If set to "CONSOLIDATED_BILLING", then only consolidated billing functionality is available. For more information, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization> in the /AWS Organizations User Guide/ .
    featureSet :: Core.Maybe Types.OrganizationFeatureSet,
    -- | The unique identifier (ID) of an organization.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for an organization ID string requires "o-" followed by from 10 to 32 lowercase letters or digits.
    id :: Core.Maybe Types.Id,
    -- | The Amazon Resource Name (ARN) of the account that is designated as the management account for the organization.
    --
    -- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
    masterAccountArn :: Core.Maybe Types.AccountArn,
    -- | The email address that is associated with the AWS account that is designated as the management account for the organization.
    masterAccountEmail :: Core.Maybe Types.Email,
    -- | The unique identifier (ID) of the management account of an organization.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
    masterAccountId :: Core.Maybe Types.AccountId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Organization' value with any optional fields omitted.
mkOrganization ::
  Organization
mkOrganization =
  Organization'
    { arn = Core.Nothing,
      availablePolicyTypes = Core.Nothing,
      featureSet = Core.Nothing,
      id = Core.Nothing,
      masterAccountArn = Core.Nothing,
      masterAccountEmail = Core.Nothing,
      masterAccountId = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of an organization.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oArn :: Lens.Lens' Organization (Core.Maybe Types.Arn)
oArn = Lens.field @"arn"
{-# DEPRECATED oArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | /Important:/ Do not use. This field is deprecated and doesn't provide complete information about the policies in your organization.
--
-- To determine the policies that are enabled and available for use in your organization, use the 'ListRoots' operation instead.
--
-- /Note:/ Consider using 'availablePolicyTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oAvailablePolicyTypes :: Lens.Lens' Organization (Core.Maybe [Types.PolicyTypeSummary])
oAvailablePolicyTypes = Lens.field @"availablePolicyTypes"
{-# DEPRECATED oAvailablePolicyTypes "Use generic-lens or generic-optics with 'availablePolicyTypes' instead." #-}

-- | Specifies the functionality that currently is available to the organization. If set to "ALL", then all features are enabled and policies can be applied to accounts in the organization. If set to "CONSOLIDATED_BILLING", then only consolidated billing functionality is available. For more information, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization> in the /AWS Organizations User Guide/ .
--
-- /Note:/ Consider using 'featureSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oFeatureSet :: Lens.Lens' Organization (Core.Maybe Types.OrganizationFeatureSet)
oFeatureSet = Lens.field @"featureSet"
{-# DEPRECATED oFeatureSet "Use generic-lens or generic-optics with 'featureSet' instead." #-}

-- | The unique identifier (ID) of an organization.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an organization ID string requires "o-" followed by from 10 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oId :: Lens.Lens' Organization (Core.Maybe Types.Id)
oId = Lens.field @"id"
{-# DEPRECATED oId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The Amazon Resource Name (ARN) of the account that is designated as the management account for the organization.
--
-- For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- /Note:/ Consider using 'masterAccountArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oMasterAccountArn :: Lens.Lens' Organization (Core.Maybe Types.AccountArn)
oMasterAccountArn = Lens.field @"masterAccountArn"
{-# DEPRECATED oMasterAccountArn "Use generic-lens or generic-optics with 'masterAccountArn' instead." #-}

-- | The email address that is associated with the AWS account that is designated as the management account for the organization.
--
-- /Note:/ Consider using 'masterAccountEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oMasterAccountEmail :: Lens.Lens' Organization (Core.Maybe Types.Email)
oMasterAccountEmail = Lens.field @"masterAccountEmail"
{-# DEPRECATED oMasterAccountEmail "Use generic-lens or generic-optics with 'masterAccountEmail' instead." #-}

-- | The unique identifier (ID) of the management account of an organization.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
--
-- /Note:/ Consider using 'masterAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oMasterAccountId :: Lens.Lens' Organization (Core.Maybe Types.AccountId)
oMasterAccountId = Lens.field @"masterAccountId"
{-# DEPRECATED oMasterAccountId "Use generic-lens or generic-optics with 'masterAccountId' instead." #-}

instance Core.FromJSON Organization where
  parseJSON =
    Core.withObject "Organization" Core.$
      \x ->
        Organization'
          Core.<$> (x Core..:? "Arn")
          Core.<*> (x Core..:? "AvailablePolicyTypes")
          Core.<*> (x Core..:? "FeatureSet")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "MasterAccountArn")
          Core.<*> (x Core..:? "MasterAccountEmail")
          Core.<*> (x Core..:? "MasterAccountId")
