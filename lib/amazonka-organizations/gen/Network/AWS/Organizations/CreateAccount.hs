{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.CreateAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS account that is automatically a member of the organization whose credentials made the request. This is an asynchronous request that AWS performs in the background. Because @CreateAccount@ operates asynchronously, it can return a successful completion message even though account initialization might still be in progress. You might need to wait a few minutes before you can successfully access the account. To check the status of the request, do one of the following:
--
--
--     * Use the @Id@ member of the @CreateAccountStatus@ response element from this operation to provide as a parameter to the 'DescribeCreateAccountStatus' operation.
--
--
--     * Check the AWS CloudTrail log for the @CreateAccountResult@ event. For information on using AWS CloudTrail with AWS Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_monitoring.html Monitoring the Activity in Your Organization> in the /AWS Organizations User Guide./
--
--
-- The user who calls the API to create an account must have the @organizations:CreateAccount@ permission. If you enabled all features in the organization, AWS Organizations creates the required service-linked role named @AWSServiceRoleForOrganizations@ . For more information, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html#orgs_integrate_services-using_slrs AWS Organizations and Service-Linked Roles> in the /AWS Organizations User Guide/ .
-- If the request includes tags, then the requester must have the @organizations:TagResource@ permission.
-- AWS Organizations preconfigures the new member account with a role (named @OrganizationAccountAccessRole@ by default) that grants users in the management account administrator permissions in the new member account. Principals in the management account can assume the role. AWS Organizations clones the company name and address information for the new account from the organization's management account.
-- This operation can be called only from the organization's management account.
-- For more information about creating accounts, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_create.html Creating an AWS Account in Your Organization> in the /AWS Organizations User Guide./
-- /Important:/
--     * When you create an account in an organization using the AWS Organizations console, API, or CLI commands, the information required for the account to operate as a standalone account, such as a payment method and signing the end user license agreement (EULA) is /not/ automatically collected. If you must remove an account from your organization later, you can do so only after you provide the missing information. Follow the steps at <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info To leave an organization as a member account> in the /AWS Organizations User Guide/ .
--
--
--     * If you get an exception that indicates that you exceeded your account limits for the organization, contact <https://console.aws.amazon.com/support/home#/ AWS Support> .
--
--
--     * If you get an exception that indicates that the operation failed because your organization is still initializing, wait one hour and then try again. If the error persists, contact <https://console.aws.amazon.com/support/home#/ AWS Support> .
--
--
--     * Using @CreateAccount@ to create multiple temporary accounts isn't recommended. You can only close an account from the Billing and Cost Management Console, and you must be signed in as the root user. For information on the requirements and process for closing an account, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_close.html Closing an AWS Account> in the /AWS Organizations User Guide/ .
module Network.AWS.Organizations.CreateAccount
  ( -- * Creating a request
    CreateAccount (..),
    mkCreateAccount,

    -- ** Request lenses
    caEmail,
    caAccountName,
    caIamUserAccessToBilling,
    caRoleName,
    caTags,

    -- * Destructuring the response
    CreateAccountResponse (..),
    mkCreateAccountResponse,

    -- ** Response lenses
    carrsCreateAccountStatus,
    carrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateAccount' smart constructor.
data CreateAccount = CreateAccount'
  { -- | The email address of the owner to assign to the new member account. This email address must not already be associated with another AWS account. You must use a valid email address to complete account creation. You can't access the root user of the account or remove an account that was created with an invalid email address.
    email :: Types.Email,
    -- | The friendly name of the member account.
    accountName :: Types.AccountName,
    -- | If set to @ALLOW@ , the new account enables IAM users to access account billing information /if/ they have the required permissions. If set to @DENY@ , only the root user of the new account can access account billing information. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html#ControllingAccessWebsite-Activate Activating Access to the Billing and Cost Management Console> in the /AWS Billing and Cost Management User Guide/ .
    --
    -- If you don't specify this parameter, the value defaults to @ALLOW@ , and IAM users and roles with the required permissions can access billing information for the new account.
    iamUserAccessToBilling :: Core.Maybe Types.IAMUserAccessToBilling,
    -- | (Optional)
    --
    -- The name of an IAM role that AWS Organizations automatically preconfigures in the new member account. This role trusts the management account, allowing users in the management account to assume the role, as permitted by the management account administrator. The role has administrator permissions in the new member account.
    -- If you don't specify this parameter, the role name defaults to @OrganizationAccountAccessRole@ .
    -- For more information about how to use this role to access the member account, see the following links:
    --
    --     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_access.html#orgs_manage_accounts_create-cross-account-role Accessing and Administering the Member Accounts in Your Organization> in the /AWS Organizations User Guide/
    --
    --
    --     * Steps 2 and 3 in <https://docs.aws.amazon.com/IAM/latest/UserGuide/tutorial_cross-account-with-roles.html Tutorial: Delegate Access Across AWS Accounts Using IAM Roles> in the /IAM User Guide/
    --
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter. The pattern can include uppercase letters, lowercase letters, digits with no spaces, and any of the following characters: =,.@-
    roleName :: Core.Maybe Types.RoleName,
    -- | A list of tags that you want to attach to the newly created account. For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAccount' value with any optional fields omitted.
mkCreateAccount ::
  -- | 'email'
  Types.Email ->
  -- | 'accountName'
  Types.AccountName ->
  CreateAccount
mkCreateAccount email accountName =
  CreateAccount'
    { email,
      accountName,
      iamUserAccessToBilling = Core.Nothing,
      roleName = Core.Nothing,
      tags = Core.Nothing
    }

-- | The email address of the owner to assign to the new member account. This email address must not already be associated with another AWS account. You must use a valid email address to complete account creation. You can't access the root user of the account or remove an account that was created with an invalid email address.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caEmail :: Lens.Lens' CreateAccount Types.Email
caEmail = Lens.field @"email"
{-# DEPRECATED caEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The friendly name of the member account.
--
-- /Note:/ Consider using 'accountName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAccountName :: Lens.Lens' CreateAccount Types.AccountName
caAccountName = Lens.field @"accountName"
{-# DEPRECATED caAccountName "Use generic-lens or generic-optics with 'accountName' instead." #-}

-- | If set to @ALLOW@ , the new account enables IAM users to access account billing information /if/ they have the required permissions. If set to @DENY@ , only the root user of the new account can access account billing information. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html#ControllingAccessWebsite-Activate Activating Access to the Billing and Cost Management Console> in the /AWS Billing and Cost Management User Guide/ .
--
-- If you don't specify this parameter, the value defaults to @ALLOW@ , and IAM users and roles with the required permissions can access billing information for the new account.
--
-- /Note:/ Consider using 'iamUserAccessToBilling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caIamUserAccessToBilling :: Lens.Lens' CreateAccount (Core.Maybe Types.IAMUserAccessToBilling)
caIamUserAccessToBilling = Lens.field @"iamUserAccessToBilling"
{-# DEPRECATED caIamUserAccessToBilling "Use generic-lens or generic-optics with 'iamUserAccessToBilling' instead." #-}

-- | (Optional)
--
-- The name of an IAM role that AWS Organizations automatically preconfigures in the new member account. This role trusts the management account, allowing users in the management account to assume the role, as permitted by the management account administrator. The role has administrator permissions in the new member account.
-- If you don't specify this parameter, the role name defaults to @OrganizationAccountAccessRole@ .
-- For more information about how to use this role to access the member account, see the following links:
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_access.html#orgs_manage_accounts_create-cross-account-role Accessing and Administering the Member Accounts in Your Organization> in the /AWS Organizations User Guide/
--
--
--     * Steps 2 and 3 in <https://docs.aws.amazon.com/IAM/latest/UserGuide/tutorial_cross-account-with-roles.html Tutorial: Delegate Access Across AWS Accounts Using IAM Roles> in the /IAM User Guide/
--
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter. The pattern can include uppercase letters, lowercase letters, digits with no spaces, and any of the following characters: =,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caRoleName :: Lens.Lens' CreateAccount (Core.Maybe Types.RoleName)
caRoleName = Lens.field @"roleName"
{-# DEPRECATED caRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | A list of tags that you want to attach to the newly created account. For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTags :: Lens.Lens' CreateAccount (Core.Maybe [Types.Tag])
caTags = Lens.field @"tags"
{-# DEPRECATED caTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateAccount where
  toJSON CreateAccount {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Email" Core..= email),
            Core.Just ("AccountName" Core..= accountName),
            ("IamUserAccessToBilling" Core..=) Core.<$> iamUserAccessToBilling,
            ("RoleName" Core..=) Core.<$> roleName,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateAccount where
  type Rs CreateAccount = CreateAccountResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSOrganizationsV20161128.CreateAccount")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAccountResponse'
            Core.<$> (x Core..:? "CreateAccountStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateAccountResponse' smart constructor.
data CreateAccountResponse = CreateAccountResponse'
  { -- | A structure that contains details about the request to create an account. This response structure might not be fully populated when you first receive it because account creation is an asynchronous process. You can pass the returned @CreateAccountStatus@ ID as a parameter to 'DescribeCreateAccountStatus' to get status about the progress of the request at later times. You can also check the AWS CloudTrail log for the @CreateAccountResult@ event. For more information, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_monitoring.html Monitoring the Activity in Your Organization> in the /AWS Organizations User Guide/ .
    createAccountStatus :: Core.Maybe Types.CreateAccountStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateAccountResponse' value with any optional fields omitted.
mkCreateAccountResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateAccountResponse
mkCreateAccountResponse responseStatus =
  CreateAccountResponse'
    { createAccountStatus = Core.Nothing,
      responseStatus
    }

-- | A structure that contains details about the request to create an account. This response structure might not be fully populated when you first receive it because account creation is an asynchronous process. You can pass the returned @CreateAccountStatus@ ID as a parameter to 'DescribeCreateAccountStatus' to get status about the progress of the request at later times. You can also check the AWS CloudTrail log for the @CreateAccountResult@ event. For more information, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_monitoring.html Monitoring the Activity in Your Organization> in the /AWS Organizations User Guide/ .
--
-- /Note:/ Consider using 'createAccountStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsCreateAccountStatus :: Lens.Lens' CreateAccountResponse (Core.Maybe Types.CreateAccountStatus)
carrsCreateAccountStatus = Lens.field @"createAccountStatus"
{-# DEPRECATED carrsCreateAccountStatus "Use generic-lens or generic-optics with 'createAccountStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsResponseStatus :: Lens.Lens' CreateAccountResponse Core.Int
carrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED carrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
