{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    caIAMUserAccessToBilling,
    caRoleName,
    caTags,
    caEmail,
    caAccountName,

    -- * Destructuring the response
    CreateAccountResponse (..),
    mkCreateAccountResponse,

    -- ** Response lenses
    carsCreateAccountStatus,
    carsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateAccount' smart constructor.
data CreateAccount = CreateAccount'
  { iamUserAccessToBilling ::
      Lude.Maybe IAMUserAccessToBilling,
    roleName :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    email :: Lude.Sensitive Lude.Text,
    accountName :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAccount' with the minimum fields required to make a request.
--
-- * 'accountName' - The friendly name of the member account.
-- * 'email' - The email address of the owner to assign to the new member account. This email address must not already be associated with another AWS account. You must use a valid email address to complete account creation. You can't access the root user of the account or remove an account that was created with an invalid email address.
-- * 'iamUserAccessToBilling' - If set to @ALLOW@ , the new account enables IAM users to access account billing information /if/ they have the required permissions. If set to @DENY@ , only the root user of the new account can access account billing information. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html#ControllingAccessWebsite-Activate Activating Access to the Billing and Cost Management Console> in the /AWS Billing and Cost Management User Guide/ .
--
-- If you don't specify this parameter, the value defaults to @ALLOW@ , and IAM users and roles with the required permissions can access billing information for the new account.
-- * 'roleName' - (Optional)
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
-- * 'tags' - A list of tags that you want to attach to the newly created account. For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
mkCreateAccount ::
  -- | 'email'
  Lude.Sensitive Lude.Text ->
  -- | 'accountName'
  Lude.Sensitive Lude.Text ->
  CreateAccount
mkCreateAccount pEmail_ pAccountName_ =
  CreateAccount'
    { iamUserAccessToBilling = Lude.Nothing,
      roleName = Lude.Nothing,
      tags = Lude.Nothing,
      email = pEmail_,
      accountName = pAccountName_
    }

-- | If set to @ALLOW@ , the new account enables IAM users to access account billing information /if/ they have the required permissions. If set to @DENY@ , only the root user of the new account can access account billing information. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html#ControllingAccessWebsite-Activate Activating Access to the Billing and Cost Management Console> in the /AWS Billing and Cost Management User Guide/ .
--
-- If you don't specify this parameter, the value defaults to @ALLOW@ , and IAM users and roles with the required permissions can access billing information for the new account.
--
-- /Note:/ Consider using 'iamUserAccessToBilling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caIAMUserAccessToBilling :: Lens.Lens' CreateAccount (Lude.Maybe IAMUserAccessToBilling)
caIAMUserAccessToBilling = Lens.lens (iamUserAccessToBilling :: CreateAccount -> Lude.Maybe IAMUserAccessToBilling) (\s a -> s {iamUserAccessToBilling = a} :: CreateAccount)
{-# DEPRECATED caIAMUserAccessToBilling "Use generic-lens or generic-optics with 'iamUserAccessToBilling' instead." #-}

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
caRoleName :: Lens.Lens' CreateAccount (Lude.Maybe Lude.Text)
caRoleName = Lens.lens (roleName :: CreateAccount -> Lude.Maybe Lude.Text) (\s a -> s {roleName = a} :: CreateAccount)
{-# DEPRECATED caRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | A list of tags that you want to attach to the newly created account. For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTags :: Lens.Lens' CreateAccount (Lude.Maybe [Tag])
caTags = Lens.lens (tags :: CreateAccount -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateAccount)
{-# DEPRECATED caTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The email address of the owner to assign to the new member account. This email address must not already be associated with another AWS account. You must use a valid email address to complete account creation. You can't access the root user of the account or remove an account that was created with an invalid email address.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caEmail :: Lens.Lens' CreateAccount (Lude.Sensitive Lude.Text)
caEmail = Lens.lens (email :: CreateAccount -> Lude.Sensitive Lude.Text) (\s a -> s {email = a} :: CreateAccount)
{-# DEPRECATED caEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The friendly name of the member account.
--
-- /Note:/ Consider using 'accountName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAccountName :: Lens.Lens' CreateAccount (Lude.Sensitive Lude.Text)
caAccountName = Lens.lens (accountName :: CreateAccount -> Lude.Sensitive Lude.Text) (\s a -> s {accountName = a} :: CreateAccount)
{-# DEPRECATED caAccountName "Use generic-lens or generic-optics with 'accountName' instead." #-}

instance Lude.AWSRequest CreateAccount where
  type Rs CreateAccount = CreateAccountResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateAccountResponse'
            Lude.<$> (x Lude..?> "CreateAccountStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateAccount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSOrganizationsV20161128.CreateAccount" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateAccount where
  toJSON CreateAccount' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IamUserAccessToBilling" Lude..=)
              Lude.<$> iamUserAccessToBilling,
            ("RoleName" Lude..=) Lude.<$> roleName,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("Email" Lude..= email),
            Lude.Just ("AccountName" Lude..= accountName)
          ]
      )

instance Lude.ToPath CreateAccount where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateAccount where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateAccountResponse' smart constructor.
data CreateAccountResponse = CreateAccountResponse'
  { createAccountStatus ::
      Lude.Maybe CreateAccountStatus,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAccountResponse' with the minimum fields required to make a request.
--
-- * 'createAccountStatus' - A structure that contains details about the request to create an account. This response structure might not be fully populated when you first receive it because account creation is an asynchronous process. You can pass the returned @CreateAccountStatus@ ID as a parameter to 'DescribeCreateAccountStatus' to get status about the progress of the request at later times. You can also check the AWS CloudTrail log for the @CreateAccountResult@ event. For more information, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_monitoring.html Monitoring the Activity in Your Organization> in the /AWS Organizations User Guide/ .
-- * 'responseStatus' - The response status code.
mkCreateAccountResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateAccountResponse
mkCreateAccountResponse pResponseStatus_ =
  CreateAccountResponse'
    { createAccountStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure that contains details about the request to create an account. This response structure might not be fully populated when you first receive it because account creation is an asynchronous process. You can pass the returned @CreateAccountStatus@ ID as a parameter to 'DescribeCreateAccountStatus' to get status about the progress of the request at later times. You can also check the AWS CloudTrail log for the @CreateAccountResult@ event. For more information, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_monitoring.html Monitoring the Activity in Your Organization> in the /AWS Organizations User Guide/ .
--
-- /Note:/ Consider using 'createAccountStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsCreateAccountStatus :: Lens.Lens' CreateAccountResponse (Lude.Maybe CreateAccountStatus)
carsCreateAccountStatus = Lens.lens (createAccountStatus :: CreateAccountResponse -> Lude.Maybe CreateAccountStatus) (\s a -> s {createAccountStatus = a} :: CreateAccountResponse)
{-# DEPRECATED carsCreateAccountStatus "Use generic-lens or generic-optics with 'createAccountStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsResponseStatus :: Lens.Lens' CreateAccountResponse Lude.Int
carsResponseStatus = Lens.lens (responseStatus :: CreateAccountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAccountResponse)
{-# DEPRECATED carsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
