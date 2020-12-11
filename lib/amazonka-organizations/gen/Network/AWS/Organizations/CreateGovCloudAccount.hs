{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.CreateGovCloudAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action is available if all of the following are true:
--
--
--     * You're authorized to create accounts in the AWS GovCloud (US) Region. For more information on the AWS GovCloud (US) Region, see the <http://docs.aws.amazon.com/govcloud-us/latest/UserGuide/welcome.html /AWS GovCloud User Guide/ .>
--
--
--     * You already have an account in the AWS GovCloud (US) Region that is paired with a management account of an organization in the commercial Region.
--
--
--     * You call this action from the management account of your organization in the commercial Region.
--
--
--     * You have the @organizations:CreateGovCloudAccount@ permission.
--
--
-- AWS Organizations automatically creates the required service-linked role named @AWSServiceRoleForOrganizations@ . For more information, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html#orgs_integrate_services-using_slrs AWS Organizations and Service-Linked Roles> in the /AWS Organizations User Guide./
-- AWS automatically enables AWS CloudTrail for AWS GovCloud (US) accounts, but you should also do the following:
--
--     * Verify that AWS CloudTrail is enabled to store logs.
--
--
--     * Create an S3 bucket for AWS CloudTrail log storage.
-- For more information, see <http://docs.aws.amazon.com/govcloud-us/latest/UserGuide/verifying-cloudtrail.html Verifying AWS CloudTrail Is Enabled> in the /AWS GovCloud User Guide/ .
--
--
-- If the request includes tags, then the requester must have the @organizations:TagResource@ permission. The tags are attached to the commercial account associated with the GovCloud account, rather than the GovCloud account itself. To add tags to the GovCloud account, call the 'TagResource' operation in the GovCloud Region after the new GovCloud account exists.
-- You call this action from the management account of your organization in the commercial Region to create a standalone AWS account in the AWS GovCloud (US) Region. After the account is created, the management account of an organization in the AWS GovCloud (US) Region can invite it to that organization. For more information on inviting standalone accounts in the AWS GovCloud (US) to join an organization, see <http://docs.aws.amazon.com/govcloud-us/latest/UserGuide/govcloud-organizations.html AWS Organizations> in the /AWS GovCloud User Guide./
-- Calling @CreateGovCloudAccount@ is an asynchronous request that AWS performs in the background. Because @CreateGovCloudAccount@ operates asynchronously, it can return a successful completion message even though account initialization might still be in progress. You might need to wait a few minutes before you can successfully access the account. To check the status of the request, do one of the following:
--
--     * Use the @OperationId@ response element from this operation to provide as a parameter to the 'DescribeCreateAccountStatus' operation.
--
--
--     * Check the AWS CloudTrail log for the @CreateAccountResult@ event. For information on using AWS CloudTrail with Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_monitoring.html Monitoring the Activity in Your Organization> in the /AWS Organizations User Guide./
--
--
--
-- When you call the @CreateGovCloudAccount@ action, you create two accounts: a standalone account in the AWS GovCloud (US) Region and an associated account in the commercial Region for billing and support purposes. The account in the commercial Region is automatically a member of the organization whose credentials made the request. Both accounts are associated with the same email address.
-- A role is created in the new account in the commercial Region that allows the management account in the organization in the commercial Region to assume it. An AWS GovCloud (US) account is then created and associated with the commercial account that you just created. A role is also created in the new AWS GovCloud (US) account that can be assumed by the AWS GovCloud (US) account that is associated with the management account of the commercial organization. For more information and to view a diagram that explains how account access works, see <http://docs.aws.amazon.com/govcloud-us/latest/UserGuide/govcloud-organizations.html AWS Organizations> in the /AWS GovCloud User Guide./
-- For more information about creating accounts, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_create.html Creating an AWS Account in Your Organization> in the /AWS Organizations User Guide./
-- /Important:/
--     * When you create an account in an organization using the AWS Organizations console, API, or CLI commands, the information required for the account to operate as a standalone account is /not/ automatically collected. This includes a payment method and signing the end user license agreement (EULA). If you must remove an account from your organization later, you can do so only after you provide the missing information. Follow the steps at <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info To leave an organization as a member account> in the /AWS Organizations User Guide./
--
--
--     * If you get an exception that indicates that you exceeded your account limits for the organization, contact <https://console.aws.amazon.com/support/home#/ AWS Support> .
--
--
--     * If you get an exception that indicates that the operation failed because your organization is still initializing, wait one hour and then try again. If the error persists, contact <https://console.aws.amazon.com/support/home#/ AWS Support> .
--
--
--     * Using @CreateGovCloudAccount@ to create multiple temporary accounts isn't recommended. You can only close an account from the AWS Billing and Cost Management console, and you must be signed in as the root user. For information on the requirements and process for closing an account, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_close.html Closing an AWS Account> in the /AWS Organizations User Guide/ .
module Network.AWS.Organizations.CreateGovCloudAccount
  ( -- * Creating a request
    CreateGovCloudAccount (..),
    mkCreateGovCloudAccount,

    -- ** Request lenses
    cgcaIAMUserAccessToBilling,
    cgcaRoleName,
    cgcaTags,
    cgcaEmail,
    cgcaAccountName,

    -- * Destructuring the response
    CreateGovCloudAccountResponse (..),
    mkCreateGovCloudAccountResponse,

    -- ** Response lenses
    cgcarsCreateAccountStatus,
    cgcarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateGovCloudAccount' smart constructor.
data CreateGovCloudAccount = CreateGovCloudAccount'
  { iamUserAccessToBilling ::
      Lude.Maybe IAMUserAccessToBilling,
    roleName :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    email :: Lude.Sensitive Lude.Text,
    accountName :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGovCloudAccount' with the minimum fields required to make a request.
--
-- * 'accountName' - The friendly name of the member account.
-- * 'email' - The email address of the owner to assign to the new member account in the commercial Region. This email address must not already be associated with another AWS account. You must use a valid email address to complete account creation. You can't access the root user of the account or remove an account that was created with an invalid email address. Like all request parameters for @CreateGovCloudAccount@ , the request for the email address for the AWS GovCloud (US) account originates from the commercial Region, not from the AWS GovCloud (US) Region.
-- * 'iamUserAccessToBilling' - If set to @ALLOW@ , the new linked account in the commercial Region enables IAM users to access account billing information /if/ they have the required permissions. If set to @DENY@ , only the root user of the new account can access account billing information. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html#ControllingAccessWebsite-Activate Activating Access to the Billing and Cost Management Console> in the /AWS Billing and Cost Management User Guide./
--
-- If you don't specify this parameter, the value defaults to @ALLOW@ , and IAM users and roles with the required permissions can access billing information for the new account.
-- * 'roleName' - (Optional)
--
-- The name of an IAM role that AWS Organizations automatically preconfigures in the new member accounts in both the AWS GovCloud (US) Region and in the commercial Region. This role trusts the management account, allowing users in the management account to assume the role, as permitted by the management account administrator. The role has administrator permissions in the new member account.
-- If you don't specify this parameter, the role name defaults to @OrganizationAccountAccessRole@ .
-- For more information about how to use this role to access the member account, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_access.html#orgs_manage_accounts_create-cross-account-role Accessing and Administering the Member Accounts in Your Organization> in the /AWS Organizations User Guide/ and steps 2 and 3 in <https://docs.aws.amazon.com/IAM/latest/UserGuide/tutorial_cross-account-with-roles.html Tutorial: Delegate Access Across AWS Accounts Using IAM Roles> in the /IAM User Guide./
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter. The pattern can include uppercase letters, lowercase letters, digits with no spaces, and any of the following characters: =,.@-
-- * 'tags' - A list of tags that you want to attach to the newly created account. These tags are attached to the commercial account associated with the GovCloud account, and not to the GovCloud account itself. To add tags to the actual GovCloud account, call the 'TagResource' operation in the GovCloud region after the new GovCloud account exists.
--
-- For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
mkCreateGovCloudAccount ::
  -- | 'email'
  Lude.Sensitive Lude.Text ->
  -- | 'accountName'
  Lude.Sensitive Lude.Text ->
  CreateGovCloudAccount
mkCreateGovCloudAccount pEmail_ pAccountName_ =
  CreateGovCloudAccount'
    { iamUserAccessToBilling = Lude.Nothing,
      roleName = Lude.Nothing,
      tags = Lude.Nothing,
      email = pEmail_,
      accountName = pAccountName_
    }

-- | If set to @ALLOW@ , the new linked account in the commercial Region enables IAM users to access account billing information /if/ they have the required permissions. If set to @DENY@ , only the root user of the new account can access account billing information. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html#ControllingAccessWebsite-Activate Activating Access to the Billing and Cost Management Console> in the /AWS Billing and Cost Management User Guide./
--
-- If you don't specify this parameter, the value defaults to @ALLOW@ , and IAM users and roles with the required permissions can access billing information for the new account.
--
-- /Note:/ Consider using 'iamUserAccessToBilling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcaIAMUserAccessToBilling :: Lens.Lens' CreateGovCloudAccount (Lude.Maybe IAMUserAccessToBilling)
cgcaIAMUserAccessToBilling = Lens.lens (iamUserAccessToBilling :: CreateGovCloudAccount -> Lude.Maybe IAMUserAccessToBilling) (\s a -> s {iamUserAccessToBilling = a} :: CreateGovCloudAccount)
{-# DEPRECATED cgcaIAMUserAccessToBilling "Use generic-lens or generic-optics with 'iamUserAccessToBilling' instead." #-}

-- | (Optional)
--
-- The name of an IAM role that AWS Organizations automatically preconfigures in the new member accounts in both the AWS GovCloud (US) Region and in the commercial Region. This role trusts the management account, allowing users in the management account to assume the role, as permitted by the management account administrator. The role has administrator permissions in the new member account.
-- If you don't specify this parameter, the role name defaults to @OrganizationAccountAccessRole@ .
-- For more information about how to use this role to access the member account, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_access.html#orgs_manage_accounts_create-cross-account-role Accessing and Administering the Member Accounts in Your Organization> in the /AWS Organizations User Guide/ and steps 2 and 3 in <https://docs.aws.amazon.com/IAM/latest/UserGuide/tutorial_cross-account-with-roles.html Tutorial: Delegate Access Across AWS Accounts Using IAM Roles> in the /IAM User Guide./
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter. The pattern can include uppercase letters, lowercase letters, digits with no spaces, and any of the following characters: =,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcaRoleName :: Lens.Lens' CreateGovCloudAccount (Lude.Maybe Lude.Text)
cgcaRoleName = Lens.lens (roleName :: CreateGovCloudAccount -> Lude.Maybe Lude.Text) (\s a -> s {roleName = a} :: CreateGovCloudAccount)
{-# DEPRECATED cgcaRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | A list of tags that you want to attach to the newly created account. These tags are attached to the commercial account associated with the GovCloud account, and not to the GovCloud account itself. To add tags to the actual GovCloud account, call the 'TagResource' operation in the GovCloud region after the new GovCloud account exists.
--
-- For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcaTags :: Lens.Lens' CreateGovCloudAccount (Lude.Maybe [Tag])
cgcaTags = Lens.lens (tags :: CreateGovCloudAccount -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateGovCloudAccount)
{-# DEPRECATED cgcaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The email address of the owner to assign to the new member account in the commercial Region. This email address must not already be associated with another AWS account. You must use a valid email address to complete account creation. You can't access the root user of the account or remove an account that was created with an invalid email address. Like all request parameters for @CreateGovCloudAccount@ , the request for the email address for the AWS GovCloud (US) account originates from the commercial Region, not from the AWS GovCloud (US) Region.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcaEmail :: Lens.Lens' CreateGovCloudAccount (Lude.Sensitive Lude.Text)
cgcaEmail = Lens.lens (email :: CreateGovCloudAccount -> Lude.Sensitive Lude.Text) (\s a -> s {email = a} :: CreateGovCloudAccount)
{-# DEPRECATED cgcaEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The friendly name of the member account.
--
-- /Note:/ Consider using 'accountName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcaAccountName :: Lens.Lens' CreateGovCloudAccount (Lude.Sensitive Lude.Text)
cgcaAccountName = Lens.lens (accountName :: CreateGovCloudAccount -> Lude.Sensitive Lude.Text) (\s a -> s {accountName = a} :: CreateGovCloudAccount)
{-# DEPRECATED cgcaAccountName "Use generic-lens or generic-optics with 'accountName' instead." #-}

instance Lude.AWSRequest CreateGovCloudAccount where
  type Rs CreateGovCloudAccount = CreateGovCloudAccountResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateGovCloudAccountResponse'
            Lude.<$> (x Lude..?> "CreateAccountStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateGovCloudAccount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrganizationsV20161128.CreateGovCloudAccount" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateGovCloudAccount where
  toJSON CreateGovCloudAccount' {..} =
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

instance Lude.ToPath CreateGovCloudAccount where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateGovCloudAccount where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateGovCloudAccountResponse' smart constructor.
data CreateGovCloudAccountResponse = CreateGovCloudAccountResponse'
  { createAccountStatus ::
      Lude.Maybe CreateAccountStatus,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGovCloudAccountResponse' with the minimum fields required to make a request.
--
-- * 'createAccountStatus' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkCreateGovCloudAccountResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateGovCloudAccountResponse
mkCreateGovCloudAccountResponse pResponseStatus_ =
  CreateGovCloudAccountResponse'
    { createAccountStatus =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'createAccountStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcarsCreateAccountStatus :: Lens.Lens' CreateGovCloudAccountResponse (Lude.Maybe CreateAccountStatus)
cgcarsCreateAccountStatus = Lens.lens (createAccountStatus :: CreateGovCloudAccountResponse -> Lude.Maybe CreateAccountStatus) (\s a -> s {createAccountStatus = a} :: CreateGovCloudAccountResponse)
{-# DEPRECATED cgcarsCreateAccountStatus "Use generic-lens or generic-optics with 'createAccountStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcarsResponseStatus :: Lens.Lens' CreateGovCloudAccountResponse Lude.Int
cgcarsResponseStatus = Lens.lens (responseStatus :: CreateGovCloudAccountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateGovCloudAccountResponse)
{-# DEPRECATED cgcarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
