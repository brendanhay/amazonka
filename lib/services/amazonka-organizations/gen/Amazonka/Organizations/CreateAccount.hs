{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Organizations.CreateAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS account that is automatically a member of the
-- organization whose credentials made the request. This is an asynchronous
-- request that AWS performs in the background. Because @CreateAccount@
-- operates asynchronously, it can return a successful completion message
-- even though account initialization might still be in progress. You might
-- need to wait a few minutes before you can successfully access the
-- account. To check the status of the request, do one of the following:
--
-- -   Use the @Id@ member of the @CreateAccountStatus@ response element
--     from this operation to provide as a parameter to the
--     DescribeCreateAccountStatus operation.
--
-- -   Check the AWS CloudTrail log for the @CreateAccountResult@ event.
--     For information on using AWS CloudTrail with AWS Organizations, see
--     <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_security_incident-response.html#orgs_cloudtrail-integration Logging and monitoring in AWS Organizations>
--     in the /AWS Organizations User Guide./
--
-- The user who calls the API to create an account must have the
-- @organizations:CreateAccount@ permission. If you enabled all features in
-- the organization, AWS Organizations creates the required service-linked
-- role named @AWSServiceRoleForOrganizations@. For more information, see
-- <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html#orgs_integrate_services-using_slrs AWS Organizations and Service-Linked Roles>
-- in the /AWS Organizations User Guide/.
--
-- If the request includes tags, then the requester must have the
-- @organizations:TagResource@ permission.
--
-- AWS Organizations preconfigures the new member account with a role
-- (named @OrganizationAccountAccessRole@ by default) that grants users in
-- the management account administrator permissions in the new member
-- account. Principals in the management account can assume the role. AWS
-- Organizations clones the company name and address information for the
-- new account from the organization\'s management account.
--
-- This operation can be called only from the organization\'s management
-- account.
--
-- For more information about creating accounts, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_create.html Creating an AWS Account in Your Organization>
-- in the /AWS Organizations User Guide./
--
-- -   When you create an account in an organization using the AWS
--     Organizations console, API, or CLI commands, the information
--     required for the account to operate as a standalone account, such as
--     a payment method and signing the end user license agreement (EULA)
--     is /not/ automatically collected. If you must remove an account from
--     your organization later, you can do so only after you provide the
--     missing information. Follow the steps at
--     <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info To leave an organization as a member account>
--     in the /AWS Organizations User Guide/.
--
-- -   If you get an exception that indicates that you exceeded your
--     account limits for the organization, contact
--     <https://console.aws.amazon.com/support/home#/ AWS Support>.
--
-- -   If you get an exception that indicates that the operation failed
--     because your organization is still initializing, wait one hour and
--     then try again. If the error persists, contact
--     <https://console.aws.amazon.com/support/home#/ AWS Support>.
--
-- -   Using @CreateAccount@ to create multiple temporary accounts isn\'t
--     recommended. You can only close an account from the Billing and Cost
--     Management Console, and you must be signed in as the root user. For
--     information on the requirements and process for closing an account,
--     see
--     <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_close.html Closing an AWS Account>
--     in the /AWS Organizations User Guide/.
--
-- When you create a member account with this operation, you can choose
-- whether to create the account with the __IAM User and Role Access to
-- Billing Information__ switch enabled. If you enable it, IAM users and
-- roles that have appropriate permissions can view billing information for
-- the account. If you disable it, only the account root user can access
-- billing information. For information about how to disable this switch
-- for an account, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html Granting Access to Your Billing Information and Tools>.
module Amazonka.Organizations.CreateAccount
  ( -- * Creating a Request
    CreateAccount (..),
    newCreateAccount,

    -- * Request Lenses
    createAccount_iamUserAccessToBilling,
    createAccount_roleName,
    createAccount_tags,
    createAccount_email,
    createAccount_accountName,

    -- * Destructuring the Response
    CreateAccountResponse (..),
    newCreateAccountResponse,

    -- * Response Lenses
    createAccountResponse_createAccountStatus,
    createAccountResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAccount' smart constructor.
data CreateAccount = CreateAccount'
  { -- | If set to @ALLOW@, the new account enables IAM users to access account
    -- billing information /if/ they have the required permissions. If set to
    -- @DENY@, only the root user of the new account can access account billing
    -- information. For more information, see
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html#ControllingAccessWebsite-Activate Activating Access to the Billing and Cost Management Console>
    -- in the /AWS Billing and Cost Management User Guide/.
    --
    -- If you don\'t specify this parameter, the value defaults to @ALLOW@, and
    -- IAM users and roles with the required permissions can access billing
    -- information for the new account.
    iamUserAccessToBilling :: Prelude.Maybe IAMUserAccessToBilling,
    -- | (Optional)
    --
    -- The name of an IAM role that AWS Organizations automatically
    -- preconfigures in the new member account. This role trusts the management
    -- account, allowing users in the management account to assume the role, as
    -- permitted by the management account administrator. The role has
    -- administrator permissions in the new member account.
    --
    -- If you don\'t specify this parameter, the role name defaults to
    -- @OrganizationAccountAccessRole@.
    --
    -- For more information about how to use this role to access the member
    -- account, see the following links:
    --
    -- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_access.html#orgs_manage_accounts_create-cross-account-role Accessing and Administering the Member Accounts in Your Organization>
    --     in the /AWS Organizations User Guide/
    --
    -- -   Steps 2 and 3 in
    --     <https://docs.aws.amazon.com/IAM/latest/UserGuide/tutorial_cross-account-with-roles.html Tutorial: Delegate Access Across AWS Accounts Using IAM Roles>
    --     in the /IAM User Guide/
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
    -- validate this parameter. The pattern can include uppercase letters,
    -- lowercase letters, digits with no spaces, and any of the following
    -- characters: =,.\@-
    roleName :: Prelude.Maybe Prelude.Text,
    -- | A list of tags that you want to attach to the newly created account. For
    -- each tag in the list, you must specify both a tag key and a value. You
    -- can set the value to an empty string, but you can\'t set it to @null@.
    -- For more information about tagging, see
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources>
    -- in the AWS Organizations User Guide.
    --
    -- If any one of the tags is invalid or if you exceed the allowed number of
    -- tags for an account, then the entire request fails and the account is
    -- not created.
    tags :: Prelude.Maybe [Tag],
    -- | The email address of the owner to assign to the new member account. This
    -- email address must not already be associated with another AWS account.
    -- You must use a valid email address to complete account creation. You
    -- can\'t access the root user of the account or remove an account that was
    -- created with an invalid email address.
    email :: Core.Sensitive Prelude.Text,
    -- | The friendly name of the member account.
    accountName :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamUserAccessToBilling', 'createAccount_iamUserAccessToBilling' - If set to @ALLOW@, the new account enables IAM users to access account
-- billing information /if/ they have the required permissions. If set to
-- @DENY@, only the root user of the new account can access account billing
-- information. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html#ControllingAccessWebsite-Activate Activating Access to the Billing and Cost Management Console>
-- in the /AWS Billing and Cost Management User Guide/.
--
-- If you don\'t specify this parameter, the value defaults to @ALLOW@, and
-- IAM users and roles with the required permissions can access billing
-- information for the new account.
--
-- 'roleName', 'createAccount_roleName' - (Optional)
--
-- The name of an IAM role that AWS Organizations automatically
-- preconfigures in the new member account. This role trusts the management
-- account, allowing users in the management account to assume the role, as
-- permitted by the management account administrator. The role has
-- administrator permissions in the new member account.
--
-- If you don\'t specify this parameter, the role name defaults to
-- @OrganizationAccountAccessRole@.
--
-- For more information about how to use this role to access the member
-- account, see the following links:
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_access.html#orgs_manage_accounts_create-cross-account-role Accessing and Administering the Member Accounts in Your Organization>
--     in the /AWS Organizations User Guide/
--
-- -   Steps 2 and 3 in
--     <https://docs.aws.amazon.com/IAM/latest/UserGuide/tutorial_cross-account-with-roles.html Tutorial: Delegate Access Across AWS Accounts Using IAM Roles>
--     in the /IAM User Guide/
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter. The pattern can include uppercase letters,
-- lowercase letters, digits with no spaces, and any of the following
-- characters: =,.\@-
--
-- 'tags', 'createAccount_tags' - A list of tags that you want to attach to the newly created account. For
-- each tag in the list, you must specify both a tag key and a value. You
-- can set the value to an empty string, but you can\'t set it to @null@.
-- For more information about tagging, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources>
-- in the AWS Organizations User Guide.
--
-- If any one of the tags is invalid or if you exceed the allowed number of
-- tags for an account, then the entire request fails and the account is
-- not created.
--
-- 'email', 'createAccount_email' - The email address of the owner to assign to the new member account. This
-- email address must not already be associated with another AWS account.
-- You must use a valid email address to complete account creation. You
-- can\'t access the root user of the account or remove an account that was
-- created with an invalid email address.
--
-- 'accountName', 'createAccount_accountName' - The friendly name of the member account.
newCreateAccount ::
  -- | 'email'
  Prelude.Text ->
  -- | 'accountName'
  Prelude.Text ->
  CreateAccount
newCreateAccount pEmail_ pAccountName_ =
  CreateAccount'
    { iamUserAccessToBilling =
        Prelude.Nothing,
      roleName = Prelude.Nothing,
      tags = Prelude.Nothing,
      email = Core._Sensitive Lens.# pEmail_,
      accountName = Core._Sensitive Lens.# pAccountName_
    }

-- | If set to @ALLOW@, the new account enables IAM users to access account
-- billing information /if/ they have the required permissions. If set to
-- @DENY@, only the root user of the new account can access account billing
-- information. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html#ControllingAccessWebsite-Activate Activating Access to the Billing and Cost Management Console>
-- in the /AWS Billing and Cost Management User Guide/.
--
-- If you don\'t specify this parameter, the value defaults to @ALLOW@, and
-- IAM users and roles with the required permissions can access billing
-- information for the new account.
createAccount_iamUserAccessToBilling :: Lens.Lens' CreateAccount (Prelude.Maybe IAMUserAccessToBilling)
createAccount_iamUserAccessToBilling = Lens.lens (\CreateAccount' {iamUserAccessToBilling} -> iamUserAccessToBilling) (\s@CreateAccount' {} a -> s {iamUserAccessToBilling = a} :: CreateAccount)

-- | (Optional)
--
-- The name of an IAM role that AWS Organizations automatically
-- preconfigures in the new member account. This role trusts the management
-- account, allowing users in the management account to assume the role, as
-- permitted by the management account administrator. The role has
-- administrator permissions in the new member account.
--
-- If you don\'t specify this parameter, the role name defaults to
-- @OrganizationAccountAccessRole@.
--
-- For more information about how to use this role to access the member
-- account, see the following links:
--
-- -   <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_access.html#orgs_manage_accounts_create-cross-account-role Accessing and Administering the Member Accounts in Your Organization>
--     in the /AWS Organizations User Guide/
--
-- -   Steps 2 and 3 in
--     <https://docs.aws.amazon.com/IAM/latest/UserGuide/tutorial_cross-account-with-roles.html Tutorial: Delegate Access Across AWS Accounts Using IAM Roles>
--     in the /IAM User Guide/
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter. The pattern can include uppercase letters,
-- lowercase letters, digits with no spaces, and any of the following
-- characters: =,.\@-
createAccount_roleName :: Lens.Lens' CreateAccount (Prelude.Maybe Prelude.Text)
createAccount_roleName = Lens.lens (\CreateAccount' {roleName} -> roleName) (\s@CreateAccount' {} a -> s {roleName = a} :: CreateAccount)

-- | A list of tags that you want to attach to the newly created account. For
-- each tag in the list, you must specify both a tag key and a value. You
-- can set the value to an empty string, but you can\'t set it to @null@.
-- For more information about tagging, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources>
-- in the AWS Organizations User Guide.
--
-- If any one of the tags is invalid or if you exceed the allowed number of
-- tags for an account, then the entire request fails and the account is
-- not created.
createAccount_tags :: Lens.Lens' CreateAccount (Prelude.Maybe [Tag])
createAccount_tags = Lens.lens (\CreateAccount' {tags} -> tags) (\s@CreateAccount' {} a -> s {tags = a} :: CreateAccount) Prelude.. Lens.mapping Lens.coerced

-- | The email address of the owner to assign to the new member account. This
-- email address must not already be associated with another AWS account.
-- You must use a valid email address to complete account creation. You
-- can\'t access the root user of the account or remove an account that was
-- created with an invalid email address.
createAccount_email :: Lens.Lens' CreateAccount Prelude.Text
createAccount_email = Lens.lens (\CreateAccount' {email} -> email) (\s@CreateAccount' {} a -> s {email = a} :: CreateAccount) Prelude.. Core._Sensitive

-- | The friendly name of the member account.
createAccount_accountName :: Lens.Lens' CreateAccount Prelude.Text
createAccount_accountName = Lens.lens (\CreateAccount' {accountName} -> accountName) (\s@CreateAccount' {} a -> s {accountName = a} :: CreateAccount) Prelude.. Core._Sensitive

instance Core.AWSRequest CreateAccount where
  type
    AWSResponse CreateAccount =
      CreateAccountResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAccountResponse'
            Prelude.<$> (x Core..?> "CreateAccountStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAccount where
  hashWithSalt salt' CreateAccount' {..} =
    salt' `Prelude.hashWithSalt` accountName
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` roleName
      `Prelude.hashWithSalt` iamUserAccessToBilling

instance Prelude.NFData CreateAccount where
  rnf CreateAccount' {..} =
    Prelude.rnf iamUserAccessToBilling
      `Prelude.seq` Prelude.rnf accountName
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf roleName

instance Core.ToHeaders CreateAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.CreateAccount" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateAccount where
  toJSON CreateAccount' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("IamUserAccessToBilling" Core..=)
              Prelude.<$> iamUserAccessToBilling,
            ("RoleName" Core..=) Prelude.<$> roleName,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Email" Core..= email),
            Prelude.Just ("AccountName" Core..= accountName)
          ]
      )

instance Core.ToPath CreateAccount where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAccountResponse' smart constructor.
data CreateAccountResponse = CreateAccountResponse'
  { -- | A structure that contains details about the request to create an
    -- account. This response structure might not be fully populated when you
    -- first receive it because account creation is an asynchronous process.
    -- You can pass the returned @CreateAccountStatus@ ID as a parameter to
    -- DescribeCreateAccountStatus to get status about the progress of the
    -- request at later times. You can also check the AWS CloudTrail log for
    -- the @CreateAccountResult@ event. For more information, see
    -- <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_monitoring.html Monitoring the Activity in Your Organization>
    -- in the /AWS Organizations User Guide/.
    createAccountStatus :: Prelude.Maybe CreateAccountStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createAccountStatus', 'createAccountResponse_createAccountStatus' - A structure that contains details about the request to create an
-- account. This response structure might not be fully populated when you
-- first receive it because account creation is an asynchronous process.
-- You can pass the returned @CreateAccountStatus@ ID as a parameter to
-- DescribeCreateAccountStatus to get status about the progress of the
-- request at later times. You can also check the AWS CloudTrail log for
-- the @CreateAccountResult@ event. For more information, see
-- <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_monitoring.html Monitoring the Activity in Your Organization>
-- in the /AWS Organizations User Guide/.
--
-- 'httpStatus', 'createAccountResponse_httpStatus' - The response's http status code.
newCreateAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAccountResponse
newCreateAccountResponse pHttpStatus_ =
  CreateAccountResponse'
    { createAccountStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains details about the request to create an
-- account. This response structure might not be fully populated when you
-- first receive it because account creation is an asynchronous process.
-- You can pass the returned @CreateAccountStatus@ ID as a parameter to
-- DescribeCreateAccountStatus to get status about the progress of the
-- request at later times. You can also check the AWS CloudTrail log for
-- the @CreateAccountResult@ event. For more information, see
-- <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_monitoring.html Monitoring the Activity in Your Organization>
-- in the /AWS Organizations User Guide/.
createAccountResponse_createAccountStatus :: Lens.Lens' CreateAccountResponse (Prelude.Maybe CreateAccountStatus)
createAccountResponse_createAccountStatus = Lens.lens (\CreateAccountResponse' {createAccountStatus} -> createAccountStatus) (\s@CreateAccountResponse' {} a -> s {createAccountStatus = a} :: CreateAccountResponse)

-- | The response's http status code.
createAccountResponse_httpStatus :: Lens.Lens' CreateAccountResponse Prelude.Int
createAccountResponse_httpStatus = Lens.lens (\CreateAccountResponse' {httpStatus} -> httpStatus) (\s@CreateAccountResponse' {} a -> s {httpStatus = a} :: CreateAccountResponse)

instance Prelude.NFData CreateAccountResponse where
  rnf CreateAccountResponse' {..} =
    Prelude.rnf createAccountStatus
      `Prelude.seq` Prelude.rnf httpStatus
