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
-- Module      : Amazonka.Organizations.CreateGovCloudAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action is available if all of the following are true:
--
-- -   You\'re authorized to create accounts in the Amazon Web Services
--     GovCloud (US) Region. For more information on the Amazon Web
--     Services GovCloud (US) Region, see the
--     <https://docs.aws.amazon.com/govcloud-us/latest/UserGuide/welcome.html Amazon Web Services GovCloud User Guide.>
--
-- -   You already have an account in the Amazon Web Services GovCloud (US)
--     Region that is paired with a management account of an organization
--     in the commercial Region.
--
-- -   You call this action from the management account of your
--     organization in the commercial Region.
--
-- -   You have the @organizations:CreateGovCloudAccount@ permission.
--
-- Organizations automatically creates the required service-linked role
-- named @AWSServiceRoleForOrganizations@. For more information, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html#orgs_integrate_services-using_slrs Organizations and Service-Linked Roles>
-- in the /Organizations User Guide./
--
-- Amazon Web Services automatically enables CloudTrail for Amazon Web
-- Services GovCloud (US) accounts, but you should also do the following:
--
-- -   Verify that CloudTrail is enabled to store logs.
--
-- -   Create an Amazon S3 bucket for CloudTrail log storage.
--
--     For more information, see
--     <https://docs.aws.amazon.com/govcloud-us/latest/UserGuide/verifying-cloudtrail.html Verifying CloudTrail Is Enabled>
--     in the /Amazon Web Services GovCloud User Guide/.
--
-- If the request includes tags, then the requester must have the
-- @organizations:TagResource@ permission. The tags are attached to the
-- commercial account associated with the GovCloud account, rather than the
-- GovCloud account itself. To add tags to the GovCloud account, call the
-- TagResource operation in the GovCloud Region after the new GovCloud
-- account exists.
--
-- You call this action from the management account of your organization in
-- the commercial Region to create a standalone Amazon Web Services account
-- in the Amazon Web Services GovCloud (US) Region. After the account is
-- created, the management account of an organization in the Amazon Web
-- Services GovCloud (US) Region can invite it to that organization. For
-- more information on inviting standalone accounts in the Amazon Web
-- Services GovCloud (US) to join an organization, see
-- <https://docs.aws.amazon.com/govcloud-us/latest/UserGuide/govcloud-organizations.html Organizations>
-- in the /Amazon Web Services GovCloud User Guide./
--
-- Calling @CreateGovCloudAccount@ is an asynchronous request that Amazon
-- Web Services performs in the background. Because @CreateGovCloudAccount@
-- operates asynchronously, it can return a successful completion message
-- even though account initialization might still be in progress. You might
-- need to wait a few minutes before you can successfully access the
-- account. To check the status of the request, do one of the following:
--
-- -   Use the @OperationId@ response element from this operation to
--     provide as a parameter to the DescribeCreateAccountStatus operation.
--
-- -   Check the CloudTrail log for the @CreateAccountResult@ event. For
--     information on using CloudTrail with Organizations, see
--     <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_monitoring.html Monitoring the Activity in Your Organization>
--     in the /Organizations User Guide./
--
-- When you call the @CreateGovCloudAccount@ action, you create two
-- accounts: a standalone account in the Amazon Web Services GovCloud (US)
-- Region and an associated account in the commercial Region for billing
-- and support purposes. The account in the commercial Region is
-- automatically a member of the organization whose credentials made the
-- request. Both accounts are associated with the same email address.
--
-- A role is created in the new account in the commercial Region that
-- allows the management account in the organization in the commercial
-- Region to assume it. An Amazon Web Services GovCloud (US) account is
-- then created and associated with the commercial account that you just
-- created. A role is also created in the new Amazon Web Services GovCloud
-- (US) account that can be assumed by the Amazon Web Services GovCloud
-- (US) account that is associated with the management account of the
-- commercial organization. For more information and to view a diagram that
-- explains how account access works, see
-- <https://docs.aws.amazon.com/govcloud-us/latest/UserGuide/govcloud-organizations.html Organizations>
-- in the /Amazon Web Services GovCloud User Guide./
--
-- For more information about creating accounts, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_create.html Creating an Amazon Web Services account in Your Organization>
-- in the /Organizations User Guide./
--
-- -   When you create an account in an organization using the
--     Organizations console, API, or CLI commands, the information
--     required for the account to operate as a standalone account is /not/
--     automatically collected. This includes a payment method and signing
--     the end user license agreement (EULA). If you must remove an account
--     from your organization later, you can do so only after you provide
--     the missing information. Follow the steps at
--     <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info To leave an organization as a member account>
--     in the /Organizations User Guide./
--
-- -   If you get an exception that indicates that you exceeded your
--     account limits for the organization, contact
--     <https://console.aws.amazon.com/support/home#/ Amazon Web Services Support>.
--
-- -   If you get an exception that indicates that the operation failed
--     because your organization is still initializing, wait one hour and
--     then try again. If the error persists, contact
--     <https://console.aws.amazon.com/support/home#/ Amazon Web Services Support>.
--
-- -   Using @CreateGovCloudAccount@ to create multiple temporary accounts
--     isn\'t recommended. You can only close an account from the Amazon
--     Web Services Billing and Cost Management console, and you must be
--     signed in as the root user. For information on the requirements and
--     process for closing an account, see
--     <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_close.html Closing an Amazon Web Services account>
--     in the /Organizations User Guide/.
--
-- When you create a member account with this operation, you can choose
-- whether to create the account with the __IAM User and Role Access to
-- Billing Information__ switch enabled. If you enable it, IAM users and
-- roles that have appropriate permissions can view billing information for
-- the account. If you disable it, only the account root user can access
-- billing information. For information about how to disable this switch
-- for an account, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html Granting Access to Your Billing Information and Tools>.
module Amazonka.Organizations.CreateGovCloudAccount
  ( -- * Creating a Request
    CreateGovCloudAccount (..),
    newCreateGovCloudAccount,

    -- * Request Lenses
    createGovCloudAccount_iamUserAccessToBilling,
    createGovCloudAccount_roleName,
    createGovCloudAccount_tags,
    createGovCloudAccount_email,
    createGovCloudAccount_accountName,

    -- * Destructuring the Response
    CreateGovCloudAccountResponse (..),
    newCreateGovCloudAccountResponse,

    -- * Response Lenses
    createGovCloudAccountResponse_createAccountStatus,
    createGovCloudAccountResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateGovCloudAccount' smart constructor.
data CreateGovCloudAccount = CreateGovCloudAccount'
  { -- | If set to @ALLOW@, the new linked account in the commercial Region
    -- enables IAM users to access account billing information /if/ they have
    -- the required permissions. If set to @DENY@, only the root user of the
    -- new account can access account billing information. For more
    -- information, see
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html#ControllingAccessWebsite-Activate Activating Access to the Billing and Cost Management Console>
    -- in the /Amazon Web Services Billing and Cost Management User Guide./
    --
    -- If you don\'t specify this parameter, the value defaults to @ALLOW@, and
    -- IAM users and roles with the required permissions can access billing
    -- information for the new account.
    iamUserAccessToBilling :: Prelude.Maybe IAMUserAccessToBilling,
    -- | (Optional)
    --
    -- The name of an IAM role that Organizations automatically preconfigures
    -- in the new member accounts in both the Amazon Web Services GovCloud (US)
    -- Region and in the commercial Region. This role trusts the management
    -- account, allowing users in the management account to assume the role, as
    -- permitted by the management account administrator. The role has
    -- administrator permissions in the new member account.
    --
    -- If you don\'t specify this parameter, the role name defaults to
    -- @OrganizationAccountAccessRole@.
    --
    -- For more information about how to use this role to access the member
    -- account, see
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_access.html#orgs_manage_accounts_create-cross-account-role Accessing and Administering the Member Accounts in Your Organization>
    -- in the /Organizations User Guide/ and steps 2 and 3 in
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/tutorial_cross-account-with-roles.html Tutorial: Delegate Access Across Amazon Web Services accounts Using IAM Roles>
    -- in the /IAM User Guide./
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
    -- validate this parameter. The pattern can include uppercase letters,
    -- lowercase letters, digits with no spaces, and any of the following
    -- characters: =,.\@-
    roleName :: Prelude.Maybe Prelude.Text,
    -- | A list of tags that you want to attach to the newly created account.
    -- These tags are attached to the commercial account associated with the
    -- GovCloud account, and not to the GovCloud account itself. To add tags to
    -- the actual GovCloud account, call the TagResource operation in the
    -- GovCloud region after the new GovCloud account exists.
    --
    -- For each tag in the list, you must specify both a tag key and a value.
    -- You can set the value to an empty string, but you can\'t set it to
    -- @null@. For more information about tagging, see
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging Organizations resources>
    -- in the Organizations User Guide.
    --
    -- If any one of the tags is invalid or if you exceed the maximum allowed
    -- number of tags for an account, then the entire request fails and the
    -- account is not created.
    tags :: Prelude.Maybe [Tag],
    -- | Specifies the email address of the owner to assign to the new member
    -- account in the commercial Region. This email address must not already be
    -- associated with another Amazon Web Services account. You must use a
    -- valid email address to complete account creation.
    --
    -- The rules for a valid email address:
    --
    -- -   The address must be a minimum of 6 and a maximum of 64 characters
    --     long.
    --
    -- -   All characters must be 7-bit ASCII characters.
    --
    -- -   There must be one and only one \@ symbol, which separates the local
    --     name from the domain name.
    --
    -- -   The local name can\'t contain any of the following characters:
    --
    --     whitespace, \" \' ( ) \< > [ ] : ; , \\ | % &
    --
    -- -   The local name can\'t begin with a dot (.)
    --
    -- -   The domain name can consist of only the characters
    --     [a-z],[A-Z],[0-9], hyphen (-), or dot (.)
    --
    -- -   The domain name can\'t begin or end with a hyphen (-) or dot (.)
    --
    -- -   The domain name must contain at least one dot
    --
    -- You can\'t access the root user of the account or remove an account that
    -- was created with an invalid email address. Like all request parameters
    -- for @CreateGovCloudAccount@, the request for the email address for the
    -- Amazon Web Services GovCloud (US) account originates from the commercial
    -- Region, not from the Amazon Web Services GovCloud (US) Region.
    email :: Data.Sensitive Prelude.Text,
    -- | The friendly name of the member account.
    --
    -- The account name can consist of only the characters [a-z],[A-Z],[0-9],
    -- hyphen (-), or dot (.) You can\'t separate characters with a dash (–).
    accountName :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGovCloudAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamUserAccessToBilling', 'createGovCloudAccount_iamUserAccessToBilling' - If set to @ALLOW@, the new linked account in the commercial Region
-- enables IAM users to access account billing information /if/ they have
-- the required permissions. If set to @DENY@, only the root user of the
-- new account can access account billing information. For more
-- information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html#ControllingAccessWebsite-Activate Activating Access to the Billing and Cost Management Console>
-- in the /Amazon Web Services Billing and Cost Management User Guide./
--
-- If you don\'t specify this parameter, the value defaults to @ALLOW@, and
-- IAM users and roles with the required permissions can access billing
-- information for the new account.
--
-- 'roleName', 'createGovCloudAccount_roleName' - (Optional)
--
-- The name of an IAM role that Organizations automatically preconfigures
-- in the new member accounts in both the Amazon Web Services GovCloud (US)
-- Region and in the commercial Region. This role trusts the management
-- account, allowing users in the management account to assume the role, as
-- permitted by the management account administrator. The role has
-- administrator permissions in the new member account.
--
-- If you don\'t specify this parameter, the role name defaults to
-- @OrganizationAccountAccessRole@.
--
-- For more information about how to use this role to access the member
-- account, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_access.html#orgs_manage_accounts_create-cross-account-role Accessing and Administering the Member Accounts in Your Organization>
-- in the /Organizations User Guide/ and steps 2 and 3 in
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/tutorial_cross-account-with-roles.html Tutorial: Delegate Access Across Amazon Web Services accounts Using IAM Roles>
-- in the /IAM User Guide./
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter. The pattern can include uppercase letters,
-- lowercase letters, digits with no spaces, and any of the following
-- characters: =,.\@-
--
-- 'tags', 'createGovCloudAccount_tags' - A list of tags that you want to attach to the newly created account.
-- These tags are attached to the commercial account associated with the
-- GovCloud account, and not to the GovCloud account itself. To add tags to
-- the actual GovCloud account, call the TagResource operation in the
-- GovCloud region after the new GovCloud account exists.
--
-- For each tag in the list, you must specify both a tag key and a value.
-- You can set the value to an empty string, but you can\'t set it to
-- @null@. For more information about tagging, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging Organizations resources>
-- in the Organizations User Guide.
--
-- If any one of the tags is invalid or if you exceed the maximum allowed
-- number of tags for an account, then the entire request fails and the
-- account is not created.
--
-- 'email', 'createGovCloudAccount_email' - Specifies the email address of the owner to assign to the new member
-- account in the commercial Region. This email address must not already be
-- associated with another Amazon Web Services account. You must use a
-- valid email address to complete account creation.
--
-- The rules for a valid email address:
--
-- -   The address must be a minimum of 6 and a maximum of 64 characters
--     long.
--
-- -   All characters must be 7-bit ASCII characters.
--
-- -   There must be one and only one \@ symbol, which separates the local
--     name from the domain name.
--
-- -   The local name can\'t contain any of the following characters:
--
--     whitespace, \" \' ( ) \< > [ ] : ; , \\ | % &
--
-- -   The local name can\'t begin with a dot (.)
--
-- -   The domain name can consist of only the characters
--     [a-z],[A-Z],[0-9], hyphen (-), or dot (.)
--
-- -   The domain name can\'t begin or end with a hyphen (-) or dot (.)
--
-- -   The domain name must contain at least one dot
--
-- You can\'t access the root user of the account or remove an account that
-- was created with an invalid email address. Like all request parameters
-- for @CreateGovCloudAccount@, the request for the email address for the
-- Amazon Web Services GovCloud (US) account originates from the commercial
-- Region, not from the Amazon Web Services GovCloud (US) Region.
--
-- 'accountName', 'createGovCloudAccount_accountName' - The friendly name of the member account.
--
-- The account name can consist of only the characters [a-z],[A-Z],[0-9],
-- hyphen (-), or dot (.) You can\'t separate characters with a dash (–).
newCreateGovCloudAccount ::
  -- | 'email'
  Prelude.Text ->
  -- | 'accountName'
  Prelude.Text ->
  CreateGovCloudAccount
newCreateGovCloudAccount pEmail_ pAccountName_ =
  CreateGovCloudAccount'
    { iamUserAccessToBilling =
        Prelude.Nothing,
      roleName = Prelude.Nothing,
      tags = Prelude.Nothing,
      email = Data._Sensitive Lens.# pEmail_,
      accountName = Data._Sensitive Lens.# pAccountName_
    }

-- | If set to @ALLOW@, the new linked account in the commercial Region
-- enables IAM users to access account billing information /if/ they have
-- the required permissions. If set to @DENY@, only the root user of the
-- new account can access account billing information. For more
-- information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html#ControllingAccessWebsite-Activate Activating Access to the Billing and Cost Management Console>
-- in the /Amazon Web Services Billing and Cost Management User Guide./
--
-- If you don\'t specify this parameter, the value defaults to @ALLOW@, and
-- IAM users and roles with the required permissions can access billing
-- information for the new account.
createGovCloudAccount_iamUserAccessToBilling :: Lens.Lens' CreateGovCloudAccount (Prelude.Maybe IAMUserAccessToBilling)
createGovCloudAccount_iamUserAccessToBilling = Lens.lens (\CreateGovCloudAccount' {iamUserAccessToBilling} -> iamUserAccessToBilling) (\s@CreateGovCloudAccount' {} a -> s {iamUserAccessToBilling = a} :: CreateGovCloudAccount)

-- | (Optional)
--
-- The name of an IAM role that Organizations automatically preconfigures
-- in the new member accounts in both the Amazon Web Services GovCloud (US)
-- Region and in the commercial Region. This role trusts the management
-- account, allowing users in the management account to assume the role, as
-- permitted by the management account administrator. The role has
-- administrator permissions in the new member account.
--
-- If you don\'t specify this parameter, the role name defaults to
-- @OrganizationAccountAccessRole@.
--
-- For more information about how to use this role to access the member
-- account, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_access.html#orgs_manage_accounts_create-cross-account-role Accessing and Administering the Member Accounts in Your Organization>
-- in the /Organizations User Guide/ and steps 2 and 3 in
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/tutorial_cross-account-with-roles.html Tutorial: Delegate Access Across Amazon Web Services accounts Using IAM Roles>
-- in the /IAM User Guide./
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter. The pattern can include uppercase letters,
-- lowercase letters, digits with no spaces, and any of the following
-- characters: =,.\@-
createGovCloudAccount_roleName :: Lens.Lens' CreateGovCloudAccount (Prelude.Maybe Prelude.Text)
createGovCloudAccount_roleName = Lens.lens (\CreateGovCloudAccount' {roleName} -> roleName) (\s@CreateGovCloudAccount' {} a -> s {roleName = a} :: CreateGovCloudAccount)

-- | A list of tags that you want to attach to the newly created account.
-- These tags are attached to the commercial account associated with the
-- GovCloud account, and not to the GovCloud account itself. To add tags to
-- the actual GovCloud account, call the TagResource operation in the
-- GovCloud region after the new GovCloud account exists.
--
-- For each tag in the list, you must specify both a tag key and a value.
-- You can set the value to an empty string, but you can\'t set it to
-- @null@. For more information about tagging, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging Organizations resources>
-- in the Organizations User Guide.
--
-- If any one of the tags is invalid or if you exceed the maximum allowed
-- number of tags for an account, then the entire request fails and the
-- account is not created.
createGovCloudAccount_tags :: Lens.Lens' CreateGovCloudAccount (Prelude.Maybe [Tag])
createGovCloudAccount_tags = Lens.lens (\CreateGovCloudAccount' {tags} -> tags) (\s@CreateGovCloudAccount' {} a -> s {tags = a} :: CreateGovCloudAccount) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the email address of the owner to assign to the new member
-- account in the commercial Region. This email address must not already be
-- associated with another Amazon Web Services account. You must use a
-- valid email address to complete account creation.
--
-- The rules for a valid email address:
--
-- -   The address must be a minimum of 6 and a maximum of 64 characters
--     long.
--
-- -   All characters must be 7-bit ASCII characters.
--
-- -   There must be one and only one \@ symbol, which separates the local
--     name from the domain name.
--
-- -   The local name can\'t contain any of the following characters:
--
--     whitespace, \" \' ( ) \< > [ ] : ; , \\ | % &
--
-- -   The local name can\'t begin with a dot (.)
--
-- -   The domain name can consist of only the characters
--     [a-z],[A-Z],[0-9], hyphen (-), or dot (.)
--
-- -   The domain name can\'t begin or end with a hyphen (-) or dot (.)
--
-- -   The domain name must contain at least one dot
--
-- You can\'t access the root user of the account or remove an account that
-- was created with an invalid email address. Like all request parameters
-- for @CreateGovCloudAccount@, the request for the email address for the
-- Amazon Web Services GovCloud (US) account originates from the commercial
-- Region, not from the Amazon Web Services GovCloud (US) Region.
createGovCloudAccount_email :: Lens.Lens' CreateGovCloudAccount Prelude.Text
createGovCloudAccount_email = Lens.lens (\CreateGovCloudAccount' {email} -> email) (\s@CreateGovCloudAccount' {} a -> s {email = a} :: CreateGovCloudAccount) Prelude.. Data._Sensitive

-- | The friendly name of the member account.
--
-- The account name can consist of only the characters [a-z],[A-Z],[0-9],
-- hyphen (-), or dot (.) You can\'t separate characters with a dash (–).
createGovCloudAccount_accountName :: Lens.Lens' CreateGovCloudAccount Prelude.Text
createGovCloudAccount_accountName = Lens.lens (\CreateGovCloudAccount' {accountName} -> accountName) (\s@CreateGovCloudAccount' {} a -> s {accountName = a} :: CreateGovCloudAccount) Prelude.. Data._Sensitive

instance Core.AWSRequest CreateGovCloudAccount where
  type
    AWSResponse CreateGovCloudAccount =
      CreateGovCloudAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGovCloudAccountResponse'
            Prelude.<$> (x Data..?> "CreateAccountStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGovCloudAccount where
  hashWithSalt _salt CreateGovCloudAccount' {..} =
    _salt
      `Prelude.hashWithSalt` iamUserAccessToBilling
      `Prelude.hashWithSalt` roleName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` accountName

instance Prelude.NFData CreateGovCloudAccount where
  rnf CreateGovCloudAccount' {..} =
    Prelude.rnf iamUserAccessToBilling
      `Prelude.seq` Prelude.rnf roleName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf accountName

instance Data.ToHeaders CreateGovCloudAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.CreateGovCloudAccount" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateGovCloudAccount where
  toJSON CreateGovCloudAccount' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IamUserAccessToBilling" Data..=)
              Prelude.<$> iamUserAccessToBilling,
            ("RoleName" Data..=) Prelude.<$> roleName,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Email" Data..= email),
            Prelude.Just ("AccountName" Data..= accountName)
          ]
      )

instance Data.ToPath CreateGovCloudAccount where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateGovCloudAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGovCloudAccountResponse' smart constructor.
data CreateGovCloudAccountResponse = CreateGovCloudAccountResponse'
  { createAccountStatus :: Prelude.Maybe CreateAccountStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGovCloudAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createAccountStatus', 'createGovCloudAccountResponse_createAccountStatus' - Undocumented member.
--
-- 'httpStatus', 'createGovCloudAccountResponse_httpStatus' - The response's http status code.
newCreateGovCloudAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateGovCloudAccountResponse
newCreateGovCloudAccountResponse pHttpStatus_ =
  CreateGovCloudAccountResponse'
    { createAccountStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createGovCloudAccountResponse_createAccountStatus :: Lens.Lens' CreateGovCloudAccountResponse (Prelude.Maybe CreateAccountStatus)
createGovCloudAccountResponse_createAccountStatus = Lens.lens (\CreateGovCloudAccountResponse' {createAccountStatus} -> createAccountStatus) (\s@CreateGovCloudAccountResponse' {} a -> s {createAccountStatus = a} :: CreateGovCloudAccountResponse)

-- | The response's http status code.
createGovCloudAccountResponse_httpStatus :: Lens.Lens' CreateGovCloudAccountResponse Prelude.Int
createGovCloudAccountResponse_httpStatus = Lens.lens (\CreateGovCloudAccountResponse' {httpStatus} -> httpStatus) (\s@CreateGovCloudAccountResponse' {} a -> s {httpStatus = a} :: CreateGovCloudAccountResponse)

instance Prelude.NFData CreateGovCloudAccountResponse where
  rnf CreateGovCloudAccountResponse' {..} =
    Prelude.rnf createAccountStatus
      `Prelude.seq` Prelude.rnf httpStatus
