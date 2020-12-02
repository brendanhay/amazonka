{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
--     * Check the AWS CloudTrail log for the @CreateAccountResult@ event. For information on using AWS CloudTrail with AWS Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_monitoring.html Monitoring the Activity in Your Organization> in the /AWS Organizations User Guide./
--
--
--
-- The user who calls the API to create an account must have the @organizations:CreateAccount@ permission. If you enabled all features in the organization, AWS Organizations creates the required service-linked role named @AWSServiceRoleForOrganizations@ . For more information, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html#orgs_integrate_services-using_slrs AWS Organizations and Service-Linked Roles> in the /AWS Organizations User Guide/ .
--
-- If the request includes tags, then the requester must have the @organizations:TagResource@ permission.
--
-- AWS Organizations preconfigures the new member account with a role (named @OrganizationAccountAccessRole@ by default) that grants users in the management account administrator permissions in the new member account. Principals in the management account can assume the role. AWS Organizations clones the company name and address information for the new account from the organization's management account.
--
-- This operation can be called only from the organization's management account.
--
-- For more information about creating accounts, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_create.html Creating an AWS Account in Your Organization> in the /AWS Organizations User Guide./
--
-- /Important:/     * When you create an account in an organization using the AWS Organizations console, API, or CLI commands, the information required for the account to operate as a standalone account, such as a payment method and signing the end user license agreement (EULA) is /not/ automatically collected. If you must remove an account from your organization later, you can do so only after you provide the missing information. Follow the steps at <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info To leave an organization as a member account> in the /AWS Organizations User Guide/ .
--
--     * If you get an exception that indicates that you exceeded your account limits for the organization, contact <https://console.aws.amazon.com/support/home#/ AWS Support> .
--
--     * If you get an exception that indicates that the operation failed because your organization is still initializing, wait one hour and then try again. If the error persists, contact <https://console.aws.amazon.com/support/home#/ AWS Support> .
--
--     * Using @CreateAccount@ to create multiple temporary accounts isn't recommended. You can only close an account from the Billing and Cost Management Console, and you must be signed in as the root user. For information on the requirements and process for closing an account, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_close.html Closing an AWS Account> in the /AWS Organizations User Guide/ .
module Network.AWS.Organizations.CreateAccount
  ( -- * Creating a Request
    createAccount,
    CreateAccount,

    -- * Request Lenses
    caIAMUserAccessToBilling,
    caRoleName,
    caTags,
    caEmail,
    caAccountName,

    -- * Destructuring the Response
    createAccountResponse,
    CreateAccountResponse,

    -- * Response Lenses
    carsCreateAccountStatus,
    carsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createAccount' smart constructor.
data CreateAccount = CreateAccount'
  { _caIAMUserAccessToBilling ::
      !(Maybe IAMUserAccessToBilling),
    _caRoleName :: !(Maybe Text),
    _caTags :: !(Maybe [Tag]),
    _caEmail :: !(Sensitive Text),
    _caAccountName :: !(Sensitive Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caIAMUserAccessToBilling' - If set to @ALLOW@ , the new account enables IAM users to access account billing information /if/ they have the required permissions. If set to @DENY@ , only the root user of the new account can access account billing information. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html#ControllingAccessWebsite-Activate Activating Access to the Billing and Cost Management Console> in the /AWS Billing and Cost Management User Guide/ . If you don't specify this parameter, the value defaults to @ALLOW@ , and IAM users and roles with the required permissions can access billing information for the new account.
--
-- * 'caRoleName' - (Optional) The name of an IAM role that AWS Organizations automatically preconfigures in the new member account. This role trusts the management account, allowing users in the management account to assume the role, as permitted by the management account administrator. The role has administrator permissions in the new member account. If you don't specify this parameter, the role name defaults to @OrganizationAccountAccessRole@ . For more information about how to use this role to access the member account, see the following links:     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_access.html#orgs_manage_accounts_create-cross-account-role Accessing and Administering the Member Accounts in Your Organization> in the /AWS Organizations User Guide/      * Steps 2 and 3 in <https://docs.aws.amazon.com/IAM/latest/UserGuide/tutorial_cross-account-with-roles.html Tutorial: Delegate Access Across AWS Accounts Using IAM Roles> in the /IAM User Guide/  The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter. The pattern can include uppercase letters, lowercase letters, digits with no spaces, and any of the following characters: =,.@-
--
-- * 'caTags' - A list of tags that you want to attach to the newly created account. For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
--
-- * 'caEmail' - The email address of the owner to assign to the new member account. This email address must not already be associated with another AWS account. You must use a valid email address to complete account creation. You can't access the root user of the account or remove an account that was created with an invalid email address.
--
-- * 'caAccountName' - The friendly name of the member account.
createAccount ::
  -- | 'caEmail'
  Text ->
  -- | 'caAccountName'
  Text ->
  CreateAccount
createAccount pEmail_ pAccountName_ =
  CreateAccount'
    { _caIAMUserAccessToBilling = Nothing,
      _caRoleName = Nothing,
      _caTags = Nothing,
      _caEmail = _Sensitive # pEmail_,
      _caAccountName = _Sensitive # pAccountName_
    }

-- | If set to @ALLOW@ , the new account enables IAM users to access account billing information /if/ they have the required permissions. If set to @DENY@ , only the root user of the new account can access account billing information. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html#ControllingAccessWebsite-Activate Activating Access to the Billing and Cost Management Console> in the /AWS Billing and Cost Management User Guide/ . If you don't specify this parameter, the value defaults to @ALLOW@ , and IAM users and roles with the required permissions can access billing information for the new account.
caIAMUserAccessToBilling :: Lens' CreateAccount (Maybe IAMUserAccessToBilling)
caIAMUserAccessToBilling = lens _caIAMUserAccessToBilling (\s a -> s {_caIAMUserAccessToBilling = a})

-- | (Optional) The name of an IAM role that AWS Organizations automatically preconfigures in the new member account. This role trusts the management account, allowing users in the management account to assume the role, as permitted by the management account administrator. The role has administrator permissions in the new member account. If you don't specify this parameter, the role name defaults to @OrganizationAccountAccessRole@ . For more information about how to use this role to access the member account, see the following links:     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_access.html#orgs_manage_accounts_create-cross-account-role Accessing and Administering the Member Accounts in Your Organization> in the /AWS Organizations User Guide/      * Steps 2 and 3 in <https://docs.aws.amazon.com/IAM/latest/UserGuide/tutorial_cross-account-with-roles.html Tutorial: Delegate Access Across AWS Accounts Using IAM Roles> in the /IAM User Guide/  The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter. The pattern can include uppercase letters, lowercase letters, digits with no spaces, and any of the following characters: =,.@-
caRoleName :: Lens' CreateAccount (Maybe Text)
caRoleName = lens _caRoleName (\s a -> s {_caRoleName = a})

-- | A list of tags that you want to attach to the newly created account. For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
caTags :: Lens' CreateAccount [Tag]
caTags = lens _caTags (\s a -> s {_caTags = a}) . _Default . _Coerce

-- | The email address of the owner to assign to the new member account. This email address must not already be associated with another AWS account. You must use a valid email address to complete account creation. You can't access the root user of the account or remove an account that was created with an invalid email address.
caEmail :: Lens' CreateAccount Text
caEmail = lens _caEmail (\s a -> s {_caEmail = a}) . _Sensitive

-- | The friendly name of the member account.
caAccountName :: Lens' CreateAccount Text
caAccountName = lens _caAccountName (\s a -> s {_caAccountName = a}) . _Sensitive

instance AWSRequest CreateAccount where
  type Rs CreateAccount = CreateAccountResponse
  request = postJSON organizations
  response =
    receiveJSON
      ( \s h x ->
          CreateAccountResponse'
            <$> (x .?> "CreateAccountStatus") <*> (pure (fromEnum s))
      )

instance Hashable CreateAccount

instance NFData CreateAccount

instance ToHeaders CreateAccount where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSOrganizationsV20161128.CreateAccount" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateAccount where
  toJSON CreateAccount' {..} =
    object
      ( catMaybes
          [ ("IamUserAccessToBilling" .=) <$> _caIAMUserAccessToBilling,
            ("RoleName" .=) <$> _caRoleName,
            ("Tags" .=) <$> _caTags,
            Just ("Email" .= _caEmail),
            Just ("AccountName" .= _caAccountName)
          ]
      )

instance ToPath CreateAccount where
  toPath = const "/"

instance ToQuery CreateAccount where
  toQuery = const mempty

-- | /See:/ 'createAccountResponse' smart constructor.
data CreateAccountResponse = CreateAccountResponse'
  { _carsCreateAccountStatus ::
      !(Maybe CreateAccountStatus),
    _carsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateAccountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'carsCreateAccountStatus' - A structure that contains details about the request to create an account. This response structure might not be fully populated when you first receive it because account creation is an asynchronous process. You can pass the returned @CreateAccountStatus@ ID as a parameter to 'DescribeCreateAccountStatus' to get status about the progress of the request at later times. You can also check the AWS CloudTrail log for the @CreateAccountResult@ event. For more information, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_monitoring.html Monitoring the Activity in Your Organization> in the /AWS Organizations User Guide/ .
--
-- * 'carsResponseStatus' - -- | The response status code.
createAccountResponse ::
  -- | 'carsResponseStatus'
  Int ->
  CreateAccountResponse
createAccountResponse pResponseStatus_ =
  CreateAccountResponse'
    { _carsCreateAccountStatus = Nothing,
      _carsResponseStatus = pResponseStatus_
    }

-- | A structure that contains details about the request to create an account. This response structure might not be fully populated when you first receive it because account creation is an asynchronous process. You can pass the returned @CreateAccountStatus@ ID as a parameter to 'DescribeCreateAccountStatus' to get status about the progress of the request at later times. You can also check the AWS CloudTrail log for the @CreateAccountResult@ event. For more information, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_monitoring.html Monitoring the Activity in Your Organization> in the /AWS Organizations User Guide/ .
carsCreateAccountStatus :: Lens' CreateAccountResponse (Maybe CreateAccountStatus)
carsCreateAccountStatus = lens _carsCreateAccountStatus (\s a -> s {_carsCreateAccountStatus = a})

-- | -- | The response status code.
carsResponseStatus :: Lens' CreateAccountResponse Int
carsResponseStatus = lens _carsResponseStatus (\s a -> s {_carsResponseStatus = a})

instance NFData CreateAccountResponse
