{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.CreateAccount
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS account that is automatically a member of the organization whose credentials made the request. This is an asynchronous request that AWS performs in the background. If you want to check the status of the request later, you need the @OperationId@ response element from this operation to provide as a parameter to the 'DescribeCreateAccountStatus' operation.
--
--
-- AWS Organizations preconfigures the new member account with a role (named @OrganizationAccountAccessRole@ by default) that grants administrator permissions to the new account. Principals in the master account can assume the role. AWS Organizations clones the company name and address information for the new account from the organization's master account.
--
-- For more information about creating accounts, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_create.html Creating an AWS Account in Your Organization> in the /AWS Organizations User Guide/ .
--
-- /Important:/ You cannot remove accounts that are created with this operation from an organization. That also means that you cannot delete an organization that contains an account that is created with this operation.
--
-- This operation can be called only from the organization's master account.
--
module Network.AWS.Organizations.CreateAccount
    (
    -- * Creating a Request
      createAccount
    , CreateAccount
    -- * Request Lenses
    , caIAMUserAccessToBilling
    , caRoleName
    , caEmail
    , caAccountName

    -- * Destructuring the Response
    , createAccountResponse
    , CreateAccountResponse
    -- * Response Lenses
    , carsCreateAccountStatus
    , carsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Organizations.Types
import           Network.AWS.Organizations.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createAccount' smart constructor.
data CreateAccount = CreateAccount'
    { _caIAMUserAccessToBilling :: !(Maybe IAMUserAccessToBilling)
    , _caRoleName               :: !(Maybe Text)
    , _caEmail                  :: !(Sensitive Text)
    , _caAccountName            :: !(Sensitive Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caIAMUserAccessToBilling' - If set to @ALLOW@ , the new account enables IAM users to access account billing information /if/ they have the required permissions. If set to @DENY@ , then only the root user of the new account can access account billing information. For more information, see <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html#ControllingAccessWebsite-Activate Activating Access to the Billing and Cost Management Console> in the /AWS Billing and Cost Management User Guide/ .
--
-- * 'caRoleName' - (Optional) The name of an IAM role that Organizations automatically preconfigures in the new member account. This role trusts the master account, allowing users in the master account to assume the role, as permitted by the master account administrator. The role has administrator permissions in the new member account. If you do not specify this parameter, the role name defaults to @OrganizationAccountAccessRole@ . For more information about how to use this role to access the member account, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_access.html#orgs_manage_accounts_create-cross-account-role Accessing and Administering the Member Accounts in Your Organization> in the /AWS Organizations User Guide/ , and steps 2 and 3 in <http://docs.aws.amazon.com/IAM/latest/UserGuide/tutorial_cross-account-with-roles.html Tutorial: Delegate Access Across AWS Accounts Using IAM Roles> in the /IAM User Guide/ . The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of characters that can consist of uppercase letters, lowercase letters, digits with no spaces, and any of the following characters: =,.@-
--
-- * 'caEmail' - The email address of the owner to assign to the new member account. This email address must not already be associated with another AWS account.
--
-- * 'caAccountName' - The friendly name of the member account.
createAccount
    :: Text -- ^ 'caEmail'
    -> Text -- ^ 'caAccountName'
    -> CreateAccount
createAccount pEmail_ pAccountName_ =
    CreateAccount'
    { _caIAMUserAccessToBilling = Nothing
    , _caRoleName = Nothing
    , _caEmail = _Sensitive # pEmail_
    , _caAccountName = _Sensitive # pAccountName_
    }

-- | If set to @ALLOW@ , the new account enables IAM users to access account billing information /if/ they have the required permissions. If set to @DENY@ , then only the root user of the new account can access account billing information. For more information, see <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html#ControllingAccessWebsite-Activate Activating Access to the Billing and Cost Management Console> in the /AWS Billing and Cost Management User Guide/ .
caIAMUserAccessToBilling :: Lens' CreateAccount (Maybe IAMUserAccessToBilling)
caIAMUserAccessToBilling = lens _caIAMUserAccessToBilling (\ s a -> s{_caIAMUserAccessToBilling = a});

-- | (Optional) The name of an IAM role that Organizations automatically preconfigures in the new member account. This role trusts the master account, allowing users in the master account to assume the role, as permitted by the master account administrator. The role has administrator permissions in the new member account. If you do not specify this parameter, the role name defaults to @OrganizationAccountAccessRole@ . For more information about how to use this role to access the member account, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_access.html#orgs_manage_accounts_create-cross-account-role Accessing and Administering the Member Accounts in Your Organization> in the /AWS Organizations User Guide/ , and steps 2 and 3 in <http://docs.aws.amazon.com/IAM/latest/UserGuide/tutorial_cross-account-with-roles.html Tutorial: Delegate Access Across AWS Accounts Using IAM Roles> in the /IAM User Guide/ . The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of characters that can consist of uppercase letters, lowercase letters, digits with no spaces, and any of the following characters: =,.@-
caRoleName :: Lens' CreateAccount (Maybe Text)
caRoleName = lens _caRoleName (\ s a -> s{_caRoleName = a});

-- | The email address of the owner to assign to the new member account. This email address must not already be associated with another AWS account.
caEmail :: Lens' CreateAccount Text
caEmail = lens _caEmail (\ s a -> s{_caEmail = a}) . _Sensitive;

-- | The friendly name of the member account.
caAccountName :: Lens' CreateAccount Text
caAccountName = lens _caAccountName (\ s a -> s{_caAccountName = a}) . _Sensitive;

instance AWSRequest CreateAccount where
        type Rs CreateAccount = CreateAccountResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 CreateAccountResponse' <$>
                   (x .?> "CreateAccountStatus") <*>
                     (pure (fromEnum s)))

instance Hashable CreateAccount

instance NFData CreateAccount

instance ToHeaders CreateAccount where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.CreateAccount" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateAccount where
        toJSON CreateAccount'{..}
          = object
              (catMaybes
                 [("IamUserAccessToBilling" .=) <$>
                    _caIAMUserAccessToBilling,
                  ("RoleName" .=) <$> _caRoleName,
                  Just ("Email" .= _caEmail),
                  Just ("AccountName" .= _caAccountName)])

instance ToPath CreateAccount where
        toPath = const "/"

instance ToQuery CreateAccount where
        toQuery = const mempty

-- | /See:/ 'createAccountResponse' smart constructor.
data CreateAccountResponse = CreateAccountResponse'
    { _carsCreateAccountStatus :: !(Maybe CreateAccountStatus)
    , _carsResponseStatus      :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateAccountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'carsCreateAccountStatus' - A structure that contains details about the request to create an account. This response structure might not be fully populated when you first receive it because account creation is an asynchronous process. You can pass the returned CreateAccountStatus ID as a parameter to @'DescribeCreateAccountStatus' @ to get status about the progress of the request at later times.
--
-- * 'carsResponseStatus' - -- | The response status code.
createAccountResponse
    :: Int -- ^ 'carsResponseStatus'
    -> CreateAccountResponse
createAccountResponse pResponseStatus_ =
    CreateAccountResponse'
    { _carsCreateAccountStatus = Nothing
    , _carsResponseStatus = pResponseStatus_
    }

-- | A structure that contains details about the request to create an account. This response structure might not be fully populated when you first receive it because account creation is an asynchronous process. You can pass the returned CreateAccountStatus ID as a parameter to @'DescribeCreateAccountStatus' @ to get status about the progress of the request at later times.
carsCreateAccountStatus :: Lens' CreateAccountResponse (Maybe CreateAccountStatus)
carsCreateAccountStatus = lens _carsCreateAccountStatus (\ s a -> s{_carsCreateAccountStatus = a});

-- | -- | The response status code.
carsResponseStatus :: Lens' CreateAccountResponse Int
carsResponseStatus = lens _carsResponseStatus (\ s a -> s{_carsResponseStatus = a});

instance NFData CreateAccountResponse
