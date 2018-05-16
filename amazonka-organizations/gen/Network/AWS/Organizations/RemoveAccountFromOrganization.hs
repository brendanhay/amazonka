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
-- Module      : Network.AWS.Organizations.RemoveAccountFromOrganization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified account from the organization.
--
--
-- The removed account becomes a stand-alone account that is not a member of any organization. It is no longer subject to any policies and is responsible for its own bill payments. The organization's master account is no longer charged for any expenses accrued by the member account after it is removed from the organization.
--
-- This operation can be called only from the organization's master account. Member accounts can remove themselves with 'LeaveOrganization' instead.
--
-- /Important:/     * You can remove an account from your organization only if the account is configured with the information required to operate as a standalone account. When you create an account in an organization using the AWS Organizations console, API, or CLI commands, the information required of standalone accounts is /not/ automatically collected. For an account that you want to make standalone, you must accept the End User License Agreement (EULA), choose a support plan, provide and verify the required contact information, and provide a current payment method. AWS uses the payment method to charge for any billable (not free tier) AWS activity that occurs while the account is not attached to an organization. To remove an account that does not yet have this information, you must sign in as the member account and follow the steps at <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info To leave an organization when all required account information has not yet been provided> in the /AWS Organizations User Guide/ .
--
--     * You can remove a member account only after you enable IAM user access to billing in the member account. For more information, see <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html#ControllingAccessWebsite-Activate Activating Access to the Billing and Cost Management Console> in the /AWS Billing and Cost Management User Guide/ .
--
--
--
module Network.AWS.Organizations.RemoveAccountFromOrganization
    (
    -- * Creating a Request
      removeAccountFromOrganization
    , RemoveAccountFromOrganization
    -- * Request Lenses
    , rafoAccountId

    -- * Destructuring the Response
    , removeAccountFromOrganizationResponse
    , RemoveAccountFromOrganizationResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removeAccountFromOrganization' smart constructor.
newtype RemoveAccountFromOrganization = RemoveAccountFromOrganization'
  { _rafoAccountId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveAccountFromOrganization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rafoAccountId' - The unique identifier (ID) of the member account that you want to remove from the organization. The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
removeAccountFromOrganization
    :: Text -- ^ 'rafoAccountId'
    -> RemoveAccountFromOrganization
removeAccountFromOrganization pAccountId_ =
  RemoveAccountFromOrganization' {_rafoAccountId = pAccountId_}


-- | The unique identifier (ID) of the member account that you want to remove from the organization. The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
rafoAccountId :: Lens' RemoveAccountFromOrganization Text
rafoAccountId = lens _rafoAccountId (\ s a -> s{_rafoAccountId = a})

instance AWSRequest RemoveAccountFromOrganization
         where
        type Rs RemoveAccountFromOrganization =
             RemoveAccountFromOrganizationResponse
        request = postJSON organizations
        response
          = receiveNull RemoveAccountFromOrganizationResponse'

instance Hashable RemoveAccountFromOrganization where

instance NFData RemoveAccountFromOrganization where

instance ToHeaders RemoveAccountFromOrganization
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.RemoveAccountFromOrganization"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RemoveAccountFromOrganization where
        toJSON RemoveAccountFromOrganization'{..}
          = object
              (catMaybes [Just ("AccountId" .= _rafoAccountId)])

instance ToPath RemoveAccountFromOrganization where
        toPath = const "/"

instance ToQuery RemoveAccountFromOrganization where
        toQuery = const mempty

-- | /See:/ 'removeAccountFromOrganizationResponse' smart constructor.
data RemoveAccountFromOrganizationResponse =
  RemoveAccountFromOrganizationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveAccountFromOrganizationResponse' with the minimum fields required to make a request.
--
removeAccountFromOrganizationResponse
    :: RemoveAccountFromOrganizationResponse
removeAccountFromOrganizationResponse = RemoveAccountFromOrganizationResponse'


instance NFData RemoveAccountFromOrganizationResponse
         where
