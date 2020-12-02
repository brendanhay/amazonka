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
-- Module      : Network.AWS.Organizations.LeaveOrganization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a member account from its parent organization. This version of the operation is performed by the account that wants to leave. To remove a member account as a user in the master account, use 'RemoveAccountFromOrganization' instead.
--
--
-- This operation can be called only from a member account in the organization.
--
-- /Important:/     * The master account in an organization with all features enabled can set service control policies (SCPs) that can restrict what administrators of member accounts can do, including preventing them from successfully calling @LeaveOrganization@ and leaving the organization.
--
--     * You can leave an organization as a member account only if the account is configured with the information required to operate as a standalone account. When you create an account in an organization using the AWS Organizations console, API, or CLI commands, the information required of standalone accounts is /not/ automatically collected. For each account that you want to make standalone, you must accept the End User License Agreement (EULA), choose a support plan, provide and verify the required contact information, and provide a current payment method. AWS uses the payment method to charge for any billable (not free tier) AWS activity that occurs while the account is not attached to an organization. Follow the steps at <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info To leave an organization when all required account information has not yet been provided> in the /AWS Organizations User Guide/ .
--
--     * You can leave an organization only after you enable IAM user access to billing in your account. For more information, see <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html#ControllingAccessWebsite-Activate Activating Access to the Billing and Cost Management Console> in the /AWS Billing and Cost Management User Guide/ .
--
--
--
module Network.AWS.Organizations.LeaveOrganization
    (
    -- * Creating a Request
      leaveOrganization
    , LeaveOrganization

    -- * Destructuring the Response
    , leaveOrganizationResponse
    , LeaveOrganizationResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'leaveOrganization' smart constructor.
data LeaveOrganization =
  LeaveOrganization'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LeaveOrganization' with the minimum fields required to make a request.
--
leaveOrganization
    :: LeaveOrganization
leaveOrganization = LeaveOrganization'


instance AWSRequest LeaveOrganization where
        type Rs LeaveOrganization = LeaveOrganizationResponse
        request = postJSON organizations
        response = receiveNull LeaveOrganizationResponse'

instance Hashable LeaveOrganization where

instance NFData LeaveOrganization where

instance ToHeaders LeaveOrganization where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.LeaveOrganization" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON LeaveOrganization where
        toJSON = const (Object mempty)

instance ToPath LeaveOrganization where
        toPath = const "/"

instance ToQuery LeaveOrganization where
        toQuery = const mempty

-- | /See:/ 'leaveOrganizationResponse' smart constructor.
data LeaveOrganizationResponse =
  LeaveOrganizationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LeaveOrganizationResponse' with the minimum fields required to make a request.
--
leaveOrganizationResponse
    :: LeaveOrganizationResponse
leaveOrganizationResponse = LeaveOrganizationResponse'


instance NFData LeaveOrganizationResponse where
