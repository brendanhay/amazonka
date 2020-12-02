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
-- Module      : Network.AWS.FMS.AssociateAdminAccount
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the AWS Firewall Manager administrator account. AWS Firewall Manager must be associated with a master account in AWS Organizations or associated with a member account that has the appropriate permissions. If the account ID that you submit is not an AWS Organizations master account, AWS Firewall Manager will set the appropriate permissions for the given member account.
--
--
-- The account that you associate with AWS Firewall Manager is called the AWS Firewall manager administrator account.
--
module Network.AWS.FMS.AssociateAdminAccount
    (
    -- * Creating a Request
      associateAdminAccount
    , AssociateAdminAccount
    -- * Request Lenses
    , aaaAdminAccount

    -- * Destructuring the Response
    , associateAdminAccountResponse
    , AssociateAdminAccountResponse
    ) where

import Network.AWS.FMS.Types
import Network.AWS.FMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateAdminAccount' smart constructor.
newtype AssociateAdminAccount = AssociateAdminAccount'
  { _aaaAdminAccount :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateAdminAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaaAdminAccount' - The AWS account ID to associate with AWS Firewall Manager as the AWS Firewall Manager administrator account. This can be an AWS Organizations master account or a member account. For more information about AWS Organizations and master accounts, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts.html Managing the AWS Accounts in Your Organization> .
associateAdminAccount
    :: Text -- ^ 'aaaAdminAccount'
    -> AssociateAdminAccount
associateAdminAccount pAdminAccount_ =
  AssociateAdminAccount' {_aaaAdminAccount = pAdminAccount_}


-- | The AWS account ID to associate with AWS Firewall Manager as the AWS Firewall Manager administrator account. This can be an AWS Organizations master account or a member account. For more information about AWS Organizations and master accounts, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts.html Managing the AWS Accounts in Your Organization> .
aaaAdminAccount :: Lens' AssociateAdminAccount Text
aaaAdminAccount = lens _aaaAdminAccount (\ s a -> s{_aaaAdminAccount = a})

instance AWSRequest AssociateAdminAccount where
        type Rs AssociateAdminAccount =
             AssociateAdminAccountResponse
        request = postJSON fms
        response = receiveNull AssociateAdminAccountResponse'

instance Hashable AssociateAdminAccount where

instance NFData AssociateAdminAccount where

instance ToHeaders AssociateAdminAccount where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSFMS_20180101.AssociateAdminAccount" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssociateAdminAccount where
        toJSON AssociateAdminAccount'{..}
          = object
              (catMaybes
                 [Just ("AdminAccount" .= _aaaAdminAccount)])

instance ToPath AssociateAdminAccount where
        toPath = const "/"

instance ToQuery AssociateAdminAccount where
        toQuery = const mempty

-- | /See:/ 'associateAdminAccountResponse' smart constructor.
data AssociateAdminAccountResponse =
  AssociateAdminAccountResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateAdminAccountResponse' with the minimum fields required to make a request.
--
associateAdminAccountResponse
    :: AssociateAdminAccountResponse
associateAdminAccountResponse = AssociateAdminAccountResponse'


instance NFData AssociateAdminAccountResponse where
