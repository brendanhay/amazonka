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
-- Module      : Network.AWS.Organizations.AcceptHandshake
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a response to the originator of a handshake agreeing to the action proposed by the handshake request.
--
--
-- This operation can be called only by the following principals when they also have the relevant IAM permissions:
--
--     * __Invitation to join__ or __Approve all features request__ handshakes: only a principal from the member account.
--
-- The user who calls the API for an invitation to join must have the @organizations:AcceptHandshake@ permission. If you enabled all features in the organization, then the user must also have the @iam:CreateServiceLinkedRole@ permission so that Organizations can create the required service-linked role named /OrgsServiceLinkedRoleName/ . For more information, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integration_services.html#orgs_integration_service-linked-roles AWS Organizations and Service-Linked Roles> in the /AWS Organizations User Guide/ .
--
--     * __Enable all features final confirmation__ handshake: only a principal from the master account.
--
-- For more information about invitations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_invites.html Inviting an AWS Account to Join Your Organization> in the /AWS Organizations User Guide/ . For more information about requests to enable all features in the organization, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization> in the /AWS Organizations User Guide/ .
--
--
--
-- After you accept a handshake, it continues to appear in the results of relevant APIs for only 30 days. After that it is deleted.
--
module Network.AWS.Organizations.AcceptHandshake
    (
    -- * Creating a Request
      acceptHandshake
    , AcceptHandshake
    -- * Request Lenses
    , ahHandshakeId

    -- * Destructuring the Response
    , acceptHandshakeResponse
    , AcceptHandshakeResponse
    -- * Response Lenses
    , ahrsHandshake
    , ahrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'acceptHandshake' smart constructor.
newtype AcceptHandshake = AcceptHandshake'
  { _ahHandshakeId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcceptHandshake' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ahHandshakeId' - The unique identifier (ID) of the handshake that you want to accept. The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lower-case letters or digits.
acceptHandshake
    :: Text -- ^ 'ahHandshakeId'
    -> AcceptHandshake
acceptHandshake pHandshakeId_ =
  AcceptHandshake' {_ahHandshakeId = pHandshakeId_}


-- | The unique identifier (ID) of the handshake that you want to accept. The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lower-case letters or digits.
ahHandshakeId :: Lens' AcceptHandshake Text
ahHandshakeId = lens _ahHandshakeId (\ s a -> s{_ahHandshakeId = a})

instance AWSRequest AcceptHandshake where
        type Rs AcceptHandshake = AcceptHandshakeResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 AcceptHandshakeResponse' <$>
                   (x .?> "Handshake") <*> (pure (fromEnum s)))

instance Hashable AcceptHandshake where

instance NFData AcceptHandshake where

instance ToHeaders AcceptHandshake where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.AcceptHandshake" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AcceptHandshake where
        toJSON AcceptHandshake'{..}
          = object
              (catMaybes [Just ("HandshakeId" .= _ahHandshakeId)])

instance ToPath AcceptHandshake where
        toPath = const "/"

instance ToQuery AcceptHandshake where
        toQuery = const mempty

-- | /See:/ 'acceptHandshakeResponse' smart constructor.
data AcceptHandshakeResponse = AcceptHandshakeResponse'
  { _ahrsHandshake      :: !(Maybe Handshake)
  , _ahrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcceptHandshakeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ahrsHandshake' - A structure that contains details about the accepted handshake.
--
-- * 'ahrsResponseStatus' - -- | The response status code.
acceptHandshakeResponse
    :: Int -- ^ 'ahrsResponseStatus'
    -> AcceptHandshakeResponse
acceptHandshakeResponse pResponseStatus_ =
  AcceptHandshakeResponse'
    {_ahrsHandshake = Nothing, _ahrsResponseStatus = pResponseStatus_}


-- | A structure that contains details about the accepted handshake.
ahrsHandshake :: Lens' AcceptHandshakeResponse (Maybe Handshake)
ahrsHandshake = lens _ahrsHandshake (\ s a -> s{_ahrsHandshake = a})

-- | -- | The response status code.
ahrsResponseStatus :: Lens' AcceptHandshakeResponse Int
ahrsResponseStatus = lens _ahrsResponseStatus (\ s a -> s{_ahrsResponseStatus = a})

instance NFData AcceptHandshakeResponse where
