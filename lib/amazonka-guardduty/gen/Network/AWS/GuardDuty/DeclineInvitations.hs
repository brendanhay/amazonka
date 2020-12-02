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
-- Module      : Network.AWS.GuardDuty.DeclineInvitations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Declines invitations sent to the current member account by AWS account specified by their account IDs.
module Network.AWS.GuardDuty.DeclineInvitations
    (
    -- * Creating a Request
      declineInvitations
    , DeclineInvitations
    -- * Request Lenses
    , dAccountIds

    -- * Destructuring the Response
    , declineInvitationsResponse
    , DeclineInvitationsResponse
    -- * Response Lenses
    , disrsUnprocessedAccounts
    , disrsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | DeclineInvitations request body.
--
-- /See:/ 'declineInvitations' smart constructor.
newtype DeclineInvitations = DeclineInvitations'
  { _dAccountIds :: Maybe [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeclineInvitations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAccountIds' - A list of account IDs of the AWS accounts that sent invitations to the current member account that you want to decline invitations from.
declineInvitations
    :: DeclineInvitations
declineInvitations = DeclineInvitations' {_dAccountIds = Nothing}


-- | A list of account IDs of the AWS accounts that sent invitations to the current member account that you want to decline invitations from.
dAccountIds :: Lens' DeclineInvitations [Text]
dAccountIds = lens _dAccountIds (\ s a -> s{_dAccountIds = a}) . _Default . _Coerce

instance AWSRequest DeclineInvitations where
        type Rs DeclineInvitations =
             DeclineInvitationsResponse
        request = postJSON guardDuty
        response
          = receiveJSON
              (\ s h x ->
                 DeclineInvitationsResponse' <$>
                   (x .?> "unprocessedAccounts" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DeclineInvitations where

instance NFData DeclineInvitations where

instance ToHeaders DeclineInvitations where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeclineInvitations where
        toJSON DeclineInvitations'{..}
          = object
              (catMaybes [("accountIds" .=) <$> _dAccountIds])

instance ToPath DeclineInvitations where
        toPath = const "/invitation/decline"

instance ToQuery DeclineInvitations where
        toQuery = const mempty

-- | /See:/ 'declineInvitationsResponse' smart constructor.
data DeclineInvitationsResponse = DeclineInvitationsResponse'
  { _disrsUnprocessedAccounts :: !(Maybe [UnprocessedAccount])
  , _disrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeclineInvitationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disrsUnprocessedAccounts' - A list of objects containing the unprocessed account and a result string explaining why it was unprocessed.
--
-- * 'disrsResponseStatus' - -- | The response status code.
declineInvitationsResponse
    :: Int -- ^ 'disrsResponseStatus'
    -> DeclineInvitationsResponse
declineInvitationsResponse pResponseStatus_ =
  DeclineInvitationsResponse'
    { _disrsUnprocessedAccounts = Nothing
    , _disrsResponseStatus = pResponseStatus_
    }


-- | A list of objects containing the unprocessed account and a result string explaining why it was unprocessed.
disrsUnprocessedAccounts :: Lens' DeclineInvitationsResponse [UnprocessedAccount]
disrsUnprocessedAccounts = lens _disrsUnprocessedAccounts (\ s a -> s{_disrsUnprocessedAccounts = a}) . _Default . _Coerce

-- | -- | The response status code.
disrsResponseStatus :: Lens' DeclineInvitationsResponse Int
disrsResponseStatus = lens _disrsResponseStatus (\ s a -> s{_disrsResponseStatus = a})

instance NFData DeclineInvitationsResponse where
