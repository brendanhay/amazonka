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
-- Module      : Network.AWS.GuardDuty.DeleteInvitations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes invitations sent to the current member account by AWS accounts specified by their account IDs.
module Network.AWS.GuardDuty.DeleteInvitations
    (
    -- * Creating a Request
      deleteInvitations
    , DeleteInvitations
    -- * Request Lenses
    , diAccountIds

    -- * Destructuring the Response
    , deleteInvitationsResponse
    , DeleteInvitationsResponse
    -- * Response Lenses
    , dirsUnprocessedAccounts
    , dirsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | DeleteInvitations request body.
--
-- /See:/ 'deleteInvitations' smart constructor.
newtype DeleteInvitations = DeleteInvitations'
  { _diAccountIds :: Maybe [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteInvitations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diAccountIds' - A list of account IDs of the AWS accounts that sent invitations to the current member account that you want to delete invitations from.
deleteInvitations
    :: DeleteInvitations
deleteInvitations = DeleteInvitations' {_diAccountIds = Nothing}


-- | A list of account IDs of the AWS accounts that sent invitations to the current member account that you want to delete invitations from.
diAccountIds :: Lens' DeleteInvitations [Text]
diAccountIds = lens _diAccountIds (\ s a -> s{_diAccountIds = a}) . _Default . _Coerce

instance AWSRequest DeleteInvitations where
        type Rs DeleteInvitations = DeleteInvitationsResponse
        request = postJSON guardDuty
        response
          = receiveJSON
              (\ s h x ->
                 DeleteInvitationsResponse' <$>
                   (x .?> "unprocessedAccounts" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DeleteInvitations where

instance NFData DeleteInvitations where

instance ToHeaders DeleteInvitations where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteInvitations where
        toJSON DeleteInvitations'{..}
          = object
              (catMaybes [("accountIds" .=) <$> _diAccountIds])

instance ToPath DeleteInvitations where
        toPath = const "/invitation/delete"

instance ToQuery DeleteInvitations where
        toQuery = const mempty

-- | /See:/ 'deleteInvitationsResponse' smart constructor.
data DeleteInvitationsResponse = DeleteInvitationsResponse'
  { _dirsUnprocessedAccounts :: !(Maybe [UnprocessedAccount])
  , _dirsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteInvitationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirsUnprocessedAccounts' - A list of objects containing the unprocessed account and a result string explaining why it was unprocessed.
--
-- * 'dirsResponseStatus' - -- | The response status code.
deleteInvitationsResponse
    :: Int -- ^ 'dirsResponseStatus'
    -> DeleteInvitationsResponse
deleteInvitationsResponse pResponseStatus_ =
  DeleteInvitationsResponse'
    {_dirsUnprocessedAccounts = Nothing, _dirsResponseStatus = pResponseStatus_}


-- | A list of objects containing the unprocessed account and a result string explaining why it was unprocessed.
dirsUnprocessedAccounts :: Lens' DeleteInvitationsResponse [UnprocessedAccount]
dirsUnprocessedAccounts = lens _dirsUnprocessedAccounts (\ s a -> s{_dirsUnprocessedAccounts = a}) . _Default . _Coerce

-- | -- | The response status code.
dirsResponseStatus :: Lens' DeleteInvitationsResponse Int
dirsResponseStatus = lens _dirsResponseStatus (\ s a -> s{_dirsResponseStatus = a})

instance NFData DeleteInvitationsResponse where
