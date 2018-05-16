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
-- Module      : Network.AWS.GuardDuty.DeleteMembers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes GuardDuty member accounts (to the current GuardDuty master account) specified by the account IDs.
module Network.AWS.GuardDuty.DeleteMembers
    (
    -- * Creating a Request
      deleteMembers
    , DeleteMembers
    -- * Request Lenses
    , dmAccountIds
    , dmDetectorId

    -- * Destructuring the Response
    , deleteMembersResponse
    , DeleteMembersResponse
    -- * Response Lenses
    , drsUnprocessedAccounts
    , drsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | DeleteMembers request body.
--
-- /See:/ 'deleteMembers' smart constructor.
data DeleteMembers = DeleteMembers'
  { _dmAccountIds :: !(Maybe [Text])
  , _dmDetectorId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMembers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmAccountIds' - A list of account IDs of the GuardDuty member accounts that you want to delete.
--
-- * 'dmDetectorId' - The unique ID of the detector of the GuardDuty account whose members you want to delete.
deleteMembers
    :: Text -- ^ 'dmDetectorId'
    -> DeleteMembers
deleteMembers pDetectorId_ =
  DeleteMembers' {_dmAccountIds = Nothing, _dmDetectorId = pDetectorId_}


-- | A list of account IDs of the GuardDuty member accounts that you want to delete.
dmAccountIds :: Lens' DeleteMembers [Text]
dmAccountIds = lens _dmAccountIds (\ s a -> s{_dmAccountIds = a}) . _Default . _Coerce

-- | The unique ID of the detector of the GuardDuty account whose members you want to delete.
dmDetectorId :: Lens' DeleteMembers Text
dmDetectorId = lens _dmDetectorId (\ s a -> s{_dmDetectorId = a})

instance AWSRequest DeleteMembers where
        type Rs DeleteMembers = DeleteMembersResponse
        request = postJSON guardDuty
        response
          = receiveJSON
              (\ s h x ->
                 DeleteMembersResponse' <$>
                   (x .?> "unprocessedAccounts" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DeleteMembers where

instance NFData DeleteMembers where

instance ToHeaders DeleteMembers where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteMembers where
        toJSON DeleteMembers'{..}
          = object
              (catMaybes [("accountIds" .=) <$> _dmAccountIds])

instance ToPath DeleteMembers where
        toPath DeleteMembers'{..}
          = mconcat
              ["/detector/", toBS _dmDetectorId, "/member/delete"]

instance ToQuery DeleteMembers where
        toQuery = const mempty

-- | /See:/ 'deleteMembersResponse' smart constructor.
data DeleteMembersResponse = DeleteMembersResponse'
  { _drsUnprocessedAccounts :: !(Maybe [UnprocessedAccount])
  , _drsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMembersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsUnprocessedAccounts' - A list of objects containing the unprocessed account and a result string explaining why it was unprocessed.
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteMembersResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteMembersResponse
deleteMembersResponse pResponseStatus_ =
  DeleteMembersResponse'
    {_drsUnprocessedAccounts = Nothing, _drsResponseStatus = pResponseStatus_}


-- | A list of objects containing the unprocessed account and a result string explaining why it was unprocessed.
drsUnprocessedAccounts :: Lens' DeleteMembersResponse [UnprocessedAccount]
drsUnprocessedAccounts = lens _drsUnprocessedAccounts (\ s a -> s{_drsUnprocessedAccounts = a}) . _Default . _Coerce

-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteMembersResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteMembersResponse where
