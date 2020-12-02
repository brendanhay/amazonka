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
-- Module      : Network.AWS.GuardDuty.CreateMembers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates member accounts of the current AWS account by specifying a list of AWS account IDs. The current AWS account can then invite these members to manage GuardDuty in their accounts.
module Network.AWS.GuardDuty.CreateMembers
    (
    -- * Creating a Request
      createMembers
    , CreateMembers
    -- * Request Lenses
    , cmAccountDetails
    , cmDetectorId

    -- * Destructuring the Response
    , createMembersResponse
    , CreateMembersResponse
    -- * Response Lenses
    , cmrsUnprocessedAccounts
    , cmrsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | CreateMembers request body.
--
-- /See:/ 'createMembers' smart constructor.
data CreateMembers = CreateMembers'
  { _cmAccountDetails :: !(Maybe [AccountDetail])
  , _cmDetectorId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateMembers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmAccountDetails' - A list of account ID and email address pairs of the accounts that you want to associate with the master GuardDuty account.
--
-- * 'cmDetectorId' - The unique ID of the detector of the GuardDuty account with which you want to associate member accounts.
createMembers
    :: Text -- ^ 'cmDetectorId'
    -> CreateMembers
createMembers pDetectorId_ =
  CreateMembers' {_cmAccountDetails = Nothing, _cmDetectorId = pDetectorId_}


-- | A list of account ID and email address pairs of the accounts that you want to associate with the master GuardDuty account.
cmAccountDetails :: Lens' CreateMembers [AccountDetail]
cmAccountDetails = lens _cmAccountDetails (\ s a -> s{_cmAccountDetails = a}) . _Default . _Coerce

-- | The unique ID of the detector of the GuardDuty account with which you want to associate member accounts.
cmDetectorId :: Lens' CreateMembers Text
cmDetectorId = lens _cmDetectorId (\ s a -> s{_cmDetectorId = a})

instance AWSRequest CreateMembers where
        type Rs CreateMembers = CreateMembersResponse
        request = postJSON guardDuty
        response
          = receiveJSON
              (\ s h x ->
                 CreateMembersResponse' <$>
                   (x .?> "unprocessedAccounts" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable CreateMembers where

instance NFData CreateMembers where

instance ToHeaders CreateMembers where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateMembers where
        toJSON CreateMembers'{..}
          = object
              (catMaybes
                 [("accountDetails" .=) <$> _cmAccountDetails])

instance ToPath CreateMembers where
        toPath CreateMembers'{..}
          = mconcat
              ["/detector/", toBS _cmDetectorId, "/member"]

instance ToQuery CreateMembers where
        toQuery = const mempty

-- | /See:/ 'createMembersResponse' smart constructor.
data CreateMembersResponse = CreateMembersResponse'
  { _cmrsUnprocessedAccounts :: !(Maybe [UnprocessedAccount])
  , _cmrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateMembersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmrsUnprocessedAccounts' - A list of objects containing the unprocessed account and a result string explaining why it was unprocessed.
--
-- * 'cmrsResponseStatus' - -- | The response status code.
createMembersResponse
    :: Int -- ^ 'cmrsResponseStatus'
    -> CreateMembersResponse
createMembersResponse pResponseStatus_ =
  CreateMembersResponse'
    {_cmrsUnprocessedAccounts = Nothing, _cmrsResponseStatus = pResponseStatus_}


-- | A list of objects containing the unprocessed account and a result string explaining why it was unprocessed.
cmrsUnprocessedAccounts :: Lens' CreateMembersResponse [UnprocessedAccount]
cmrsUnprocessedAccounts = lens _cmrsUnprocessedAccounts (\ s a -> s{_cmrsUnprocessedAccounts = a}) . _Default . _Coerce

-- | -- | The response status code.
cmrsResponseStatus :: Lens' CreateMembersResponse Int
cmrsResponseStatus = lens _cmrsResponseStatus (\ s a -> s{_cmrsResponseStatus = a})

instance NFData CreateMembersResponse where
