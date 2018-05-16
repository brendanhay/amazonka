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
-- Module      : Network.AWS.GuardDuty.GetMembers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves GuardDuty member accounts (to the current GuardDuty master account) specified by the account IDs.
module Network.AWS.GuardDuty.GetMembers
    (
    -- * Creating a Request
      getMembers
    , GetMembers
    -- * Request Lenses
    , gmAccountIds
    , gmDetectorId

    -- * Destructuring the Response
    , getMembersResponse
    , GetMembersResponse
    -- * Response Lenses
    , gmrsMembers
    , gmrsUnprocessedAccounts
    , gmrsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | GetMembers request body.
--
-- /See:/ 'getMembers' smart constructor.
data GetMembers = GetMembers'
  { _gmAccountIds :: !(Maybe [Text])
  , _gmDetectorId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMembers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmAccountIds' - A list of account IDs of the GuardDuty member accounts that you want to describe.
--
-- * 'gmDetectorId' - The unique ID of the detector of the GuardDuty account whose members you want to retrieve.
getMembers
    :: Text -- ^ 'gmDetectorId'
    -> GetMembers
getMembers pDetectorId_ =
  GetMembers' {_gmAccountIds = Nothing, _gmDetectorId = pDetectorId_}


-- | A list of account IDs of the GuardDuty member accounts that you want to describe.
gmAccountIds :: Lens' GetMembers [Text]
gmAccountIds = lens _gmAccountIds (\ s a -> s{_gmAccountIds = a}) . _Default . _Coerce

-- | The unique ID of the detector of the GuardDuty account whose members you want to retrieve.
gmDetectorId :: Lens' GetMembers Text
gmDetectorId = lens _gmDetectorId (\ s a -> s{_gmDetectorId = a})

instance AWSRequest GetMembers where
        type Rs GetMembers = GetMembersResponse
        request = postJSON guardDuty
        response
          = receiveJSON
              (\ s h x ->
                 GetMembersResponse' <$>
                   (x .?> "members" .!@ mempty) <*>
                     (x .?> "unprocessedAccounts" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetMembers where

instance NFData GetMembers where

instance ToHeaders GetMembers where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetMembers where
        toJSON GetMembers'{..}
          = object
              (catMaybes [("accountIds" .=) <$> _gmAccountIds])

instance ToPath GetMembers where
        toPath GetMembers'{..}
          = mconcat
              ["/detector/", toBS _gmDetectorId, "/member/get"]

instance ToQuery GetMembers where
        toQuery = const mempty

-- | /See:/ 'getMembersResponse' smart constructor.
data GetMembersResponse = GetMembersResponse'
  { _gmrsMembers             :: !(Maybe [Member])
  , _gmrsUnprocessedAccounts :: !(Maybe [UnprocessedAccount])
  , _gmrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetMembersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmrsMembers' - Undocumented member.
--
-- * 'gmrsUnprocessedAccounts' - A list of objects containing the unprocessed account and a result string explaining why it was unprocessed.
--
-- * 'gmrsResponseStatus' - -- | The response status code.
getMembersResponse
    :: Int -- ^ 'gmrsResponseStatus'
    -> GetMembersResponse
getMembersResponse pResponseStatus_ =
  GetMembersResponse'
    { _gmrsMembers = Nothing
    , _gmrsUnprocessedAccounts = Nothing
    , _gmrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
gmrsMembers :: Lens' GetMembersResponse [Member]
gmrsMembers = lens _gmrsMembers (\ s a -> s{_gmrsMembers = a}) . _Default . _Coerce

-- | A list of objects containing the unprocessed account and a result string explaining why it was unprocessed.
gmrsUnprocessedAccounts :: Lens' GetMembersResponse [UnprocessedAccount]
gmrsUnprocessedAccounts = lens _gmrsUnprocessedAccounts (\ s a -> s{_gmrsUnprocessedAccounts = a}) . _Default . _Coerce

-- | -- | The response status code.
gmrsResponseStatus :: Lens' GetMembersResponse Int
gmrsResponseStatus = lens _gmrsResponseStatus (\ s a -> s{_gmrsResponseStatus = a})

instance NFData GetMembersResponse where
