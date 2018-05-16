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
-- Module      : Network.AWS.GuardDuty.StartMonitoringMembers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Re-enables GuardDuty to monitor findings of the member accounts specified by the account IDs. A master GuardDuty account can run this command after disabling GuardDuty from monitoring these members' findings by running StopMonitoringMembers.
module Network.AWS.GuardDuty.StartMonitoringMembers
    (
    -- * Creating a Request
      startMonitoringMembers
    , StartMonitoringMembers
    -- * Request Lenses
    , sAccountIds
    , sDetectorId

    -- * Destructuring the Response
    , startMonitoringMembersResponse
    , StartMonitoringMembersResponse
    -- * Response Lenses
    , srsUnprocessedAccounts
    , srsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | StartMonitoringMembers request body.
--
-- /See:/ 'startMonitoringMembers' smart constructor.
data StartMonitoringMembers = StartMonitoringMembers'
  { _sAccountIds :: !(Maybe [Text])
  , _sDetectorId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartMonitoringMembers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sAccountIds' - A list of account IDs of the GuardDuty member accounts whose findings you want the master account to monitor.
--
-- * 'sDetectorId' - The unique ID of the detector of the GuardDuty account whom you want to re-enable to monitor members' findings.
startMonitoringMembers
    :: Text -- ^ 'sDetectorId'
    -> StartMonitoringMembers
startMonitoringMembers pDetectorId_ =
  StartMonitoringMembers' {_sAccountIds = Nothing, _sDetectorId = pDetectorId_}


-- | A list of account IDs of the GuardDuty member accounts whose findings you want the master account to monitor.
sAccountIds :: Lens' StartMonitoringMembers [Text]
sAccountIds = lens _sAccountIds (\ s a -> s{_sAccountIds = a}) . _Default . _Coerce

-- | The unique ID of the detector of the GuardDuty account whom you want to re-enable to monitor members' findings.
sDetectorId :: Lens' StartMonitoringMembers Text
sDetectorId = lens _sDetectorId (\ s a -> s{_sDetectorId = a})

instance AWSRequest StartMonitoringMembers where
        type Rs StartMonitoringMembers =
             StartMonitoringMembersResponse
        request = postJSON guardDuty
        response
          = receiveJSON
              (\ s h x ->
                 StartMonitoringMembersResponse' <$>
                   (x .?> "unprocessedAccounts" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable StartMonitoringMembers where

instance NFData StartMonitoringMembers where

instance ToHeaders StartMonitoringMembers where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartMonitoringMembers where
        toJSON StartMonitoringMembers'{..}
          = object
              (catMaybes [("accountIds" .=) <$> _sAccountIds])

instance ToPath StartMonitoringMembers where
        toPath StartMonitoringMembers'{..}
          = mconcat
              ["/detector/", toBS _sDetectorId, "/member/start"]

instance ToQuery StartMonitoringMembers where
        toQuery = const mempty

-- | /See:/ 'startMonitoringMembersResponse' smart constructor.
data StartMonitoringMembersResponse = StartMonitoringMembersResponse'
  { _srsUnprocessedAccounts :: !(Maybe [UnprocessedAccount])
  , _srsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartMonitoringMembersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsUnprocessedAccounts' - A list of objects containing the unprocessed account and a result string explaining why it was unprocessed.
--
-- * 'srsResponseStatus' - -- | The response status code.
startMonitoringMembersResponse
    :: Int -- ^ 'srsResponseStatus'
    -> StartMonitoringMembersResponse
startMonitoringMembersResponse pResponseStatus_ =
  StartMonitoringMembersResponse'
    {_srsUnprocessedAccounts = Nothing, _srsResponseStatus = pResponseStatus_}


-- | A list of objects containing the unprocessed account and a result string explaining why it was unprocessed.
srsUnprocessedAccounts :: Lens' StartMonitoringMembersResponse [UnprocessedAccount]
srsUnprocessedAccounts = lens _srsUnprocessedAccounts (\ s a -> s{_srsUnprocessedAccounts = a}) . _Default . _Coerce

-- | -- | The response status code.
srsResponseStatus :: Lens' StartMonitoringMembersResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a})

instance NFData StartMonitoringMembersResponse where
