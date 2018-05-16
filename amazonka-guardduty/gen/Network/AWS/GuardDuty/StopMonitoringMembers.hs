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
-- Module      : Network.AWS.GuardDuty.StopMonitoringMembers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables GuardDuty from monitoring findings of the member accounts specified by the account IDs. After running this command, a master GuardDuty account can run StartMonitoringMembers to re-enable GuardDuty to monitor these members’ findings.
module Network.AWS.GuardDuty.StopMonitoringMembers
    (
    -- * Creating a Request
      stopMonitoringMembers
    , StopMonitoringMembers
    -- * Request Lenses
    , smmAccountIds
    , smmDetectorId

    -- * Destructuring the Response
    , stopMonitoringMembersResponse
    , StopMonitoringMembersResponse
    -- * Response Lenses
    , smmrsUnprocessedAccounts
    , smmrsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | StopMonitoringMembers request body.
--
-- /See:/ 'stopMonitoringMembers' smart constructor.
data StopMonitoringMembers = StopMonitoringMembers'
  { _smmAccountIds :: !(Maybe [Text])
  , _smmDetectorId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopMonitoringMembers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smmAccountIds' - A list of account IDs of the GuardDuty member accounts whose findings you want the master account to stop monitoring.
--
-- * 'smmDetectorId' - The unique ID of the detector of the GuardDuty account that you want to stop from monitor members' findings.
stopMonitoringMembers
    :: Text -- ^ 'smmDetectorId'
    -> StopMonitoringMembers
stopMonitoringMembers pDetectorId_ =
  StopMonitoringMembers'
    {_smmAccountIds = Nothing, _smmDetectorId = pDetectorId_}


-- | A list of account IDs of the GuardDuty member accounts whose findings you want the master account to stop monitoring.
smmAccountIds :: Lens' StopMonitoringMembers [Text]
smmAccountIds = lens _smmAccountIds (\ s a -> s{_smmAccountIds = a}) . _Default . _Coerce

-- | The unique ID of the detector of the GuardDuty account that you want to stop from monitor members' findings.
smmDetectorId :: Lens' StopMonitoringMembers Text
smmDetectorId = lens _smmDetectorId (\ s a -> s{_smmDetectorId = a})

instance AWSRequest StopMonitoringMembers where
        type Rs StopMonitoringMembers =
             StopMonitoringMembersResponse
        request = postJSON guardDuty
        response
          = receiveJSON
              (\ s h x ->
                 StopMonitoringMembersResponse' <$>
                   (x .?> "unprocessedAccounts" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable StopMonitoringMembers where

instance NFData StopMonitoringMembers where

instance ToHeaders StopMonitoringMembers where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopMonitoringMembers where
        toJSON StopMonitoringMembers'{..}
          = object
              (catMaybes [("accountIds" .=) <$> _smmAccountIds])

instance ToPath StopMonitoringMembers where
        toPath StopMonitoringMembers'{..}
          = mconcat
              ["/detector/", toBS _smmDetectorId, "/member/stop"]

instance ToQuery StopMonitoringMembers where
        toQuery = const mempty

-- | /See:/ 'stopMonitoringMembersResponse' smart constructor.
data StopMonitoringMembersResponse = StopMonitoringMembersResponse'
  { _smmrsUnprocessedAccounts :: !(Maybe [UnprocessedAccount])
  , _smmrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopMonitoringMembersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smmrsUnprocessedAccounts' - A list of objects containing the unprocessed account and a result string explaining why it was unprocessed.
--
-- * 'smmrsResponseStatus' - -- | The response status code.
stopMonitoringMembersResponse
    :: Int -- ^ 'smmrsResponseStatus'
    -> StopMonitoringMembersResponse
stopMonitoringMembersResponse pResponseStatus_ =
  StopMonitoringMembersResponse'
    { _smmrsUnprocessedAccounts = Nothing
    , _smmrsResponseStatus = pResponseStatus_
    }


-- | A list of objects containing the unprocessed account and a result string explaining why it was unprocessed.
smmrsUnprocessedAccounts :: Lens' StopMonitoringMembersResponse [UnprocessedAccount]
smmrsUnprocessedAccounts = lens _smmrsUnprocessedAccounts (\ s a -> s{_smmrsUnprocessedAccounts = a}) . _Default . _Coerce

-- | -- | The response status code.
smmrsResponseStatus :: Lens' StopMonitoringMembersResponse Int
smmrsResponseStatus = lens _smmrsResponseStatus (\ s a -> s{_smmrsResponseStatus = a})

instance NFData StopMonitoringMembersResponse where
