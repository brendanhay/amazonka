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
-- Module      : Network.AWS.SMS.StartOnDemandReplicationRun
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The StartOnDemandReplicationRun API is used to start a ReplicationRun on demand (in addition to those that are scheduled based on your frequency). This ReplicationRun will start immediately. StartOnDemandReplicationRun is subject to limits on how many on demand ReplicationRuns you may call per 24-hour period.
module Network.AWS.SMS.StartOnDemandReplicationRun
    (
    -- * Creating a Request
      startOnDemandReplicationRun
    , StartOnDemandReplicationRun
    -- * Request Lenses
    , sodrrDescription
    , sodrrReplicationJobId

    -- * Destructuring the Response
    , startOnDemandReplicationRunResponse
    , StartOnDemandReplicationRunResponse
    -- * Response Lenses
    , sodrrrsReplicationRunId
    , sodrrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types
import Network.AWS.SMS.Types.Product

-- | /See:/ 'startOnDemandReplicationRun' smart constructor.
data StartOnDemandReplicationRun = StartOnDemandReplicationRun'
  { _sodrrDescription      :: !(Maybe Text)
  , _sodrrReplicationJobId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartOnDemandReplicationRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sodrrDescription' - Undocumented member.
--
-- * 'sodrrReplicationJobId' - Undocumented member.
startOnDemandReplicationRun
    :: Text -- ^ 'sodrrReplicationJobId'
    -> StartOnDemandReplicationRun
startOnDemandReplicationRun pReplicationJobId_ =
  StartOnDemandReplicationRun'
    {_sodrrDescription = Nothing, _sodrrReplicationJobId = pReplicationJobId_}


-- | Undocumented member.
sodrrDescription :: Lens' StartOnDemandReplicationRun (Maybe Text)
sodrrDescription = lens _sodrrDescription (\ s a -> s{_sodrrDescription = a})

-- | Undocumented member.
sodrrReplicationJobId :: Lens' StartOnDemandReplicationRun Text
sodrrReplicationJobId = lens _sodrrReplicationJobId (\ s a -> s{_sodrrReplicationJobId = a})

instance AWSRequest StartOnDemandReplicationRun where
        type Rs StartOnDemandReplicationRun =
             StartOnDemandReplicationRunResponse
        request = postJSON sms
        response
          = receiveJSON
              (\ s h x ->
                 StartOnDemandReplicationRunResponse' <$>
                   (x .?> "replicationRunId") <*> (pure (fromEnum s)))

instance Hashable StartOnDemandReplicationRun where

instance NFData StartOnDemandReplicationRun where

instance ToHeaders StartOnDemandReplicationRun where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSServerMigrationService_V2016_10_24.StartOnDemandReplicationRun"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartOnDemandReplicationRun where
        toJSON StartOnDemandReplicationRun'{..}
          = object
              (catMaybes
                 [("description" .=) <$> _sodrrDescription,
                  Just ("replicationJobId" .= _sodrrReplicationJobId)])

instance ToPath StartOnDemandReplicationRun where
        toPath = const "/"

instance ToQuery StartOnDemandReplicationRun where
        toQuery = const mempty

-- | /See:/ 'startOnDemandReplicationRunResponse' smart constructor.
data StartOnDemandReplicationRunResponse = StartOnDemandReplicationRunResponse'
  { _sodrrrsReplicationRunId :: !(Maybe Text)
  , _sodrrrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartOnDemandReplicationRunResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sodrrrsReplicationRunId' - Undocumented member.
--
-- * 'sodrrrsResponseStatus' - -- | The response status code.
startOnDemandReplicationRunResponse
    :: Int -- ^ 'sodrrrsResponseStatus'
    -> StartOnDemandReplicationRunResponse
startOnDemandReplicationRunResponse pResponseStatus_ =
  StartOnDemandReplicationRunResponse'
    { _sodrrrsReplicationRunId = Nothing
    , _sodrrrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
sodrrrsReplicationRunId :: Lens' StartOnDemandReplicationRunResponse (Maybe Text)
sodrrrsReplicationRunId = lens _sodrrrsReplicationRunId (\ s a -> s{_sodrrrsReplicationRunId = a})

-- | -- | The response status code.
sodrrrsResponseStatus :: Lens' StartOnDemandReplicationRunResponse Int
sodrrrsResponseStatus = lens _sodrrrsResponseStatus (\ s a -> s{_sodrrrsResponseStatus = a})

instance NFData StartOnDemandReplicationRunResponse
         where
