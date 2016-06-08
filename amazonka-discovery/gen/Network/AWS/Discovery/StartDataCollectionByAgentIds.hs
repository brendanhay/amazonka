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
-- Module      : Network.AWS.Discovery.StartDataCollectionByAgentIds
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Instructs the specified agents to start collecting data. Agents can reside on host servers or virtual machines in your data center.
module Network.AWS.Discovery.StartDataCollectionByAgentIds
    (
    -- * Creating a Request
      startDataCollectionByAgentIds
    , StartDataCollectionByAgentIds
    -- * Request Lenses
    , sAgentIds

    -- * Destructuring the Response
    , startDataCollectionByAgentIdsResponse
    , StartDataCollectionByAgentIdsResponse
    -- * Response Lenses
    , srsAgentsConfigurationStatus
    , srsResponseStatus
    ) where

import           Network.AWS.Discovery.Types
import           Network.AWS.Discovery.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'startDataCollectionByAgentIds' smart constructor.
newtype StartDataCollectionByAgentIds = StartDataCollectionByAgentIds'
    { _sAgentIds :: [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StartDataCollectionByAgentIds' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sAgentIds'
startDataCollectionByAgentIds
    :: StartDataCollectionByAgentIds
startDataCollectionByAgentIds =
    StartDataCollectionByAgentIds'
    { _sAgentIds = mempty
    }

-- | The IDs of the agents that you want to start collecting data. If you send a request to an AWS agent ID that you do not have permission to contact, according to your AWS account, the service does not throw an exception. Instead, it returns the error in the /Description/ field. If you send a request to multiple agents and you do not have permission to contact some of those agents, the system does not throw an exception. Instead, the system shows 'Failed' in the /Description/ field.
sAgentIds :: Lens' StartDataCollectionByAgentIds [Text]
sAgentIds = lens _sAgentIds (\ s a -> s{_sAgentIds = a}) . _Coerce;

instance AWSRequest StartDataCollectionByAgentIds
         where
        type Rs StartDataCollectionByAgentIds =
             StartDataCollectionByAgentIdsResponse
        request = postJSON discovery
        response
          = receiveJSON
              (\ s h x ->
                 StartDataCollectionByAgentIdsResponse' <$>
                   (x .?> "agentsConfigurationStatus" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable StartDataCollectionByAgentIds

instance NFData StartDataCollectionByAgentIds

instance ToHeaders StartDataCollectionByAgentIds
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSPoseidonService_V2015_11_01.StartDataCollectionByAgentIds"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartDataCollectionByAgentIds where
        toJSON StartDataCollectionByAgentIds'{..}
          = object
              (catMaybes [Just ("agentIds" .= _sAgentIds)])

instance ToPath StartDataCollectionByAgentIds where
        toPath = const "/"

instance ToQuery StartDataCollectionByAgentIds where
        toQuery = const mempty

-- | /See:/ 'startDataCollectionByAgentIdsResponse' smart constructor.
data StartDataCollectionByAgentIdsResponse = StartDataCollectionByAgentIdsResponse'
    { _srsAgentsConfigurationStatus :: !(Maybe [AgentConfigurationStatus])
    , _srsResponseStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StartDataCollectionByAgentIdsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsAgentsConfigurationStatus'
--
-- * 'srsResponseStatus'
startDataCollectionByAgentIdsResponse
    :: Int -- ^ 'srsResponseStatus'
    -> StartDataCollectionByAgentIdsResponse
startDataCollectionByAgentIdsResponse pResponseStatus_ =
    StartDataCollectionByAgentIdsResponse'
    { _srsAgentsConfigurationStatus = Nothing
    , _srsResponseStatus = pResponseStatus_
    }

-- | Information about agents that were instructed to start collecting data. Information includes the agent ID, a description of the operation performed, and whether or not the agent configuration was updated.
srsAgentsConfigurationStatus :: Lens' StartDataCollectionByAgentIdsResponse [AgentConfigurationStatus]
srsAgentsConfigurationStatus = lens _srsAgentsConfigurationStatus (\ s a -> s{_srsAgentsConfigurationStatus = a}) . _Default . _Coerce;

-- | The response status code.
srsResponseStatus :: Lens' StartDataCollectionByAgentIdsResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a});

instance NFData StartDataCollectionByAgentIdsResponse
