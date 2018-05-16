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
-- Module      : Network.AWS.Discovery.StopDataCollectionByAgentIds
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Instructs the specified agents or connectors to stop collecting data.
--
--
module Network.AWS.Discovery.StopDataCollectionByAgentIds
    (
    -- * Creating a Request
      stopDataCollectionByAgentIds
    , StopDataCollectionByAgentIds
    -- * Request Lenses
    , sdcbaiAgentIds

    -- * Destructuring the Response
    , stopDataCollectionByAgentIdsResponse
    , StopDataCollectionByAgentIdsResponse
    -- * Response Lenses
    , sdcbairsAgentsConfigurationStatus
    , sdcbairsResponseStatus
    ) where

import Network.AWS.Discovery.Types
import Network.AWS.Discovery.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopDataCollectionByAgentIds' smart constructor.
newtype StopDataCollectionByAgentIds = StopDataCollectionByAgentIds'
  { _sdcbaiAgentIds :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopDataCollectionByAgentIds' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdcbaiAgentIds' - The IDs of the agents or connectors from which to stop collecting data.
stopDataCollectionByAgentIds
    :: StopDataCollectionByAgentIds
stopDataCollectionByAgentIds =
  StopDataCollectionByAgentIds' {_sdcbaiAgentIds = mempty}


-- | The IDs of the agents or connectors from which to stop collecting data.
sdcbaiAgentIds :: Lens' StopDataCollectionByAgentIds [Text]
sdcbaiAgentIds = lens _sdcbaiAgentIds (\ s a -> s{_sdcbaiAgentIds = a}) . _Coerce

instance AWSRequest StopDataCollectionByAgentIds
         where
        type Rs StopDataCollectionByAgentIds =
             StopDataCollectionByAgentIdsResponse
        request = postJSON discovery
        response
          = receiveJSON
              (\ s h x ->
                 StopDataCollectionByAgentIdsResponse' <$>
                   (x .?> "agentsConfigurationStatus" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable StopDataCollectionByAgentIds where

instance NFData StopDataCollectionByAgentIds where

instance ToHeaders StopDataCollectionByAgentIds where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSPoseidonService_V2015_11_01.StopDataCollectionByAgentIds"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopDataCollectionByAgentIds where
        toJSON StopDataCollectionByAgentIds'{..}
          = object
              (catMaybes [Just ("agentIds" .= _sdcbaiAgentIds)])

instance ToPath StopDataCollectionByAgentIds where
        toPath = const "/"

instance ToQuery StopDataCollectionByAgentIds where
        toQuery = const mempty

-- | /See:/ 'stopDataCollectionByAgentIdsResponse' smart constructor.
data StopDataCollectionByAgentIdsResponse = StopDataCollectionByAgentIdsResponse'
  { _sdcbairsAgentsConfigurationStatus :: !(Maybe [AgentConfigurationStatus])
  , _sdcbairsResponseStatus            :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopDataCollectionByAgentIdsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdcbairsAgentsConfigurationStatus' - Information about the agents or connector that were instructed to stop collecting data. Information includes the agent/connector ID, a description of the operation performed, and whether the agent/connector configuration was updated.
--
-- * 'sdcbairsResponseStatus' - -- | The response status code.
stopDataCollectionByAgentIdsResponse
    :: Int -- ^ 'sdcbairsResponseStatus'
    -> StopDataCollectionByAgentIdsResponse
stopDataCollectionByAgentIdsResponse pResponseStatus_ =
  StopDataCollectionByAgentIdsResponse'
    { _sdcbairsAgentsConfigurationStatus = Nothing
    , _sdcbairsResponseStatus = pResponseStatus_
    }


-- | Information about the agents or connector that were instructed to stop collecting data. Information includes the agent/connector ID, a description of the operation performed, and whether the agent/connector configuration was updated.
sdcbairsAgentsConfigurationStatus :: Lens' StopDataCollectionByAgentIdsResponse [AgentConfigurationStatus]
sdcbairsAgentsConfigurationStatus = lens _sdcbairsAgentsConfigurationStatus (\ s a -> s{_sdcbairsAgentsConfigurationStatus = a}) . _Default . _Coerce

-- | -- | The response status code.
sdcbairsResponseStatus :: Lens' StopDataCollectionByAgentIdsResponse Int
sdcbairsResponseStatus = lens _sdcbairsResponseStatus (\ s a -> s{_sdcbairsResponseStatus = a})

instance NFData StopDataCollectionByAgentIdsResponse
         where
