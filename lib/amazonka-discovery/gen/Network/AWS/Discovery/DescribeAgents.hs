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
-- Module      : Network.AWS.Discovery.DescribeAgents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists agents or the Connector by ID or lists all agents/Connectors associated with your user account if you did not specify an ID.
--
--
module Network.AWS.Discovery.DescribeAgents
    (
    -- * Creating a Request
      describeAgents
    , DescribeAgents
    -- * Request Lenses
    , daAgentIds
    , daFilters
    , daNextToken
    , daMaxResults

    -- * Destructuring the Response
    , describeAgentsResponse
    , DescribeAgentsResponse
    -- * Response Lenses
    , dasrsAgentsInfo
    , dasrsNextToken
    , dasrsResponseStatus
    ) where

import Network.AWS.Discovery.Types
import Network.AWS.Discovery.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAgents' smart constructor.
data DescribeAgents = DescribeAgents'
  { _daAgentIds   :: !(Maybe [Text])
  , _daFilters    :: !(Maybe [Filter])
  , _daNextToken  :: !(Maybe Text)
  , _daMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAgents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daAgentIds' - The agent or the Connector IDs for which you want information. If you specify no IDs, the system returns information about all agents/Connectors associated with your AWS user account.
--
-- * 'daFilters' - You can filter the request using various logical operators and a /key/ -/value/ format. For example:  @{"key": "collectionStatus", "value": "STARTED"}@
--
-- * 'daNextToken' - Token to retrieve the next set of results. For example, if you previously specified 100 IDs for @DescribeAgentsRequest$agentIds@ but set @DescribeAgentsRequest$maxResults@ to 10, you received a set of 10 results along with a token. Use that token in this query to get the next set of 10.
--
-- * 'daMaxResults' - The total number of agents/Connectors to return in a single page of output. The maximum value is 100.
describeAgents
    :: DescribeAgents
describeAgents =
  DescribeAgents'
    { _daAgentIds = Nothing
    , _daFilters = Nothing
    , _daNextToken = Nothing
    , _daMaxResults = Nothing
    }


-- | The agent or the Connector IDs for which you want information. If you specify no IDs, the system returns information about all agents/Connectors associated with your AWS user account.
daAgentIds :: Lens' DescribeAgents [Text]
daAgentIds = lens _daAgentIds (\ s a -> s{_daAgentIds = a}) . _Default . _Coerce

-- | You can filter the request using various logical operators and a /key/ -/value/ format. For example:  @{"key": "collectionStatus", "value": "STARTED"}@
daFilters :: Lens' DescribeAgents [Filter]
daFilters = lens _daFilters (\ s a -> s{_daFilters = a}) . _Default . _Coerce

-- | Token to retrieve the next set of results. For example, if you previously specified 100 IDs for @DescribeAgentsRequest$agentIds@ but set @DescribeAgentsRequest$maxResults@ to 10, you received a set of 10 results along with a token. Use that token in this query to get the next set of 10.
daNextToken :: Lens' DescribeAgents (Maybe Text)
daNextToken = lens _daNextToken (\ s a -> s{_daNextToken = a})

-- | The total number of agents/Connectors to return in a single page of output. The maximum value is 100.
daMaxResults :: Lens' DescribeAgents (Maybe Int)
daMaxResults = lens _daMaxResults (\ s a -> s{_daMaxResults = a})

instance AWSRequest DescribeAgents where
        type Rs DescribeAgents = DescribeAgentsResponse
        request = postJSON discovery
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAgentsResponse' <$>
                   (x .?> "agentsInfo" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeAgents where

instance NFData DescribeAgents where

instance ToHeaders DescribeAgents where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSPoseidonService_V2015_11_01.DescribeAgents" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeAgents where
        toJSON DescribeAgents'{..}
          = object
              (catMaybes
                 [("agentIds" .=) <$> _daAgentIds,
                  ("filters" .=) <$> _daFilters,
                  ("nextToken" .=) <$> _daNextToken,
                  ("maxResults" .=) <$> _daMaxResults])

instance ToPath DescribeAgents where
        toPath = const "/"

instance ToQuery DescribeAgents where
        toQuery = const mempty

-- | /See:/ 'describeAgentsResponse' smart constructor.
data DescribeAgentsResponse = DescribeAgentsResponse'
  { _dasrsAgentsInfo     :: !(Maybe [AgentInfo])
  , _dasrsNextToken      :: !(Maybe Text)
  , _dasrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAgentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasrsAgentsInfo' - Lists agents or the Connector by ID or lists all agents/Connectors associated with your user account if you did not specify an agent/Connector ID. The output includes agent/Connector IDs, IP addresses, media access control (MAC) addresses, agent/Connector health, host name where the agent/Connector resides, and the version number of each agent/Connector.
--
-- * 'dasrsNextToken' - Token to retrieve the next set of results. For example, if you specified 100 IDs for @DescribeAgentsRequest$agentIds@ but set @DescribeAgentsRequest$maxResults@ to 10, you received a set of 10 results along with this token. Use this token in the next query to retrieve the next set of 10.
--
-- * 'dasrsResponseStatus' - -- | The response status code.
describeAgentsResponse
    :: Int -- ^ 'dasrsResponseStatus'
    -> DescribeAgentsResponse
describeAgentsResponse pResponseStatus_ =
  DescribeAgentsResponse'
    { _dasrsAgentsInfo = Nothing
    , _dasrsNextToken = Nothing
    , _dasrsResponseStatus = pResponseStatus_
    }


-- | Lists agents or the Connector by ID or lists all agents/Connectors associated with your user account if you did not specify an agent/Connector ID. The output includes agent/Connector IDs, IP addresses, media access control (MAC) addresses, agent/Connector health, host name where the agent/Connector resides, and the version number of each agent/Connector.
dasrsAgentsInfo :: Lens' DescribeAgentsResponse [AgentInfo]
dasrsAgentsInfo = lens _dasrsAgentsInfo (\ s a -> s{_dasrsAgentsInfo = a}) . _Default . _Coerce

-- | Token to retrieve the next set of results. For example, if you specified 100 IDs for @DescribeAgentsRequest$agentIds@ but set @DescribeAgentsRequest$maxResults@ to 10, you received a set of 10 results along with this token. Use this token in the next query to retrieve the next set of 10.
dasrsNextToken :: Lens' DescribeAgentsResponse (Maybe Text)
dasrsNextToken = lens _dasrsNextToken (\ s a -> s{_dasrsNextToken = a})

-- | -- | The response status code.
dasrsResponseStatus :: Lens' DescribeAgentsResponse Int
dasrsResponseStatus = lens _dasrsResponseStatus (\ s a -> s{_dasrsResponseStatus = a})

instance NFData DescribeAgentsResponse where
