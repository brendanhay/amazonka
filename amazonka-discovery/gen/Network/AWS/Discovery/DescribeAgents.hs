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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists AWS agents by ID or lists all agents associated with your user
-- account if you did not specify an agent ID.
module Network.AWS.Discovery.DescribeAgents
    (
    -- * Creating a Request
      describeAgents
    , DescribeAgents
    -- * Request Lenses
    , daAgentIds
    , daNextToken
    , daMaxResults

    -- * Destructuring the Response
    , describeAgentsResponse
    , DescribeAgentsResponse
    -- * Response Lenses
    , darsAgentsInfo
    , darsNextToken
    , darsResponseStatus
    ) where

import           Network.AWS.Discovery.Types
import           Network.AWS.Discovery.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeAgents' smart constructor.
data DescribeAgents = DescribeAgents'
    { _daAgentIds   :: !(Maybe [Text])
    , _daNextToken  :: !(Maybe Text)
    , _daMaxResults :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeAgents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daAgentIds'
--
-- * 'daNextToken'
--
-- * 'daMaxResults'
describeAgents
    :: DescribeAgents
describeAgents =
    DescribeAgents'
    { _daAgentIds = Nothing
    , _daNextToken = Nothing
    , _daMaxResults = Nothing
    }

-- | The agent IDs for which you want information. If you specify no IDs, the
-- system returns information about all agents associated with your AWS
-- user account.
daAgentIds :: Lens' DescribeAgents [Text]
daAgentIds = lens _daAgentIds (\ s a -> s{_daAgentIds = a}) . _Default . _Coerce;

-- | A token to start the list. Use this token to get the next set of
-- results.
daNextToken :: Lens' DescribeAgents (Maybe Text)
daNextToken = lens _daNextToken (\ s a -> s{_daNextToken = a});

-- | The total number of agents to return. The maximum value is 100.
daMaxResults :: Lens' DescribeAgents (Maybe Int)
daMaxResults = lens _daMaxResults (\ s a -> s{_daMaxResults = a});

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

instance Hashable DescribeAgents

instance NFData DescribeAgents

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
                  ("nextToken" .=) <$> _daNextToken,
                  ("maxResults" .=) <$> _daMaxResults])

instance ToPath DescribeAgents where
        toPath = const "/"

instance ToQuery DescribeAgents where
        toQuery = const mempty

-- | /See:/ 'describeAgentsResponse' smart constructor.
data DescribeAgentsResponse = DescribeAgentsResponse'
    { _darsAgentsInfo     :: !(Maybe [AgentInfo])
    , _darsNextToken      :: !(Maybe Text)
    , _darsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeAgentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsAgentsInfo'
--
-- * 'darsNextToken'
--
-- * 'darsResponseStatus'
describeAgentsResponse
    :: Int -- ^ 'darsResponseStatus'
    -> DescribeAgentsResponse
describeAgentsResponse pResponseStatus_ =
    DescribeAgentsResponse'
    { _darsAgentsInfo = Nothing
    , _darsNextToken = Nothing
    , _darsResponseStatus = pResponseStatus_
    }

-- | Lists AWS agents by ID or lists all agents associated with your user
-- account if you did not specify an agent ID. The output includes agent
-- IDs, IP addresses, media access control (MAC) addresses, agent health,
-- host name where the agent resides, and the version number of each agent.
darsAgentsInfo :: Lens' DescribeAgentsResponse [AgentInfo]
darsAgentsInfo = lens _darsAgentsInfo (\ s a -> s{_darsAgentsInfo = a}) . _Default . _Coerce;

-- | The call returns a token. Use this token to get the next set of results.
darsNextToken :: Lens' DescribeAgentsResponse (Maybe Text)
darsNextToken = lens _darsNextToken (\ s a -> s{_darsNextToken = a});

-- | The response status code.
darsResponseStatus :: Lens' DescribeAgentsResponse Int
darsResponseStatus = lens _darsResponseStatus (\ s a -> s{_darsResponseStatus = a});

instance NFData DescribeAgentsResponse
