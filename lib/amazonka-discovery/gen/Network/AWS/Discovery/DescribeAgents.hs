{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.DescribeAgents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists agents or connectors as specified by ID or other filters. All agents/connectors associated with your user account can be listed if you call @DescribeAgents@ as is without passing any parameters.
--
-- This operation returns paginated results.
module Network.AWS.Discovery.DescribeAgents
  ( -- * Creating a request
    DescribeAgents (..),
    mkDescribeAgents,

    -- ** Request lenses
    daAgentIds,
    daFilters,
    daNextToken,
    daMaxResults,

    -- * Destructuring the response
    DescribeAgentsResponse (..),
    mkDescribeAgentsResponse,

    -- ** Response lenses
    dasrsAgentsInfo,
    dasrsNextToken,
    dasrsResponseStatus,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAgents' smart constructor.
data DescribeAgents = DescribeAgents'
  { -- | The agent or the Connector IDs for which you want information. If you specify no IDs, the system returns information about all agents/Connectors associated with your AWS user account.
    agentIds :: Lude.Maybe [Lude.Text],
    -- | You can filter the request using various logical operators and a /key/ -/value/ format. For example:
    --
    -- @{"key": "collectionStatus", "value": "STARTED"}@
    filters :: Lude.Maybe [Filter],
    -- | Token to retrieve the next set of results. For example, if you previously specified 100 IDs for @DescribeAgentsRequest$agentIds@ but set @DescribeAgentsRequest$maxResults@ to 10, you received a set of 10 results along with a token. Use that token in this query to get the next set of 10.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The total number of agents/Connectors to return in a single page of output. The maximum value is 100.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAgents' with the minimum fields required to make a request.
--
-- * 'agentIds' - The agent or the Connector IDs for which you want information. If you specify no IDs, the system returns information about all agents/Connectors associated with your AWS user account.
-- * 'filters' - You can filter the request using various logical operators and a /key/ -/value/ format. For example:
--
-- @{"key": "collectionStatus", "value": "STARTED"}@
-- * 'nextToken' - Token to retrieve the next set of results. For example, if you previously specified 100 IDs for @DescribeAgentsRequest$agentIds@ but set @DescribeAgentsRequest$maxResults@ to 10, you received a set of 10 results along with a token. Use that token in this query to get the next set of 10.
-- * 'maxResults' - The total number of agents/Connectors to return in a single page of output. The maximum value is 100.
mkDescribeAgents ::
  DescribeAgents
mkDescribeAgents =
  DescribeAgents'
    { agentIds = Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The agent or the Connector IDs for which you want information. If you specify no IDs, the system returns information about all agents/Connectors associated with your AWS user account.
--
-- /Note:/ Consider using 'agentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAgentIds :: Lens.Lens' DescribeAgents (Lude.Maybe [Lude.Text])
daAgentIds = Lens.lens (agentIds :: DescribeAgents -> Lude.Maybe [Lude.Text]) (\s a -> s {agentIds = a} :: DescribeAgents)
{-# DEPRECATED daAgentIds "Use generic-lens or generic-optics with 'agentIds' instead." #-}

-- | You can filter the request using various logical operators and a /key/ -/value/ format. For example:
--
-- @{"key": "collectionStatus", "value": "STARTED"}@
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daFilters :: Lens.Lens' DescribeAgents (Lude.Maybe [Filter])
daFilters = Lens.lens (filters :: DescribeAgents -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeAgents)
{-# DEPRECATED daFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | Token to retrieve the next set of results. For example, if you previously specified 100 IDs for @DescribeAgentsRequest$agentIds@ but set @DescribeAgentsRequest$maxResults@ to 10, you received a set of 10 results along with a token. Use that token in this query to get the next set of 10.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daNextToken :: Lens.Lens' DescribeAgents (Lude.Maybe Lude.Text)
daNextToken = Lens.lens (nextToken :: DescribeAgents -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAgents)
{-# DEPRECATED daNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total number of agents/Connectors to return in a single page of output. The maximum value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daMaxResults :: Lens.Lens' DescribeAgents (Lude.Maybe Lude.Int)
daMaxResults = Lens.lens (maxResults :: DescribeAgents -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeAgents)
{-# DEPRECATED daMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeAgents where
  page rq rs
    | Page.stop (rs Lens.^. dasrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dasrsAgentsInfo) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& daNextToken Lens..~ rs Lens.^. dasrsNextToken

instance Lude.AWSRequest DescribeAgents where
  type Rs DescribeAgents = DescribeAgentsResponse
  request = Req.postJSON discoveryService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAgentsResponse'
            Lude.<$> (x Lude..?> "agentsInfo" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAgents where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSPoseidonService_V2015_11_01.DescribeAgents" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAgents where
  toJSON DescribeAgents' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("agentIds" Lude..=) Lude.<$> agentIds,
            ("filters" Lude..=) Lude.<$> filters,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeAgents where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAgents where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAgentsResponse' smart constructor.
data DescribeAgentsResponse = DescribeAgentsResponse'
  { -- | Lists agents or the Connector by ID or lists all agents/Connectors associated with your user account if you did not specify an agent/Connector ID. The output includes agent/Connector IDs, IP addresses, media access control (MAC) addresses, agent/Connector health, host name where the agent/Connector resides, and the version number of each agent/Connector.
    agentsInfo :: Lude.Maybe [AgentInfo],
    -- | Token to retrieve the next set of results. For example, if you specified 100 IDs for @DescribeAgentsRequest$agentIds@ but set @DescribeAgentsRequest$maxResults@ to 10, you received a set of 10 results along with this token. Use this token in the next query to retrieve the next set of 10.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAgentsResponse' with the minimum fields required to make a request.
--
-- * 'agentsInfo' - Lists agents or the Connector by ID or lists all agents/Connectors associated with your user account if you did not specify an agent/Connector ID. The output includes agent/Connector IDs, IP addresses, media access control (MAC) addresses, agent/Connector health, host name where the agent/Connector resides, and the version number of each agent/Connector.
-- * 'nextToken' - Token to retrieve the next set of results. For example, if you specified 100 IDs for @DescribeAgentsRequest$agentIds@ but set @DescribeAgentsRequest$maxResults@ to 10, you received a set of 10 results along with this token. Use this token in the next query to retrieve the next set of 10.
-- * 'responseStatus' - The response status code.
mkDescribeAgentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAgentsResponse
mkDescribeAgentsResponse pResponseStatus_ =
  DescribeAgentsResponse'
    { agentsInfo = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Lists agents or the Connector by ID or lists all agents/Connectors associated with your user account if you did not specify an agent/Connector ID. The output includes agent/Connector IDs, IP addresses, media access control (MAC) addresses, agent/Connector health, host name where the agent/Connector resides, and the version number of each agent/Connector.
--
-- /Note:/ Consider using 'agentsInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrsAgentsInfo :: Lens.Lens' DescribeAgentsResponse (Lude.Maybe [AgentInfo])
dasrsAgentsInfo = Lens.lens (agentsInfo :: DescribeAgentsResponse -> Lude.Maybe [AgentInfo]) (\s a -> s {agentsInfo = a} :: DescribeAgentsResponse)
{-# DEPRECATED dasrsAgentsInfo "Use generic-lens or generic-optics with 'agentsInfo' instead." #-}

-- | Token to retrieve the next set of results. For example, if you specified 100 IDs for @DescribeAgentsRequest$agentIds@ but set @DescribeAgentsRequest$maxResults@ to 10, you received a set of 10 results along with this token. Use this token in the next query to retrieve the next set of 10.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrsNextToken :: Lens.Lens' DescribeAgentsResponse (Lude.Maybe Lude.Text)
dasrsNextToken = Lens.lens (nextToken :: DescribeAgentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAgentsResponse)
{-# DEPRECATED dasrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrsResponseStatus :: Lens.Lens' DescribeAgentsResponse Lude.Int
dasrsResponseStatus = Lens.lens (responseStatus :: DescribeAgentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAgentsResponse)
{-# DEPRECATED dasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
