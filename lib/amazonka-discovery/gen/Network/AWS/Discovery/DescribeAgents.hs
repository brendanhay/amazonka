{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeAgents (..)
    , mkDescribeAgents
    -- ** Request lenses
    , daAgentIds
    , daFilters
    , daMaxResults
    , daNextToken

    -- * Destructuring the response
    , DescribeAgentsResponse (..)
    , mkDescribeAgentsResponse
    -- ** Response lenses
    , darfrsAgentsInfo
    , darfrsNextToken
    , darfrsResponseStatus
    ) where

import qualified Network.AWS.Discovery.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAgents' smart constructor.
data DescribeAgents = DescribeAgents'
  { agentIds :: Core.Maybe [Types.AgentId]
    -- ^ The agent or the Connector IDs for which you want information. If you specify no IDs, the system returns information about all agents/Connectors associated with your AWS user account.
  , filters :: Core.Maybe [Types.Filter]
    -- ^ You can filter the request using various logical operators and a /key/ -/value/ format. For example: 
--
-- @{"key": "collectionStatus", "value": "STARTED"}@ 
  , maxResults :: Core.Maybe Core.Int
    -- ^ The total number of agents/Connectors to return in a single page of output. The maximum value is 100.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ Token to retrieve the next set of results. For example, if you previously specified 100 IDs for @DescribeAgentsRequest$agentIds@ but set @DescribeAgentsRequest$maxResults@ to 10, you received a set of 10 results along with a token. Use that token in this query to get the next set of 10.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAgents' value with any optional fields omitted.
mkDescribeAgents
    :: DescribeAgents
mkDescribeAgents
  = DescribeAgents'{agentIds = Core.Nothing, filters = Core.Nothing,
                    maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The agent or the Connector IDs for which you want information. If you specify no IDs, the system returns information about all agents/Connectors associated with your AWS user account.
--
-- /Note:/ Consider using 'agentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAgentIds :: Lens.Lens' DescribeAgents (Core.Maybe [Types.AgentId])
daAgentIds = Lens.field @"agentIds"
{-# INLINEABLE daAgentIds #-}
{-# DEPRECATED agentIds "Use generic-lens or generic-optics with 'agentIds' instead"  #-}

-- | You can filter the request using various logical operators and a /key/ -/value/ format. For example: 
--
-- @{"key": "collectionStatus", "value": "STARTED"}@ 
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daFilters :: Lens.Lens' DescribeAgents (Core.Maybe [Types.Filter])
daFilters = Lens.field @"filters"
{-# INLINEABLE daFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The total number of agents/Connectors to return in a single page of output. The maximum value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daMaxResults :: Lens.Lens' DescribeAgents (Core.Maybe Core.Int)
daMaxResults = Lens.field @"maxResults"
{-# INLINEABLE daMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Token to retrieve the next set of results. For example, if you previously specified 100 IDs for @DescribeAgentsRequest$agentIds@ but set @DescribeAgentsRequest$maxResults@ to 10, you received a set of 10 results along with a token. Use that token in this query to get the next set of 10.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daNextToken :: Lens.Lens' DescribeAgents (Core.Maybe Types.NextToken)
daNextToken = Lens.field @"nextToken"
{-# INLINEABLE daNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeAgents where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeAgents where
        toHeaders DescribeAgents{..}
          = Core.pure
              ("X-Amz-Target", "AWSPoseidonService_V2015_11_01.DescribeAgents")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeAgents where
        toJSON DescribeAgents{..}
          = Core.object
              (Core.catMaybes
                 [("agentIds" Core..=) Core.<$> agentIds,
                  ("filters" Core..=) Core.<$> filters,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeAgents where
        type Rs DescribeAgents = DescribeAgentsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeAgentsResponse' Core.<$>
                   (x Core..:? "agentsInfo") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeAgents where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"agentsInfo" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeAgentsResponse' smart constructor.
data DescribeAgentsResponse = DescribeAgentsResponse'
  { agentsInfo :: Core.Maybe [Types.AgentInfo]
    -- ^ Lists agents or the Connector by ID or lists all agents/Connectors associated with your user account if you did not specify an agent/Connector ID. The output includes agent/Connector IDs, IP addresses, media access control (MAC) addresses, agent/Connector health, host name where the agent/Connector resides, and the version number of each agent/Connector.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ Token to retrieve the next set of results. For example, if you specified 100 IDs for @DescribeAgentsRequest$agentIds@ but set @DescribeAgentsRequest$maxResults@ to 10, you received a set of 10 results along with this token. Use this token in the next query to retrieve the next set of 10.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAgentsResponse' value with any optional fields omitted.
mkDescribeAgentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAgentsResponse
mkDescribeAgentsResponse responseStatus
  = DescribeAgentsResponse'{agentsInfo = Core.Nothing,
                            nextToken = Core.Nothing, responseStatus}

-- | Lists agents or the Connector by ID or lists all agents/Connectors associated with your user account if you did not specify an agent/Connector ID. The output includes agent/Connector IDs, IP addresses, media access control (MAC) addresses, agent/Connector health, host name where the agent/Connector resides, and the version number of each agent/Connector.
--
-- /Note:/ Consider using 'agentsInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darfrsAgentsInfo :: Lens.Lens' DescribeAgentsResponse (Core.Maybe [Types.AgentInfo])
darfrsAgentsInfo = Lens.field @"agentsInfo"
{-# INLINEABLE darfrsAgentsInfo #-}
{-# DEPRECATED agentsInfo "Use generic-lens or generic-optics with 'agentsInfo' instead"  #-}

-- | Token to retrieve the next set of results. For example, if you specified 100 IDs for @DescribeAgentsRequest$agentIds@ but set @DescribeAgentsRequest$maxResults@ to 10, you received a set of 10 results along with this token. Use this token in the next query to retrieve the next set of 10.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darfrsNextToken :: Lens.Lens' DescribeAgentsResponse (Core.Maybe Types.NextToken)
darfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE darfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darfrsResponseStatus :: Lens.Lens' DescribeAgentsResponse Core.Int
darfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE darfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
