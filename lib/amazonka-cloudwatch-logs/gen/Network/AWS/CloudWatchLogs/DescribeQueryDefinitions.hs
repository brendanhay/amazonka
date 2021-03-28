{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DescribeQueryDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns a paginated list of your saved CloudWatch Logs Insights query definitions.
--
-- You can use the @queryDefinitionNamePrefix@ parameter to limit the results to only the query definitions that have names that start with a certain string.
module Network.AWS.CloudWatchLogs.DescribeQueryDefinitions
    (
    -- * Creating a request
      DescribeQueryDefinitions (..)
    , mkDescribeQueryDefinitions
    -- ** Request lenses
    , dqdMaxResults
    , dqdNextToken
    , dqdQueryDefinitionNamePrefix

    -- * Destructuring the response
    , DescribeQueryDefinitionsResponse (..)
    , mkDescribeQueryDefinitionsResponse
    -- ** Response lenses
    , dqdrrsNextToken
    , dqdrrsQueryDefinitions
    , dqdrrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeQueryDefinitions' smart constructor.
data DescribeQueryDefinitions = DescribeQueryDefinitions'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ Limits the number of returned query definitions to the specified number.
  , nextToken :: Core.Maybe Types.NextToken
  , queryDefinitionNamePrefix :: Core.Maybe Types.QueryDefinitionName
    -- ^ Use this parameter to filter your results to only the query definitions that have names that start with the prefix you specify.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeQueryDefinitions' value with any optional fields omitted.
mkDescribeQueryDefinitions
    :: DescribeQueryDefinitions
mkDescribeQueryDefinitions
  = DescribeQueryDefinitions'{maxResults = Core.Nothing,
                              nextToken = Core.Nothing, queryDefinitionNamePrefix = Core.Nothing}

-- | Limits the number of returned query definitions to the specified number.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqdMaxResults :: Lens.Lens' DescribeQueryDefinitions (Core.Maybe Core.Natural)
dqdMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dqdMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqdNextToken :: Lens.Lens' DescribeQueryDefinitions (Core.Maybe Types.NextToken)
dqdNextToken = Lens.field @"nextToken"
{-# INLINEABLE dqdNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Use this parameter to filter your results to only the query definitions that have names that start with the prefix you specify.
--
-- /Note:/ Consider using 'queryDefinitionNamePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqdQueryDefinitionNamePrefix :: Lens.Lens' DescribeQueryDefinitions (Core.Maybe Types.QueryDefinitionName)
dqdQueryDefinitionNamePrefix = Lens.field @"queryDefinitionNamePrefix"
{-# INLINEABLE dqdQueryDefinitionNamePrefix #-}
{-# DEPRECATED queryDefinitionNamePrefix "Use generic-lens or generic-optics with 'queryDefinitionNamePrefix' instead"  #-}

instance Core.ToQuery DescribeQueryDefinitions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeQueryDefinitions where
        toHeaders DescribeQueryDefinitions{..}
          = Core.pure
              ("X-Amz-Target", "Logs_20140328.DescribeQueryDefinitions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeQueryDefinitions where
        toJSON DescribeQueryDefinitions{..}
          = Core.object
              (Core.catMaybes
                 [("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("queryDefinitionNamePrefix" Core..=) Core.<$>
                    queryDefinitionNamePrefix])

instance Core.AWSRequest DescribeQueryDefinitions where
        type Rs DescribeQueryDefinitions = DescribeQueryDefinitionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeQueryDefinitionsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "queryDefinitions"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeQueryDefinitionsResponse' smart constructor.
data DescribeQueryDefinitionsResponse = DescribeQueryDefinitionsResponse'
  { nextToken :: Core.Maybe Types.NextToken
  , queryDefinitions :: Core.Maybe [Types.QueryDefinition]
    -- ^ The list of query definitions that match your request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeQueryDefinitionsResponse' value with any optional fields omitted.
mkDescribeQueryDefinitionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeQueryDefinitionsResponse
mkDescribeQueryDefinitionsResponse responseStatus
  = DescribeQueryDefinitionsResponse'{nextToken = Core.Nothing,
                                      queryDefinitions = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqdrrsNextToken :: Lens.Lens' DescribeQueryDefinitionsResponse (Core.Maybe Types.NextToken)
dqdrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dqdrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The list of query definitions that match your request.
--
-- /Note:/ Consider using 'queryDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqdrrsQueryDefinitions :: Lens.Lens' DescribeQueryDefinitionsResponse (Core.Maybe [Types.QueryDefinition])
dqdrrsQueryDefinitions = Lens.field @"queryDefinitions"
{-# INLINEABLE dqdrrsQueryDefinitions #-}
{-# DEPRECATED queryDefinitions "Use generic-lens or generic-optics with 'queryDefinitions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqdrrsResponseStatus :: Lens.Lens' DescribeQueryDefinitionsResponse Core.Int
dqdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dqdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
