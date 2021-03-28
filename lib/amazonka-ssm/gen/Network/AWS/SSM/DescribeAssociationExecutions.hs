{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeAssociationExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this API action to view all executions for a specific association ID. 
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeAssociationExecutions
    (
    -- * Creating a request
      DescribeAssociationExecutions (..)
    , mkDescribeAssociationExecutions
    -- ** Request lenses
    , daeAssociationId
    , daeFilters
    , daeMaxResults
    , daeNextToken

    -- * Destructuring the response
    , DescribeAssociationExecutionsResponse (..)
    , mkDescribeAssociationExecutionsResponse
    -- ** Response lenses
    , daerrsAssociationExecutions
    , daerrsNextToken
    , daerrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeAssociationExecutions' smart constructor.
data DescribeAssociationExecutions = DescribeAssociationExecutions'
  { associationId :: Types.AssociationId
    -- ^ The association ID for which you want to view execution history details.
  , filters :: Core.Maybe (Core.NonEmpty Types.AssociationExecutionFilter)
    -- ^ Filters for the request. You can specify the following filters and values.
--
-- ExecutionId (EQUAL)
-- Status (EQUAL)
-- CreatedTime (EQUAL, GREATER_THAN, LESS_THAN)
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token to start the list. Use this token to get the next set of results. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAssociationExecutions' value with any optional fields omitted.
mkDescribeAssociationExecutions
    :: Types.AssociationId -- ^ 'associationId'
    -> DescribeAssociationExecutions
mkDescribeAssociationExecutions associationId
  = DescribeAssociationExecutions'{associationId,
                                   filters = Core.Nothing, maxResults = Core.Nothing,
                                   nextToken = Core.Nothing}

-- | The association ID for which you want to view execution history details.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daeAssociationId :: Lens.Lens' DescribeAssociationExecutions Types.AssociationId
daeAssociationId = Lens.field @"associationId"
{-# INLINEABLE daeAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | Filters for the request. You can specify the following filters and values.
--
-- ExecutionId (EQUAL)
-- Status (EQUAL)
-- CreatedTime (EQUAL, GREATER_THAN, LESS_THAN)
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daeFilters :: Lens.Lens' DescribeAssociationExecutions (Core.Maybe (Core.NonEmpty Types.AssociationExecutionFilter))
daeFilters = Lens.field @"filters"
{-# INLINEABLE daeFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daeMaxResults :: Lens.Lens' DescribeAssociationExecutions (Core.Maybe Core.Natural)
daeMaxResults = Lens.field @"maxResults"
{-# INLINEABLE daeMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A token to start the list. Use this token to get the next set of results. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daeNextToken :: Lens.Lens' DescribeAssociationExecutions (Core.Maybe Types.NextToken)
daeNextToken = Lens.field @"nextToken"
{-# INLINEABLE daeNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeAssociationExecutions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeAssociationExecutions where
        toHeaders DescribeAssociationExecutions{..}
          = Core.pure
              ("X-Amz-Target", "AmazonSSM.DescribeAssociationExecutions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeAssociationExecutions where
        toJSON DescribeAssociationExecutions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AssociationId" Core..= associationId),
                  ("Filters" Core..=) Core.<$> filters,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeAssociationExecutions where
        type Rs DescribeAssociationExecutions =
             DescribeAssociationExecutionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeAssociationExecutionsResponse' Core.<$>
                   (x Core..:? "AssociationExecutions") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeAssociationExecutions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"associationExecutions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeAssociationExecutionsResponse' smart constructor.
data DescribeAssociationExecutionsResponse = DescribeAssociationExecutionsResponse'
  { associationExecutions :: Core.Maybe [Types.AssociationExecution]
    -- ^ A list of the executions for the specified association ID.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items to return. Use this token to get the next set of results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeAssociationExecutionsResponse' value with any optional fields omitted.
mkDescribeAssociationExecutionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAssociationExecutionsResponse
mkDescribeAssociationExecutionsResponse responseStatus
  = DescribeAssociationExecutionsResponse'{associationExecutions =
                                             Core.Nothing,
                                           nextToken = Core.Nothing, responseStatus}

-- | A list of the executions for the specified association ID.
--
-- /Note:/ Consider using 'associationExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daerrsAssociationExecutions :: Lens.Lens' DescribeAssociationExecutionsResponse (Core.Maybe [Types.AssociationExecution])
daerrsAssociationExecutions = Lens.field @"associationExecutions"
{-# INLINEABLE daerrsAssociationExecutions #-}
{-# DEPRECATED associationExecutions "Use generic-lens or generic-optics with 'associationExecutions' instead"  #-}

-- | The token for the next set of items to return. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daerrsNextToken :: Lens.Lens' DescribeAssociationExecutionsResponse (Core.Maybe Types.NextToken)
daerrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE daerrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daerrsResponseStatus :: Lens.Lens' DescribeAssociationExecutionsResponse Core.Int
daerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE daerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
