{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeAssociationExecutionTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this API action to view information about a specific execution of a specific association.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeAssociationExecutionTargets
    (
    -- * Creating a request
      DescribeAssociationExecutionTargets (..)
    , mkDescribeAssociationExecutionTargets
    -- ** Request lenses
    , daetAssociationId
    , daetExecutionId
    , daetFilters
    , daetMaxResults
    , daetNextToken

    -- * Destructuring the response
    , DescribeAssociationExecutionTargetsResponse (..)
    , mkDescribeAssociationExecutionTargetsResponse
    -- ** Response lenses
    , daetrrsAssociationExecutionTargets
    , daetrrsNextToken
    , daetrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeAssociationExecutionTargets' smart constructor.
data DescribeAssociationExecutionTargets = DescribeAssociationExecutionTargets'
  { associationId :: Types.AssociationId
    -- ^ The association ID that includes the execution for which you want to view details.
  , executionId :: Types.ExecutionId
    -- ^ The execution ID for which you want to view details.
  , filters :: Core.Maybe (Core.NonEmpty Types.AssociationExecutionTargetsFilter)
    -- ^ Filters for the request. You can specify the following filters and values.
--
-- Status (EQUAL)
-- ResourceId (EQUAL)
-- ResourceType (EQUAL)
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token to start the list. Use this token to get the next set of results. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAssociationExecutionTargets' value with any optional fields omitted.
mkDescribeAssociationExecutionTargets
    :: Types.AssociationId -- ^ 'associationId'
    -> Types.ExecutionId -- ^ 'executionId'
    -> DescribeAssociationExecutionTargets
mkDescribeAssociationExecutionTargets associationId executionId
  = DescribeAssociationExecutionTargets'{associationId, executionId,
                                         filters = Core.Nothing, maxResults = Core.Nothing,
                                         nextToken = Core.Nothing}

-- | The association ID that includes the execution for which you want to view details.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daetAssociationId :: Lens.Lens' DescribeAssociationExecutionTargets Types.AssociationId
daetAssociationId = Lens.field @"associationId"
{-# INLINEABLE daetAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | The execution ID for which you want to view details.
--
-- /Note:/ Consider using 'executionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daetExecutionId :: Lens.Lens' DescribeAssociationExecutionTargets Types.ExecutionId
daetExecutionId = Lens.field @"executionId"
{-# INLINEABLE daetExecutionId #-}
{-# DEPRECATED executionId "Use generic-lens or generic-optics with 'executionId' instead"  #-}

-- | Filters for the request. You can specify the following filters and values.
--
-- Status (EQUAL)
-- ResourceId (EQUAL)
-- ResourceType (EQUAL)
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daetFilters :: Lens.Lens' DescribeAssociationExecutionTargets (Core.Maybe (Core.NonEmpty Types.AssociationExecutionTargetsFilter))
daetFilters = Lens.field @"filters"
{-# INLINEABLE daetFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daetMaxResults :: Lens.Lens' DescribeAssociationExecutionTargets (Core.Maybe Core.Natural)
daetMaxResults = Lens.field @"maxResults"
{-# INLINEABLE daetMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A token to start the list. Use this token to get the next set of results. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daetNextToken :: Lens.Lens' DescribeAssociationExecutionTargets (Core.Maybe Types.NextToken)
daetNextToken = Lens.field @"nextToken"
{-# INLINEABLE daetNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeAssociationExecutionTargets where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeAssociationExecutionTargets where
        toHeaders DescribeAssociationExecutionTargets{..}
          = Core.pure
              ("X-Amz-Target", "AmazonSSM.DescribeAssociationExecutionTargets")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeAssociationExecutionTargets where
        toJSON DescribeAssociationExecutionTargets{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AssociationId" Core..= associationId),
                  Core.Just ("ExecutionId" Core..= executionId),
                  ("Filters" Core..=) Core.<$> filters,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeAssociationExecutionTargets where
        type Rs DescribeAssociationExecutionTargets =
             DescribeAssociationExecutionTargetsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeAssociationExecutionTargetsResponse' Core.<$>
                   (x Core..:? "AssociationExecutionTargets") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeAssociationExecutionTargets where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"associationExecutionTargets" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeAssociationExecutionTargetsResponse' smart constructor.
data DescribeAssociationExecutionTargetsResponse = DescribeAssociationExecutionTargetsResponse'
  { associationExecutionTargets :: Core.Maybe [Types.AssociationExecutionTarget]
    -- ^ Information about the execution.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items to return. Use this token to get the next set of results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeAssociationExecutionTargetsResponse' value with any optional fields omitted.
mkDescribeAssociationExecutionTargetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAssociationExecutionTargetsResponse
mkDescribeAssociationExecutionTargetsResponse responseStatus
  = DescribeAssociationExecutionTargetsResponse'{associationExecutionTargets
                                                   = Core.Nothing,
                                                 nextToken = Core.Nothing, responseStatus}

-- | Information about the execution.
--
-- /Note:/ Consider using 'associationExecutionTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daetrrsAssociationExecutionTargets :: Lens.Lens' DescribeAssociationExecutionTargetsResponse (Core.Maybe [Types.AssociationExecutionTarget])
daetrrsAssociationExecutionTargets = Lens.field @"associationExecutionTargets"
{-# INLINEABLE daetrrsAssociationExecutionTargets #-}
{-# DEPRECATED associationExecutionTargets "Use generic-lens or generic-optics with 'associationExecutionTargets' instead"  #-}

-- | The token for the next set of items to return. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daetrrsNextToken :: Lens.Lens' DescribeAssociationExecutionTargetsResponse (Core.Maybe Types.NextToken)
daetrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE daetrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daetrrsResponseStatus :: Lens.Lens' DescribeAssociationExecutionTargetsResponse Core.Int
daetrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE daetrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
