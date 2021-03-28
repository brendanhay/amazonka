{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.ListInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists summary information about the instances that you registered by using a specified service.
--
-- This operation returns paginated results.
module Network.AWS.Route53AutoNaming.ListInstances
    (
    -- * Creating a request
      ListInstances (..)
    , mkListInstances
    -- ** Request lenses
    , liServiceId
    , liMaxResults
    , liNextToken

    -- * Destructuring the response
    , ListInstancesResponse (..)
    , mkListInstancesResponse
    -- ** Response lenses
    , lirrsInstances
    , lirrsNextToken
    , lirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53AutoNaming.Types as Types

-- | /See:/ 'mkListInstances' smart constructor.
data ListInstances = ListInstances'
  { serviceId :: Types.ResourceId
    -- ^ The ID of the service that you want to list instances for.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of instances that you want AWS Cloud Map to return in the response to a @ListInstances@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 instances.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ For the first @ListInstances@ request, omit this value.
--
-- If more than @MaxResults@ instances match the specified criteria, you can submit another @ListInstances@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInstances' value with any optional fields omitted.
mkListInstances
    :: Types.ResourceId -- ^ 'serviceId'
    -> ListInstances
mkListInstances serviceId
  = ListInstances'{serviceId, maxResults = Core.Nothing,
                   nextToken = Core.Nothing}

-- | The ID of the service that you want to list instances for.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liServiceId :: Lens.Lens' ListInstances Types.ResourceId
liServiceId = Lens.field @"serviceId"
{-# INLINEABLE liServiceId #-}
{-# DEPRECATED serviceId "Use generic-lens or generic-optics with 'serviceId' instead"  #-}

-- | The maximum number of instances that you want AWS Cloud Map to return in the response to a @ListInstances@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 instances.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liMaxResults :: Lens.Lens' ListInstances (Core.Maybe Core.Natural)
liMaxResults = Lens.field @"maxResults"
{-# INLINEABLE liMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | For the first @ListInstances@ request, omit this value.
--
-- If more than @MaxResults@ instances match the specified criteria, you can submit another @ListInstances@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liNextToken :: Lens.Lens' ListInstances (Core.Maybe Types.NextToken)
liNextToken = Lens.field @"nextToken"
{-# INLINEABLE liNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListInstances where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListInstances where
        toHeaders ListInstances{..}
          = Core.pure
              ("X-Amz-Target", "Route53AutoNaming_v20170314.ListInstances")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListInstances where
        toJSON ListInstances{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ServiceId" Core..= serviceId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListInstances where
        type Rs ListInstances = ListInstancesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListInstancesResponse' Core.<$>
                   (x Core..:? "Instances") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListInstances where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"instances" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListInstancesResponse' smart constructor.
data ListInstancesResponse = ListInstancesResponse'
  { instances :: Core.Maybe [Types.InstanceSummary]
    -- ^ Summary information about the instances that are associated with the specified service.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If more than @MaxResults@ instances match the specified criteria, you can submit another @ListInstances@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInstancesResponse' value with any optional fields omitted.
mkListInstancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListInstancesResponse
mkListInstancesResponse responseStatus
  = ListInstancesResponse'{instances = Core.Nothing,
                           nextToken = Core.Nothing, responseStatus}

-- | Summary information about the instances that are associated with the specified service.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsInstances :: Lens.Lens' ListInstancesResponse (Core.Maybe [Types.InstanceSummary])
lirrsInstances = Lens.field @"instances"
{-# INLINEABLE lirrsInstances #-}
{-# DEPRECATED instances "Use generic-lens or generic-optics with 'instances' instead"  #-}

-- | If more than @MaxResults@ instances match the specified criteria, you can submit another @ListInstances@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsNextToken :: Lens.Lens' ListInstancesResponse (Core.Maybe Types.NextToken)
lirrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lirrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsResponseStatus :: Lens.Lens' ListInstancesResponse Core.Int
lirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
