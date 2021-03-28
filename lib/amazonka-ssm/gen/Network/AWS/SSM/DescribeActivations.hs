{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeActivations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes details about the activation, such as the date and time the activation was created, its expiration date, the IAM role assigned to the instances in the activation, and the number of instances registered by using this activation.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeActivations
    (
    -- * Creating a request
      DescribeActivations (..)
    , mkDescribeActivations
    -- ** Request lenses
    , daFilters
    , daMaxResults
    , daNextToken

    -- * Destructuring the response
    , DescribeActivationsResponse (..)
    , mkDescribeActivationsResponse
    -- ** Response lenses
    , darrsActivationList
    , darrsNextToken
    , darrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeActivations' smart constructor.
data DescribeActivations = DescribeActivations'
  { filters :: Core.Maybe [Types.DescribeActivationsFilter]
    -- ^ A filter to view information about your activations.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token to start the list. Use this token to get the next set of results. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeActivations' value with any optional fields omitted.
mkDescribeActivations
    :: DescribeActivations
mkDescribeActivations
  = DescribeActivations'{filters = Core.Nothing,
                         maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | A filter to view information about your activations.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daFilters :: Lens.Lens' DescribeActivations (Core.Maybe [Types.DescribeActivationsFilter])
daFilters = Lens.field @"filters"
{-# INLINEABLE daFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daMaxResults :: Lens.Lens' DescribeActivations (Core.Maybe Core.Natural)
daMaxResults = Lens.field @"maxResults"
{-# INLINEABLE daMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A token to start the list. Use this token to get the next set of results. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daNextToken :: Lens.Lens' DescribeActivations (Core.Maybe Types.NextToken)
daNextToken = Lens.field @"nextToken"
{-# INLINEABLE daNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeActivations where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeActivations where
        toHeaders DescribeActivations{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.DescribeActivations")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeActivations where
        toJSON DescribeActivations{..}
          = Core.object
              (Core.catMaybes
                 [("Filters" Core..=) Core.<$> filters,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeActivations where
        type Rs DescribeActivations = DescribeActivationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeActivationsResponse' Core.<$>
                   (x Core..:? "ActivationList") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeActivations where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"activationList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeActivationsResponse' smart constructor.
data DescribeActivationsResponse = DescribeActivationsResponse'
  { activationList :: Core.Maybe [Types.Activation]
    -- ^ A list of activations for your AWS account.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items to return. Use this token to get the next set of results. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeActivationsResponse' value with any optional fields omitted.
mkDescribeActivationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeActivationsResponse
mkDescribeActivationsResponse responseStatus
  = DescribeActivationsResponse'{activationList = Core.Nothing,
                                 nextToken = Core.Nothing, responseStatus}

-- | A list of activations for your AWS account.
--
-- /Note:/ Consider using 'activationList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsActivationList :: Lens.Lens' DescribeActivationsResponse (Core.Maybe [Types.Activation])
darrsActivationList = Lens.field @"activationList"
{-# INLINEABLE darrsActivationList #-}
{-# DEPRECATED activationList "Use generic-lens or generic-optics with 'activationList' instead"  #-}

-- | The token for the next set of items to return. Use this token to get the next set of results. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsNextToken :: Lens.Lens' DescribeActivationsResponse (Core.Maybe Types.NextToken)
darrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE darrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DescribeActivationsResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE darrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
