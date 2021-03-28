{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.ListPartnerEventSources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An SaaS partner can use this operation to list all the partner event source names that they have created. This operation is not used by AWS customers.
module Network.AWS.CloudWatchEvents.ListPartnerEventSources
    (
    -- * Creating a request
      ListPartnerEventSources (..)
    , mkListPartnerEventSources
    -- ** Request lenses
    , lpesNamePrefix
    , lpesLimit
    , lpesNextToken

    -- * Destructuring the response
    , ListPartnerEventSourcesResponse (..)
    , mkListPartnerEventSourcesResponse
    -- ** Response lenses
    , lpesrrsNextToken
    , lpesrrsPartnerEventSources
    , lpesrrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListPartnerEventSources' smart constructor.
data ListPartnerEventSources = ListPartnerEventSources'
  { namePrefix :: Types.NamePrefix
    -- ^ If you specify this, the results are limited to only those partner event sources that start with the string you specify.
  , limit :: Core.Maybe Core.Natural
    -- ^ pecifying this limits the number of results returned by this operation. The operation also returns a NextToken which you can use in a subsequent operation to retrieve the next set of results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token returned by a previous call to this operation. Specifying this retrieves the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPartnerEventSources' value with any optional fields omitted.
mkListPartnerEventSources
    :: Types.NamePrefix -- ^ 'namePrefix'
    -> ListPartnerEventSources
mkListPartnerEventSources namePrefix
  = ListPartnerEventSources'{namePrefix, limit = Core.Nothing,
                             nextToken = Core.Nothing}

-- | If you specify this, the results are limited to only those partner event sources that start with the string you specify.
--
-- /Note:/ Consider using 'namePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpesNamePrefix :: Lens.Lens' ListPartnerEventSources Types.NamePrefix
lpesNamePrefix = Lens.field @"namePrefix"
{-# INLINEABLE lpesNamePrefix #-}
{-# DEPRECATED namePrefix "Use generic-lens or generic-optics with 'namePrefix' instead"  #-}

-- | pecifying this limits the number of results returned by this operation. The operation also returns a NextToken which you can use in a subsequent operation to retrieve the next set of results.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpesLimit :: Lens.Lens' ListPartnerEventSources (Core.Maybe Core.Natural)
lpesLimit = Lens.field @"limit"
{-# INLINEABLE lpesLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The token returned by a previous call to this operation. Specifying this retrieves the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpesNextToken :: Lens.Lens' ListPartnerEventSources (Core.Maybe Types.NextToken)
lpesNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpesNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListPartnerEventSources where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListPartnerEventSources where
        toHeaders ListPartnerEventSources{..}
          = Core.pure ("X-Amz-Target", "AWSEvents.ListPartnerEventSources")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListPartnerEventSources where
        toJSON ListPartnerEventSources{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("NamePrefix" Core..= namePrefix),
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListPartnerEventSources where
        type Rs ListPartnerEventSources = ListPartnerEventSourcesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListPartnerEventSourcesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "PartnerEventSources"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListPartnerEventSourcesResponse' smart constructor.
data ListPartnerEventSourcesResponse = ListPartnerEventSourcesResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ A token you can use in a subsequent operation to retrieve the next set of results.
  , partnerEventSources :: Core.Maybe [Types.PartnerEventSource]
    -- ^ The list of partner event sources returned by the operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPartnerEventSourcesResponse' value with any optional fields omitted.
mkListPartnerEventSourcesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPartnerEventSourcesResponse
mkListPartnerEventSourcesResponse responseStatus
  = ListPartnerEventSourcesResponse'{nextToken = Core.Nothing,
                                     partnerEventSources = Core.Nothing, responseStatus}

-- | A token you can use in a subsequent operation to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpesrrsNextToken :: Lens.Lens' ListPartnerEventSourcesResponse (Core.Maybe Types.NextToken)
lpesrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpesrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The list of partner event sources returned by the operation.
--
-- /Note:/ Consider using 'partnerEventSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpesrrsPartnerEventSources :: Lens.Lens' ListPartnerEventSourcesResponse (Core.Maybe [Types.PartnerEventSource])
lpesrrsPartnerEventSources = Lens.field @"partnerEventSources"
{-# INLINEABLE lpesrrsPartnerEventSources #-}
{-# DEPRECATED partnerEventSources "Use generic-lens or generic-optics with 'partnerEventSources' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpesrrsResponseStatus :: Lens.Lens' ListPartnerEventSourcesResponse Core.Int
lpesrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lpesrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
