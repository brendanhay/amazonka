{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListAuditSuppressions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your Device Defender audit listings. 
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListAuditSuppressions
    (
    -- * Creating a request
      ListAuditSuppressions (..)
    , mkListAuditSuppressions
    -- ** Request lenses
    , lasAscendingOrder
    , lasCheckName
    , lasMaxResults
    , lasNextToken
    , lasResourceIdentifier

    -- * Destructuring the response
    , ListAuditSuppressionsResponse (..)
    , mkListAuditSuppressionsResponse
    -- ** Response lenses
    , lasrrsNextToken
    , lasrrsSuppressions
    , lasrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAuditSuppressions' smart constructor.
data ListAuditSuppressions = ListAuditSuppressions'
  { ascendingOrder :: Core.Maybe Core.Bool
    -- ^ Determines whether suppressions are listed in ascending order by expiration date or not. If parameter isn't provided, @ascendingOrder=true@ . 
  , checkName :: Core.Maybe Types.AuditCheckName
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return at one time. The default is 25. 
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results. 
  , resourceIdentifier :: Core.Maybe Types.ResourceIdentifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAuditSuppressions' value with any optional fields omitted.
mkListAuditSuppressions
    :: ListAuditSuppressions
mkListAuditSuppressions
  = ListAuditSuppressions'{ascendingOrder = Core.Nothing,
                           checkName = Core.Nothing, maxResults = Core.Nothing,
                           nextToken = Core.Nothing, resourceIdentifier = Core.Nothing}

-- | Determines whether suppressions are listed in ascending order by expiration date or not. If parameter isn't provided, @ascendingOrder=true@ . 
--
-- /Note:/ Consider using 'ascendingOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasAscendingOrder :: Lens.Lens' ListAuditSuppressions (Core.Maybe Core.Bool)
lasAscendingOrder = Lens.field @"ascendingOrder"
{-# INLINEABLE lasAscendingOrder #-}
{-# DEPRECATED ascendingOrder "Use generic-lens or generic-optics with 'ascendingOrder' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'checkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasCheckName :: Lens.Lens' ListAuditSuppressions (Core.Maybe Types.AuditCheckName)
lasCheckName = Lens.field @"checkName"
{-# INLINEABLE lasCheckName #-}
{-# DEPRECATED checkName "Use generic-lens or generic-optics with 'checkName' instead"  #-}

-- | The maximum number of results to return at one time. The default is 25. 
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasMaxResults :: Lens.Lens' ListAuditSuppressions (Core.Maybe Core.Natural)
lasMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lasMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasNextToken :: Lens.Lens' ListAuditSuppressions (Core.Maybe Types.NextToken)
lasNextToken = Lens.field @"nextToken"
{-# INLINEABLE lasNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasResourceIdentifier :: Lens.Lens' ListAuditSuppressions (Core.Maybe Types.ResourceIdentifier)
lasResourceIdentifier = Lens.field @"resourceIdentifier"
{-# INLINEABLE lasResourceIdentifier #-}
{-# DEPRECATED resourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead"  #-}

instance Core.ToQuery ListAuditSuppressions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListAuditSuppressions where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON ListAuditSuppressions where
        toJSON ListAuditSuppressions{..}
          = Core.object
              (Core.catMaybes
                 [("ascendingOrder" Core..=) Core.<$> ascendingOrder,
                  ("checkName" Core..=) Core.<$> checkName,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("resourceIdentifier" Core..=) Core.<$> resourceIdentifier])

instance Core.AWSRequest ListAuditSuppressions where
        type Rs ListAuditSuppressions = ListAuditSuppressionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/audit/suppressions/list",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListAuditSuppressionsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "suppressions"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListAuditSuppressions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"suppressions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListAuditSuppressionsResponse' smart constructor.
data ListAuditSuppressionsResponse = ListAuditSuppressionsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ A token that can be used to retrieve the next set of results, or @null@ if there are no additional results. 
  , suppressions :: Core.Maybe [Types.AuditSuppression]
    -- ^ List of audit suppressions. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListAuditSuppressionsResponse' value with any optional fields omitted.
mkListAuditSuppressionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListAuditSuppressionsResponse
mkListAuditSuppressionsResponse responseStatus
  = ListAuditSuppressionsResponse'{nextToken = Core.Nothing,
                                   suppressions = Core.Nothing, responseStatus}

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasrrsNextToken :: Lens.Lens' ListAuditSuppressionsResponse (Core.Maybe Types.NextToken)
lasrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lasrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | List of audit suppressions. 
--
-- /Note:/ Consider using 'suppressions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasrrsSuppressions :: Lens.Lens' ListAuditSuppressionsResponse (Core.Maybe [Types.AuditSuppression])
lasrrsSuppressions = Lens.field @"suppressions"
{-# INLINEABLE lasrrsSuppressions #-}
{-# DEPRECATED suppressions "Use generic-lens or generic-optics with 'suppressions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lasrrsResponseStatus :: Lens.Lens' ListAuditSuppressionsResponse Core.Int
lasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
