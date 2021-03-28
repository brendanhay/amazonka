{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.ListProtocolsLists
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @ProtocolsListDataSummary@ objects.
module Network.AWS.FMS.ListProtocolsLists
    (
    -- * Creating a request
      ListProtocolsLists (..)
    , mkListProtocolsLists
    -- ** Request lenses
    , lplMaxResults
    , lplDefaultLists
    , lplNextToken

    -- * Destructuring the response
    , ListProtocolsListsResponse (..)
    , mkListProtocolsListsResponse
    -- ** Response lenses
    , lplrrsNextToken
    , lplrrsProtocolsLists
    , lplrrsResponseStatus
    ) where

import qualified Network.AWS.FMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListProtocolsLists' smart constructor.
data ListProtocolsLists = ListProtocolsLists'
  { maxResults :: Core.Natural
    -- ^ The maximum number of objects that you want AWS Firewall Manager to return for this request. If more objects are available, in the response, AWS Firewall Manager provides a @NextToken@ value that you can use in a subsequent call to get the next batch of objects.
--
-- If you don't specify this, AWS Firewall Manager returns all available objects.
  , defaultLists :: Core.Maybe Core.Bool
    -- ^ Specifies whether the lists to retrieve are default lists owned by AWS Firewall Manager.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. For all but the first request, you provide the token returned by the prior request in the request parameters, to retrieve the next batch of objects.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListProtocolsLists' value with any optional fields omitted.
mkListProtocolsLists
    :: Core.Natural -- ^ 'maxResults'
    -> ListProtocolsLists
mkListProtocolsLists maxResults
  = ListProtocolsLists'{maxResults, defaultLists = Core.Nothing,
                        nextToken = Core.Nothing}

-- | The maximum number of objects that you want AWS Firewall Manager to return for this request. If more objects are available, in the response, AWS Firewall Manager provides a @NextToken@ value that you can use in a subsequent call to get the next batch of objects.
--
-- If you don't specify this, AWS Firewall Manager returns all available objects.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lplMaxResults :: Lens.Lens' ListProtocolsLists Core.Natural
lplMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lplMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Specifies whether the lists to retrieve are default lists owned by AWS Firewall Manager.
--
-- /Note:/ Consider using 'defaultLists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lplDefaultLists :: Lens.Lens' ListProtocolsLists (Core.Maybe Core.Bool)
lplDefaultLists = Lens.field @"defaultLists"
{-# INLINEABLE lplDefaultLists #-}
{-# DEPRECATED defaultLists "Use generic-lens or generic-optics with 'defaultLists' instead"  #-}

-- | If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. For all but the first request, you provide the token returned by the prior request in the request parameters, to retrieve the next batch of objects.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lplNextToken :: Lens.Lens' ListProtocolsLists (Core.Maybe Types.PaginationToken)
lplNextToken = Lens.field @"nextToken"
{-# INLINEABLE lplNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListProtocolsLists where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListProtocolsLists where
        toHeaders ListProtocolsLists{..}
          = Core.pure ("X-Amz-Target", "AWSFMS_20180101.ListProtocolsLists")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListProtocolsLists where
        toJSON ListProtocolsLists{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("MaxResults" Core..= maxResults),
                  ("DefaultLists" Core..=) Core.<$> defaultLists,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListProtocolsLists where
        type Rs ListProtocolsLists = ListProtocolsListsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListProtocolsListsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "ProtocolsLists"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListProtocolsListsResponse' smart constructor.
data ListProtocolsListsResponse = ListProtocolsListsResponse'
  { nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. You can use this token in subsequent requests to retrieve the next batch of objects.
  , protocolsLists :: Core.Maybe [Types.ProtocolsListDataSummary]
    -- ^ An array of @ProtocolsListDataSummary@ objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListProtocolsListsResponse' value with any optional fields omitted.
mkListProtocolsListsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListProtocolsListsResponse
mkListProtocolsListsResponse responseStatus
  = ListProtocolsListsResponse'{nextToken = Core.Nothing,
                                protocolsLists = Core.Nothing, responseStatus}

-- | If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. You can use this token in subsequent requests to retrieve the next batch of objects.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lplrrsNextToken :: Lens.Lens' ListProtocolsListsResponse (Core.Maybe Types.PaginationToken)
lplrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lplrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | An array of @ProtocolsListDataSummary@ objects.
--
-- /Note:/ Consider using 'protocolsLists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lplrrsProtocolsLists :: Lens.Lens' ListProtocolsListsResponse (Core.Maybe [Types.ProtocolsListDataSummary])
lplrrsProtocolsLists = Lens.field @"protocolsLists"
{-# INLINEABLE lplrrsProtocolsLists #-}
{-# DEPRECATED protocolsLists "Use generic-lens or generic-optics with 'protocolsLists' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lplrrsResponseStatus :: Lens.Lens' ListProtocolsListsResponse Core.Int
lplrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lplrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
