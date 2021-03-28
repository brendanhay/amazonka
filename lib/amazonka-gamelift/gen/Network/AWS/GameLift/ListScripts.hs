{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.ListScripts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves script records for all Realtime scripts that are associated with the AWS account in use. 
--
-- __Learn more__ 
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/realtime-intro.html Amazon GameLift Realtime Servers> 
-- __Related operations__ 
--
--     * 'CreateScript' 
--
--
--     * 'ListScripts' 
--
--
--     * 'DescribeScript' 
--
--
--     * 'UpdateScript' 
--
--
--     * 'DeleteScript' 
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.ListScripts
    (
    -- * Creating a request
      ListScripts (..)
    , mkListScripts
    -- ** Request lenses
    , lsLimit
    , lsNextToken

    -- * Destructuring the response
    , ListScriptsResponse (..)
    , mkListScriptsResponse
    -- ** Response lenses
    , lsrrsNextToken
    , lsrrsScripts
    , lsrrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListScripts' smart constructor.
data ListScripts = ListScripts'
  { limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
  , nextToken :: Core.Maybe Types.NonEmptyString
    -- ^ A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListScripts' value with any optional fields omitted.
mkListScripts
    :: ListScripts
mkListScripts
  = ListScripts'{limit = Core.Nothing, nextToken = Core.Nothing}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsLimit :: Lens.Lens' ListScripts (Core.Maybe Core.Natural)
lsLimit = Lens.field @"limit"
{-# INLINEABLE lsLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | A token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsNextToken :: Lens.Lens' ListScripts (Core.Maybe Types.NonEmptyString)
lsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListScripts where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListScripts where
        toHeaders ListScripts{..}
          = Core.pure ("X-Amz-Target", "GameLift.ListScripts") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListScripts where
        toJSON ListScripts{..}
          = Core.object
              (Core.catMaybes
                 [("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListScripts where
        type Rs ListScripts = ListScriptsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListScriptsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Scripts" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListScripts where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"scripts" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListScriptsResponse' smart constructor.
data ListScriptsResponse = ListScriptsResponse'
  { nextToken :: Core.Maybe Types.NonEmptyString
    -- ^ A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
  , scripts :: Core.Maybe [Types.Script]
    -- ^ A set of properties describing the requested script.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListScriptsResponse' value with any optional fields omitted.
mkListScriptsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListScriptsResponse
mkListScriptsResponse responseStatus
  = ListScriptsResponse'{nextToken = Core.Nothing,
                         scripts = Core.Nothing, responseStatus}

-- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsNextToken :: Lens.Lens' ListScriptsResponse (Core.Maybe Types.NonEmptyString)
lsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A set of properties describing the requested script.
--
-- /Note:/ Consider using 'scripts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsScripts :: Lens.Lens' ListScriptsResponse (Core.Maybe [Types.Script])
lsrrsScripts = Lens.field @"scripts"
{-# INLINEABLE lsrrsScripts #-}
{-# DEPRECATED scripts "Use generic-lens or generic-optics with 'scripts' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsResponseStatus :: Lens.Lens' ListScriptsResponse Core.Int
lsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
