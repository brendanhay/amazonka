{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListTestGridSessionActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the actions taken in a 'TestGridSession' .
module Network.AWS.DeviceFarm.ListTestGridSessionActions
    (
    -- * Creating a request
      ListTestGridSessionActions (..)
    , mkListTestGridSessionActions
    -- ** Request lenses
    , ltgsaSessionArn
    , ltgsaMaxResult
    , ltgsaNextToken

    -- * Destructuring the response
    , ListTestGridSessionActionsResponse (..)
    , mkListTestGridSessionActionsResponse
    -- ** Response lenses
    , ltgsarrsActions
    , ltgsarrsNextToken
    , ltgsarrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTestGridSessionActions' smart constructor.
data ListTestGridSessionActions = ListTestGridSessionActions'
  { sessionArn :: Types.DeviceFarmArn
    -- ^ The ARN of the session to retrieve.
  , maxResult :: Core.Maybe Core.Natural
    -- ^ The maximum number of sessions to return per response.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ Pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTestGridSessionActions' value with any optional fields omitted.
mkListTestGridSessionActions
    :: Types.DeviceFarmArn -- ^ 'sessionArn'
    -> ListTestGridSessionActions
mkListTestGridSessionActions sessionArn
  = ListTestGridSessionActions'{sessionArn, maxResult = Core.Nothing,
                                nextToken = Core.Nothing}

-- | The ARN of the session to retrieve.
--
-- /Note:/ Consider using 'sessionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsaSessionArn :: Lens.Lens' ListTestGridSessionActions Types.DeviceFarmArn
ltgsaSessionArn = Lens.field @"sessionArn"
{-# INLINEABLE ltgsaSessionArn #-}
{-# DEPRECATED sessionArn "Use generic-lens or generic-optics with 'sessionArn' instead"  #-}

-- | The maximum number of sessions to return per response.
--
-- /Note:/ Consider using 'maxResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsaMaxResult :: Lens.Lens' ListTestGridSessionActions (Core.Maybe Core.Natural)
ltgsaMaxResult = Lens.field @"maxResult"
{-# INLINEABLE ltgsaMaxResult #-}
{-# DEPRECATED maxResult "Use generic-lens or generic-optics with 'maxResult' instead"  #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsaNextToken :: Lens.Lens' ListTestGridSessionActions (Core.Maybe Types.PaginationToken)
ltgsaNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltgsaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListTestGridSessionActions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTestGridSessionActions where
        toHeaders ListTestGridSessionActions{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.ListTestGridSessionActions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListTestGridSessionActions where
        toJSON ListTestGridSessionActions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("sessionArn" Core..= sessionArn),
                  ("maxResult" Core..=) Core.<$> maxResult,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListTestGridSessionActions where
        type Rs ListTestGridSessionActions =
             ListTestGridSessionActionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTestGridSessionActionsResponse' Core.<$>
                   (x Core..:? "actions") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListTestGridSessionActionsResponse' smart constructor.
data ListTestGridSessionActionsResponse = ListTestGridSessionActionsResponse'
  { actions :: Core.Maybe [Types.TestGridSessionAction]
    -- ^ The action taken by the session.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ Pagination token.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListTestGridSessionActionsResponse' value with any optional fields omitted.
mkListTestGridSessionActionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTestGridSessionActionsResponse
mkListTestGridSessionActionsResponse responseStatus
  = ListTestGridSessionActionsResponse'{actions = Core.Nothing,
                                        nextToken = Core.Nothing, responseStatus}

-- | The action taken by the session.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsarrsActions :: Lens.Lens' ListTestGridSessionActionsResponse (Core.Maybe [Types.TestGridSessionAction])
ltgsarrsActions = Lens.field @"actions"
{-# INLINEABLE ltgsarrsActions #-}
{-# DEPRECATED actions "Use generic-lens or generic-optics with 'actions' instead"  #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsarrsNextToken :: Lens.Lens' ListTestGridSessionActionsResponse (Core.Maybe Types.PaginationToken)
ltgsarrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltgsarrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsarrsResponseStatus :: Lens.Lens' ListTestGridSessionActionsResponse Core.Int
ltgsarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltgsarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
