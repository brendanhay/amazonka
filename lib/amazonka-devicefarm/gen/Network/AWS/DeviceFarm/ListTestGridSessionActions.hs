{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListTestGridSessionActions (..),
    mkListTestGridSessionActions,

    -- ** Request lenses
    ltgsaSessionArn,
    ltgsaMaxResult,
    ltgsaNextToken,

    -- * Destructuring the response
    ListTestGridSessionActionsResponse (..),
    mkListTestGridSessionActionsResponse,

    -- ** Response lenses
    ltgsarrsActions,
    ltgsarrsNextToken,
    ltgsarrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTestGridSessionActions' smart constructor.
data ListTestGridSessionActions = ListTestGridSessionActions'
  { -- | The ARN of the session to retrieve.
    sessionArn :: Types.DeviceFarmArn,
    -- | The maximum number of sessions to return per response.
    maxResult :: Core.Maybe Core.Natural,
    -- | Pagination token.
    nextToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTestGridSessionActions' value with any optional fields omitted.
mkListTestGridSessionActions ::
  -- | 'sessionArn'
  Types.DeviceFarmArn ->
  ListTestGridSessionActions
mkListTestGridSessionActions sessionArn =
  ListTestGridSessionActions'
    { sessionArn,
      maxResult = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ARN of the session to retrieve.
--
-- /Note:/ Consider using 'sessionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsaSessionArn :: Lens.Lens' ListTestGridSessionActions Types.DeviceFarmArn
ltgsaSessionArn = Lens.field @"sessionArn"
{-# DEPRECATED ltgsaSessionArn "Use generic-lens or generic-optics with 'sessionArn' instead." #-}

-- | The maximum number of sessions to return per response.
--
-- /Note:/ Consider using 'maxResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsaMaxResult :: Lens.Lens' ListTestGridSessionActions (Core.Maybe Core.Natural)
ltgsaMaxResult = Lens.field @"maxResult"
{-# DEPRECATED ltgsaMaxResult "Use generic-lens or generic-optics with 'maxResult' instead." #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsaNextToken :: Lens.Lens' ListTestGridSessionActions (Core.Maybe Types.PaginationToken)
ltgsaNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltgsaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListTestGridSessionActions where
  toJSON ListTestGridSessionActions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("sessionArn" Core..= sessionArn),
            ("maxResult" Core..=) Core.<$> maxResult,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListTestGridSessionActions where
  type
    Rs ListTestGridSessionActions =
      ListTestGridSessionActionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DeviceFarm_20150623.ListTestGridSessionActions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTestGridSessionActionsResponse'
            Core.<$> (x Core..:? "actions")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListTestGridSessionActionsResponse' smart constructor.
data ListTestGridSessionActionsResponse = ListTestGridSessionActionsResponse'
  { -- | The action taken by the session.
    actions :: Core.Maybe [Types.TestGridSessionAction],
    -- | Pagination token.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListTestGridSessionActionsResponse' value with any optional fields omitted.
mkListTestGridSessionActionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListTestGridSessionActionsResponse
mkListTestGridSessionActionsResponse responseStatus =
  ListTestGridSessionActionsResponse'
    { actions = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The action taken by the session.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsarrsActions :: Lens.Lens' ListTestGridSessionActionsResponse (Core.Maybe [Types.TestGridSessionAction])
ltgsarrsActions = Lens.field @"actions"
{-# DEPRECATED ltgsarrsActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsarrsNextToken :: Lens.Lens' ListTestGridSessionActionsResponse (Core.Maybe Types.PaginationToken)
ltgsarrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltgsarrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgsarrsResponseStatus :: Lens.Lens' ListTestGridSessionActionsResponse Core.Int
ltgsarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltgsarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
