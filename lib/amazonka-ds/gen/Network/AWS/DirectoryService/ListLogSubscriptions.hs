{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.ListLogSubscriptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the active log subscriptions for the AWS account.
--
-- This operation returns paginated results.
module Network.AWS.DirectoryService.ListLogSubscriptions
  ( -- * Creating a request
    ListLogSubscriptions (..),
    mkListLogSubscriptions,

    -- ** Request lenses
    llsDirectoryId,
    llsLimit,
    llsNextToken,

    -- * Destructuring the response
    ListLogSubscriptionsResponse (..),
    mkListLogSubscriptionsResponse,

    -- ** Response lenses
    llsrrsLogSubscriptions,
    llsrrsNextToken,
    llsrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListLogSubscriptions' smart constructor.
data ListLogSubscriptions = ListLogSubscriptions'
  { -- | If a /DirectoryID/ is provided, lists only the log subscription associated with that directory. If no /DirectoryId/ is provided, lists all log subscriptions associated with your AWS account. If there are no log subscriptions for the AWS account or the directory, an empty list will be returned.
    directoryId :: Core.Maybe Types.DirectoryId,
    -- | The maximum number of items returned.
    limit :: Core.Maybe Core.Natural,
    -- | The token for the next set of items to return.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListLogSubscriptions' value with any optional fields omitted.
mkListLogSubscriptions ::
  ListLogSubscriptions
mkListLogSubscriptions =
  ListLogSubscriptions'
    { directoryId = Core.Nothing,
      limit = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | If a /DirectoryID/ is provided, lists only the log subscription associated with that directory. If no /DirectoryId/ is provided, lists all log subscriptions associated with your AWS account. If there are no log subscriptions for the AWS account or the directory, an empty list will be returned.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llsDirectoryId :: Lens.Lens' ListLogSubscriptions (Core.Maybe Types.DirectoryId)
llsDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED llsDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The maximum number of items returned.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llsLimit :: Lens.Lens' ListLogSubscriptions (Core.Maybe Core.Natural)
llsLimit = Lens.field @"limit"
{-# DEPRECATED llsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The token for the next set of items to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llsNextToken :: Lens.Lens' ListLogSubscriptions (Core.Maybe Types.NextToken)
llsNextToken = Lens.field @"nextToken"
{-# DEPRECATED llsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListLogSubscriptions where
  toJSON ListLogSubscriptions {..} =
    Core.object
      ( Core.catMaybes
          [ ("DirectoryId" Core..=) Core.<$> directoryId,
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListLogSubscriptions where
  type Rs ListLogSubscriptions = ListLogSubscriptionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DirectoryService_20150416.ListLogSubscriptions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLogSubscriptionsResponse'
            Core.<$> (x Core..:? "LogSubscriptions")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListLogSubscriptions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"logSubscriptions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListLogSubscriptionsResponse' smart constructor.
data ListLogSubscriptionsResponse = ListLogSubscriptionsResponse'
  { -- | A list of active 'LogSubscription' objects for calling the AWS account.
    logSubscriptions :: Core.Maybe [Types.LogSubscription],
    -- | The token for the next set of items to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListLogSubscriptionsResponse' value with any optional fields omitted.
mkListLogSubscriptionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListLogSubscriptionsResponse
mkListLogSubscriptionsResponse responseStatus =
  ListLogSubscriptionsResponse'
    { logSubscriptions = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of active 'LogSubscription' objects for calling the AWS account.
--
-- /Note:/ Consider using 'logSubscriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llsrrsLogSubscriptions :: Lens.Lens' ListLogSubscriptionsResponse (Core.Maybe [Types.LogSubscription])
llsrrsLogSubscriptions = Lens.field @"logSubscriptions"
{-# DEPRECATED llsrrsLogSubscriptions "Use generic-lens or generic-optics with 'logSubscriptions' instead." #-}

-- | The token for the next set of items to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llsrrsNextToken :: Lens.Lens' ListLogSubscriptionsResponse (Core.Maybe Types.NextToken)
llsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED llsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llsrrsResponseStatus :: Lens.Lens' ListLogSubscriptionsResponse Core.Int
llsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED llsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
