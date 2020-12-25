{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.ListQueues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a JSON array of up to twenty of your queues. This will return the queues themselves, not just a list of them. To retrieve the next twenty queues, use the nextToken string returned with the array.
--
-- This operation returns paginated results.
module Network.AWS.MediaConvert.ListQueues
  ( -- * Creating a request
    ListQueues (..),
    mkListQueues,

    -- ** Request lenses
    lqListBy,
    lqMaxResults,
    lqNextToken,
    lqOrder,

    -- * Destructuring the response
    ListQueuesResponse (..),
    mkListQueuesResponse,

    -- ** Response lenses
    lqrrsNextToken,
    lqrrsQueues,
    lqrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListQueues' smart constructor.
data ListQueues = ListQueues'
  { -- | Optional. When you request a list of queues, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by creation date.
    listBy :: Core.Maybe Types.QueueListBy,
    -- | Optional. Number of queues, up to twenty, that will be returned at one time.
    maxResults :: Core.Maybe Core.Natural,
    -- | Use this string, provided with the response to a previous request, to request the next batch of queues.
    nextToken :: Core.Maybe Core.Text,
    -- | Optional. When you request lists of resources, you can specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.
    order :: Core.Maybe Types.Order
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListQueues' value with any optional fields omitted.
mkListQueues ::
  ListQueues
mkListQueues =
  ListQueues'
    { listBy = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      order = Core.Nothing
    }

-- | Optional. When you request a list of queues, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by creation date.
--
-- /Note:/ Consider using 'listBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqListBy :: Lens.Lens' ListQueues (Core.Maybe Types.QueueListBy)
lqListBy = Lens.field @"listBy"
{-# DEPRECATED lqListBy "Use generic-lens or generic-optics with 'listBy' instead." #-}

-- | Optional. Number of queues, up to twenty, that will be returned at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqMaxResults :: Lens.Lens' ListQueues (Core.Maybe Core.Natural)
lqMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lqMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Use this string, provided with the response to a previous request, to request the next batch of queues.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqNextToken :: Lens.Lens' ListQueues (Core.Maybe Core.Text)
lqNextToken = Lens.field @"nextToken"
{-# DEPRECATED lqNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Optional. When you request lists of resources, you can specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.
--
-- /Note:/ Consider using 'order' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqOrder :: Lens.Lens' ListQueues (Core.Maybe Types.Order)
lqOrder = Lens.field @"order"
{-# DEPRECATED lqOrder "Use generic-lens or generic-optics with 'order' instead." #-}

instance Core.AWSRequest ListQueues where
  type Rs ListQueues = ListQueuesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/2017-08-29/queues",
        Core._rqQuery =
          Core.toQueryValue "listBy" Core.<$> listBy
            Core.<> (Core.toQueryValue "maxResults" Core.<$> maxResults)
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken)
            Core.<> (Core.toQueryValue "order" Core.<$> order),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListQueuesResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "queues")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListQueues where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"queues" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListQueuesResponse' smart constructor.
data ListQueuesResponse = ListQueuesResponse'
  { -- | Use this string to request the next batch of queues.
    nextToken :: Core.Maybe Core.Text,
    -- | List of queues.
    queues :: Core.Maybe [Types.Queue],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListQueuesResponse' value with any optional fields omitted.
mkListQueuesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListQueuesResponse
mkListQueuesResponse responseStatus =
  ListQueuesResponse'
    { nextToken = Core.Nothing,
      queues = Core.Nothing,
      responseStatus
    }

-- | Use this string to request the next batch of queues.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrrsNextToken :: Lens.Lens' ListQueuesResponse (Core.Maybe Core.Text)
lqrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lqrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | List of queues.
--
-- /Note:/ Consider using 'queues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrrsQueues :: Lens.Lens' ListQueuesResponse (Core.Maybe [Types.Queue])
lqrrsQueues = Lens.field @"queues"
{-# DEPRECATED lqrrsQueues "Use generic-lens or generic-optics with 'queues' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrrsResponseStatus :: Lens.Lens' ListQueuesResponse Core.Int
lqrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lqrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
