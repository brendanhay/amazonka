{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.ListEventSources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can use this to see all the partner event sources that have been shared with your AWS account. For more information about partner event sources, see 'CreateEventBus' .
module Network.AWS.CloudWatchEvents.ListEventSources
  ( -- * Creating a request
    ListEventSources (..),
    mkListEventSources,

    -- ** Request lenses
    lesLimit,
    lesNamePrefix,
    lesNextToken,

    -- * Destructuring the response
    ListEventSourcesResponse (..),
    mkListEventSourcesResponse,

    -- ** Response lenses
    lesrrsEventSources,
    lesrrsNextToken,
    lesrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListEventSources' smart constructor.
data ListEventSources = ListEventSources'
  { -- | Specifying this limits the number of results returned by this operation. The operation also returns a NextToken which you can use in a subsequent operation to retrieve the next set of results.
    limit :: Core.Maybe Core.Natural,
    -- | Specifying this limits the results to only those partner event sources with names that start with the specified prefix.
    namePrefix :: Core.Maybe Types.NamePrefix,
    -- | The token returned by a previous call to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListEventSources' value with any optional fields omitted.
mkListEventSources ::
  ListEventSources
mkListEventSources =
  ListEventSources'
    { limit = Core.Nothing,
      namePrefix = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Specifying this limits the number of results returned by this operation. The operation also returns a NextToken which you can use in a subsequent operation to retrieve the next set of results.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesLimit :: Lens.Lens' ListEventSources (Core.Maybe Core.Natural)
lesLimit = Lens.field @"limit"
{-# DEPRECATED lesLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | Specifying this limits the results to only those partner event sources with names that start with the specified prefix.
--
-- /Note:/ Consider using 'namePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesNamePrefix :: Lens.Lens' ListEventSources (Core.Maybe Types.NamePrefix)
lesNamePrefix = Lens.field @"namePrefix"
{-# DEPRECATED lesNamePrefix "Use generic-lens or generic-optics with 'namePrefix' instead." #-}

-- | The token returned by a previous call to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesNextToken :: Lens.Lens' ListEventSources (Core.Maybe Types.NextToken)
lesNextToken = Lens.field @"nextToken"
{-# DEPRECATED lesNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListEventSources where
  toJSON ListEventSources {..} =
    Core.object
      ( Core.catMaybes
          [ ("Limit" Core..=) Core.<$> limit,
            ("NamePrefix" Core..=) Core.<$> namePrefix,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListEventSources where
  type Rs ListEventSources = ListEventSourcesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSEvents.ListEventSources")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEventSourcesResponse'
            Core.<$> (x Core..:? "EventSources")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListEventSourcesResponse' smart constructor.
data ListEventSourcesResponse = ListEventSourcesResponse'
  { -- | The list of event sources.
    eventSources :: Core.Maybe [Types.EventSource],
    -- | A token you can use in a subsequent operation to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListEventSourcesResponse' value with any optional fields omitted.
mkListEventSourcesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListEventSourcesResponse
mkListEventSourcesResponse responseStatus =
  ListEventSourcesResponse'
    { eventSources = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The list of event sources.
--
-- /Note:/ Consider using 'eventSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesrrsEventSources :: Lens.Lens' ListEventSourcesResponse (Core.Maybe [Types.EventSource])
lesrrsEventSources = Lens.field @"eventSources"
{-# DEPRECATED lesrrsEventSources "Use generic-lens or generic-optics with 'eventSources' instead." #-}

-- | A token you can use in a subsequent operation to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesrrsNextToken :: Lens.Lens' ListEventSourcesResponse (Core.Maybe Types.NextToken)
lesrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lesrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesrrsResponseStatus :: Lens.Lens' ListEventSourcesResponse Core.Int
lesrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lesrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
