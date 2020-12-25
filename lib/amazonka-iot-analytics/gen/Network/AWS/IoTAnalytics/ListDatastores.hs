{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.ListDatastores
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of data stores.
--
-- This operation returns paginated results.
module Network.AWS.IoTAnalytics.ListDatastores
  ( -- * Creating a request
    ListDatastores (..),
    mkListDatastores,

    -- ** Request lenses
    ldMaxResults,
    ldNextToken,

    -- * Destructuring the response
    ListDatastoresResponse (..),
    mkListDatastoresResponse,

    -- ** Response lenses
    ldrrsDatastoreSummaries,
    ldrrsNextToken,
    ldrrsResponseStatus,
  )
where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDatastores' smart constructor.
data ListDatastores = ListDatastores'
  { -- | The maximum number of results to return in this request.
    --
    -- The default value is 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDatastores' value with any optional fields omitted.
mkListDatastores ::
  ListDatastores
mkListDatastores =
  ListDatastores'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of results to return in this request.
--
-- The default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMaxResults :: Lens.Lens' ListDatastores (Core.Maybe Core.Natural)
ldMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ldMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldNextToken :: Lens.Lens' ListDatastores (Core.Maybe Types.NextToken)
ldNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListDatastores where
  type Rs ListDatastores = ListDatastoresResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/datastores",
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDatastoresResponse'
            Core.<$> (x Core..:? "datastoreSummaries")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListDatastores where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"datastoreSummaries" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListDatastoresResponse' smart constructor.
data ListDatastoresResponse = ListDatastoresResponse'
  { -- | A list of @DatastoreSummary@ objects.
    datastoreSummaries :: Core.Maybe [Types.DatastoreSummary],
    -- | The token to retrieve the next set of results, or @null@ if there are no more results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListDatastoresResponse' value with any optional fields omitted.
mkListDatastoresResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDatastoresResponse
mkListDatastoresResponse responseStatus =
  ListDatastoresResponse'
    { datastoreSummaries = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of @DatastoreSummary@ objects.
--
-- /Note:/ Consider using 'datastoreSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsDatastoreSummaries :: Lens.Lens' ListDatastoresResponse (Core.Maybe [Types.DatastoreSummary])
ldrrsDatastoreSummaries = Lens.field @"datastoreSummaries"
{-# DEPRECATED ldrrsDatastoreSummaries "Use generic-lens or generic-optics with 'datastoreSummaries' instead." #-}

-- | The token to retrieve the next set of results, or @null@ if there are no more results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsNextToken :: Lens.Lens' ListDatastoresResponse (Core.Maybe Types.NextToken)
ldrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsResponseStatus :: Lens.Lens' ListDatastoresResponse Core.Int
ldrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
