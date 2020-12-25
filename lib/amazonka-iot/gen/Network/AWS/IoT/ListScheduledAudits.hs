{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListScheduledAudits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of your scheduled audits.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListScheduledAudits
  ( -- * Creating a request
    ListScheduledAudits (..),
    mkListScheduledAudits,

    -- ** Request lenses
    lsaMaxResults,
    lsaNextToken,

    -- * Destructuring the response
    ListScheduledAuditsResponse (..),
    mkListScheduledAuditsResponse,

    -- ** Response lenses
    lsarrsNextToken,
    lsarrsScheduledAudits,
    lsarrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListScheduledAudits' smart constructor.
data ListScheduledAudits = ListScheduledAudits'
  { -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListScheduledAudits' value with any optional fields omitted.
mkListScheduledAudits ::
  ListScheduledAudits
mkListScheduledAudits =
  ListScheduledAudits'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of results to return at one time. The default is 25.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsaMaxResults :: Lens.Lens' ListScheduledAudits (Core.Maybe Core.Natural)
lsaMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lsaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsaNextToken :: Lens.Lens' ListScheduledAudits (Core.Maybe Types.NextToken)
lsaNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListScheduledAudits where
  type Rs ListScheduledAudits = ListScheduledAuditsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/audit/scheduledaudits",
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListScheduledAuditsResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "scheduledAudits")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListScheduledAudits where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"scheduledAudits" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListScheduledAuditsResponse' smart constructor.
data ListScheduledAuditsResponse = ListScheduledAuditsResponse'
  { -- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The list of scheduled audits.
    scheduledAudits :: Core.Maybe [Types.ScheduledAuditMetadata],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListScheduledAuditsResponse' value with any optional fields omitted.
mkListScheduledAuditsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListScheduledAuditsResponse
mkListScheduledAuditsResponse responseStatus =
  ListScheduledAuditsResponse'
    { nextToken = Core.Nothing,
      scheduledAudits = Core.Nothing,
      responseStatus
    }

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsarrsNextToken :: Lens.Lens' ListScheduledAuditsResponse (Core.Maybe Types.NextToken)
lsarrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsarrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of scheduled audits.
--
-- /Note:/ Consider using 'scheduledAudits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsarrsScheduledAudits :: Lens.Lens' ListScheduledAuditsResponse (Core.Maybe [Types.ScheduledAuditMetadata])
lsarrsScheduledAudits = Lens.field @"scheduledAudits"
{-# DEPRECATED lsarrsScheduledAudits "Use generic-lens or generic-optics with 'scheduledAudits' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsarrsResponseStatus :: Lens.Lens' ListScheduledAuditsResponse Core.Int
lsarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lsarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
