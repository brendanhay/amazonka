{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListOTAUpdates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists OTA updates.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListOTAUpdates
  ( -- * Creating a request
    ListOTAUpdates (..),
    mkListOTAUpdates,

    -- ** Request lenses
    lotauMaxResults,
    lotauNextToken,
    lotauOtaUpdateStatus,

    -- * Destructuring the response
    ListOTAUpdatesResponse (..),
    mkListOTAUpdatesResponse,

    -- ** Response lenses
    lotaurrsNextToken,
    lotaurrsOtaUpdates,
    lotaurrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListOTAUpdates' smart constructor.
data ListOTAUpdates = ListOTAUpdates'
  { -- | The maximum number of results to return at one time.
    maxResults :: Core.Maybe Core.Natural,
    -- | A token used to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The OTA update job status.
    otaUpdateStatus :: Core.Maybe Types.OTAUpdateStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListOTAUpdates' value with any optional fields omitted.
mkListOTAUpdates ::
  ListOTAUpdates
mkListOTAUpdates =
  ListOTAUpdates'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      otaUpdateStatus = Core.Nothing
    }

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotauMaxResults :: Lens.Lens' ListOTAUpdates (Core.Maybe Core.Natural)
lotauMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lotauMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A token used to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotauNextToken :: Lens.Lens' ListOTAUpdates (Core.Maybe Types.NextToken)
lotauNextToken = Lens.field @"nextToken"
{-# DEPRECATED lotauNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The OTA update job status.
--
-- /Note:/ Consider using 'otaUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotauOtaUpdateStatus :: Lens.Lens' ListOTAUpdates (Core.Maybe Types.OTAUpdateStatus)
lotauOtaUpdateStatus = Lens.field @"otaUpdateStatus"
{-# DEPRECATED lotauOtaUpdateStatus "Use generic-lens or generic-optics with 'otaUpdateStatus' instead." #-}

instance Core.AWSRequest ListOTAUpdates where
  type Rs ListOTAUpdates = ListOTAUpdatesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/otaUpdates",
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken)
            Core.<> (Core.toQueryValue "otaUpdateStatus" Core.<$> otaUpdateStatus),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOTAUpdatesResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "otaUpdates")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListOTAUpdates where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"otaUpdates" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListOTAUpdatesResponse' smart constructor.
data ListOTAUpdatesResponse = ListOTAUpdatesResponse'
  { -- | A token to use to get the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of OTA update jobs.
    otaUpdates :: Core.Maybe [Types.OTAUpdateSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListOTAUpdatesResponse' value with any optional fields omitted.
mkListOTAUpdatesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListOTAUpdatesResponse
mkListOTAUpdatesResponse responseStatus =
  ListOTAUpdatesResponse'
    { nextToken = Core.Nothing,
      otaUpdates = Core.Nothing,
      responseStatus
    }

-- | A token to use to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotaurrsNextToken :: Lens.Lens' ListOTAUpdatesResponse (Core.Maybe Types.NextToken)
lotaurrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lotaurrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of OTA update jobs.
--
-- /Note:/ Consider using 'otaUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotaurrsOtaUpdates :: Lens.Lens' ListOTAUpdatesResponse (Core.Maybe [Types.OTAUpdateSummary])
lotaurrsOtaUpdates = Lens.field @"otaUpdates"
{-# DEPRECATED lotaurrsOtaUpdates "Use generic-lens or generic-optics with 'otaUpdates' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotaurrsResponseStatus :: Lens.Lens' ListOTAUpdatesResponse Core.Int
lotaurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lotaurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
