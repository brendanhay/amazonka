{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetInsight
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the summary information of an insight. This includes impact to clients and root cause services, the top anomalous services, the category, the state of the insight, and the start and end time of the insight.
module Network.AWS.XRay.GetInsight
  ( -- * Creating a request
    GetInsight (..),
    mkGetInsight,

    -- ** Request lenses
    giInsightId,

    -- * Destructuring the response
    GetInsightResponse (..),
    mkGetInsightResponse,

    -- ** Response lenses
    girrsInsight,
    girrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkGetInsight' smart constructor.
newtype GetInsight = GetInsight'
  { -- | The insight's unique identifier. Use the GetInsightSummaries action to retrieve an InsightId.
    insightId :: Types.InsightId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetInsight' value with any optional fields omitted.
mkGetInsight ::
  -- | 'insightId'
  Types.InsightId ->
  GetInsight
mkGetInsight insightId = GetInsight' {insightId}

-- | The insight's unique identifier. Use the GetInsightSummaries action to retrieve an InsightId.
--
-- /Note:/ Consider using 'insightId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giInsightId :: Lens.Lens' GetInsight Types.InsightId
giInsightId = Lens.field @"insightId"
{-# DEPRECATED giInsightId "Use generic-lens or generic-optics with 'insightId' instead." #-}

instance Core.FromJSON GetInsight where
  toJSON GetInsight {..} =
    Core.object
      (Core.catMaybes [Core.Just ("InsightId" Core..= insightId)])

instance Core.AWSRequest GetInsight where
  type Rs GetInsight = GetInsightResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/Insight",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInsightResponse'
            Core.<$> (x Core..:? "Insight") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetInsightResponse' smart constructor.
data GetInsightResponse = GetInsightResponse'
  { -- | The summary information of an insight.
    insight :: Core.Maybe Types.Insight,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetInsightResponse' value with any optional fields omitted.
mkGetInsightResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetInsightResponse
mkGetInsightResponse responseStatus =
  GetInsightResponse' {insight = Core.Nothing, responseStatus}

-- | The summary information of an insight.
--
-- /Note:/ Consider using 'insight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsInsight :: Lens.Lens' GetInsightResponse (Core.Maybe Types.Insight)
girrsInsight = Lens.field @"insight"
{-# DEPRECATED girrsInsight "Use generic-lens or generic-optics with 'insight' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsResponseStatus :: Lens.Lens' GetInsightResponse Core.Int
girrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED girrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
