{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.GetReportGroupTrend
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.CodeBuild.GetReportGroupTrend
  ( -- * Creating a request
    GetReportGroupTrend (..),
    mkGetReportGroupTrend,

    -- ** Request lenses
    grgtReportGroupArn,
    grgtTrendField,
    grgtNumOfReports,

    -- * Destructuring the response
    GetReportGroupTrendResponse (..),
    mkGetReportGroupTrendResponse,

    -- ** Response lenses
    grgtrrsRawData,
    grgtrrsStats,
    grgtrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetReportGroupTrend' smart constructor.
data GetReportGroupTrend = GetReportGroupTrend'
  { reportGroupArn :: Types.NonEmptyString,
    trendField :: Types.ReportGroupTrendFieldType,
    numOfReports :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetReportGroupTrend' value with any optional fields omitted.
mkGetReportGroupTrend ::
  -- | 'reportGroupArn'
  Types.NonEmptyString ->
  -- | 'trendField'
  Types.ReportGroupTrendFieldType ->
  GetReportGroupTrend
mkGetReportGroupTrend reportGroupArn trendField =
  GetReportGroupTrend'
    { reportGroupArn,
      trendField,
      numOfReports = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'reportGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgtReportGroupArn :: Lens.Lens' GetReportGroupTrend Types.NonEmptyString
grgtReportGroupArn = Lens.field @"reportGroupArn"
{-# DEPRECATED grgtReportGroupArn "Use generic-lens or generic-optics with 'reportGroupArn' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'trendField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgtTrendField :: Lens.Lens' GetReportGroupTrend Types.ReportGroupTrendFieldType
grgtTrendField = Lens.field @"trendField"
{-# DEPRECATED grgtTrendField "Use generic-lens or generic-optics with 'trendField' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'numOfReports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgtNumOfReports :: Lens.Lens' GetReportGroupTrend (Core.Maybe Core.Natural)
grgtNumOfReports = Lens.field @"numOfReports"
{-# DEPRECATED grgtNumOfReports "Use generic-lens or generic-optics with 'numOfReports' instead." #-}

instance Core.FromJSON GetReportGroupTrend where
  toJSON GetReportGroupTrend {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("reportGroupArn" Core..= reportGroupArn),
            Core.Just ("trendField" Core..= trendField),
            ("numOfReports" Core..=) Core.<$> numOfReports
          ]
      )

instance Core.AWSRequest GetReportGroupTrend where
  type Rs GetReportGroupTrend = GetReportGroupTrendResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeBuild_20161006.GetReportGroupTrend")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetReportGroupTrendResponse'
            Core.<$> (x Core..:? "rawData")
            Core.<*> (x Core..:? "stats")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetReportGroupTrendResponse' smart constructor.
data GetReportGroupTrendResponse = GetReportGroupTrendResponse'
  { rawData :: Core.Maybe [Types.ReportWithRawData],
    stats :: Core.Maybe Types.ReportGroupTrendStats,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetReportGroupTrendResponse' value with any optional fields omitted.
mkGetReportGroupTrendResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetReportGroupTrendResponse
mkGetReportGroupTrendResponse responseStatus =
  GetReportGroupTrendResponse'
    { rawData = Core.Nothing,
      stats = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'rawData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgtrrsRawData :: Lens.Lens' GetReportGroupTrendResponse (Core.Maybe [Types.ReportWithRawData])
grgtrrsRawData = Lens.field @"rawData"
{-# DEPRECATED grgtrrsRawData "Use generic-lens or generic-optics with 'rawData' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'stats' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgtrrsStats :: Lens.Lens' GetReportGroupTrendResponse (Core.Maybe Types.ReportGroupTrendStats)
grgtrrsStats = Lens.field @"stats"
{-# DEPRECATED grgtrrsStats "Use generic-lens or generic-optics with 'stats' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgtrrsResponseStatus :: Lens.Lens' GetReportGroupTrendResponse Core.Int
grgtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grgtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
