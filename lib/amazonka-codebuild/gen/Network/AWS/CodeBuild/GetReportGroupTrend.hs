{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetReportGroupTrend (..)
    , mkGetReportGroupTrend
    -- ** Request lenses
    , grgtReportGroupArn
    , grgtTrendField
    , grgtNumOfReports

    -- * Destructuring the response
    , GetReportGroupTrendResponse (..)
    , mkGetReportGroupTrendResponse
    -- ** Response lenses
    , grgtrrsRawData
    , grgtrrsStats
    , grgtrrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetReportGroupTrend' smart constructor.
data GetReportGroupTrend = GetReportGroupTrend'
  { reportGroupArn :: Types.NonEmptyString
  , trendField :: Types.ReportGroupTrendFieldType
  , numOfReports :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetReportGroupTrend' value with any optional fields omitted.
mkGetReportGroupTrend
    :: Types.NonEmptyString -- ^ 'reportGroupArn'
    -> Types.ReportGroupTrendFieldType -- ^ 'trendField'
    -> GetReportGroupTrend
mkGetReportGroupTrend reportGroupArn trendField
  = GetReportGroupTrend'{reportGroupArn, trendField,
                         numOfReports = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'reportGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgtReportGroupArn :: Lens.Lens' GetReportGroupTrend Types.NonEmptyString
grgtReportGroupArn = Lens.field @"reportGroupArn"
{-# INLINEABLE grgtReportGroupArn #-}
{-# DEPRECATED reportGroupArn "Use generic-lens or generic-optics with 'reportGroupArn' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'trendField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgtTrendField :: Lens.Lens' GetReportGroupTrend Types.ReportGroupTrendFieldType
grgtTrendField = Lens.field @"trendField"
{-# INLINEABLE grgtTrendField #-}
{-# DEPRECATED trendField "Use generic-lens or generic-optics with 'trendField' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'numOfReports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgtNumOfReports :: Lens.Lens' GetReportGroupTrend (Core.Maybe Core.Natural)
grgtNumOfReports = Lens.field @"numOfReports"
{-# INLINEABLE grgtNumOfReports #-}
{-# DEPRECATED numOfReports "Use generic-lens or generic-optics with 'numOfReports' instead"  #-}

instance Core.ToQuery GetReportGroupTrend where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetReportGroupTrend where
        toHeaders GetReportGroupTrend{..}
          = Core.pure
              ("X-Amz-Target", "CodeBuild_20161006.GetReportGroupTrend")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetReportGroupTrend where
        toJSON GetReportGroupTrend{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("reportGroupArn" Core..= reportGroupArn),
                  Core.Just ("trendField" Core..= trendField),
                  ("numOfReports" Core..=) Core.<$> numOfReports])

instance Core.AWSRequest GetReportGroupTrend where
        type Rs GetReportGroupTrend = GetReportGroupTrendResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetReportGroupTrendResponse' Core.<$>
                   (x Core..:? "rawData") Core.<*> x Core..:? "stats" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetReportGroupTrendResponse' smart constructor.
data GetReportGroupTrendResponse = GetReportGroupTrendResponse'
  { rawData :: Core.Maybe [Types.ReportWithRawData]
  , stats :: Core.Maybe Types.ReportGroupTrendStats
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetReportGroupTrendResponse' value with any optional fields omitted.
mkGetReportGroupTrendResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetReportGroupTrendResponse
mkGetReportGroupTrendResponse responseStatus
  = GetReportGroupTrendResponse'{rawData = Core.Nothing,
                                 stats = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'rawData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgtrrsRawData :: Lens.Lens' GetReportGroupTrendResponse (Core.Maybe [Types.ReportWithRawData])
grgtrrsRawData = Lens.field @"rawData"
{-# INLINEABLE grgtrrsRawData #-}
{-# DEPRECATED rawData "Use generic-lens or generic-optics with 'rawData' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'stats' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgtrrsStats :: Lens.Lens' GetReportGroupTrendResponse (Core.Maybe Types.ReportGroupTrendStats)
grgtrrsStats = Lens.field @"stats"
{-# INLINEABLE grgtrrsStats #-}
{-# DEPRECATED stats "Use generic-lens or generic-optics with 'stats' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgtrrsResponseStatus :: Lens.Lens' GetReportGroupTrendResponse Core.Int
grgtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grgtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
