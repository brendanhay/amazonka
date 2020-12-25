{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetSegmentExportJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the export jobs for a segment.
module Network.AWS.Pinpoint.GetSegmentExportJobs
  ( -- * Creating a request
    GetSegmentExportJobs (..),
    mkGetSegmentExportJobs,

    -- ** Request lenses
    gsejSegmentId,
    gsejApplicationId,
    gsejPageSize,
    gsejToken,

    -- * Destructuring the response
    GetSegmentExportJobsResponse (..),
    mkGetSegmentExportJobsResponse,

    -- ** Response lenses
    gsejrrsExportJobsResponse,
    gsejrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSegmentExportJobs' smart constructor.
data GetSegmentExportJobs = GetSegmentExportJobs'
  { -- | The unique identifier for the segment.
    segmentId :: Core.Text,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    -- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    pageSize :: Core.Maybe Core.Text,
    -- | The NextToken string that specifies which page of results to return in a paginated response.
    token :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSegmentExportJobs' value with any optional fields omitted.
mkGetSegmentExportJobs ::
  -- | 'segmentId'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  GetSegmentExportJobs
mkGetSegmentExportJobs segmentId applicationId =
  GetSegmentExportJobs'
    { segmentId,
      applicationId,
      pageSize = Core.Nothing,
      token = Core.Nothing
    }

-- | The unique identifier for the segment.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsejSegmentId :: Lens.Lens' GetSegmentExportJobs Core.Text
gsejSegmentId = Lens.field @"segmentId"
{-# DEPRECATED gsejSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsejApplicationId :: Lens.Lens' GetSegmentExportJobs Core.Text
gsejApplicationId = Lens.field @"applicationId"
{-# DEPRECATED gsejApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsejPageSize :: Lens.Lens' GetSegmentExportJobs (Core.Maybe Core.Text)
gsejPageSize = Lens.field @"pageSize"
{-# DEPRECATED gsejPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsejToken :: Lens.Lens' GetSegmentExportJobs (Core.Maybe Core.Text)
gsejToken = Lens.field @"token"
{-# DEPRECATED gsejToken "Use generic-lens or generic-optics with 'token' instead." #-}

instance Core.AWSRequest GetSegmentExportJobs where
  type Rs GetSegmentExportJobs = GetSegmentExportJobsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/segments/")
                Core.<> (Core.toText segmentId)
                Core.<> ("/jobs/export")
            ),
        Core._rqQuery =
          Core.toQueryValue "page-size" Core.<$> pageSize
            Core.<> (Core.toQueryValue "token" Core.<$> token),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSegmentExportJobsResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetSegmentExportJobsResponse' smart constructor.
data GetSegmentExportJobsResponse = GetSegmentExportJobsResponse'
  { exportJobsResponse :: Types.ExportJobsResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSegmentExportJobsResponse' value with any optional fields omitted.
mkGetSegmentExportJobsResponse ::
  -- | 'exportJobsResponse'
  Types.ExportJobsResponse ->
  -- | 'responseStatus'
  Core.Int ->
  GetSegmentExportJobsResponse
mkGetSegmentExportJobsResponse exportJobsResponse responseStatus =
  GetSegmentExportJobsResponse' {exportJobsResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'exportJobsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsejrrsExportJobsResponse :: Lens.Lens' GetSegmentExportJobsResponse Types.ExportJobsResponse
gsejrrsExportJobsResponse = Lens.field @"exportJobsResponse"
{-# DEPRECATED gsejrrsExportJobsResponse "Use generic-lens or generic-optics with 'exportJobsResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsejrrsResponseStatus :: Lens.Lens' GetSegmentExportJobsResponse Core.Int
gsejrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gsejrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
