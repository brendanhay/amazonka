{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetSegmentImportJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the import jobs for a segment.
module Network.AWS.Pinpoint.GetSegmentImportJobs
  ( -- * Creating a request
    GetSegmentImportJobs (..),
    mkGetSegmentImportJobs,

    -- ** Request lenses
    gsijSegmentId,
    gsijApplicationId,
    gsijPageSize,
    gsijToken,

    -- * Destructuring the response
    GetSegmentImportJobsResponse (..),
    mkGetSegmentImportJobsResponse,

    -- ** Response lenses
    gsijrrsImportJobsResponse,
    gsijrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSegmentImportJobs' smart constructor.
data GetSegmentImportJobs = GetSegmentImportJobs'
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

-- | Creates a 'GetSegmentImportJobs' value with any optional fields omitted.
mkGetSegmentImportJobs ::
  -- | 'segmentId'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  GetSegmentImportJobs
mkGetSegmentImportJobs segmentId applicationId =
  GetSegmentImportJobs'
    { segmentId,
      applicationId,
      pageSize = Core.Nothing,
      token = Core.Nothing
    }

-- | The unique identifier for the segment.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsijSegmentId :: Lens.Lens' GetSegmentImportJobs Core.Text
gsijSegmentId = Lens.field @"segmentId"
{-# DEPRECATED gsijSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsijApplicationId :: Lens.Lens' GetSegmentImportJobs Core.Text
gsijApplicationId = Lens.field @"applicationId"
{-# DEPRECATED gsijApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsijPageSize :: Lens.Lens' GetSegmentImportJobs (Core.Maybe Core.Text)
gsijPageSize = Lens.field @"pageSize"
{-# DEPRECATED gsijPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsijToken :: Lens.Lens' GetSegmentImportJobs (Core.Maybe Core.Text)
gsijToken = Lens.field @"token"
{-# DEPRECATED gsijToken "Use generic-lens or generic-optics with 'token' instead." #-}

instance Core.AWSRequest GetSegmentImportJobs where
  type Rs GetSegmentImportJobs = GetSegmentImportJobsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/segments/")
                Core.<> (Core.toText segmentId)
                Core.<> ("/jobs/import")
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
          GetSegmentImportJobsResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetSegmentImportJobsResponse' smart constructor.
data GetSegmentImportJobsResponse = GetSegmentImportJobsResponse'
  { importJobsResponse :: Types.ImportJobsResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSegmentImportJobsResponse' value with any optional fields omitted.
mkGetSegmentImportJobsResponse ::
  -- | 'importJobsResponse'
  Types.ImportJobsResponse ->
  -- | 'responseStatus'
  Core.Int ->
  GetSegmentImportJobsResponse
mkGetSegmentImportJobsResponse importJobsResponse responseStatus =
  GetSegmentImportJobsResponse' {importJobsResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'importJobsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsijrrsImportJobsResponse :: Lens.Lens' GetSegmentImportJobsResponse Types.ImportJobsResponse
gsijrrsImportJobsResponse = Lens.field @"importJobsResponse"
{-# DEPRECATED gsijrrsImportJobsResponse "Use generic-lens or generic-optics with 'importJobsResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsijrrsResponseStatus :: Lens.Lens' GetSegmentImportJobsResponse Core.Int
gsijrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gsijrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
