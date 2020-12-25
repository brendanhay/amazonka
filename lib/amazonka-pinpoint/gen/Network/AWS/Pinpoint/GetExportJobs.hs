{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetExportJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of all the export jobs for an application.
module Network.AWS.Pinpoint.GetExportJobs
  ( -- * Creating a request
    GetExportJobs (..),
    mkGetExportJobs,

    -- ** Request lenses
    gejsApplicationId,
    gejsPageSize,
    gejsToken,

    -- * Destructuring the response
    GetExportJobsResponse (..),
    mkGetExportJobsResponse,

    -- ** Response lenses
    gejrrsExportJobsResponse,
    gejrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetExportJobs' smart constructor.
data GetExportJobs = GetExportJobs'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    -- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    pageSize :: Core.Maybe Core.Text,
    -- | The NextToken string that specifies which page of results to return in a paginated response.
    token :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetExportJobs' value with any optional fields omitted.
mkGetExportJobs ::
  -- | 'applicationId'
  Core.Text ->
  GetExportJobs
mkGetExportJobs applicationId =
  GetExportJobs'
    { applicationId,
      pageSize = Core.Nothing,
      token = Core.Nothing
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejsApplicationId :: Lens.Lens' GetExportJobs Core.Text
gejsApplicationId = Lens.field @"applicationId"
{-# DEPRECATED gejsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejsPageSize :: Lens.Lens' GetExportJobs (Core.Maybe Core.Text)
gejsPageSize = Lens.field @"pageSize"
{-# DEPRECATED gejsPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejsToken :: Lens.Lens' GetExportJobs (Core.Maybe Core.Text)
gejsToken = Lens.field @"token"
{-# DEPRECATED gejsToken "Use generic-lens or generic-optics with 'token' instead." #-}

instance Core.AWSRequest GetExportJobs where
  type Rs GetExportJobs = GetExportJobsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
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
          GetExportJobsResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetExportJobsResponse' smart constructor.
data GetExportJobsResponse = GetExportJobsResponse'
  { exportJobsResponse :: Types.ExportJobsResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetExportJobsResponse' value with any optional fields omitted.
mkGetExportJobsResponse ::
  -- | 'exportJobsResponse'
  Types.ExportJobsResponse ->
  -- | 'responseStatus'
  Core.Int ->
  GetExportJobsResponse
mkGetExportJobsResponse exportJobsResponse responseStatus =
  GetExportJobsResponse' {exportJobsResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'exportJobsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejrrsExportJobsResponse :: Lens.Lens' GetExportJobsResponse Types.ExportJobsResponse
gejrrsExportJobsResponse = Lens.field @"exportJobsResponse"
{-# DEPRECATED gejrrsExportJobsResponse "Use generic-lens or generic-optics with 'exportJobsResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejrrsResponseStatus :: Lens.Lens' GetExportJobsResponse Core.Int
gejrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gejrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
