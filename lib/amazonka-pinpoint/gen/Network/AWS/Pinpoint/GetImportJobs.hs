{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetImportJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of all the import jobs for an application.
module Network.AWS.Pinpoint.GetImportJobs
  ( -- * Creating a request
    GetImportJobs (..),
    mkGetImportJobs,

    -- ** Request lenses
    gijsApplicationId,
    gijsPageSize,
    gijsToken,

    -- * Destructuring the response
    GetImportJobsResponse (..),
    mkGetImportJobsResponse,

    -- ** Response lenses
    gijrfrsImportJobsResponse,
    gijrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetImportJobs' smart constructor.
data GetImportJobs = GetImportJobs'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    -- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    pageSize :: Core.Maybe Core.Text,
    -- | The NextToken string that specifies which page of results to return in a paginated response.
    token :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetImportJobs' value with any optional fields omitted.
mkGetImportJobs ::
  -- | 'applicationId'
  Core.Text ->
  GetImportJobs
mkGetImportJobs applicationId =
  GetImportJobs'
    { applicationId,
      pageSize = Core.Nothing,
      token = Core.Nothing
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gijsApplicationId :: Lens.Lens' GetImportJobs Core.Text
gijsApplicationId = Lens.field @"applicationId"
{-# DEPRECATED gijsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gijsPageSize :: Lens.Lens' GetImportJobs (Core.Maybe Core.Text)
gijsPageSize = Lens.field @"pageSize"
{-# DEPRECATED gijsPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gijsToken :: Lens.Lens' GetImportJobs (Core.Maybe Core.Text)
gijsToken = Lens.field @"token"
{-# DEPRECATED gijsToken "Use generic-lens or generic-optics with 'token' instead." #-}

instance Core.AWSRequest GetImportJobs where
  type Rs GetImportJobs = GetImportJobsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
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
          GetImportJobsResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetImportJobsResponse' smart constructor.
data GetImportJobsResponse = GetImportJobsResponse'
  { importJobsResponse :: Types.ImportJobsResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetImportJobsResponse' value with any optional fields omitted.
mkGetImportJobsResponse ::
  -- | 'importJobsResponse'
  Types.ImportJobsResponse ->
  -- | 'responseStatus'
  Core.Int ->
  GetImportJobsResponse
mkGetImportJobsResponse importJobsResponse responseStatus =
  GetImportJobsResponse' {importJobsResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'importJobsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gijrfrsImportJobsResponse :: Lens.Lens' GetImportJobsResponse Types.ImportJobsResponse
gijrfrsImportJobsResponse = Lens.field @"importJobsResponse"
{-# DEPRECATED gijrfrsImportJobsResponse "Use generic-lens or generic-optics with 'importJobsResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gijrfrsResponseStatus :: Lens.Lens' GetImportJobsResponse Core.Int
gijrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gijrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
