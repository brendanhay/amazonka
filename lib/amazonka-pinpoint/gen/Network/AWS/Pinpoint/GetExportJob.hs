{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetExportJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of a specific export job for an application.
module Network.AWS.Pinpoint.GetExportJob
  ( -- * Creating a request
    GetExportJob (..),
    mkGetExportJob,

    -- ** Request lenses
    gejApplicationId,
    gejJobId,

    -- * Destructuring the response
    GetExportJobResponse (..),
    mkGetExportJobResponse,

    -- ** Response lenses
    gejrfrsExportJobResponse,
    gejrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetExportJob' smart constructor.
data GetExportJob = GetExportJob'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    -- | The unique identifier for the job.
    jobId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetExportJob' value with any optional fields omitted.
mkGetExportJob ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'jobId'
  Core.Text ->
  GetExportJob
mkGetExportJob applicationId jobId =
  GetExportJob' {applicationId, jobId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejApplicationId :: Lens.Lens' GetExportJob Core.Text
gejApplicationId = Lens.field @"applicationId"
{-# DEPRECATED gejApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The unique identifier for the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejJobId :: Lens.Lens' GetExportJob Core.Text
gejJobId = Lens.field @"jobId"
{-# DEPRECATED gejJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Core.AWSRequest GetExportJob where
  type Rs GetExportJob = GetExportJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/jobs/export/")
                Core.<> (Core.toText jobId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetExportJobResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetExportJobResponse' smart constructor.
data GetExportJobResponse = GetExportJobResponse'
  { exportJobResponse :: Types.ExportJobResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetExportJobResponse' value with any optional fields omitted.
mkGetExportJobResponse ::
  -- | 'exportJobResponse'
  Types.ExportJobResponse ->
  -- | 'responseStatus'
  Core.Int ->
  GetExportJobResponse
mkGetExportJobResponse exportJobResponse responseStatus =
  GetExportJobResponse' {exportJobResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'exportJobResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejrfrsExportJobResponse :: Lens.Lens' GetExportJobResponse Types.ExportJobResponse
gejrfrsExportJobResponse = Lens.field @"exportJobResponse"
{-# DEPRECATED gejrfrsExportJobResponse "Use generic-lens or generic-optics with 'exportJobResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gejrfrsResponseStatus :: Lens.Lens' GetExportJobResponse Core.Int
gejrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gejrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
