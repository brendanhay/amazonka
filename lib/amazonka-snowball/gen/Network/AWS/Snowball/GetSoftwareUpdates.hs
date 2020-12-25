{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.GetSoftwareUpdates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an Amazon S3 presigned URL for an update file associated with a specified @JobId@ .
module Network.AWS.Snowball.GetSoftwareUpdates
  ( -- * Creating a request
    GetSoftwareUpdates (..),
    mkGetSoftwareUpdates,

    -- ** Request lenses
    gsuJobId,

    -- * Destructuring the response
    GetSoftwareUpdatesResponse (..),
    mkGetSoftwareUpdatesResponse,

    -- ** Response lenses
    grsUpdatesURI,
    grsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkGetSoftwareUpdates' smart constructor.
newtype GetSoftwareUpdates = GetSoftwareUpdates'
  { -- | The ID for a job that you want to get the software update file for, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
    jobId :: Types.JobId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetSoftwareUpdates' value with any optional fields omitted.
mkGetSoftwareUpdates ::
  -- | 'jobId'
  Types.JobId ->
  GetSoftwareUpdates
mkGetSoftwareUpdates jobId = GetSoftwareUpdates' {jobId}

-- | The ID for a job that you want to get the software update file for, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsuJobId :: Lens.Lens' GetSoftwareUpdates Types.JobId
gsuJobId = Lens.field @"jobId"
{-# DEPRECATED gsuJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Core.FromJSON GetSoftwareUpdates where
  toJSON GetSoftwareUpdates {..} =
    Core.object (Core.catMaybes [Core.Just ("JobId" Core..= jobId)])

instance Core.AWSRequest GetSoftwareUpdates where
  type Rs GetSoftwareUpdates = GetSoftwareUpdatesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSIESnowballJobManagementService.GetSoftwareUpdates"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSoftwareUpdatesResponse'
            Core.<$> (x Core..:? "UpdatesURI") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetSoftwareUpdatesResponse' smart constructor.
data GetSoftwareUpdatesResponse = GetSoftwareUpdatesResponse'
  { -- | The Amazon S3 presigned URL for the update file associated with the specified @JobId@ value. The software update will be available for 2 days after this request is made. To access an update after the 2 days have passed, you'll have to make another call to @GetSoftwareUpdates@ .
    updatesURI :: Core.Maybe Types.UpdatesURI,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSoftwareUpdatesResponse' value with any optional fields omitted.
mkGetSoftwareUpdatesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetSoftwareUpdatesResponse
mkGetSoftwareUpdatesResponse responseStatus =
  GetSoftwareUpdatesResponse'
    { updatesURI = Core.Nothing,
      responseStatus
    }

-- | The Amazon S3 presigned URL for the update file associated with the specified @JobId@ value. The software update will be available for 2 days after this request is made. To access an update after the 2 days have passed, you'll have to make another call to @GetSoftwareUpdates@ .
--
-- /Note:/ Consider using 'updatesURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsUpdatesURI :: Lens.Lens' GetSoftwareUpdatesResponse (Core.Maybe Types.UpdatesURI)
grsUpdatesURI = Lens.field @"updatesURI"
{-# DEPRECATED grsUpdatesURI "Use generic-lens or generic-optics with 'updatesURI' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetSoftwareUpdatesResponse Core.Int
grsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
