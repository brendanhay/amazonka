{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetSegmentVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the configuration, dimension, and other settings for a specific version of a segment that's associated with an application.
module Network.AWS.Pinpoint.GetSegmentVersion
  ( -- * Creating a request
    GetSegmentVersion (..),
    mkGetSegmentVersion,

    -- ** Request lenses
    gSegmentId,
    gVersion,
    gApplicationId,

    -- * Destructuring the response
    GetSegmentVersionResponse (..),
    mkGetSegmentVersionResponse,

    -- ** Response lenses
    gsvrrsSegmentResponse,
    gsvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSegmentVersion' smart constructor.
data GetSegmentVersion = GetSegmentVersion'
  { -- | The unique identifier for the segment.
    segmentId :: Core.Text,
    -- | The unique version number (Version property) for the campaign version.
    version :: Core.Text,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSegmentVersion' value with any optional fields omitted.
mkGetSegmentVersion ::
  -- | 'segmentId'
  Core.Text ->
  -- | 'version'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  GetSegmentVersion
mkGetSegmentVersion segmentId version applicationId =
  GetSegmentVersion' {segmentId, version, applicationId}

-- | The unique identifier for the segment.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gSegmentId :: Lens.Lens' GetSegmentVersion Core.Text
gSegmentId = Lens.field @"segmentId"
{-# DEPRECATED gSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

-- | The unique version number (Version property) for the campaign version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gVersion :: Lens.Lens' GetSegmentVersion Core.Text
gVersion = Lens.field @"version"
{-# DEPRECATED gVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gApplicationId :: Lens.Lens' GetSegmentVersion Core.Text
gApplicationId = Lens.field @"applicationId"
{-# DEPRECATED gApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Core.AWSRequest GetSegmentVersion where
  type Rs GetSegmentVersion = GetSegmentVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/segments/")
                Core.<> (Core.toText segmentId)
                Core.<> ("/versions/")
                Core.<> (Core.toText version)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSegmentVersionResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetSegmentVersionResponse' smart constructor.
data GetSegmentVersionResponse = GetSegmentVersionResponse'
  { segmentResponse :: Types.SegmentResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSegmentVersionResponse' value with any optional fields omitted.
mkGetSegmentVersionResponse ::
  -- | 'segmentResponse'
  Types.SegmentResponse ->
  -- | 'responseStatus'
  Core.Int ->
  GetSegmentVersionResponse
mkGetSegmentVersionResponse segmentResponse responseStatus =
  GetSegmentVersionResponse' {segmentResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'segmentResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsSegmentResponse :: Lens.Lens' GetSegmentVersionResponse Types.SegmentResponse
gsvrrsSegmentResponse = Lens.field @"segmentResponse"
{-# DEPRECATED gsvrrsSegmentResponse "Use generic-lens or generic-optics with 'segmentResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsResponseStatus :: Lens.Lens' GetSegmentVersionResponse Core.Int
gsvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gsvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
