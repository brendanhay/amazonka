{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetSegmentVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the configuration, dimension, and other settings for all the versions of a specific segment that's associated with an application.
module Network.AWS.Pinpoint.GetSegmentVersions
  ( -- * Creating a request
    GetSegmentVersions (..),
    mkGetSegmentVersions,

    -- ** Request lenses
    gsvSegmentId,
    gsvApplicationId,
    gsvPageSize,
    gsvToken,

    -- * Destructuring the response
    GetSegmentVersionsResponse (..),
    mkGetSegmentVersionsResponse,

    -- ** Response lenses
    grsSegmentsResponse,
    grsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSegmentVersions' smart constructor.
data GetSegmentVersions = GetSegmentVersions'
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

-- | Creates a 'GetSegmentVersions' value with any optional fields omitted.
mkGetSegmentVersions ::
  -- | 'segmentId'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  GetSegmentVersions
mkGetSegmentVersions segmentId applicationId =
  GetSegmentVersions'
    { segmentId,
      applicationId,
      pageSize = Core.Nothing,
      token = Core.Nothing
    }

-- | The unique identifier for the segment.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvSegmentId :: Lens.Lens' GetSegmentVersions Core.Text
gsvSegmentId = Lens.field @"segmentId"
{-# DEPRECATED gsvSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvApplicationId :: Lens.Lens' GetSegmentVersions Core.Text
gsvApplicationId = Lens.field @"applicationId"
{-# DEPRECATED gsvApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvPageSize :: Lens.Lens' GetSegmentVersions (Core.Maybe Core.Text)
gsvPageSize = Lens.field @"pageSize"
{-# DEPRECATED gsvPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvToken :: Lens.Lens' GetSegmentVersions (Core.Maybe Core.Text)
gsvToken = Lens.field @"token"
{-# DEPRECATED gsvToken "Use generic-lens or generic-optics with 'token' instead." #-}

instance Core.AWSRequest GetSegmentVersions where
  type Rs GetSegmentVersions = GetSegmentVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/segments/")
                Core.<> (Core.toText segmentId)
                Core.<> ("/versions")
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
          GetSegmentVersionsResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetSegmentVersionsResponse' smart constructor.
data GetSegmentVersionsResponse = GetSegmentVersionsResponse'
  { segmentsResponse :: Types.SegmentsResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSegmentVersionsResponse' value with any optional fields omitted.
mkGetSegmentVersionsResponse ::
  -- | 'segmentsResponse'
  Types.SegmentsResponse ->
  -- | 'responseStatus'
  Core.Int ->
  GetSegmentVersionsResponse
mkGetSegmentVersionsResponse segmentsResponse responseStatus =
  GetSegmentVersionsResponse' {segmentsResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'segmentsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsSegmentsResponse :: Lens.Lens' GetSegmentVersionsResponse Types.SegmentsResponse
grsSegmentsResponse = Lens.field @"segmentsResponse"
{-# DEPRECATED grsSegmentsResponse "Use generic-lens or generic-optics with 'segmentsResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetSegmentVersionsResponse Core.Int
grsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
