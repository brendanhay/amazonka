{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateSegment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new segment for an application or updates the configuration, dimension, and other settings for an existing segment that's associated with an application.
module Network.AWS.Pinpoint.UpdateSegment
  ( -- * Creating a request
    UpdateSegment (..),
    mkUpdateSegment,

    -- ** Request lenses
    usSegmentId,
    usApplicationId,
    usWriteSegmentRequest,

    -- * Destructuring the response
    UpdateSegmentResponse (..),
    mkUpdateSegmentResponse,

    -- ** Response lenses
    usrrsSegmentResponse,
    usrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateSegment' smart constructor.
data UpdateSegment = UpdateSegment'
  { -- | The unique identifier for the segment.
    segmentId :: Core.Text,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    writeSegmentRequest :: Types.WriteSegmentRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSegment' value with any optional fields omitted.
mkUpdateSegment ::
  -- | 'segmentId'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  -- | 'writeSegmentRequest'
  Types.WriteSegmentRequest ->
  UpdateSegment
mkUpdateSegment segmentId applicationId writeSegmentRequest =
  UpdateSegment' {segmentId, applicationId, writeSegmentRequest}

-- | The unique identifier for the segment.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usSegmentId :: Lens.Lens' UpdateSegment Core.Text
usSegmentId = Lens.field @"segmentId"
{-# DEPRECATED usSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usApplicationId :: Lens.Lens' UpdateSegment Core.Text
usApplicationId = Lens.field @"applicationId"
{-# DEPRECATED usApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'writeSegmentRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usWriteSegmentRequest :: Lens.Lens' UpdateSegment Types.WriteSegmentRequest
usWriteSegmentRequest = Lens.field @"writeSegmentRequest"
{-# DEPRECATED usWriteSegmentRequest "Use generic-lens or generic-optics with 'writeSegmentRequest' instead." #-}

instance Core.FromJSON UpdateSegment where
  toJSON UpdateSegment {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("WriteSegmentRequest" Core..= writeSegmentRequest)]
      )

instance Core.AWSRequest UpdateSegment where
  type Rs UpdateSegment = UpdateSegmentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/v1/apps/" Core.<> (Core.toText applicationId)
                Core.<> ("/segments/")
                Core.<> (Core.toText segmentId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSegmentResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateSegmentResponse' smart constructor.
data UpdateSegmentResponse = UpdateSegmentResponse'
  { segmentResponse :: Types.SegmentResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSegmentResponse' value with any optional fields omitted.
mkUpdateSegmentResponse ::
  -- | 'segmentResponse'
  Types.SegmentResponse ->
  -- | 'responseStatus'
  Core.Int ->
  UpdateSegmentResponse
mkUpdateSegmentResponse segmentResponse responseStatus =
  UpdateSegmentResponse' {segmentResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'segmentResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsSegmentResponse :: Lens.Lens' UpdateSegmentResponse Types.SegmentResponse
usrrsSegmentResponse = Lens.field @"segmentResponse"
{-# DEPRECATED usrrsSegmentResponse "Use generic-lens or generic-optics with 'segmentResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsResponseStatus :: Lens.Lens' UpdateSegmentResponse Core.Int
usrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED usrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
