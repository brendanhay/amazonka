{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DeleteRealtimeEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a real time endpoint of an @MLModel@ .
module Network.AWS.MachineLearning.DeleteRealtimeEndpoint
  ( -- * Creating a request
    DeleteRealtimeEndpoint (..),
    mkDeleteRealtimeEndpoint,

    -- ** Request lenses
    dreMLModelId,

    -- * Destructuring the response
    DeleteRealtimeEndpointResponse (..),
    mkDeleteRealtimeEndpointResponse,

    -- ** Response lenses
    drerrsMLModelId,
    drerrsRealtimeEndpointInfo,
    drerrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteRealtimeEndpoint' smart constructor.
newtype DeleteRealtimeEndpoint = DeleteRealtimeEndpoint'
  { -- | The ID assigned to the @MLModel@ during creation.
    mLModelId :: Types.MLModelId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRealtimeEndpoint' value with any optional fields omitted.
mkDeleteRealtimeEndpoint ::
  -- | 'mLModelId'
  Types.MLModelId ->
  DeleteRealtimeEndpoint
mkDeleteRealtimeEndpoint mLModelId =
  DeleteRealtimeEndpoint' {mLModelId}

-- | The ID assigned to the @MLModel@ during creation.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreMLModelId :: Lens.Lens' DeleteRealtimeEndpoint Types.MLModelId
dreMLModelId = Lens.field @"mLModelId"
{-# DEPRECATED dreMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

instance Core.FromJSON DeleteRealtimeEndpoint where
  toJSON DeleteRealtimeEndpoint {..} =
    Core.object
      (Core.catMaybes [Core.Just ("MLModelId" Core..= mLModelId)])

instance Core.AWSRequest DeleteRealtimeEndpoint where
  type Rs DeleteRealtimeEndpoint = DeleteRealtimeEndpointResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonML_20141212.DeleteRealtimeEndpoint")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRealtimeEndpointResponse'
            Core.<$> (x Core..:? "MLModelId")
            Core.<*> (x Core..:? "RealtimeEndpointInfo")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of an @DeleteRealtimeEndpoint@ operation.
--
-- The result contains the @MLModelId@ and the endpoint information for the @MLModel@ .
--
-- /See:/ 'mkDeleteRealtimeEndpointResponse' smart constructor.
data DeleteRealtimeEndpointResponse = DeleteRealtimeEndpointResponse'
  { -- | A user-supplied ID that uniquely identifies the @MLModel@ . This value should be identical to the value of the @MLModelId@ in the request.
    mLModelId :: Core.Maybe Types.MLModelId,
    -- | The endpoint information of the @MLModel@
    realtimeEndpointInfo :: Core.Maybe Types.RealtimeEndpointInfo,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteRealtimeEndpointResponse' value with any optional fields omitted.
mkDeleteRealtimeEndpointResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteRealtimeEndpointResponse
mkDeleteRealtimeEndpointResponse responseStatus =
  DeleteRealtimeEndpointResponse'
    { mLModelId = Core.Nothing,
      realtimeEndpointInfo = Core.Nothing,
      responseStatus
    }

-- | A user-supplied ID that uniquely identifies the @MLModel@ . This value should be identical to the value of the @MLModelId@ in the request.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drerrsMLModelId :: Lens.Lens' DeleteRealtimeEndpointResponse (Core.Maybe Types.MLModelId)
drerrsMLModelId = Lens.field @"mLModelId"
{-# DEPRECATED drerrsMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | The endpoint information of the @MLModel@
--
-- /Note:/ Consider using 'realtimeEndpointInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drerrsRealtimeEndpointInfo :: Lens.Lens' DeleteRealtimeEndpointResponse (Core.Maybe Types.RealtimeEndpointInfo)
drerrsRealtimeEndpointInfo = Lens.field @"realtimeEndpointInfo"
{-# DEPRECATED drerrsRealtimeEndpointInfo "Use generic-lens or generic-optics with 'realtimeEndpointInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drerrsResponseStatus :: Lens.Lens' DeleteRealtimeEndpointResponse Core.Int
drerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
