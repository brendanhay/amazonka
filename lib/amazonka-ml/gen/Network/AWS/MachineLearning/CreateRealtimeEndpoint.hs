{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.CreateRealtimeEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a real-time endpoint for the @MLModel@ . The endpoint contains the URI of the @MLModel@ ; that is, the location to send real-time prediction requests for the specified @MLModel@ .
module Network.AWS.MachineLearning.CreateRealtimeEndpoint
  ( -- * Creating a request
    CreateRealtimeEndpoint (..),
    mkCreateRealtimeEndpoint,

    -- ** Request lenses
    creMLModelId,

    -- * Destructuring the response
    CreateRealtimeEndpointResponse (..),
    mkCreateRealtimeEndpointResponse,

    -- ** Response lenses
    crerrsMLModelId,
    crerrsRealtimeEndpointInfo,
    crerrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateRealtimeEndpoint' smart constructor.
newtype CreateRealtimeEndpoint = CreateRealtimeEndpoint'
  { -- | The ID assigned to the @MLModel@ during creation.
    mLModelId :: Types.EntityId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRealtimeEndpoint' value with any optional fields omitted.
mkCreateRealtimeEndpoint ::
  -- | 'mLModelId'
  Types.EntityId ->
  CreateRealtimeEndpoint
mkCreateRealtimeEndpoint mLModelId =
  CreateRealtimeEndpoint' {mLModelId}

-- | The ID assigned to the @MLModel@ during creation.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creMLModelId :: Lens.Lens' CreateRealtimeEndpoint Types.EntityId
creMLModelId = Lens.field @"mLModelId"
{-# DEPRECATED creMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

instance Core.FromJSON CreateRealtimeEndpoint where
  toJSON CreateRealtimeEndpoint {..} =
    Core.object
      (Core.catMaybes [Core.Just ("MLModelId" Core..= mLModelId)])

instance Core.AWSRequest CreateRealtimeEndpoint where
  type Rs CreateRealtimeEndpoint = CreateRealtimeEndpointResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonML_20141212.CreateRealtimeEndpoint")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRealtimeEndpointResponse'
            Core.<$> (x Core..:? "MLModelId")
            Core.<*> (x Core..:? "RealtimeEndpointInfo")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of an @CreateRealtimeEndpoint@ operation.
--
-- The result contains the @MLModelId@ and the endpoint information for the @MLModel@ .
--
-- /See:/ 'mkCreateRealtimeEndpointResponse' smart constructor.
data CreateRealtimeEndpointResponse = CreateRealtimeEndpointResponse'
  { -- | A user-supplied ID that uniquely identifies the @MLModel@ . This value should be identical to the value of the @MLModelId@ in the request.
    mLModelId :: Core.Maybe Types.MLModelId,
    -- | The endpoint information of the @MLModel@
    realtimeEndpointInfo :: Core.Maybe Types.RealtimeEndpointInfo,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateRealtimeEndpointResponse' value with any optional fields omitted.
mkCreateRealtimeEndpointResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateRealtimeEndpointResponse
mkCreateRealtimeEndpointResponse responseStatus =
  CreateRealtimeEndpointResponse'
    { mLModelId = Core.Nothing,
      realtimeEndpointInfo = Core.Nothing,
      responseStatus
    }

-- | A user-supplied ID that uniquely identifies the @MLModel@ . This value should be identical to the value of the @MLModelId@ in the request.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crerrsMLModelId :: Lens.Lens' CreateRealtimeEndpointResponse (Core.Maybe Types.MLModelId)
crerrsMLModelId = Lens.field @"mLModelId"
{-# DEPRECATED crerrsMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | The endpoint information of the @MLModel@
--
-- /Note:/ Consider using 'realtimeEndpointInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crerrsRealtimeEndpointInfo :: Lens.Lens' CreateRealtimeEndpointResponse (Core.Maybe Types.RealtimeEndpointInfo)
crerrsRealtimeEndpointInfo = Lens.field @"realtimeEndpointInfo"
{-# DEPRECATED crerrsRealtimeEndpointInfo "Use generic-lens or generic-optics with 'realtimeEndpointInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crerrsResponseStatus :: Lens.Lens' CreateRealtimeEndpointResponse Core.Int
crerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
