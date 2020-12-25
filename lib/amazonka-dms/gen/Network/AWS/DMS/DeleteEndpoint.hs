{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DeleteEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified endpoint.
module Network.AWS.DMS.DeleteEndpoint
  ( -- * Creating a request
    DeleteEndpoint (..),
    mkDeleteEndpoint,

    -- ** Request lenses
    deEndpointArn,

    -- * Destructuring the response
    DeleteEndpointResponse (..),
    mkDeleteEndpointResponse,

    -- ** Response lenses
    derfrsEndpoint,
    derfrsResponseStatus,
  )
where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDeleteEndpoint' smart constructor.
newtype DeleteEndpoint = DeleteEndpoint'
  { -- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
    endpointArn :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEndpoint' value with any optional fields omitted.
mkDeleteEndpoint ::
  -- | 'endpointArn'
  Types.String ->
  DeleteEndpoint
mkDeleteEndpoint endpointArn = DeleteEndpoint' {endpointArn}

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEndpointArn :: Lens.Lens' DeleteEndpoint Types.String
deEndpointArn = Lens.field @"endpointArn"
{-# DEPRECATED deEndpointArn "Use generic-lens or generic-optics with 'endpointArn' instead." #-}

instance Core.FromJSON DeleteEndpoint where
  toJSON DeleteEndpoint {..} =
    Core.object
      (Core.catMaybes [Core.Just ("EndpointArn" Core..= endpointArn)])

instance Core.AWSRequest DeleteEndpoint where
  type Rs DeleteEndpoint = DeleteEndpointResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonDMSv20160101.DeleteEndpoint")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteEndpointResponse'
            Core.<$> (x Core..:? "Endpoint") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkDeleteEndpointResponse' smart constructor.
data DeleteEndpointResponse = DeleteEndpointResponse'
  { -- | The endpoint that was deleted.
    endpoint :: Core.Maybe Types.Endpoint,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEndpointResponse' value with any optional fields omitted.
mkDeleteEndpointResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteEndpointResponse
mkDeleteEndpointResponse responseStatus =
  DeleteEndpointResponse' {endpoint = Core.Nothing, responseStatus}

-- | The endpoint that was deleted.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derfrsEndpoint :: Lens.Lens' DeleteEndpointResponse (Core.Maybe Types.Endpoint)
derfrsEndpoint = Lens.field @"endpoint"
{-# DEPRECATED derfrsEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derfrsResponseStatus :: Lens.Lens' DeleteEndpointResponse Core.Int
derfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED derfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
