{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.UpdateEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about the specified endpoint.
module Network.AWS.Comprehend.UpdateEndpoint
  ( -- * Creating a request
    UpdateEndpoint (..),
    mkUpdateEndpoint,

    -- ** Request lenses
    ueEndpointArn,
    ueDesiredInferenceUnits,

    -- * Destructuring the response
    UpdateEndpointResponse (..),
    mkUpdateEndpointResponse,

    -- ** Response lenses
    uerrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateEndpoint' smart constructor.
data UpdateEndpoint = UpdateEndpoint'
  { -- | The Amazon Resource Number (ARN) of the endpoint being updated.
    endpointArn :: Types.ComprehendEndpointArn,
    -- | The desired number of inference units to be used by the model using this endpoint. Each inference unit represents of a throughput of 100 characters per second.
    desiredInferenceUnits :: Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEndpoint' value with any optional fields omitted.
mkUpdateEndpoint ::
  -- | 'endpointArn'
  Types.ComprehendEndpointArn ->
  -- | 'desiredInferenceUnits'
  Core.Natural ->
  UpdateEndpoint
mkUpdateEndpoint endpointArn desiredInferenceUnits =
  UpdateEndpoint' {endpointArn, desiredInferenceUnits}

-- | The Amazon Resource Number (ARN) of the endpoint being updated.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueEndpointArn :: Lens.Lens' UpdateEndpoint Types.ComprehendEndpointArn
ueEndpointArn = Lens.field @"endpointArn"
{-# DEPRECATED ueEndpointArn "Use generic-lens or generic-optics with 'endpointArn' instead." #-}

-- | The desired number of inference units to be used by the model using this endpoint. Each inference unit represents of a throughput of 100 characters per second.
--
-- /Note:/ Consider using 'desiredInferenceUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueDesiredInferenceUnits :: Lens.Lens' UpdateEndpoint Core.Natural
ueDesiredInferenceUnits = Lens.field @"desiredInferenceUnits"
{-# DEPRECATED ueDesiredInferenceUnits "Use generic-lens or generic-optics with 'desiredInferenceUnits' instead." #-}

instance Core.FromJSON UpdateEndpoint where
  toJSON UpdateEndpoint {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("EndpointArn" Core..= endpointArn),
            Core.Just ("DesiredInferenceUnits" Core..= desiredInferenceUnits)
          ]
      )

instance Core.AWSRequest UpdateEndpoint where
  type Rs UpdateEndpoint = UpdateEndpointResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Comprehend_20171127.UpdateEndpoint")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateEndpointResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateEndpointResponse' smart constructor.
newtype UpdateEndpointResponse = UpdateEndpointResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEndpointResponse' value with any optional fields omitted.
mkUpdateEndpointResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateEndpointResponse
mkUpdateEndpointResponse responseStatus =
  UpdateEndpointResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uerrsResponseStatus :: Lens.Lens' UpdateEndpointResponse Core.Int
uerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
