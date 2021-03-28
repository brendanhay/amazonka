{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateEndpoint (..)
    , mkUpdateEndpoint
    -- ** Request lenses
    , ueEndpointArn
    , ueDesiredInferenceUnits

    -- * Destructuring the response
    , UpdateEndpointResponse (..)
    , mkUpdateEndpointResponse
    -- ** Response lenses
    , uerrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateEndpoint' smart constructor.
data UpdateEndpoint = UpdateEndpoint'
  { endpointArn :: Types.ComprehendEndpointArn
    -- ^ The Amazon Resource Number (ARN) of the endpoint being updated.
  , desiredInferenceUnits :: Core.Natural
    -- ^ The desired number of inference units to be used by the model using this endpoint. Each inference unit represents of a throughput of 100 characters per second.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEndpoint' value with any optional fields omitted.
mkUpdateEndpoint
    :: Types.ComprehendEndpointArn -- ^ 'endpointArn'
    -> Core.Natural -- ^ 'desiredInferenceUnits'
    -> UpdateEndpoint
mkUpdateEndpoint endpointArn desiredInferenceUnits
  = UpdateEndpoint'{endpointArn, desiredInferenceUnits}

-- | The Amazon Resource Number (ARN) of the endpoint being updated.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueEndpointArn :: Lens.Lens' UpdateEndpoint Types.ComprehendEndpointArn
ueEndpointArn = Lens.field @"endpointArn"
{-# INLINEABLE ueEndpointArn #-}
{-# DEPRECATED endpointArn "Use generic-lens or generic-optics with 'endpointArn' instead"  #-}

-- | The desired number of inference units to be used by the model using this endpoint. Each inference unit represents of a throughput of 100 characters per second.
--
-- /Note:/ Consider using 'desiredInferenceUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueDesiredInferenceUnits :: Lens.Lens' UpdateEndpoint Core.Natural
ueDesiredInferenceUnits = Lens.field @"desiredInferenceUnits"
{-# INLINEABLE ueDesiredInferenceUnits #-}
{-# DEPRECATED desiredInferenceUnits "Use generic-lens or generic-optics with 'desiredInferenceUnits' instead"  #-}

instance Core.ToQuery UpdateEndpoint where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateEndpoint where
        toHeaders UpdateEndpoint{..}
          = Core.pure ("X-Amz-Target", "Comprehend_20171127.UpdateEndpoint")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateEndpoint where
        toJSON UpdateEndpoint{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("EndpointArn" Core..= endpointArn),
                  Core.Just ("DesiredInferenceUnits" Core..= desiredInferenceUnits)])

instance Core.AWSRequest UpdateEndpoint where
        type Rs UpdateEndpoint = UpdateEndpointResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateEndpointResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateEndpointResponse' smart constructor.
newtype UpdateEndpointResponse = UpdateEndpointResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEndpointResponse' value with any optional fields omitted.
mkUpdateEndpointResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateEndpointResponse
mkUpdateEndpointResponse responseStatus
  = UpdateEndpointResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uerrsResponseStatus :: Lens.Lens' UpdateEndpointResponse Core.Int
uerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
