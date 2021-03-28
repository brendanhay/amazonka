{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new endpoint for an application or updates the settings and attributes of an existing endpoint for an application. You can also use this operation to define custom attributes for an endpoint. If an update includes one or more values for a custom attribute, Amazon Pinpoint replaces (overwrites) any existing values with the new values.
module Network.AWS.Pinpoint.UpdateEndpoint
    (
    -- * Creating a request
      UpdateEndpoint (..)
    , mkUpdateEndpoint
    -- ** Request lenses
    , ueApplicationId
    , ueEndpointId
    , ueEndpointRequest

    -- * Destructuring the response
    , UpdateEndpointResponse (..)
    , mkUpdateEndpointResponse
    -- ** Response lenses
    , uerrsMessageBody
    , uerrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateEndpoint' smart constructor.
data UpdateEndpoint = UpdateEndpoint'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , endpointId :: Core.Text
    -- ^ The unique identifier for the endpoint.
  , endpointRequest :: Types.EndpointRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEndpoint' value with any optional fields omitted.
mkUpdateEndpoint
    :: Core.Text -- ^ 'applicationId'
    -> Core.Text -- ^ 'endpointId'
    -> Types.EndpointRequest -- ^ 'endpointRequest'
    -> UpdateEndpoint
mkUpdateEndpoint applicationId endpointId endpointRequest
  = UpdateEndpoint'{applicationId, endpointId, endpointRequest}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueApplicationId :: Lens.Lens' UpdateEndpoint Core.Text
ueApplicationId = Lens.field @"applicationId"
{-# INLINEABLE ueApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The unique identifier for the endpoint.
--
-- /Note:/ Consider using 'endpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueEndpointId :: Lens.Lens' UpdateEndpoint Core.Text
ueEndpointId = Lens.field @"endpointId"
{-# INLINEABLE ueEndpointId #-}
{-# DEPRECATED endpointId "Use generic-lens or generic-optics with 'endpointId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'endpointRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueEndpointRequest :: Lens.Lens' UpdateEndpoint Types.EndpointRequest
ueEndpointRequest = Lens.field @"endpointRequest"
{-# INLINEABLE ueEndpointRequest #-}
{-# DEPRECATED endpointRequest "Use generic-lens or generic-optics with 'endpointRequest' instead"  #-}

instance Core.ToQuery UpdateEndpoint where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateEndpoint where
        toHeaders UpdateEndpoint{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateEndpoint where
        toJSON UpdateEndpoint{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("EndpointRequest" Core..= endpointRequest)])

instance Core.AWSRequest UpdateEndpoint where
        type Rs UpdateEndpoint = UpdateEndpointResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<> "/endpoints/"
                             Core.<> Core.toText endpointId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateEndpointResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateEndpointResponse' smart constructor.
data UpdateEndpointResponse = UpdateEndpointResponse'
  { messageBody :: Types.MessageBody
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEndpointResponse' value with any optional fields omitted.
mkUpdateEndpointResponse
    :: Types.MessageBody -- ^ 'messageBody'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateEndpointResponse
mkUpdateEndpointResponse messageBody responseStatus
  = UpdateEndpointResponse'{messageBody, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'messageBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uerrsMessageBody :: Lens.Lens' UpdateEndpointResponse Types.MessageBody
uerrsMessageBody = Lens.field @"messageBody"
{-# INLINEABLE uerrsMessageBody #-}
{-# DEPRECATED messageBody "Use generic-lens or generic-optics with 'messageBody' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uerrsResponseStatus :: Lens.Lens' UpdateEndpointResponse Core.Int
uerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
