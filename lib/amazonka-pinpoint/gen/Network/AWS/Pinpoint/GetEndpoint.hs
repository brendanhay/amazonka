{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the settings and attributes of a specific endpoint for an application.
module Network.AWS.Pinpoint.GetEndpoint
    (
    -- * Creating a request
      GetEndpoint (..)
    , mkGetEndpoint
    -- ** Request lenses
    , geApplicationId
    , geEndpointId

    -- * Destructuring the response
    , GetEndpointResponse (..)
    , mkGetEndpointResponse
    -- ** Response lenses
    , gerrsEndpointResponse
    , gerrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetEndpoint' smart constructor.
data GetEndpoint = GetEndpoint'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , endpointId :: Core.Text
    -- ^ The unique identifier for the endpoint.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetEndpoint' value with any optional fields omitted.
mkGetEndpoint
    :: Core.Text -- ^ 'applicationId'
    -> Core.Text -- ^ 'endpointId'
    -> GetEndpoint
mkGetEndpoint applicationId endpointId
  = GetEndpoint'{applicationId, endpointId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geApplicationId :: Lens.Lens' GetEndpoint Core.Text
geApplicationId = Lens.field @"applicationId"
{-# INLINEABLE geApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The unique identifier for the endpoint.
--
-- /Note:/ Consider using 'endpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geEndpointId :: Lens.Lens' GetEndpoint Core.Text
geEndpointId = Lens.field @"endpointId"
{-# INLINEABLE geEndpointId #-}
{-# DEPRECATED endpointId "Use generic-lens or generic-optics with 'endpointId' instead"  #-}

instance Core.ToQuery GetEndpoint where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetEndpoint where
        toHeaders GetEndpoint{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetEndpoint where
        type Rs GetEndpoint = GetEndpointResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<> "/endpoints/"
                             Core.<> Core.toText endpointId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetEndpointResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetEndpointResponse' smart constructor.
data GetEndpointResponse = GetEndpointResponse'
  { endpointResponse :: Types.EndpointResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetEndpointResponse' value with any optional fields omitted.
mkGetEndpointResponse
    :: Types.EndpointResponse -- ^ 'endpointResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> GetEndpointResponse
mkGetEndpointResponse endpointResponse responseStatus
  = GetEndpointResponse'{endpointResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'endpointResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsEndpointResponse :: Lens.Lens' GetEndpointResponse Types.EndpointResponse
gerrsEndpointResponse = Lens.field @"endpointResponse"
{-# INLINEABLE gerrsEndpointResponse #-}
{-# DEPRECATED endpointResponse "Use generic-lens or generic-optics with 'endpointResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsResponseStatus :: Lens.Lens' GetEndpointResponse Core.Int
gerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
