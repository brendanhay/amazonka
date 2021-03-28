{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an endpoint from an application.
module Network.AWS.Pinpoint.DeleteEndpoint
    (
    -- * Creating a request
      DeleteEndpoint (..)
    , mkDeleteEndpoint
    -- ** Request lenses
    , deApplicationId
    , deEndpointId

    -- * Destructuring the response
    , DeleteEndpointResponse (..)
    , mkDeleteEndpointResponse
    -- ** Response lenses
    , derrsEndpointResponse
    , derrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteEndpoint' smart constructor.
data DeleteEndpoint = DeleteEndpoint'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , endpointId :: Core.Text
    -- ^ The unique identifier for the endpoint.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEndpoint' value with any optional fields omitted.
mkDeleteEndpoint
    :: Core.Text -- ^ 'applicationId'
    -> Core.Text -- ^ 'endpointId'
    -> DeleteEndpoint
mkDeleteEndpoint applicationId endpointId
  = DeleteEndpoint'{applicationId, endpointId}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deApplicationId :: Lens.Lens' DeleteEndpoint Core.Text
deApplicationId = Lens.field @"applicationId"
{-# INLINEABLE deApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The unique identifier for the endpoint.
--
-- /Note:/ Consider using 'endpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEndpointId :: Lens.Lens' DeleteEndpoint Core.Text
deEndpointId = Lens.field @"endpointId"
{-# INLINEABLE deEndpointId #-}
{-# DEPRECATED endpointId "Use generic-lens or generic-optics with 'endpointId' instead"  #-}

instance Core.ToQuery DeleteEndpoint where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteEndpoint where
        toHeaders DeleteEndpoint{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteEndpoint where
        type Rs DeleteEndpoint = DeleteEndpointResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<> "/endpoints/"
                             Core.<> Core.toText endpointId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteEndpointResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteEndpointResponse' smart constructor.
data DeleteEndpointResponse = DeleteEndpointResponse'
  { endpointResponse :: Types.EndpointResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEndpointResponse' value with any optional fields omitted.
mkDeleteEndpointResponse
    :: Types.EndpointResponse -- ^ 'endpointResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> DeleteEndpointResponse
mkDeleteEndpointResponse endpointResponse responseStatus
  = DeleteEndpointResponse'{endpointResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'endpointResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsEndpointResponse :: Lens.Lens' DeleteEndpointResponse Types.EndpointResponse
derrsEndpointResponse = Lens.field @"endpointResponse"
{-# INLINEABLE derrsEndpointResponse #-}
{-# DEPRECATED endpointResponse "Use generic-lens or generic-optics with 'endpointResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsResponseStatus :: Lens.Lens' DeleteEndpointResponse Core.Int
derrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE derrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
