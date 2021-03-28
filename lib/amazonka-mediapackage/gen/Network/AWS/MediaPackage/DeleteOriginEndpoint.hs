{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.DeleteOriginEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing OriginEndpoint.
module Network.AWS.MediaPackage.DeleteOriginEndpoint
    (
    -- * Creating a request
      DeleteOriginEndpoint (..)
    , mkDeleteOriginEndpoint
    -- ** Request lenses
    , doefId

    -- * Destructuring the response
    , DeleteOriginEndpointResponse (..)
    , mkDeleteOriginEndpointResponse
    -- ** Response lenses
    , doerrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteOriginEndpoint' smart constructor.
newtype DeleteOriginEndpoint = DeleteOriginEndpoint'
  { id :: Core.Text
    -- ^ The ID of the OriginEndpoint to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteOriginEndpoint' value with any optional fields omitted.
mkDeleteOriginEndpoint
    :: Core.Text -- ^ 'id'
    -> DeleteOriginEndpoint
mkDeleteOriginEndpoint id = DeleteOriginEndpoint'{id}

-- | The ID of the OriginEndpoint to delete.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doefId :: Lens.Lens' DeleteOriginEndpoint Core.Text
doefId = Lens.field @"id"
{-# INLINEABLE doefId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery DeleteOriginEndpoint where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteOriginEndpoint where
        toHeaders DeleteOriginEndpoint{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteOriginEndpoint where
        type Rs DeleteOriginEndpoint = DeleteOriginEndpointResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/origin_endpoints/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteOriginEndpointResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteOriginEndpointResponse' smart constructor.
newtype DeleteOriginEndpointResponse = DeleteOriginEndpointResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteOriginEndpointResponse' value with any optional fields omitted.
mkDeleteOriginEndpointResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteOriginEndpointResponse
mkDeleteOriginEndpointResponse responseStatus
  = DeleteOriginEndpointResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doerrsResponseStatus :: Lens.Lens' DeleteOriginEndpointResponse Core.Int
doerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE doerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
