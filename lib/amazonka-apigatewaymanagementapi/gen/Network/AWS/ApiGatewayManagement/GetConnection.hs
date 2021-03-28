{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGatewayManagement.GetConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about the connection with the provided id.
module Network.AWS.ApiGatewayManagement.GetConnection
    (
    -- * Creating a request
      GetConnection (..)
    , mkGetConnection
    -- ** Request lenses
    , gcConnectionId

    -- * Destructuring the response
    , GetConnectionResponse (..)
    , mkGetConnectionResponse
    -- ** Response lenses
    , gcrrsConnectedAt
    , gcrrsIdentity
    , gcrrsLastActiveAt
    , gcrrsResponseStatus
    ) where

import qualified Network.AWS.ApiGatewayManagement.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetConnection' smart constructor.
newtype GetConnection = GetConnection'
  { connectionId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetConnection' value with any optional fields omitted.
mkGetConnection
    :: Core.Text -- ^ 'connectionId'
    -> GetConnection
mkGetConnection connectionId = GetConnection'{connectionId}

-- | Undocumented field.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcConnectionId :: Lens.Lens' GetConnection Core.Text
gcConnectionId = Lens.field @"connectionId"
{-# INLINEABLE gcConnectionId #-}
{-# DEPRECATED connectionId "Use generic-lens or generic-optics with 'connectionId' instead"  #-}

instance Core.ToQuery GetConnection where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetConnection where
        toHeaders GetConnection{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetConnection where
        type Rs GetConnection = GetConnectionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/@connections/" Core.<> Core.toText connectionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetConnectionResponse' Core.<$>
                   (x Core..:? "connectedAt") Core.<*> x Core..:? "identity" Core.<*>
                     x Core..:? "lastActiveAt"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetConnectionResponse' smart constructor.
data GetConnectionResponse = GetConnectionResponse'
  { connectedAt :: Core.Maybe Core.UTCTime
    -- ^ The time in ISO 8601 format for when the connection was established.
  , identity :: Core.Maybe Types.Identity
  , lastActiveAt :: Core.Maybe Core.UTCTime
    -- ^ The time in ISO 8601 format for when the connection was last active.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetConnectionResponse' value with any optional fields omitted.
mkGetConnectionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetConnectionResponse
mkGetConnectionResponse responseStatus
  = GetConnectionResponse'{connectedAt = Core.Nothing,
                           identity = Core.Nothing, lastActiveAt = Core.Nothing,
                           responseStatus}

-- | The time in ISO 8601 format for when the connection was established.
--
-- /Note:/ Consider using 'connectedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsConnectedAt :: Lens.Lens' GetConnectionResponse (Core.Maybe Core.UTCTime)
gcrrsConnectedAt = Lens.field @"connectedAt"
{-# INLINEABLE gcrrsConnectedAt #-}
{-# DEPRECATED connectedAt "Use generic-lens or generic-optics with 'connectedAt' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsIdentity :: Lens.Lens' GetConnectionResponse (Core.Maybe Types.Identity)
gcrrsIdentity = Lens.field @"identity"
{-# INLINEABLE gcrrsIdentity #-}
{-# DEPRECATED identity "Use generic-lens or generic-optics with 'identity' instead"  #-}

-- | The time in ISO 8601 format for when the connection was last active.
--
-- /Note:/ Consider using 'lastActiveAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsLastActiveAt :: Lens.Lens' GetConnectionResponse (Core.Maybe Core.UTCTime)
gcrrsLastActiveAt = Lens.field @"lastActiveAt"
{-# INLINEABLE gcrrsLastActiveAt #-}
{-# DEPRECATED lastActiveAt "Use generic-lens or generic-optics with 'lastActiveAt' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsResponseStatus :: Lens.Lens' GetConnectionResponse Core.Int
gcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
