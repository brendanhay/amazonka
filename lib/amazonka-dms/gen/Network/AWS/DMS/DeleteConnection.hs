{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DeleteConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the connection between a replication instance and an endpoint.
module Network.AWS.DMS.DeleteConnection
    (
    -- * Creating a request
      DeleteConnection (..)
    , mkDeleteConnection
    -- ** Request lenses
    , dcEndpointArn
    , dcReplicationInstanceArn

    -- * Destructuring the response
    , DeleteConnectionResponse (..)
    , mkDeleteConnectionResponse
    -- ** Response lenses
    , dcrrsConnection
    , dcrrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDeleteConnection' smart constructor.
data DeleteConnection = DeleteConnection'
  { endpointArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
  , replicationInstanceArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the replication instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConnection' value with any optional fields omitted.
mkDeleteConnection
    :: Core.Text -- ^ 'endpointArn'
    -> Core.Text -- ^ 'replicationInstanceArn'
    -> DeleteConnection
mkDeleteConnection endpointArn replicationInstanceArn
  = DeleteConnection'{endpointArn, replicationInstanceArn}

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcEndpointArn :: Lens.Lens' DeleteConnection Core.Text
dcEndpointArn = Lens.field @"endpointArn"
{-# INLINEABLE dcEndpointArn #-}
{-# DEPRECATED endpointArn "Use generic-lens or generic-optics with 'endpointArn' instead"  #-}

-- | The Amazon Resource Name (ARN) of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcReplicationInstanceArn :: Lens.Lens' DeleteConnection Core.Text
dcReplicationInstanceArn = Lens.field @"replicationInstanceArn"
{-# INLINEABLE dcReplicationInstanceArn #-}
{-# DEPRECATED replicationInstanceArn "Use generic-lens or generic-optics with 'replicationInstanceArn' instead"  #-}

instance Core.ToQuery DeleteConnection where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteConnection where
        toHeaders DeleteConnection{..}
          = Core.pure ("X-Amz-Target", "AmazonDMSv20160101.DeleteConnection")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteConnection where
        toJSON DeleteConnection{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("EndpointArn" Core..= endpointArn),
                  Core.Just
                    ("ReplicationInstanceArn" Core..= replicationInstanceArn)])

instance Core.AWSRequest DeleteConnection where
        type Rs DeleteConnection = DeleteConnectionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteConnectionResponse' Core.<$>
                   (x Core..:? "Connection") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkDeleteConnectionResponse' smart constructor.
data DeleteConnectionResponse = DeleteConnectionResponse'
  { connection :: Core.Maybe Types.Connection
    -- ^ The connection that is being deleted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConnectionResponse' value with any optional fields omitted.
mkDeleteConnectionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteConnectionResponse
mkDeleteConnectionResponse responseStatus
  = DeleteConnectionResponse'{connection = Core.Nothing,
                              responseStatus}

-- | The connection that is being deleted.
--
-- /Note:/ Consider using 'connection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsConnection :: Lens.Lens' DeleteConnectionResponse (Core.Maybe Types.Connection)
dcrrsConnection = Lens.field @"connection"
{-# INLINEABLE dcrrsConnection #-}
{-# DEPRECATED connection "Use generic-lens or generic-optics with 'connection' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DeleteConnectionResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
