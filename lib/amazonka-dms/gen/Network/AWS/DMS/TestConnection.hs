{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.TestConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests the connection between the replication instance and the endpoint.
module Network.AWS.DMS.TestConnection
    (
    -- * Creating a request
      TestConnection (..)
    , mkTestConnection
    -- ** Request lenses
    , tcReplicationInstanceArn
    , tcEndpointArn

    -- * Destructuring the response
    , TestConnectionResponse (..)
    , mkTestConnectionResponse
    -- ** Response lenses
    , tcrrsConnection
    , tcrrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkTestConnection' smart constructor.
data TestConnection = TestConnection'
  { replicationInstanceArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the replication instance.
  , endpointArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestConnection' value with any optional fields omitted.
mkTestConnection
    :: Core.Text -- ^ 'replicationInstanceArn'
    -> Core.Text -- ^ 'endpointArn'
    -> TestConnection
mkTestConnection replicationInstanceArn endpointArn
  = TestConnection'{replicationInstanceArn, endpointArn}

-- | The Amazon Resource Name (ARN) of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcReplicationInstanceArn :: Lens.Lens' TestConnection Core.Text
tcReplicationInstanceArn = Lens.field @"replicationInstanceArn"
{-# INLINEABLE tcReplicationInstanceArn #-}
{-# DEPRECATED replicationInstanceArn "Use generic-lens or generic-optics with 'replicationInstanceArn' instead"  #-}

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcEndpointArn :: Lens.Lens' TestConnection Core.Text
tcEndpointArn = Lens.field @"endpointArn"
{-# INLINEABLE tcEndpointArn #-}
{-# DEPRECATED endpointArn "Use generic-lens or generic-optics with 'endpointArn' instead"  #-}

instance Core.ToQuery TestConnection where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders TestConnection where
        toHeaders TestConnection{..}
          = Core.pure ("X-Amz-Target", "AmazonDMSv20160101.TestConnection")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON TestConnection where
        toJSON TestConnection{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("ReplicationInstanceArn" Core..= replicationInstanceArn),
                  Core.Just ("EndpointArn" Core..= endpointArn)])

instance Core.AWSRequest TestConnection where
        type Rs TestConnection = TestConnectionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 TestConnectionResponse' Core.<$>
                   (x Core..:? "Connection") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkTestConnectionResponse' smart constructor.
data TestConnectionResponse = TestConnectionResponse'
  { connection :: Core.Maybe Types.Connection
    -- ^ The connection tested.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestConnectionResponse' value with any optional fields omitted.
mkTestConnectionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> TestConnectionResponse
mkTestConnectionResponse responseStatus
  = TestConnectionResponse'{connection = Core.Nothing,
                            responseStatus}

-- | The connection tested.
--
-- /Note:/ Consider using 'connection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcrrsConnection :: Lens.Lens' TestConnectionResponse (Core.Maybe Types.Connection)
tcrrsConnection = Lens.field @"connection"
{-# INLINEABLE tcrrsConnection #-}
{-# DEPRECATED connection "Use generic-lens or generic-optics with 'connection' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcrrsResponseStatus :: Lens.Lens' TestConnectionResponse Core.Int
tcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE tcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
