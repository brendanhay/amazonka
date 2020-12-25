{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteConnection (..),
    mkDeleteConnection,

    -- ** Request lenses
    dcEndpointArn,
    dcReplicationInstanceArn,

    -- * Destructuring the response
    DeleteConnectionResponse (..),
    mkDeleteConnectionResponse,

    -- ** Response lenses
    dcrrsConnection,
    dcrrsResponseStatus,
  )
where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDeleteConnection' smart constructor.
data DeleteConnection = DeleteConnection'
  { -- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
    endpointArn :: Types.EndpointArn,
    -- | The Amazon Resource Name (ARN) of the replication instance.
    replicationInstanceArn :: Types.ReplicationInstanceArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConnection' value with any optional fields omitted.
mkDeleteConnection ::
  -- | 'endpointArn'
  Types.EndpointArn ->
  -- | 'replicationInstanceArn'
  Types.ReplicationInstanceArn ->
  DeleteConnection
mkDeleteConnection endpointArn replicationInstanceArn =
  DeleteConnection' {endpointArn, replicationInstanceArn}

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcEndpointArn :: Lens.Lens' DeleteConnection Types.EndpointArn
dcEndpointArn = Lens.field @"endpointArn"
{-# DEPRECATED dcEndpointArn "Use generic-lens or generic-optics with 'endpointArn' instead." #-}

-- | The Amazon Resource Name (ARN) of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcReplicationInstanceArn :: Lens.Lens' DeleteConnection Types.ReplicationInstanceArn
dcReplicationInstanceArn = Lens.field @"replicationInstanceArn"
{-# DEPRECATED dcReplicationInstanceArn "Use generic-lens or generic-optics with 'replicationInstanceArn' instead." #-}

instance Core.FromJSON DeleteConnection where
  toJSON DeleteConnection {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("EndpointArn" Core..= endpointArn),
            Core.Just
              ("ReplicationInstanceArn" Core..= replicationInstanceArn)
          ]
      )

instance Core.AWSRequest DeleteConnection where
  type Rs DeleteConnection = DeleteConnectionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonDMSv20160101.DeleteConnection")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteConnectionResponse'
            Core.<$> (x Core..:? "Connection") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkDeleteConnectionResponse' smart constructor.
data DeleteConnectionResponse = DeleteConnectionResponse'
  { -- | The connection that is being deleted.
    connection :: Core.Maybe Types.Connection,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConnectionResponse' value with any optional fields omitted.
mkDeleteConnectionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteConnectionResponse
mkDeleteConnectionResponse responseStatus =
  DeleteConnectionResponse'
    { connection = Core.Nothing,
      responseStatus
    }

-- | The connection that is being deleted.
--
-- /Note:/ Consider using 'connection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsConnection :: Lens.Lens' DeleteConnectionResponse (Core.Maybe Types.Connection)
dcrrsConnection = Lens.field @"connection"
{-# DEPRECATED dcrrsConnection "Use generic-lens or generic-optics with 'connection' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DeleteConnectionResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
