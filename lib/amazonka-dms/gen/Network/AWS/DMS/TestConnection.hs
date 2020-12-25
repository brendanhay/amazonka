{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    TestConnection (..),
    mkTestConnection,

    -- ** Request lenses
    tcReplicationInstanceArn,
    tcEndpointArn,

    -- * Destructuring the response
    TestConnectionResponse (..),
    mkTestConnectionResponse,

    -- ** Response lenses
    tcrrsConnection,
    tcrrsResponseStatus,
  )
where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkTestConnection' smart constructor.
data TestConnection = TestConnection'
  { -- | The Amazon Resource Name (ARN) of the replication instance.
    replicationInstanceArn :: Types.String,
    -- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
    endpointArn :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestConnection' value with any optional fields omitted.
mkTestConnection ::
  -- | 'replicationInstanceArn'
  Types.String ->
  -- | 'endpointArn'
  Types.String ->
  TestConnection
mkTestConnection replicationInstanceArn endpointArn =
  TestConnection' {replicationInstanceArn, endpointArn}

-- | The Amazon Resource Name (ARN) of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcReplicationInstanceArn :: Lens.Lens' TestConnection Types.String
tcReplicationInstanceArn = Lens.field @"replicationInstanceArn"
{-# DEPRECATED tcReplicationInstanceArn "Use generic-lens or generic-optics with 'replicationInstanceArn' instead." #-}

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcEndpointArn :: Lens.Lens' TestConnection Types.String
tcEndpointArn = Lens.field @"endpointArn"
{-# DEPRECATED tcEndpointArn "Use generic-lens or generic-optics with 'endpointArn' instead." #-}

instance Core.FromJSON TestConnection where
  toJSON TestConnection {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ReplicationInstanceArn" Core..= replicationInstanceArn),
            Core.Just ("EndpointArn" Core..= endpointArn)
          ]
      )

instance Core.AWSRequest TestConnection where
  type Rs TestConnection = TestConnectionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonDMSv20160101.TestConnection")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          TestConnectionResponse'
            Core.<$> (x Core..:? "Connection") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkTestConnectionResponse' smart constructor.
data TestConnectionResponse = TestConnectionResponse'
  { -- | The connection tested.
    connection :: Core.Maybe Types.Connection,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestConnectionResponse' value with any optional fields omitted.
mkTestConnectionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  TestConnectionResponse
mkTestConnectionResponse responseStatus =
  TestConnectionResponse'
    { connection = Core.Nothing,
      responseStatus
    }

-- | The connection tested.
--
-- /Note:/ Consider using 'connection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcrrsConnection :: Lens.Lens' TestConnectionResponse (Core.Maybe Types.Connection)
tcrrsConnection = Lens.field @"connection"
{-# DEPRECATED tcrrsConnection "Use generic-lens or generic-optics with 'connection' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcrrsResponseStatus :: Lens.Lens' TestConnectionResponse Core.Int
tcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED tcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
