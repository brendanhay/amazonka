{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DeleteReplicationInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified replication instance.
module Network.AWS.DMS.DeleteReplicationInstance
  ( -- * Creating a request
    DeleteReplicationInstance (..),
    mkDeleteReplicationInstance,

    -- ** Request lenses
    driReplicationInstanceArn,

    -- * Destructuring the response
    DeleteReplicationInstanceResponse (..),
    mkDeleteReplicationInstanceResponse,

    -- ** Response lenses
    drirrsReplicationInstance,
    drirrsResponseStatus,
  )
where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDeleteReplicationInstance' smart constructor.
newtype DeleteReplicationInstance = DeleteReplicationInstance'
  { -- | The Amazon Resource Name (ARN) of the replication instance to be deleted.
    replicationInstanceArn :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReplicationInstance' value with any optional fields omitted.
mkDeleteReplicationInstance ::
  -- | 'replicationInstanceArn'
  Types.String ->
  DeleteReplicationInstance
mkDeleteReplicationInstance replicationInstanceArn =
  DeleteReplicationInstance' {replicationInstanceArn}

-- | The Amazon Resource Name (ARN) of the replication instance to be deleted.
--
-- /Note:/ Consider using 'replicationInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
driReplicationInstanceArn :: Lens.Lens' DeleteReplicationInstance Types.String
driReplicationInstanceArn = Lens.field @"replicationInstanceArn"
{-# DEPRECATED driReplicationInstanceArn "Use generic-lens or generic-optics with 'replicationInstanceArn' instead." #-}

instance Core.FromJSON DeleteReplicationInstance where
  toJSON DeleteReplicationInstance {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ReplicationInstanceArn" Core..= replicationInstanceArn)
          ]
      )

instance Core.AWSRequest DeleteReplicationInstance where
  type
    Rs DeleteReplicationInstance =
      DeleteReplicationInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonDMSv20160101.DeleteReplicationInstance")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteReplicationInstanceResponse'
            Core.<$> (x Core..:? "ReplicationInstance")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkDeleteReplicationInstanceResponse' smart constructor.
data DeleteReplicationInstanceResponse = DeleteReplicationInstanceResponse'
  { -- | The replication instance that was deleted.
    replicationInstance :: Core.Maybe Types.ReplicationInstance,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteReplicationInstanceResponse' value with any optional fields omitted.
mkDeleteReplicationInstanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteReplicationInstanceResponse
mkDeleteReplicationInstanceResponse responseStatus =
  DeleteReplicationInstanceResponse'
    { replicationInstance =
        Core.Nothing,
      responseStatus
    }

-- | The replication instance that was deleted.
--
-- /Note:/ Consider using 'replicationInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drirrsReplicationInstance :: Lens.Lens' DeleteReplicationInstanceResponse (Core.Maybe Types.ReplicationInstance)
drirrsReplicationInstance = Lens.field @"replicationInstance"
{-# DEPRECATED drirrsReplicationInstance "Use generic-lens or generic-optics with 'replicationInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drirrsResponseStatus :: Lens.Lens' DeleteReplicationInstanceResponse Core.Int
drirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
