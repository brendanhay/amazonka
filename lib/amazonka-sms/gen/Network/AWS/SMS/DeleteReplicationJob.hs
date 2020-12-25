{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.DeleteReplicationJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified replication job.
--
-- After you delete a replication job, there are no further replication runs. AWS deletes the contents of the Amazon S3 bucket used to store AWS SMS artifacts. The AMIs created by the replication runs are not deleted.
module Network.AWS.SMS.DeleteReplicationJob
  ( -- * Creating a request
    DeleteReplicationJob (..),
    mkDeleteReplicationJob,

    -- ** Request lenses
    drjReplicationJobId,

    -- * Destructuring the response
    DeleteReplicationJobResponse (..),
    mkDeleteReplicationJobResponse,

    -- ** Response lenses
    drjrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkDeleteReplicationJob' smart constructor.
newtype DeleteReplicationJob = DeleteReplicationJob'
  { -- | The ID of the replication job.
    replicationJobId :: Types.ReplicationJobId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReplicationJob' value with any optional fields omitted.
mkDeleteReplicationJob ::
  -- | 'replicationJobId'
  Types.ReplicationJobId ->
  DeleteReplicationJob
mkDeleteReplicationJob replicationJobId =
  DeleteReplicationJob' {replicationJobId}

-- | The ID of the replication job.
--
-- /Note:/ Consider using 'replicationJobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drjReplicationJobId :: Lens.Lens' DeleteReplicationJob Types.ReplicationJobId
drjReplicationJobId = Lens.field @"replicationJobId"
{-# DEPRECATED drjReplicationJobId "Use generic-lens or generic-optics with 'replicationJobId' instead." #-}

instance Core.FromJSON DeleteReplicationJob where
  toJSON DeleteReplicationJob {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("replicationJobId" Core..= replicationJobId)]
      )

instance Core.AWSRequest DeleteReplicationJob where
  type Rs DeleteReplicationJob = DeleteReplicationJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSServerMigrationService_V2016_10_24.DeleteReplicationJob"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteReplicationJobResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteReplicationJobResponse' smart constructor.
newtype DeleteReplicationJobResponse = DeleteReplicationJobResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReplicationJobResponse' value with any optional fields omitted.
mkDeleteReplicationJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteReplicationJobResponse
mkDeleteReplicationJobResponse responseStatus =
  DeleteReplicationJobResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drjrrsResponseStatus :: Lens.Lens' DeleteReplicationJobResponse Core.Int
drjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
