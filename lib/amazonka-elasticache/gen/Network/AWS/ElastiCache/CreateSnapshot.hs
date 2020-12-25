{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CreateSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a copy of an entire cluster or replication group at a specific moment in time.
module Network.AWS.ElastiCache.CreateSnapshot
  ( -- * Creating a request
    CreateSnapshot (..),
    mkCreateSnapshot,

    -- ** Request lenses
    cSnapshotName,
    cCacheClusterId,
    cKmsKeyId,
    cReplicationGroupId,

    -- * Destructuring the response
    CreateSnapshotResponse (..),
    mkCreateSnapshotResponse,

    -- ** Response lenses
    csrfrsSnapshot,
    csrfrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @CreateSnapshot@ operation.
--
-- /See:/ 'mkCreateSnapshot' smart constructor.
data CreateSnapshot = CreateSnapshot'
  { -- | A name for the snapshot being created.
    snapshotName :: Types.String,
    -- | The identifier of an existing cluster. The snapshot is created from this cluster.
    cacheClusterId :: Core.Maybe Types.String,
    -- | The ID of the KMS key used to encrypt the snapshot.
    kmsKeyId :: Core.Maybe Types.String,
    -- | The identifier of an existing replication group. The snapshot is created from this replication group.
    replicationGroupId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSnapshot' value with any optional fields omitted.
mkCreateSnapshot ::
  -- | 'snapshotName'
  Types.String ->
  CreateSnapshot
mkCreateSnapshot snapshotName =
  CreateSnapshot'
    { snapshotName,
      cacheClusterId = Core.Nothing,
      kmsKeyId = Core.Nothing,
      replicationGroupId = Core.Nothing
    }

-- | A name for the snapshot being created.
--
-- /Note:/ Consider using 'snapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSnapshotName :: Lens.Lens' CreateSnapshot Types.String
cSnapshotName = Lens.field @"snapshotName"
{-# DEPRECATED cSnapshotName "Use generic-lens or generic-optics with 'snapshotName' instead." #-}

-- | The identifier of an existing cluster. The snapshot is created from this cluster.
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCacheClusterId :: Lens.Lens' CreateSnapshot (Core.Maybe Types.String)
cCacheClusterId = Lens.field @"cacheClusterId"
{-# DEPRECATED cCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

-- | The ID of the KMS key used to encrypt the snapshot.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cKmsKeyId :: Lens.Lens' CreateSnapshot (Core.Maybe Types.String)
cKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED cKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The identifier of an existing replication group. The snapshot is created from this replication group.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cReplicationGroupId :: Lens.Lens' CreateSnapshot (Core.Maybe Types.String)
cReplicationGroupId = Lens.field @"replicationGroupId"
{-# DEPRECATED cReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

instance Core.AWSRequest CreateSnapshot where
  type Rs CreateSnapshot = CreateSnapshotResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateSnapshot")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> (Core.toQueryValue "SnapshotName" snapshotName)
                Core.<> (Core.toQueryValue "CacheClusterId" Core.<$> cacheClusterId)
                Core.<> (Core.toQueryValue "KmsKeyId" Core.<$> kmsKeyId)
                Core.<> ( Core.toQueryValue "ReplicationGroupId"
                            Core.<$> replicationGroupId
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateSnapshotResult"
      ( \s h x ->
          CreateSnapshotResponse'
            Core.<$> (x Core..@? "Snapshot") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateSnapshotResponse' smart constructor.
data CreateSnapshotResponse = CreateSnapshotResponse'
  { snapshot :: Core.Maybe Types.Snapshot,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateSnapshotResponse' value with any optional fields omitted.
mkCreateSnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateSnapshotResponse
mkCreateSnapshotResponse responseStatus =
  CreateSnapshotResponse' {snapshot = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'snapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrfrsSnapshot :: Lens.Lens' CreateSnapshotResponse (Core.Maybe Types.Snapshot)
csrfrsSnapshot = Lens.field @"snapshot"
{-# DEPRECATED csrfrsSnapshot "Use generic-lens or generic-optics with 'snapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrfrsResponseStatus :: Lens.Lens' CreateSnapshotResponse Core.Int
csrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
