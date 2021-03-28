{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateSnapshot (..)
    , mkCreateSnapshot
    -- ** Request lenses
    , cSnapshotName
    , cCacheClusterId
    , cKmsKeyId
    , cReplicationGroupId

    -- * Destructuring the response
    , CreateSnapshotResponse (..)
    , mkCreateSnapshotResponse
    -- ** Response lenses
    , csrfrsSnapshot
    , csrfrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @CreateSnapshot@ operation.
--
-- /See:/ 'mkCreateSnapshot' smart constructor.
data CreateSnapshot = CreateSnapshot'
  { snapshotName :: Core.Text
    -- ^ A name for the snapshot being created.
  , cacheClusterId :: Core.Maybe Core.Text
    -- ^ The identifier of an existing cluster. The snapshot is created from this cluster.
  , kmsKeyId :: Core.Maybe Core.Text
    -- ^ The ID of the KMS key used to encrypt the snapshot.
  , replicationGroupId :: Core.Maybe Core.Text
    -- ^ The identifier of an existing replication group. The snapshot is created from this replication group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSnapshot' value with any optional fields omitted.
mkCreateSnapshot
    :: Core.Text -- ^ 'snapshotName'
    -> CreateSnapshot
mkCreateSnapshot snapshotName
  = CreateSnapshot'{snapshotName, cacheClusterId = Core.Nothing,
                    kmsKeyId = Core.Nothing, replicationGroupId = Core.Nothing}

-- | A name for the snapshot being created.
--
-- /Note:/ Consider using 'snapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSnapshotName :: Lens.Lens' CreateSnapshot Core.Text
cSnapshotName = Lens.field @"snapshotName"
{-# INLINEABLE cSnapshotName #-}
{-# DEPRECATED snapshotName "Use generic-lens or generic-optics with 'snapshotName' instead"  #-}

-- | The identifier of an existing cluster. The snapshot is created from this cluster.
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCacheClusterId :: Lens.Lens' CreateSnapshot (Core.Maybe Core.Text)
cCacheClusterId = Lens.field @"cacheClusterId"
{-# INLINEABLE cCacheClusterId #-}
{-# DEPRECATED cacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead"  #-}

-- | The ID of the KMS key used to encrypt the snapshot.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cKmsKeyId :: Lens.Lens' CreateSnapshot (Core.Maybe Core.Text)
cKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE cKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The identifier of an existing replication group. The snapshot is created from this replication group.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cReplicationGroupId :: Lens.Lens' CreateSnapshot (Core.Maybe Core.Text)
cReplicationGroupId = Lens.field @"replicationGroupId"
{-# INLINEABLE cReplicationGroupId #-}
{-# DEPRECATED replicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead"  #-}

instance Core.ToQuery CreateSnapshot where
        toQuery CreateSnapshot{..}
          = Core.toQueryPair "Action" ("CreateSnapshot" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<> Core.toQueryPair "SnapshotName" snapshotName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CacheClusterId")
                cacheClusterId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "KmsKeyId") kmsKeyId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ReplicationGroupId")
                replicationGroupId

instance Core.ToHeaders CreateSnapshot where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateSnapshot where
        type Rs CreateSnapshot = CreateSnapshotResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "CreateSnapshotResult"
              (\ s h x ->
                 CreateSnapshotResponse' Core.<$>
                   (x Core..@? "Snapshot") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateSnapshotResponse' smart constructor.
data CreateSnapshotResponse = CreateSnapshotResponse'
  { snapshot :: Core.Maybe Types.Snapshot
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateSnapshotResponse' value with any optional fields omitted.
mkCreateSnapshotResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateSnapshotResponse
mkCreateSnapshotResponse responseStatus
  = CreateSnapshotResponse'{snapshot = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'snapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrfrsSnapshot :: Lens.Lens' CreateSnapshotResponse (Core.Maybe Types.Snapshot)
csrfrsSnapshot = Lens.field @"snapshot"
{-# INLINEABLE csrfrsSnapshot #-}
{-# DEPRECATED snapshot "Use generic-lens or generic-optics with 'snapshot' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrfrsResponseStatus :: Lens.Lens' CreateSnapshotResponse Core.Int
csrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
