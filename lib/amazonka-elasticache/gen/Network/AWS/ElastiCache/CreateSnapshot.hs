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
    cCacheClusterId,
    cKMSKeyId,
    cSnapshotName,
    cReplicationGroupId,

    -- * Destructuring the response
    CreateSnapshotResponse (..),
    mkCreateSnapshotResponse,

    -- ** Response lenses
    csfrsSnapshot,
    csfrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @CreateSnapshot@ operation.
--
-- /See:/ 'mkCreateSnapshot' smart constructor.
data CreateSnapshot = CreateSnapshot'
  { -- | The identifier of an existing cluster. The snapshot is created from this cluster.
    cacheClusterId :: Lude.Maybe Lude.Text,
    -- | The ID of the KMS key used to encrypt the snapshot.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | A name for the snapshot being created.
    snapshotName :: Lude.Text,
    -- | The identifier of an existing replication group. The snapshot is created from this replication group.
    replicationGroupId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSnapshot' with the minimum fields required to make a request.
--
-- * 'cacheClusterId' - The identifier of an existing cluster. The snapshot is created from this cluster.
-- * 'kmsKeyId' - The ID of the KMS key used to encrypt the snapshot.
-- * 'snapshotName' - A name for the snapshot being created.
-- * 'replicationGroupId' - The identifier of an existing replication group. The snapshot is created from this replication group.
mkCreateSnapshot ::
  -- | 'snapshotName'
  Lude.Text ->
  CreateSnapshot
mkCreateSnapshot pSnapshotName_ =
  CreateSnapshot'
    { cacheClusterId = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      snapshotName = pSnapshotName_,
      replicationGroupId = Lude.Nothing
    }

-- | The identifier of an existing cluster. The snapshot is created from this cluster.
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCacheClusterId :: Lens.Lens' CreateSnapshot (Lude.Maybe Lude.Text)
cCacheClusterId = Lens.lens (cacheClusterId :: CreateSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {cacheClusterId = a} :: CreateSnapshot)
{-# DEPRECATED cCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

-- | The ID of the KMS key used to encrypt the snapshot.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cKMSKeyId :: Lens.Lens' CreateSnapshot (Lude.Maybe Lude.Text)
cKMSKeyId = Lens.lens (kmsKeyId :: CreateSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: CreateSnapshot)
{-# DEPRECATED cKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | A name for the snapshot being created.
--
-- /Note:/ Consider using 'snapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSnapshotName :: Lens.Lens' CreateSnapshot Lude.Text
cSnapshotName = Lens.lens (snapshotName :: CreateSnapshot -> Lude.Text) (\s a -> s {snapshotName = a} :: CreateSnapshot)
{-# DEPRECATED cSnapshotName "Use generic-lens or generic-optics with 'snapshotName' instead." #-}

-- | The identifier of an existing replication group. The snapshot is created from this replication group.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cReplicationGroupId :: Lens.Lens' CreateSnapshot (Lude.Maybe Lude.Text)
cReplicationGroupId = Lens.lens (replicationGroupId :: CreateSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {replicationGroupId = a} :: CreateSnapshot)
{-# DEPRECATED cReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

instance Lude.AWSRequest CreateSnapshot where
  type Rs CreateSnapshot = CreateSnapshotResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "CreateSnapshotResult"
      ( \s h x ->
          CreateSnapshotResponse'
            Lude.<$> (x Lude..@? "Snapshot") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSnapshot where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateSnapshot where
  toQuery CreateSnapshot' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateSnapshot" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "CacheClusterId" Lude.=: cacheClusterId,
        "KmsKeyId" Lude.=: kmsKeyId,
        "SnapshotName" Lude.=: snapshotName,
        "ReplicationGroupId" Lude.=: replicationGroupId
      ]

-- | /See:/ 'mkCreateSnapshotResponse' smart constructor.
data CreateSnapshotResponse = CreateSnapshotResponse'
  { snapshot :: Lude.Maybe Snapshot,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'snapshot' -
-- * 'responseStatus' - The response status code.
mkCreateSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSnapshotResponse
mkCreateSnapshotResponse pResponseStatus_ =
  CreateSnapshotResponse'
    { snapshot = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'snapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfrsSnapshot :: Lens.Lens' CreateSnapshotResponse (Lude.Maybe Snapshot)
csfrsSnapshot = Lens.lens (snapshot :: CreateSnapshotResponse -> Lude.Maybe Snapshot) (\s a -> s {snapshot = a} :: CreateSnapshotResponse)
{-# DEPRECATED csfrsSnapshot "Use generic-lens or generic-optics with 'snapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfrsResponseStatus :: Lens.Lens' CreateSnapshotResponse Lude.Int
csfrsResponseStatus = Lens.lens (responseStatus :: CreateSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSnapshotResponse)
{-# DEPRECATED csfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
