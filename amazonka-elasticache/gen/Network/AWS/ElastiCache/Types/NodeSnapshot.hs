{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.NodeSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.NodeSnapshot where

import Network.AWS.ElastiCache.Types.NodeGroupConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents an individual cache node in a snapshot of a cluster.
--
-- /See:/ 'newNodeSnapshot' smart constructor.
data NodeSnapshot = NodeSnapshot'
  { -- | The configuration for the source node group (shard).
    nodeGroupConfiguration :: Prelude.Maybe NodeGroupConfiguration,
    -- | The size of the cache on the source cache node.
    cacheSize :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the source cluster.
    cacheClusterId :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the source node\'s metadata and cache data set
    -- was obtained for the snapshot.
    snapshotCreateTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The date and time when the cache node was created in the source cluster.
    cacheNodeCreateTime :: Prelude.Maybe Prelude.ISO8601,
    -- | A unique identifier for the source node group (shard).
    nodeGroupId :: Prelude.Maybe Prelude.Text,
    -- | The cache node identifier for the node in the source cluster.
    cacheNodeId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NodeSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodeGroupConfiguration', 'nodeSnapshot_nodeGroupConfiguration' - The configuration for the source node group (shard).
--
-- 'cacheSize', 'nodeSnapshot_cacheSize' - The size of the cache on the source cache node.
--
-- 'cacheClusterId', 'nodeSnapshot_cacheClusterId' - A unique identifier for the source cluster.
--
-- 'snapshotCreateTime', 'nodeSnapshot_snapshotCreateTime' - The date and time when the source node\'s metadata and cache data set
-- was obtained for the snapshot.
--
-- 'cacheNodeCreateTime', 'nodeSnapshot_cacheNodeCreateTime' - The date and time when the cache node was created in the source cluster.
--
-- 'nodeGroupId', 'nodeSnapshot_nodeGroupId' - A unique identifier for the source node group (shard).
--
-- 'cacheNodeId', 'nodeSnapshot_cacheNodeId' - The cache node identifier for the node in the source cluster.
newNodeSnapshot ::
  NodeSnapshot
newNodeSnapshot =
  NodeSnapshot'
    { nodeGroupConfiguration =
        Prelude.Nothing,
      cacheSize = Prelude.Nothing,
      cacheClusterId = Prelude.Nothing,
      snapshotCreateTime = Prelude.Nothing,
      cacheNodeCreateTime = Prelude.Nothing,
      nodeGroupId = Prelude.Nothing,
      cacheNodeId = Prelude.Nothing
    }

-- | The configuration for the source node group (shard).
nodeSnapshot_nodeGroupConfiguration :: Lens.Lens' NodeSnapshot (Prelude.Maybe NodeGroupConfiguration)
nodeSnapshot_nodeGroupConfiguration = Lens.lens (\NodeSnapshot' {nodeGroupConfiguration} -> nodeGroupConfiguration) (\s@NodeSnapshot' {} a -> s {nodeGroupConfiguration = a} :: NodeSnapshot)

-- | The size of the cache on the source cache node.
nodeSnapshot_cacheSize :: Lens.Lens' NodeSnapshot (Prelude.Maybe Prelude.Text)
nodeSnapshot_cacheSize = Lens.lens (\NodeSnapshot' {cacheSize} -> cacheSize) (\s@NodeSnapshot' {} a -> s {cacheSize = a} :: NodeSnapshot)

-- | A unique identifier for the source cluster.
nodeSnapshot_cacheClusterId :: Lens.Lens' NodeSnapshot (Prelude.Maybe Prelude.Text)
nodeSnapshot_cacheClusterId = Lens.lens (\NodeSnapshot' {cacheClusterId} -> cacheClusterId) (\s@NodeSnapshot' {} a -> s {cacheClusterId = a} :: NodeSnapshot)

-- | The date and time when the source node\'s metadata and cache data set
-- was obtained for the snapshot.
nodeSnapshot_snapshotCreateTime :: Lens.Lens' NodeSnapshot (Prelude.Maybe Prelude.UTCTime)
nodeSnapshot_snapshotCreateTime = Lens.lens (\NodeSnapshot' {snapshotCreateTime} -> snapshotCreateTime) (\s@NodeSnapshot' {} a -> s {snapshotCreateTime = a} :: NodeSnapshot) Prelude.. Lens.mapping Prelude._Time

-- | The date and time when the cache node was created in the source cluster.
nodeSnapshot_cacheNodeCreateTime :: Lens.Lens' NodeSnapshot (Prelude.Maybe Prelude.UTCTime)
nodeSnapshot_cacheNodeCreateTime = Lens.lens (\NodeSnapshot' {cacheNodeCreateTime} -> cacheNodeCreateTime) (\s@NodeSnapshot' {} a -> s {cacheNodeCreateTime = a} :: NodeSnapshot) Prelude.. Lens.mapping Prelude._Time

-- | A unique identifier for the source node group (shard).
nodeSnapshot_nodeGroupId :: Lens.Lens' NodeSnapshot (Prelude.Maybe Prelude.Text)
nodeSnapshot_nodeGroupId = Lens.lens (\NodeSnapshot' {nodeGroupId} -> nodeGroupId) (\s@NodeSnapshot' {} a -> s {nodeGroupId = a} :: NodeSnapshot)

-- | The cache node identifier for the node in the source cluster.
nodeSnapshot_cacheNodeId :: Lens.Lens' NodeSnapshot (Prelude.Maybe Prelude.Text)
nodeSnapshot_cacheNodeId = Lens.lens (\NodeSnapshot' {cacheNodeId} -> cacheNodeId) (\s@NodeSnapshot' {} a -> s {cacheNodeId = a} :: NodeSnapshot)

instance Prelude.FromXML NodeSnapshot where
  parseXML x =
    NodeSnapshot'
      Prelude.<$> (x Prelude..@? "NodeGroupConfiguration")
      Prelude.<*> (x Prelude..@? "CacheSize")
      Prelude.<*> (x Prelude..@? "CacheClusterId")
      Prelude.<*> (x Prelude..@? "SnapshotCreateTime")
      Prelude.<*> (x Prelude..@? "CacheNodeCreateTime")
      Prelude.<*> (x Prelude..@? "NodeGroupId")
      Prelude.<*> (x Prelude..@? "CacheNodeId")

instance Prelude.Hashable NodeSnapshot

instance Prelude.NFData NodeSnapshot
