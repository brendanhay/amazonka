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
-- Module      : Amazonka.ElastiCache.Types.NodeSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.NodeSnapshot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types.NodeGroupConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Represents an individual cache node in a snapshot of a cluster.
--
-- /See:/ 'newNodeSnapshot' smart constructor.
data NodeSnapshot = NodeSnapshot'
  { -- | A unique identifier for the source cluster.
    cacheClusterId :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the cache node was created in the source cluster.
    cacheNodeCreateTime :: Prelude.Maybe Data.ISO8601,
    -- | The cache node identifier for the node in the source cluster.
    cacheNodeId :: Prelude.Maybe Prelude.Text,
    -- | The size of the cache on the source cache node.
    cacheSize :: Prelude.Maybe Prelude.Text,
    -- | The configuration for the source node group (shard).
    nodeGroupConfiguration :: Prelude.Maybe NodeGroupConfiguration,
    -- | A unique identifier for the source node group (shard).
    nodeGroupId :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the source node\'s metadata and cache data set
    -- was obtained for the snapshot.
    snapshotCreateTime :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheClusterId', 'nodeSnapshot_cacheClusterId' - A unique identifier for the source cluster.
--
-- 'cacheNodeCreateTime', 'nodeSnapshot_cacheNodeCreateTime' - The date and time when the cache node was created in the source cluster.
--
-- 'cacheNodeId', 'nodeSnapshot_cacheNodeId' - The cache node identifier for the node in the source cluster.
--
-- 'cacheSize', 'nodeSnapshot_cacheSize' - The size of the cache on the source cache node.
--
-- 'nodeGroupConfiguration', 'nodeSnapshot_nodeGroupConfiguration' - The configuration for the source node group (shard).
--
-- 'nodeGroupId', 'nodeSnapshot_nodeGroupId' - A unique identifier for the source node group (shard).
--
-- 'snapshotCreateTime', 'nodeSnapshot_snapshotCreateTime' - The date and time when the source node\'s metadata and cache data set
-- was obtained for the snapshot.
newNodeSnapshot ::
  NodeSnapshot
newNodeSnapshot =
  NodeSnapshot'
    { cacheClusterId = Prelude.Nothing,
      cacheNodeCreateTime = Prelude.Nothing,
      cacheNodeId = Prelude.Nothing,
      cacheSize = Prelude.Nothing,
      nodeGroupConfiguration = Prelude.Nothing,
      nodeGroupId = Prelude.Nothing,
      snapshotCreateTime = Prelude.Nothing
    }

-- | A unique identifier for the source cluster.
nodeSnapshot_cacheClusterId :: Lens.Lens' NodeSnapshot (Prelude.Maybe Prelude.Text)
nodeSnapshot_cacheClusterId = Lens.lens (\NodeSnapshot' {cacheClusterId} -> cacheClusterId) (\s@NodeSnapshot' {} a -> s {cacheClusterId = a} :: NodeSnapshot)

-- | The date and time when the cache node was created in the source cluster.
nodeSnapshot_cacheNodeCreateTime :: Lens.Lens' NodeSnapshot (Prelude.Maybe Prelude.UTCTime)
nodeSnapshot_cacheNodeCreateTime = Lens.lens (\NodeSnapshot' {cacheNodeCreateTime} -> cacheNodeCreateTime) (\s@NodeSnapshot' {} a -> s {cacheNodeCreateTime = a} :: NodeSnapshot) Prelude.. Lens.mapping Data._Time

-- | The cache node identifier for the node in the source cluster.
nodeSnapshot_cacheNodeId :: Lens.Lens' NodeSnapshot (Prelude.Maybe Prelude.Text)
nodeSnapshot_cacheNodeId = Lens.lens (\NodeSnapshot' {cacheNodeId} -> cacheNodeId) (\s@NodeSnapshot' {} a -> s {cacheNodeId = a} :: NodeSnapshot)

-- | The size of the cache on the source cache node.
nodeSnapshot_cacheSize :: Lens.Lens' NodeSnapshot (Prelude.Maybe Prelude.Text)
nodeSnapshot_cacheSize = Lens.lens (\NodeSnapshot' {cacheSize} -> cacheSize) (\s@NodeSnapshot' {} a -> s {cacheSize = a} :: NodeSnapshot)

-- | The configuration for the source node group (shard).
nodeSnapshot_nodeGroupConfiguration :: Lens.Lens' NodeSnapshot (Prelude.Maybe NodeGroupConfiguration)
nodeSnapshot_nodeGroupConfiguration = Lens.lens (\NodeSnapshot' {nodeGroupConfiguration} -> nodeGroupConfiguration) (\s@NodeSnapshot' {} a -> s {nodeGroupConfiguration = a} :: NodeSnapshot)

-- | A unique identifier for the source node group (shard).
nodeSnapshot_nodeGroupId :: Lens.Lens' NodeSnapshot (Prelude.Maybe Prelude.Text)
nodeSnapshot_nodeGroupId = Lens.lens (\NodeSnapshot' {nodeGroupId} -> nodeGroupId) (\s@NodeSnapshot' {} a -> s {nodeGroupId = a} :: NodeSnapshot)

-- | The date and time when the source node\'s metadata and cache data set
-- was obtained for the snapshot.
nodeSnapshot_snapshotCreateTime :: Lens.Lens' NodeSnapshot (Prelude.Maybe Prelude.UTCTime)
nodeSnapshot_snapshotCreateTime = Lens.lens (\NodeSnapshot' {snapshotCreateTime} -> snapshotCreateTime) (\s@NodeSnapshot' {} a -> s {snapshotCreateTime = a} :: NodeSnapshot) Prelude.. Lens.mapping Data._Time

instance Data.FromXML NodeSnapshot where
  parseXML x =
    NodeSnapshot'
      Prelude.<$> (x Data..@? "CacheClusterId")
      Prelude.<*> (x Data..@? "CacheNodeCreateTime")
      Prelude.<*> (x Data..@? "CacheNodeId")
      Prelude.<*> (x Data..@? "CacheSize")
      Prelude.<*> (x Data..@? "NodeGroupConfiguration")
      Prelude.<*> (x Data..@? "NodeGroupId")
      Prelude.<*> (x Data..@? "SnapshotCreateTime")

instance Prelude.Hashable NodeSnapshot where
  hashWithSalt _salt NodeSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` cacheClusterId
      `Prelude.hashWithSalt` cacheNodeCreateTime
      `Prelude.hashWithSalt` cacheNodeId
      `Prelude.hashWithSalt` cacheSize
      `Prelude.hashWithSalt` nodeGroupConfiguration
      `Prelude.hashWithSalt` nodeGroupId
      `Prelude.hashWithSalt` snapshotCreateTime

instance Prelude.NFData NodeSnapshot where
  rnf NodeSnapshot' {..} =
    Prelude.rnf cacheClusterId `Prelude.seq`
      Prelude.rnf cacheNodeCreateTime `Prelude.seq`
        Prelude.rnf cacheNodeId `Prelude.seq`
          Prelude.rnf cacheSize `Prelude.seq`
            Prelude.rnf nodeGroupConfiguration `Prelude.seq`
              Prelude.rnf nodeGroupId `Prelude.seq`
                Prelude.rnf snapshotCreateTime
