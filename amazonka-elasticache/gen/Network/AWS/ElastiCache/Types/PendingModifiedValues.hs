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
-- Module      : Network.AWS.ElastiCache.Types.PendingModifiedValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.PendingModifiedValues where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types.AuthTokenUpdateStatus
import qualified Network.AWS.Lens as Lens

-- | A group of settings that are applied to the cluster in the future, or
-- that are currently being applied.
--
-- /See:/ 'newPendingModifiedValues' smart constructor.
data PendingModifiedValues = PendingModifiedValues'
  { -- | The new number of cache nodes for the cluster.
    --
    -- For clusters running Redis, this value must be 1. For clusters running
    -- Memcached, this value must be between 1 and 20.
    numCacheNodes :: Core.Maybe Core.Int,
    -- | A list of cache node IDs that are being removed (or will be removed)
    -- from the cluster. A node ID is a 4-digit numeric identifier (0001, 0002,
    -- etc.).
    cacheNodeIdsToRemove :: Core.Maybe [Core.Text],
    -- | The auth token status
    authTokenStatus :: Core.Maybe AuthTokenUpdateStatus,
    -- | The new cache engine version that the cluster runs.
    engineVersion :: Core.Maybe Core.Text,
    -- | The cache node type that this cluster or replication group is scaled to.
    cacheNodeType :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PendingModifiedValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numCacheNodes', 'pendingModifiedValues_numCacheNodes' - The new number of cache nodes for the cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running
-- Memcached, this value must be between 1 and 20.
--
-- 'cacheNodeIdsToRemove', 'pendingModifiedValues_cacheNodeIdsToRemove' - A list of cache node IDs that are being removed (or will be removed)
-- from the cluster. A node ID is a 4-digit numeric identifier (0001, 0002,
-- etc.).
--
-- 'authTokenStatus', 'pendingModifiedValues_authTokenStatus' - The auth token status
--
-- 'engineVersion', 'pendingModifiedValues_engineVersion' - The new cache engine version that the cluster runs.
--
-- 'cacheNodeType', 'pendingModifiedValues_cacheNodeType' - The cache node type that this cluster or replication group is scaled to.
newPendingModifiedValues ::
  PendingModifiedValues
newPendingModifiedValues =
  PendingModifiedValues'
    { numCacheNodes =
        Core.Nothing,
      cacheNodeIdsToRemove = Core.Nothing,
      authTokenStatus = Core.Nothing,
      engineVersion = Core.Nothing,
      cacheNodeType = Core.Nothing
    }

-- | The new number of cache nodes for the cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running
-- Memcached, this value must be between 1 and 20.
pendingModifiedValues_numCacheNodes :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Int)
pendingModifiedValues_numCacheNodes = Lens.lens (\PendingModifiedValues' {numCacheNodes} -> numCacheNodes) (\s@PendingModifiedValues' {} a -> s {numCacheNodes = a} :: PendingModifiedValues)

-- | A list of cache node IDs that are being removed (or will be removed)
-- from the cluster. A node ID is a 4-digit numeric identifier (0001, 0002,
-- etc.).
pendingModifiedValues_cacheNodeIdsToRemove :: Lens.Lens' PendingModifiedValues (Core.Maybe [Core.Text])
pendingModifiedValues_cacheNodeIdsToRemove = Lens.lens (\PendingModifiedValues' {cacheNodeIdsToRemove} -> cacheNodeIdsToRemove) (\s@PendingModifiedValues' {} a -> s {cacheNodeIdsToRemove = a} :: PendingModifiedValues) Core.. Lens.mapping Lens._Coerce

-- | The auth token status
pendingModifiedValues_authTokenStatus :: Lens.Lens' PendingModifiedValues (Core.Maybe AuthTokenUpdateStatus)
pendingModifiedValues_authTokenStatus = Lens.lens (\PendingModifiedValues' {authTokenStatus} -> authTokenStatus) (\s@PendingModifiedValues' {} a -> s {authTokenStatus = a} :: PendingModifiedValues)

-- | The new cache engine version that the cluster runs.
pendingModifiedValues_engineVersion :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Text)
pendingModifiedValues_engineVersion = Lens.lens (\PendingModifiedValues' {engineVersion} -> engineVersion) (\s@PendingModifiedValues' {} a -> s {engineVersion = a} :: PendingModifiedValues)

-- | The cache node type that this cluster or replication group is scaled to.
pendingModifiedValues_cacheNodeType :: Lens.Lens' PendingModifiedValues (Core.Maybe Core.Text)
pendingModifiedValues_cacheNodeType = Lens.lens (\PendingModifiedValues' {cacheNodeType} -> cacheNodeType) (\s@PendingModifiedValues' {} a -> s {cacheNodeType = a} :: PendingModifiedValues)

instance Core.FromXML PendingModifiedValues where
  parseXML x =
    PendingModifiedValues'
      Core.<$> (x Core..@? "NumCacheNodes")
      Core.<*> ( x Core..@? "CacheNodeIdsToRemove"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "CacheNodeId")
               )
      Core.<*> (x Core..@? "AuthTokenStatus")
      Core.<*> (x Core..@? "EngineVersion")
      Core.<*> (x Core..@? "CacheNodeType")

instance Core.Hashable PendingModifiedValues

instance Core.NFData PendingModifiedValues
