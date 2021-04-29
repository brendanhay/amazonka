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
-- Module      : Network.AWS.ElastiCache.Types.PendingModifiedValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.PendingModifiedValues where

import Network.AWS.ElastiCache.Types.AuthTokenUpdateStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A group of settings that are applied to the cluster in the future, or
-- that are currently being applied.
--
-- /See:/ 'newPendingModifiedValues' smart constructor.
data PendingModifiedValues = PendingModifiedValues'
  { -- | The new number of cache nodes for the cluster.
    --
    -- For clusters running Redis, this value must be 1. For clusters running
    -- Memcached, this value must be between 1 and 20.
    numCacheNodes :: Prelude.Maybe Prelude.Int,
    -- | A list of cache node IDs that are being removed (or will be removed)
    -- from the cluster. A node ID is a 4-digit numeric identifier (0001, 0002,
    -- etc.).
    cacheNodeIdsToRemove :: Prelude.Maybe [Prelude.Text],
    -- | The auth token status
    authTokenStatus :: Prelude.Maybe AuthTokenUpdateStatus,
    -- | The new cache engine version that the cluster runs.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The cache node type that this cluster or replication group is scaled to.
    cacheNodeType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      cacheNodeIdsToRemove = Prelude.Nothing,
      authTokenStatus = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      cacheNodeType = Prelude.Nothing
    }

-- | The new number of cache nodes for the cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running
-- Memcached, this value must be between 1 and 20.
pendingModifiedValues_numCacheNodes :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Int)
pendingModifiedValues_numCacheNodes = Lens.lens (\PendingModifiedValues' {numCacheNodes} -> numCacheNodes) (\s@PendingModifiedValues' {} a -> s {numCacheNodes = a} :: PendingModifiedValues)

-- | A list of cache node IDs that are being removed (or will be removed)
-- from the cluster. A node ID is a 4-digit numeric identifier (0001, 0002,
-- etc.).
pendingModifiedValues_cacheNodeIdsToRemove :: Lens.Lens' PendingModifiedValues (Prelude.Maybe [Prelude.Text])
pendingModifiedValues_cacheNodeIdsToRemove = Lens.lens (\PendingModifiedValues' {cacheNodeIdsToRemove} -> cacheNodeIdsToRemove) (\s@PendingModifiedValues' {} a -> s {cacheNodeIdsToRemove = a} :: PendingModifiedValues) Prelude.. Lens.mapping Prelude._Coerce

-- | The auth token status
pendingModifiedValues_authTokenStatus :: Lens.Lens' PendingModifiedValues (Prelude.Maybe AuthTokenUpdateStatus)
pendingModifiedValues_authTokenStatus = Lens.lens (\PendingModifiedValues' {authTokenStatus} -> authTokenStatus) (\s@PendingModifiedValues' {} a -> s {authTokenStatus = a} :: PendingModifiedValues)

-- | The new cache engine version that the cluster runs.
pendingModifiedValues_engineVersion :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_engineVersion = Lens.lens (\PendingModifiedValues' {engineVersion} -> engineVersion) (\s@PendingModifiedValues' {} a -> s {engineVersion = a} :: PendingModifiedValues)

-- | The cache node type that this cluster or replication group is scaled to.
pendingModifiedValues_cacheNodeType :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_cacheNodeType = Lens.lens (\PendingModifiedValues' {cacheNodeType} -> cacheNodeType) (\s@PendingModifiedValues' {} a -> s {cacheNodeType = a} :: PendingModifiedValues)

instance Prelude.FromXML PendingModifiedValues where
  parseXML x =
    PendingModifiedValues'
      Prelude.<$> (x Prelude..@? "NumCacheNodes")
      Prelude.<*> ( x Prelude..@? "CacheNodeIdsToRemove"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "CacheNodeId")
                  )
      Prelude.<*> (x Prelude..@? "AuthTokenStatus")
      Prelude.<*> (x Prelude..@? "EngineVersion")
      Prelude.<*> (x Prelude..@? "CacheNodeType")

instance Prelude.Hashable PendingModifiedValues

instance Prelude.NFData PendingModifiedValues
