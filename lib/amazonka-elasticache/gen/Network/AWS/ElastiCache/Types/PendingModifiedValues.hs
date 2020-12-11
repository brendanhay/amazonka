-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.PendingModifiedValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.PendingModifiedValues
  ( PendingModifiedValues (..),

    -- * Smart constructor
    mkPendingModifiedValues,

    -- * Lenses
    pmvEngineVersion,
    pmvCacheNodeType,
    pmvAuthTokenStatus,
    pmvCacheNodeIdsToRemove,
    pmvNumCacheNodes,
  )
where

import Network.AWS.ElastiCache.Types.AuthTokenUpdateStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A group of settings that are applied to the cluster in the future, or that are currently being applied.
--
-- /See:/ 'mkPendingModifiedValues' smart constructor.
data PendingModifiedValues = PendingModifiedValues'
  { engineVersion ::
      Lude.Maybe Lude.Text,
    cacheNodeType :: Lude.Maybe Lude.Text,
    authTokenStatus ::
      Lude.Maybe AuthTokenUpdateStatus,
    cacheNodeIdsToRemove :: Lude.Maybe [Lude.Text],
    numCacheNodes :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PendingModifiedValues' with the minimum fields required to make a request.
--
-- * 'authTokenStatus' - The auth token status
-- * 'cacheNodeIdsToRemove' - A list of cache node IDs that are being removed (or will be removed) from the cluster. A node ID is a 4-digit numeric identifier (0001, 0002, etc.).
-- * 'cacheNodeType' - The cache node type that this cluster or replication group is scaled to.
-- * 'engineVersion' - The new cache engine version that the cluster runs.
-- * 'numCacheNodes' - The new number of cache nodes for the cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
mkPendingModifiedValues ::
  PendingModifiedValues
mkPendingModifiedValues =
  PendingModifiedValues'
    { engineVersion = Lude.Nothing,
      cacheNodeType = Lude.Nothing,
      authTokenStatus = Lude.Nothing,
      cacheNodeIdsToRemove = Lude.Nothing,
      numCacheNodes = Lude.Nothing
    }

-- | The new cache engine version that the cluster runs.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvEngineVersion :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Text)
pmvEngineVersion = Lens.lens (engineVersion :: PendingModifiedValues -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: PendingModifiedValues)
{-# DEPRECATED pmvEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The cache node type that this cluster or replication group is scaled to.
--
-- /Note:/ Consider using 'cacheNodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvCacheNodeType :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Text)
pmvCacheNodeType = Lens.lens (cacheNodeType :: PendingModifiedValues -> Lude.Maybe Lude.Text) (\s a -> s {cacheNodeType = a} :: PendingModifiedValues)
{-# DEPRECATED pmvCacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead." #-}

-- | The auth token status
--
-- /Note:/ Consider using 'authTokenStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvAuthTokenStatus :: Lens.Lens' PendingModifiedValues (Lude.Maybe AuthTokenUpdateStatus)
pmvAuthTokenStatus = Lens.lens (authTokenStatus :: PendingModifiedValues -> Lude.Maybe AuthTokenUpdateStatus) (\s a -> s {authTokenStatus = a} :: PendingModifiedValues)
{-# DEPRECATED pmvAuthTokenStatus "Use generic-lens or generic-optics with 'authTokenStatus' instead." #-}

-- | A list of cache node IDs that are being removed (or will be removed) from the cluster. A node ID is a 4-digit numeric identifier (0001, 0002, etc.).
--
-- /Note:/ Consider using 'cacheNodeIdsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvCacheNodeIdsToRemove :: Lens.Lens' PendingModifiedValues (Lude.Maybe [Lude.Text])
pmvCacheNodeIdsToRemove = Lens.lens (cacheNodeIdsToRemove :: PendingModifiedValues -> Lude.Maybe [Lude.Text]) (\s a -> s {cacheNodeIdsToRemove = a} :: PendingModifiedValues)
{-# DEPRECATED pmvCacheNodeIdsToRemove "Use generic-lens or generic-optics with 'cacheNodeIdsToRemove' instead." #-}

-- | The new number of cache nodes for the cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
--
-- /Note:/ Consider using 'numCacheNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmvNumCacheNodes :: Lens.Lens' PendingModifiedValues (Lude.Maybe Lude.Int)
pmvNumCacheNodes = Lens.lens (numCacheNodes :: PendingModifiedValues -> Lude.Maybe Lude.Int) (\s a -> s {numCacheNodes = a} :: PendingModifiedValues)
{-# DEPRECATED pmvNumCacheNodes "Use generic-lens or generic-optics with 'numCacheNodes' instead." #-}

instance Lude.FromXML PendingModifiedValues where
  parseXML x =
    PendingModifiedValues'
      Lude.<$> (x Lude..@? "EngineVersion")
      Lude.<*> (x Lude..@? "CacheNodeType")
      Lude.<*> (x Lude..@? "AuthTokenStatus")
      Lude.<*> ( x Lude..@? "CacheNodeIdsToRemove" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "CacheNodeId")
               )
      Lude.<*> (x Lude..@? "NumCacheNodes")
