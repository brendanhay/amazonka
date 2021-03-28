{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.RegionalConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.RegionalConfiguration
  ( RegionalConfiguration (..)
  -- * Smart constructor
  , mkRegionalConfiguration
  -- * Lenses
  , rcReplicationGroupId
  , rcReplicationGroupRegion
  , rcReshardingConfiguration
  ) where

import qualified Network.AWS.ElastiCache.Types.ReshardingConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of the replication groups 
--
-- /See:/ 'mkRegionalConfiguration' smart constructor.
data RegionalConfiguration = RegionalConfiguration'
  { replicationGroupId :: Core.Text
    -- ^ The name of the secondary cluster
  , replicationGroupRegion :: Core.Text
    -- ^ The AWS region where the cluster is stored
  , reshardingConfiguration :: [Types.ReshardingConfiguration]
    -- ^ A list of @PreferredAvailabilityZones@ objects that specifies the configuration of a node group in the resharded cluster. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegionalConfiguration' value with any optional fields omitted.
mkRegionalConfiguration
    :: Core.Text -- ^ 'replicationGroupId'
    -> Core.Text -- ^ 'replicationGroupRegion'
    -> RegionalConfiguration
mkRegionalConfiguration replicationGroupId replicationGroupRegion
  = RegionalConfiguration'{replicationGroupId,
                           replicationGroupRegion, reshardingConfiguration = Core.mempty}

-- | The name of the secondary cluster
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcReplicationGroupId :: Lens.Lens' RegionalConfiguration Core.Text
rcReplicationGroupId = Lens.field @"replicationGroupId"
{-# INLINEABLE rcReplicationGroupId #-}
{-# DEPRECATED replicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead"  #-}

-- | The AWS region where the cluster is stored
--
-- /Note:/ Consider using 'replicationGroupRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcReplicationGroupRegion :: Lens.Lens' RegionalConfiguration Core.Text
rcReplicationGroupRegion = Lens.field @"replicationGroupRegion"
{-# INLINEABLE rcReplicationGroupRegion #-}
{-# DEPRECATED replicationGroupRegion "Use generic-lens or generic-optics with 'replicationGroupRegion' instead"  #-}

-- | A list of @PreferredAvailabilityZones@ objects that specifies the configuration of a node group in the resharded cluster. 
--
-- /Note:/ Consider using 'reshardingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcReshardingConfiguration :: Lens.Lens' RegionalConfiguration [Types.ReshardingConfiguration]
rcReshardingConfiguration = Lens.field @"reshardingConfiguration"
{-# INLINEABLE rcReshardingConfiguration #-}
{-# DEPRECATED reshardingConfiguration "Use generic-lens or generic-optics with 'reshardingConfiguration' instead"  #-}

instance Core.ToQuery RegionalConfiguration where
        toQuery RegionalConfiguration{..}
          = Core.toQueryPair "ReplicationGroupId" replicationGroupId Core.<>
              Core.toQueryPair "ReplicationGroupRegion" replicationGroupRegion
              Core.<>
              Core.toQueryPair "ReshardingConfiguration"
                (Core.toQueryList "ReshardingConfiguration"
                   reshardingConfiguration)
