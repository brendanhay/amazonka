{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.RegionalConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.RegionalConfiguration
  ( RegionalConfiguration (..),

    -- * Smart constructor
    mkRegionalConfiguration,

    -- * Lenses
    rcReplicationGroupId,
    rcReplicationGroupRegion,
    rcReshardingConfiguration,
  )
where

import qualified Network.AWS.ElastiCache.Types.ReshardingConfiguration as Types
import qualified Network.AWS.ElastiCache.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of the replication groups
--
-- /See:/ 'mkRegionalConfiguration' smart constructor.
data RegionalConfiguration = RegionalConfiguration'
  { -- | The name of the secondary cluster
    replicationGroupId :: Types.String,
    -- | The AWS region where the cluster is stored
    replicationGroupRegion :: Types.String,
    -- | A list of @PreferredAvailabilityZones@ objects that specifies the configuration of a node group in the resharded cluster.
    reshardingConfiguration :: [Types.ReshardingConfiguration]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegionalConfiguration' value with any optional fields omitted.
mkRegionalConfiguration ::
  -- | 'replicationGroupId'
  Types.String ->
  -- | 'replicationGroupRegion'
  Types.String ->
  RegionalConfiguration
mkRegionalConfiguration replicationGroupId replicationGroupRegion =
  RegionalConfiguration'
    { replicationGroupId,
      replicationGroupRegion,
      reshardingConfiguration = Core.mempty
    }

-- | The name of the secondary cluster
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcReplicationGroupId :: Lens.Lens' RegionalConfiguration Types.String
rcReplicationGroupId = Lens.field @"replicationGroupId"
{-# DEPRECATED rcReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | The AWS region where the cluster is stored
--
-- /Note:/ Consider using 'replicationGroupRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcReplicationGroupRegion :: Lens.Lens' RegionalConfiguration Types.String
rcReplicationGroupRegion = Lens.field @"replicationGroupRegion"
{-# DEPRECATED rcReplicationGroupRegion "Use generic-lens or generic-optics with 'replicationGroupRegion' instead." #-}

-- | A list of @PreferredAvailabilityZones@ objects that specifies the configuration of a node group in the resharded cluster.
--
-- /Note:/ Consider using 'reshardingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcReshardingConfiguration :: Lens.Lens' RegionalConfiguration [Types.ReshardingConfiguration]
rcReshardingConfiguration = Lens.field @"reshardingConfiguration"
{-# DEPRECATED rcReshardingConfiguration "Use generic-lens or generic-optics with 'reshardingConfiguration' instead." #-}
