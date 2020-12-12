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

import Network.AWS.ElastiCache.Types.ReshardingConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of the replication groups
--
-- /See:/ 'mkRegionalConfiguration' smart constructor.
data RegionalConfiguration = RegionalConfiguration'
  { replicationGroupId ::
      Lude.Text,
    replicationGroupRegion :: Lude.Text,
    reshardingConfiguration ::
      [ReshardingConfiguration]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegionalConfiguration' with the minimum fields required to make a request.
--
-- * 'replicationGroupId' - The name of the secondary cluster
-- * 'replicationGroupRegion' - The AWS region where the cluster is stored
-- * 'reshardingConfiguration' - A list of @PreferredAvailabilityZones@ objects that specifies the configuration of a node group in the resharded cluster.
mkRegionalConfiguration ::
  -- | 'replicationGroupId'
  Lude.Text ->
  -- | 'replicationGroupRegion'
  Lude.Text ->
  RegionalConfiguration
mkRegionalConfiguration
  pReplicationGroupId_
  pReplicationGroupRegion_ =
    RegionalConfiguration'
      { replicationGroupId = pReplicationGroupId_,
        replicationGroupRegion = pReplicationGroupRegion_,
        reshardingConfiguration = Lude.mempty
      }

-- | The name of the secondary cluster
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcReplicationGroupId :: Lens.Lens' RegionalConfiguration Lude.Text
rcReplicationGroupId = Lens.lens (replicationGroupId :: RegionalConfiguration -> Lude.Text) (\s a -> s {replicationGroupId = a} :: RegionalConfiguration)
{-# DEPRECATED rcReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | The AWS region where the cluster is stored
--
-- /Note:/ Consider using 'replicationGroupRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcReplicationGroupRegion :: Lens.Lens' RegionalConfiguration Lude.Text
rcReplicationGroupRegion = Lens.lens (replicationGroupRegion :: RegionalConfiguration -> Lude.Text) (\s a -> s {replicationGroupRegion = a} :: RegionalConfiguration)
{-# DEPRECATED rcReplicationGroupRegion "Use generic-lens or generic-optics with 'replicationGroupRegion' instead." #-}

-- | A list of @PreferredAvailabilityZones@ objects that specifies the configuration of a node group in the resharded cluster.
--
-- /Note:/ Consider using 'reshardingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcReshardingConfiguration :: Lens.Lens' RegionalConfiguration [ReshardingConfiguration]
rcReshardingConfiguration = Lens.lens (reshardingConfiguration :: RegionalConfiguration -> [ReshardingConfiguration]) (\s a -> s {reshardingConfiguration = a} :: RegionalConfiguration)
{-# DEPRECATED rcReshardingConfiguration "Use generic-lens or generic-optics with 'reshardingConfiguration' instead." #-}

instance Lude.ToQuery RegionalConfiguration where
  toQuery RegionalConfiguration' {..} =
    Lude.mconcat
      [ "ReplicationGroupId" Lude.=: replicationGroupId,
        "ReplicationGroupRegion" Lude.=: replicationGroupRegion,
        "ReshardingConfiguration"
          Lude.=: Lude.toQueryList "ReshardingConfiguration" reshardingConfiguration
      ]
