-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ReshardingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ReshardingConfiguration
  ( ReshardingConfiguration (..),

    -- * Smart constructor
    mkReshardingConfiguration,

    -- * Lenses
    rcPreferredAvailabilityZones,
    rcNodeGroupId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of @PreferredAvailabilityZones@ objects that specifies the configuration of a node group in the resharded cluster.
--
-- /See:/ 'mkReshardingConfiguration' smart constructor.
data ReshardingConfiguration = ReshardingConfiguration'
  { preferredAvailabilityZones ::
      Lude.Maybe [Lude.Text],
    nodeGroupId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReshardingConfiguration' with the minimum fields required to make a request.
--
-- * 'nodeGroupId' - Either the ElastiCache for Redis supplied 4-digit id or a user supplied id for the node group these configuration values apply to.
-- * 'preferredAvailabilityZones' - A list of preferred availability zones for the nodes in this cluster.
mkReshardingConfiguration ::
  ReshardingConfiguration
mkReshardingConfiguration =
  ReshardingConfiguration'
    { preferredAvailabilityZones =
        Lude.Nothing,
      nodeGroupId = Lude.Nothing
    }

-- | A list of preferred availability zones for the nodes in this cluster.
--
-- /Note:/ Consider using 'preferredAvailabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcPreferredAvailabilityZones :: Lens.Lens' ReshardingConfiguration (Lude.Maybe [Lude.Text])
rcPreferredAvailabilityZones = Lens.lens (preferredAvailabilityZones :: ReshardingConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {preferredAvailabilityZones = a} :: ReshardingConfiguration)
{-# DEPRECATED rcPreferredAvailabilityZones "Use generic-lens or generic-optics with 'preferredAvailabilityZones' instead." #-}

-- | Either the ElastiCache for Redis supplied 4-digit id or a user supplied id for the node group these configuration values apply to.
--
-- /Note:/ Consider using 'nodeGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcNodeGroupId :: Lens.Lens' ReshardingConfiguration (Lude.Maybe Lude.Text)
rcNodeGroupId = Lens.lens (nodeGroupId :: ReshardingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {nodeGroupId = a} :: ReshardingConfiguration)
{-# DEPRECATED rcNodeGroupId "Use generic-lens or generic-optics with 'nodeGroupId' instead." #-}

instance Lude.ToQuery ReshardingConfiguration where
  toQuery ReshardingConfiguration' {..} =
    Lude.mconcat
      [ "PreferredAvailabilityZones"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "AvailabilityZone"
                Lude.<$> preferredAvailabilityZones
            ),
        "NodeGroupId" Lude.=: nodeGroupId
      ]
