{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.PlacementGroupConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.PlacementGroupConfig
  ( PlacementGroupConfig (..),

    -- * Smart constructor
    mkPlacementGroupConfig,

    -- * Lenses
    pgcPlacementStrategy,
    pgcInstanceRole,
  )
where

import Network.AWS.EMR.Types.InstanceRoleType
import Network.AWS.EMR.Types.PlacementGroupStrategy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Placement group configuration for an Amazon EMR cluster. The configuration specifies the placement strategy that can be applied to instance roles during cluster creation.
--
-- To use this configuration, consider attaching managed policy AmazonElasticMapReducePlacementGroupPolicy to the EMR role.
--
-- /See:/ 'mkPlacementGroupConfig' smart constructor.
data PlacementGroupConfig = PlacementGroupConfig'
  { placementStrategy ::
      Lude.Maybe PlacementGroupStrategy,
    instanceRole :: InstanceRoleType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PlacementGroupConfig' with the minimum fields required to make a request.
--
-- * 'instanceRole' - Role of the instance in the cluster.
--
-- Starting with Amazon EMR version 5.23.0, the only supported instance role is @MASTER@ .
-- * 'placementStrategy' - EC2 Placement Group strategy associated with instance role.
--
-- Starting with Amazon EMR version 5.23.0, the only supported placement strategy is @SPREAD@ for the @MASTER@ instance role.
mkPlacementGroupConfig ::
  -- | 'instanceRole'
  InstanceRoleType ->
  PlacementGroupConfig
mkPlacementGroupConfig pInstanceRole_ =
  PlacementGroupConfig'
    { placementStrategy = Lude.Nothing,
      instanceRole = pInstanceRole_
    }

-- | EC2 Placement Group strategy associated with instance role.
--
-- Starting with Amazon EMR version 5.23.0, the only supported placement strategy is @SPREAD@ for the @MASTER@ instance role.
--
-- /Note:/ Consider using 'placementStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgcPlacementStrategy :: Lens.Lens' PlacementGroupConfig (Lude.Maybe PlacementGroupStrategy)
pgcPlacementStrategy = Lens.lens (placementStrategy :: PlacementGroupConfig -> Lude.Maybe PlacementGroupStrategy) (\s a -> s {placementStrategy = a} :: PlacementGroupConfig)
{-# DEPRECATED pgcPlacementStrategy "Use generic-lens or generic-optics with 'placementStrategy' instead." #-}

-- | Role of the instance in the cluster.
--
-- Starting with Amazon EMR version 5.23.0, the only supported instance role is @MASTER@ .
--
-- /Note:/ Consider using 'instanceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgcInstanceRole :: Lens.Lens' PlacementGroupConfig InstanceRoleType
pgcInstanceRole = Lens.lens (instanceRole :: PlacementGroupConfig -> InstanceRoleType) (\s a -> s {instanceRole = a} :: PlacementGroupConfig)
{-# DEPRECATED pgcInstanceRole "Use generic-lens or generic-optics with 'instanceRole' instead." #-}

instance Lude.FromJSON PlacementGroupConfig where
  parseJSON =
    Lude.withObject
      "PlacementGroupConfig"
      ( \x ->
          PlacementGroupConfig'
            Lude.<$> (x Lude..:? "PlacementStrategy")
            Lude.<*> (x Lude..: "InstanceRole")
      )

instance Lude.ToJSON PlacementGroupConfig where
  toJSON PlacementGroupConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PlacementStrategy" Lude..=) Lude.<$> placementStrategy,
            Lude.Just ("InstanceRole" Lude..= instanceRole)
          ]
      )
