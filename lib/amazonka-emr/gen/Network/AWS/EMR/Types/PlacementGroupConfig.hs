{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.PlacementGroupConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.PlacementGroupConfig
  ( PlacementGroupConfig (..)
  -- * Smart constructor
  , mkPlacementGroupConfig
  -- * Lenses
  , pgcInstanceRole
  , pgcPlacementStrategy
  ) where

import qualified Network.AWS.EMR.Types.InstanceRoleType as Types
import qualified Network.AWS.EMR.Types.PlacementGroupStrategy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Placement group configuration for an Amazon EMR cluster. The configuration specifies the placement strategy that can be applied to instance roles during cluster creation.
--
-- To use this configuration, consider attaching managed policy AmazonElasticMapReducePlacementGroupPolicy to the EMR role.
--
-- /See:/ 'mkPlacementGroupConfig' smart constructor.
data PlacementGroupConfig = PlacementGroupConfig'
  { instanceRole :: Types.InstanceRoleType
    -- ^ Role of the instance in the cluster.
--
-- Starting with Amazon EMR version 5.23.0, the only supported instance role is @MASTER@ .
  , placementStrategy :: Core.Maybe Types.PlacementGroupStrategy
    -- ^ EC2 Placement Group strategy associated with instance role.
--
-- Starting with Amazon EMR version 5.23.0, the only supported placement strategy is @SPREAD@ for the @MASTER@ instance role.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PlacementGroupConfig' value with any optional fields omitted.
mkPlacementGroupConfig
    :: Types.InstanceRoleType -- ^ 'instanceRole'
    -> PlacementGroupConfig
mkPlacementGroupConfig instanceRole
  = PlacementGroupConfig'{instanceRole,
                          placementStrategy = Core.Nothing}

-- | Role of the instance in the cluster.
--
-- Starting with Amazon EMR version 5.23.0, the only supported instance role is @MASTER@ .
--
-- /Note:/ Consider using 'instanceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgcInstanceRole :: Lens.Lens' PlacementGroupConfig Types.InstanceRoleType
pgcInstanceRole = Lens.field @"instanceRole"
{-# INLINEABLE pgcInstanceRole #-}
{-# DEPRECATED instanceRole "Use generic-lens or generic-optics with 'instanceRole' instead"  #-}

-- | EC2 Placement Group strategy associated with instance role.
--
-- Starting with Amazon EMR version 5.23.0, the only supported placement strategy is @SPREAD@ for the @MASTER@ instance role.
--
-- /Note:/ Consider using 'placementStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgcPlacementStrategy :: Lens.Lens' PlacementGroupConfig (Core.Maybe Types.PlacementGroupStrategy)
pgcPlacementStrategy = Lens.field @"placementStrategy"
{-# INLINEABLE pgcPlacementStrategy #-}
{-# DEPRECATED placementStrategy "Use generic-lens or generic-optics with 'placementStrategy' instead"  #-}

instance Core.FromJSON PlacementGroupConfig where
        toJSON PlacementGroupConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InstanceRole" Core..= instanceRole),
                  ("PlacementStrategy" Core..=) Core.<$> placementStrategy])

instance Core.FromJSON PlacementGroupConfig where
        parseJSON
          = Core.withObject "PlacementGroupConfig" Core.$
              \ x ->
                PlacementGroupConfig' Core.<$>
                  (x Core..: "InstanceRole") Core.<*> x Core..:? "PlacementStrategy"
