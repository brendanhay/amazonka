{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceFleetModifyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceFleetModifyConfig
  ( InstanceFleetModifyConfig (..),

    -- * Smart constructor
    mkInstanceFleetModifyConfig,

    -- * Lenses
    ifmcInstanceFleetId,
    ifmcTargetOnDemandCapacity,
    ifmcTargetSpotCapacity,
  )
where

import qualified Network.AWS.EMR.Types.InstanceFleetId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration parameters for an instance fleet modification request.
--
-- /See:/ 'mkInstanceFleetModifyConfig' smart constructor.
data InstanceFleetModifyConfig = InstanceFleetModifyConfig'
  { -- | A unique identifier for the instance fleet.
    instanceFleetId :: Types.InstanceFleetId,
    -- | The target capacity of On-Demand units for the instance fleet. For more information see 'InstanceFleetConfig$TargetOnDemandCapacity' .
    targetOnDemandCapacity :: Core.Maybe Core.Natural,
    -- | The target capacity of Spot units for the instance fleet. For more information, see 'InstanceFleetConfig$TargetSpotCapacity' .
    targetSpotCapacity :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceFleetModifyConfig' value with any optional fields omitted.
mkInstanceFleetModifyConfig ::
  -- | 'instanceFleetId'
  Types.InstanceFleetId ->
  InstanceFleetModifyConfig
mkInstanceFleetModifyConfig instanceFleetId =
  InstanceFleetModifyConfig'
    { instanceFleetId,
      targetOnDemandCapacity = Core.Nothing,
      targetSpotCapacity = Core.Nothing
    }

-- | A unique identifier for the instance fleet.
--
-- /Note:/ Consider using 'instanceFleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifmcInstanceFleetId :: Lens.Lens' InstanceFleetModifyConfig Types.InstanceFleetId
ifmcInstanceFleetId = Lens.field @"instanceFleetId"
{-# DEPRECATED ifmcInstanceFleetId "Use generic-lens or generic-optics with 'instanceFleetId' instead." #-}

-- | The target capacity of On-Demand units for the instance fleet. For more information see 'InstanceFleetConfig$TargetOnDemandCapacity' .
--
-- /Note:/ Consider using 'targetOnDemandCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifmcTargetOnDemandCapacity :: Lens.Lens' InstanceFleetModifyConfig (Core.Maybe Core.Natural)
ifmcTargetOnDemandCapacity = Lens.field @"targetOnDemandCapacity"
{-# DEPRECATED ifmcTargetOnDemandCapacity "Use generic-lens or generic-optics with 'targetOnDemandCapacity' instead." #-}

-- | The target capacity of Spot units for the instance fleet. For more information, see 'InstanceFleetConfig$TargetSpotCapacity' .
--
-- /Note:/ Consider using 'targetSpotCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifmcTargetSpotCapacity :: Lens.Lens' InstanceFleetModifyConfig (Core.Maybe Core.Natural)
ifmcTargetSpotCapacity = Lens.field @"targetSpotCapacity"
{-# DEPRECATED ifmcTargetSpotCapacity "Use generic-lens or generic-optics with 'targetSpotCapacity' instead." #-}

instance Core.FromJSON InstanceFleetModifyConfig where
  toJSON InstanceFleetModifyConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InstanceFleetId" Core..= instanceFleetId),
            ("TargetOnDemandCapacity" Core..=) Core.<$> targetOnDemandCapacity,
            ("TargetSpotCapacity" Core..=) Core.<$> targetSpotCapacity
          ]
      )
