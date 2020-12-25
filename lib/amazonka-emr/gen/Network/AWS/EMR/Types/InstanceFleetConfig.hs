{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceFleetConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceFleetConfig
  ( InstanceFleetConfig (..),

    -- * Smart constructor
    mkInstanceFleetConfig,

    -- * Lenses
    ifcInstanceFleetType,
    ifcInstanceTypeConfigs,
    ifcLaunchSpecifications,
    ifcName,
    ifcTargetOnDemandCapacity,
    ifcTargetSpotCapacity,
  )
where

import qualified Network.AWS.EMR.Types.InstanceFleetProvisioningSpecifications as Types
import qualified Network.AWS.EMR.Types.InstanceFleetType as Types
import qualified Network.AWS.EMR.Types.InstanceTypeConfig as Types
import qualified Network.AWS.EMR.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The configuration that defines an instance fleet.
--
-- /See:/ 'mkInstanceFleetConfig' smart constructor.
data InstanceFleetConfig = InstanceFleetConfig'
  { -- | The node type that the instance fleet hosts. Valid values are MASTER,CORE,and TASK.
    instanceFleetType :: Types.InstanceFleetType,
    -- | The instance type configurations that define the EC2 instances in the instance fleet.
    instanceTypeConfigs :: Core.Maybe [Types.InstanceTypeConfig],
    -- | The launch specification for the instance fleet.
    launchSpecifications :: Core.Maybe Types.InstanceFleetProvisioningSpecifications,
    -- | The friendly name of the instance fleet.
    name :: Core.Maybe Types.Name,
    -- | The target capacity of On-Demand units for the instance fleet, which determines how many On-Demand Instances to provision. When the instance fleet launches, Amazon EMR tries to provision On-Demand Instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When an On-Demand Instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units.
    targetOnDemandCapacity :: Core.Maybe Core.Natural,
    -- | The target capacity of Spot units for the instance fleet, which determines how many Spot Instances to provision. When the instance fleet launches, Amazon EMR tries to provision Spot Instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When a Spot Instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units.
    targetSpotCapacity :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceFleetConfig' value with any optional fields omitted.
mkInstanceFleetConfig ::
  -- | 'instanceFleetType'
  Types.InstanceFleetType ->
  InstanceFleetConfig
mkInstanceFleetConfig instanceFleetType =
  InstanceFleetConfig'
    { instanceFleetType,
      instanceTypeConfigs = Core.Nothing,
      launchSpecifications = Core.Nothing,
      name = Core.Nothing,
      targetOnDemandCapacity = Core.Nothing,
      targetSpotCapacity = Core.Nothing
    }

-- | The node type that the instance fleet hosts. Valid values are MASTER,CORE,and TASK.
--
-- /Note:/ Consider using 'instanceFleetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifcInstanceFleetType :: Lens.Lens' InstanceFleetConfig Types.InstanceFleetType
ifcInstanceFleetType = Lens.field @"instanceFleetType"
{-# DEPRECATED ifcInstanceFleetType "Use generic-lens or generic-optics with 'instanceFleetType' instead." #-}

-- | The instance type configurations that define the EC2 instances in the instance fleet.
--
-- /Note:/ Consider using 'instanceTypeConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifcInstanceTypeConfigs :: Lens.Lens' InstanceFleetConfig (Core.Maybe [Types.InstanceTypeConfig])
ifcInstanceTypeConfigs = Lens.field @"instanceTypeConfigs"
{-# DEPRECATED ifcInstanceTypeConfigs "Use generic-lens or generic-optics with 'instanceTypeConfigs' instead." #-}

-- | The launch specification for the instance fleet.
--
-- /Note:/ Consider using 'launchSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifcLaunchSpecifications :: Lens.Lens' InstanceFleetConfig (Core.Maybe Types.InstanceFleetProvisioningSpecifications)
ifcLaunchSpecifications = Lens.field @"launchSpecifications"
{-# DEPRECATED ifcLaunchSpecifications "Use generic-lens or generic-optics with 'launchSpecifications' instead." #-}

-- | The friendly name of the instance fleet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifcName :: Lens.Lens' InstanceFleetConfig (Core.Maybe Types.Name)
ifcName = Lens.field @"name"
{-# DEPRECATED ifcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The target capacity of On-Demand units for the instance fleet, which determines how many On-Demand Instances to provision. When the instance fleet launches, Amazon EMR tries to provision On-Demand Instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When an On-Demand Instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units.
--
-- /Note:/ Consider using 'targetOnDemandCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifcTargetOnDemandCapacity :: Lens.Lens' InstanceFleetConfig (Core.Maybe Core.Natural)
ifcTargetOnDemandCapacity = Lens.field @"targetOnDemandCapacity"
{-# DEPRECATED ifcTargetOnDemandCapacity "Use generic-lens or generic-optics with 'targetOnDemandCapacity' instead." #-}

-- | The target capacity of Spot units for the instance fleet, which determines how many Spot Instances to provision. When the instance fleet launches, Amazon EMR tries to provision Spot Instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When a Spot Instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units.
--
-- /Note:/ Consider using 'targetSpotCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifcTargetSpotCapacity :: Lens.Lens' InstanceFleetConfig (Core.Maybe Core.Natural)
ifcTargetSpotCapacity = Lens.field @"targetSpotCapacity"
{-# DEPRECATED ifcTargetSpotCapacity "Use generic-lens or generic-optics with 'targetSpotCapacity' instead." #-}

instance Core.FromJSON InstanceFleetConfig where
  toJSON InstanceFleetConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InstanceFleetType" Core..= instanceFleetType),
            ("InstanceTypeConfigs" Core..=) Core.<$> instanceTypeConfigs,
            ("LaunchSpecifications" Core..=) Core.<$> launchSpecifications,
            ("Name" Core..=) Core.<$> name,
            ("TargetOnDemandCapacity" Core..=) Core.<$> targetOnDemandCapacity,
            ("TargetSpotCapacity" Core..=) Core.<$> targetSpotCapacity
          ]
      )
