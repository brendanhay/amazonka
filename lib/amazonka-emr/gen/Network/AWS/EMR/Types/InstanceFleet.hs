{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.InstanceFleet
  ( InstanceFleet (..)
  -- * Smart constructor
  , mkInstanceFleet
  -- * Lenses
  , ifId
  , ifInstanceFleetType
  , ifInstanceTypeSpecifications
  , ifLaunchSpecifications
  , ifName
  , ifProvisionedOnDemandCapacity
  , ifProvisionedSpotCapacity
  , ifStatus
  , ifTargetOnDemandCapacity
  , ifTargetSpotCapacity
  ) where

import qualified Network.AWS.EMR.Types.Id as Types
import qualified Network.AWS.EMR.Types.InstanceFleetProvisioningSpecifications as Types
import qualified Network.AWS.EMR.Types.InstanceFleetStatus as Types
import qualified Network.AWS.EMR.Types.InstanceFleetType as Types
import qualified Network.AWS.EMR.Types.InstanceTypeSpecification as Types
import qualified Network.AWS.EMR.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an instance fleet, which is a group of EC2 instances that host a particular node type (master, core, or task) in an Amazon EMR cluster. Instance fleets can consist of a mix of instance types and On-Demand and Spot Instances, which are provisioned to meet a defined target capacity. 
--
-- /See:/ 'mkInstanceFleet' smart constructor.
data InstanceFleet = InstanceFleet'
  { id :: Core.Maybe Types.Id
    -- ^ The unique identifier of the instance fleet.
  , instanceFleetType :: Core.Maybe Types.InstanceFleetType
    -- ^ The node type that the instance fleet hosts. Valid values are MASTER, CORE, or TASK. 
  , instanceTypeSpecifications :: Core.Maybe [Types.InstanceTypeSpecification]
    -- ^ The specification for the instance types that comprise an instance fleet. Up to five unique instance specifications may be defined for each instance fleet. 
  , launchSpecifications :: Core.Maybe Types.InstanceFleetProvisioningSpecifications
    -- ^ Describes the launch specification for an instance fleet. 
  , name :: Core.Maybe Types.Name
    -- ^ A friendly name for the instance fleet.
  , provisionedOnDemandCapacity :: Core.Maybe Core.Natural
    -- ^ The number of On-Demand units that have been provisioned for the instance fleet to fulfill @TargetOnDemandCapacity@ . This provisioned capacity might be less than or greater than @TargetOnDemandCapacity@ .
  , provisionedSpotCapacity :: Core.Maybe Core.Natural
    -- ^ The number of Spot units that have been provisioned for this instance fleet to fulfill @TargetSpotCapacity@ . This provisioned capacity might be less than or greater than @TargetSpotCapacity@ .
  , status :: Core.Maybe Types.InstanceFleetStatus
    -- ^ The current status of the instance fleet. 
  , targetOnDemandCapacity :: Core.Maybe Core.Natural
    -- ^ The target capacity of On-Demand units for the instance fleet, which determines how many On-Demand Instances to provision. When the instance fleet launches, Amazon EMR tries to provision On-Demand Instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When an On-Demand Instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units. You can use 'InstanceFleet$ProvisionedOnDemandCapacity' to determine the Spot capacity units that have been provisioned for the instance fleet.
  , targetSpotCapacity :: Core.Maybe Core.Natural
    -- ^ The target capacity of Spot units for the instance fleet, which determines how many Spot instances to provision. When the instance fleet launches, Amazon EMR tries to provision Spot instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When a Spot instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units. You can use 'InstanceFleet$ProvisionedSpotCapacity' to determine the Spot capacity units that have been provisioned for the instance fleet.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'InstanceFleet' value with any optional fields omitted.
mkInstanceFleet
    :: InstanceFleet
mkInstanceFleet
  = InstanceFleet'{id = Core.Nothing,
                   instanceFleetType = Core.Nothing,
                   instanceTypeSpecifications = Core.Nothing,
                   launchSpecifications = Core.Nothing, name = Core.Nothing,
                   provisionedOnDemandCapacity = Core.Nothing,
                   provisionedSpotCapacity = Core.Nothing, status = Core.Nothing,
                   targetOnDemandCapacity = Core.Nothing,
                   targetSpotCapacity = Core.Nothing}

-- | The unique identifier of the instance fleet.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifId :: Lens.Lens' InstanceFleet (Core.Maybe Types.Id)
ifId = Lens.field @"id"
{-# INLINEABLE ifId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The node type that the instance fleet hosts. Valid values are MASTER, CORE, or TASK. 
--
-- /Note:/ Consider using 'instanceFleetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifInstanceFleetType :: Lens.Lens' InstanceFleet (Core.Maybe Types.InstanceFleetType)
ifInstanceFleetType = Lens.field @"instanceFleetType"
{-# INLINEABLE ifInstanceFleetType #-}
{-# DEPRECATED instanceFleetType "Use generic-lens or generic-optics with 'instanceFleetType' instead"  #-}

-- | The specification for the instance types that comprise an instance fleet. Up to five unique instance specifications may be defined for each instance fleet. 
--
-- /Note:/ Consider using 'instanceTypeSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifInstanceTypeSpecifications :: Lens.Lens' InstanceFleet (Core.Maybe [Types.InstanceTypeSpecification])
ifInstanceTypeSpecifications = Lens.field @"instanceTypeSpecifications"
{-# INLINEABLE ifInstanceTypeSpecifications #-}
{-# DEPRECATED instanceTypeSpecifications "Use generic-lens or generic-optics with 'instanceTypeSpecifications' instead"  #-}

-- | Describes the launch specification for an instance fleet. 
--
-- /Note:/ Consider using 'launchSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifLaunchSpecifications :: Lens.Lens' InstanceFleet (Core.Maybe Types.InstanceFleetProvisioningSpecifications)
ifLaunchSpecifications = Lens.field @"launchSpecifications"
{-# INLINEABLE ifLaunchSpecifications #-}
{-# DEPRECATED launchSpecifications "Use generic-lens or generic-optics with 'launchSpecifications' instead"  #-}

-- | A friendly name for the instance fleet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifName :: Lens.Lens' InstanceFleet (Core.Maybe Types.Name)
ifName = Lens.field @"name"
{-# INLINEABLE ifName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The number of On-Demand units that have been provisioned for the instance fleet to fulfill @TargetOnDemandCapacity@ . This provisioned capacity might be less than or greater than @TargetOnDemandCapacity@ .
--
-- /Note:/ Consider using 'provisionedOnDemandCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifProvisionedOnDemandCapacity :: Lens.Lens' InstanceFleet (Core.Maybe Core.Natural)
ifProvisionedOnDemandCapacity = Lens.field @"provisionedOnDemandCapacity"
{-# INLINEABLE ifProvisionedOnDemandCapacity #-}
{-# DEPRECATED provisionedOnDemandCapacity "Use generic-lens or generic-optics with 'provisionedOnDemandCapacity' instead"  #-}

-- | The number of Spot units that have been provisioned for this instance fleet to fulfill @TargetSpotCapacity@ . This provisioned capacity might be less than or greater than @TargetSpotCapacity@ .
--
-- /Note:/ Consider using 'provisionedSpotCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifProvisionedSpotCapacity :: Lens.Lens' InstanceFleet (Core.Maybe Core.Natural)
ifProvisionedSpotCapacity = Lens.field @"provisionedSpotCapacity"
{-# INLINEABLE ifProvisionedSpotCapacity #-}
{-# DEPRECATED provisionedSpotCapacity "Use generic-lens or generic-optics with 'provisionedSpotCapacity' instead"  #-}

-- | The current status of the instance fleet. 
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifStatus :: Lens.Lens' InstanceFleet (Core.Maybe Types.InstanceFleetStatus)
ifStatus = Lens.field @"status"
{-# INLINEABLE ifStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The target capacity of On-Demand units for the instance fleet, which determines how many On-Demand Instances to provision. When the instance fleet launches, Amazon EMR tries to provision On-Demand Instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When an On-Demand Instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units. You can use 'InstanceFleet$ProvisionedOnDemandCapacity' to determine the Spot capacity units that have been provisioned for the instance fleet.
--
-- /Note:/ Consider using 'targetOnDemandCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifTargetOnDemandCapacity :: Lens.Lens' InstanceFleet (Core.Maybe Core.Natural)
ifTargetOnDemandCapacity = Lens.field @"targetOnDemandCapacity"
{-# INLINEABLE ifTargetOnDemandCapacity #-}
{-# DEPRECATED targetOnDemandCapacity "Use generic-lens or generic-optics with 'targetOnDemandCapacity' instead"  #-}

-- | The target capacity of Spot units for the instance fleet, which determines how many Spot instances to provision. When the instance fleet launches, Amazon EMR tries to provision Spot instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When a Spot instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units. You can use 'InstanceFleet$ProvisionedSpotCapacity' to determine the Spot capacity units that have been provisioned for the instance fleet.
--
-- /Note:/ Consider using 'targetSpotCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifTargetSpotCapacity :: Lens.Lens' InstanceFleet (Core.Maybe Core.Natural)
ifTargetSpotCapacity = Lens.field @"targetSpotCapacity"
{-# INLINEABLE ifTargetSpotCapacity #-}
{-# DEPRECATED targetSpotCapacity "Use generic-lens or generic-optics with 'targetSpotCapacity' instead"  #-}

instance Core.FromJSON InstanceFleet where
        parseJSON
          = Core.withObject "InstanceFleet" Core.$
              \ x ->
                InstanceFleet' Core.<$>
                  (x Core..:? "Id") Core.<*> x Core..:? "InstanceFleetType" Core.<*>
                    x Core..:? "InstanceTypeSpecifications"
                    Core.<*> x Core..:? "LaunchSpecifications"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "ProvisionedOnDemandCapacity"
                    Core.<*> x Core..:? "ProvisionedSpotCapacity"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "TargetOnDemandCapacity"
                    Core.<*> x Core..:? "TargetSpotCapacity"
