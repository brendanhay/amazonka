{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceFleet
  ( InstanceFleet (..),

    -- * Smart constructor
    mkInstanceFleet,

    -- * Lenses
    ifProvisionedSpotCapacity,
    ifStatus,
    ifTargetOnDemandCapacity,
    ifInstanceFleetType,
    ifInstanceTypeSpecifications,
    ifName,
    ifProvisionedOnDemandCapacity,
    ifTargetSpotCapacity,
    ifId,
    ifLaunchSpecifications,
  )
where

import Network.AWS.EMR.Types.InstanceFleetProvisioningSpecifications
import Network.AWS.EMR.Types.InstanceFleetStatus
import Network.AWS.EMR.Types.InstanceFleetType
import Network.AWS.EMR.Types.InstanceTypeSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an instance fleet, which is a group of EC2 instances that host a particular node type (master, core, or task) in an Amazon EMR cluster. Instance fleets can consist of a mix of instance types and On-Demand and Spot Instances, which are provisioned to meet a defined target capacity.
--
-- /See:/ 'mkInstanceFleet' smart constructor.
data InstanceFleet = InstanceFleet'
  { -- | The number of Spot units that have been provisioned for this instance fleet to fulfill @TargetSpotCapacity@ . This provisioned capacity might be less than or greater than @TargetSpotCapacity@ .
    provisionedSpotCapacity :: Lude.Maybe Lude.Natural,
    -- | The current status of the instance fleet.
    status :: Lude.Maybe InstanceFleetStatus,
    -- | The target capacity of On-Demand units for the instance fleet, which determines how many On-Demand Instances to provision. When the instance fleet launches, Amazon EMR tries to provision On-Demand Instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When an On-Demand Instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units. You can use 'InstanceFleet$ProvisionedOnDemandCapacity' to determine the Spot capacity units that have been provisioned for the instance fleet.
    targetOnDemandCapacity :: Lude.Maybe Lude.Natural,
    -- | The node type that the instance fleet hosts. Valid values are MASTER, CORE, or TASK.
    instanceFleetType :: Lude.Maybe InstanceFleetType,
    -- | The specification for the instance types that comprise an instance fleet. Up to five unique instance specifications may be defined for each instance fleet.
    instanceTypeSpecifications :: Lude.Maybe [InstanceTypeSpecification],
    -- | A friendly name for the instance fleet.
    name :: Lude.Maybe Lude.Text,
    -- | The number of On-Demand units that have been provisioned for the instance fleet to fulfill @TargetOnDemandCapacity@ . This provisioned capacity might be less than or greater than @TargetOnDemandCapacity@ .
    provisionedOnDemandCapacity :: Lude.Maybe Lude.Natural,
    -- | The target capacity of Spot units for the instance fleet, which determines how many Spot instances to provision. When the instance fleet launches, Amazon EMR tries to provision Spot instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When a Spot instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units. You can use 'InstanceFleet$ProvisionedSpotCapacity' to determine the Spot capacity units that have been provisioned for the instance fleet.
    targetSpotCapacity :: Lude.Maybe Lude.Natural,
    -- | The unique identifier of the instance fleet.
    id :: Lude.Maybe Lude.Text,
    -- | Describes the launch specification for an instance fleet.
    launchSpecifications :: Lude.Maybe InstanceFleetProvisioningSpecifications
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceFleet' with the minimum fields required to make a request.
--
-- * 'provisionedSpotCapacity' - The number of Spot units that have been provisioned for this instance fleet to fulfill @TargetSpotCapacity@ . This provisioned capacity might be less than or greater than @TargetSpotCapacity@ .
-- * 'status' - The current status of the instance fleet.
-- * 'targetOnDemandCapacity' - The target capacity of On-Demand units for the instance fleet, which determines how many On-Demand Instances to provision. When the instance fleet launches, Amazon EMR tries to provision On-Demand Instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When an On-Demand Instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units. You can use 'InstanceFleet$ProvisionedOnDemandCapacity' to determine the Spot capacity units that have been provisioned for the instance fleet.
-- * 'instanceFleetType' - The node type that the instance fleet hosts. Valid values are MASTER, CORE, or TASK.
-- * 'instanceTypeSpecifications' - The specification for the instance types that comprise an instance fleet. Up to five unique instance specifications may be defined for each instance fleet.
-- * 'name' - A friendly name for the instance fleet.
-- * 'provisionedOnDemandCapacity' - The number of On-Demand units that have been provisioned for the instance fleet to fulfill @TargetOnDemandCapacity@ . This provisioned capacity might be less than or greater than @TargetOnDemandCapacity@ .
-- * 'targetSpotCapacity' - The target capacity of Spot units for the instance fleet, which determines how many Spot instances to provision. When the instance fleet launches, Amazon EMR tries to provision Spot instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When a Spot instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units. You can use 'InstanceFleet$ProvisionedSpotCapacity' to determine the Spot capacity units that have been provisioned for the instance fleet.
-- * 'id' - The unique identifier of the instance fleet.
-- * 'launchSpecifications' - Describes the launch specification for an instance fleet.
mkInstanceFleet ::
  InstanceFleet
mkInstanceFleet =
  InstanceFleet'
    { provisionedSpotCapacity = Lude.Nothing,
      status = Lude.Nothing,
      targetOnDemandCapacity = Lude.Nothing,
      instanceFleetType = Lude.Nothing,
      instanceTypeSpecifications = Lude.Nothing,
      name = Lude.Nothing,
      provisionedOnDemandCapacity = Lude.Nothing,
      targetSpotCapacity = Lude.Nothing,
      id = Lude.Nothing,
      launchSpecifications = Lude.Nothing
    }

-- | The number of Spot units that have been provisioned for this instance fleet to fulfill @TargetSpotCapacity@ . This provisioned capacity might be less than or greater than @TargetSpotCapacity@ .
--
-- /Note:/ Consider using 'provisionedSpotCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifProvisionedSpotCapacity :: Lens.Lens' InstanceFleet (Lude.Maybe Lude.Natural)
ifProvisionedSpotCapacity = Lens.lens (provisionedSpotCapacity :: InstanceFleet -> Lude.Maybe Lude.Natural) (\s a -> s {provisionedSpotCapacity = a} :: InstanceFleet)
{-# DEPRECATED ifProvisionedSpotCapacity "Use generic-lens or generic-optics with 'provisionedSpotCapacity' instead." #-}

-- | The current status of the instance fleet.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifStatus :: Lens.Lens' InstanceFleet (Lude.Maybe InstanceFleetStatus)
ifStatus = Lens.lens (status :: InstanceFleet -> Lude.Maybe InstanceFleetStatus) (\s a -> s {status = a} :: InstanceFleet)
{-# DEPRECATED ifStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The target capacity of On-Demand units for the instance fleet, which determines how many On-Demand Instances to provision. When the instance fleet launches, Amazon EMR tries to provision On-Demand Instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When an On-Demand Instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units. You can use 'InstanceFleet$ProvisionedOnDemandCapacity' to determine the Spot capacity units that have been provisioned for the instance fleet.
--
-- /Note:/ Consider using 'targetOnDemandCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifTargetOnDemandCapacity :: Lens.Lens' InstanceFleet (Lude.Maybe Lude.Natural)
ifTargetOnDemandCapacity = Lens.lens (targetOnDemandCapacity :: InstanceFleet -> Lude.Maybe Lude.Natural) (\s a -> s {targetOnDemandCapacity = a} :: InstanceFleet)
{-# DEPRECATED ifTargetOnDemandCapacity "Use generic-lens or generic-optics with 'targetOnDemandCapacity' instead." #-}

-- | The node type that the instance fleet hosts. Valid values are MASTER, CORE, or TASK.
--
-- /Note:/ Consider using 'instanceFleetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifInstanceFleetType :: Lens.Lens' InstanceFleet (Lude.Maybe InstanceFleetType)
ifInstanceFleetType = Lens.lens (instanceFleetType :: InstanceFleet -> Lude.Maybe InstanceFleetType) (\s a -> s {instanceFleetType = a} :: InstanceFleet)
{-# DEPRECATED ifInstanceFleetType "Use generic-lens or generic-optics with 'instanceFleetType' instead." #-}

-- | The specification for the instance types that comprise an instance fleet. Up to five unique instance specifications may be defined for each instance fleet.
--
-- /Note:/ Consider using 'instanceTypeSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifInstanceTypeSpecifications :: Lens.Lens' InstanceFleet (Lude.Maybe [InstanceTypeSpecification])
ifInstanceTypeSpecifications = Lens.lens (instanceTypeSpecifications :: InstanceFleet -> Lude.Maybe [InstanceTypeSpecification]) (\s a -> s {instanceTypeSpecifications = a} :: InstanceFleet)
{-# DEPRECATED ifInstanceTypeSpecifications "Use generic-lens or generic-optics with 'instanceTypeSpecifications' instead." #-}

-- | A friendly name for the instance fleet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifName :: Lens.Lens' InstanceFleet (Lude.Maybe Lude.Text)
ifName = Lens.lens (name :: InstanceFleet -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: InstanceFleet)
{-# DEPRECATED ifName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The number of On-Demand units that have been provisioned for the instance fleet to fulfill @TargetOnDemandCapacity@ . This provisioned capacity might be less than or greater than @TargetOnDemandCapacity@ .
--
-- /Note:/ Consider using 'provisionedOnDemandCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifProvisionedOnDemandCapacity :: Lens.Lens' InstanceFleet (Lude.Maybe Lude.Natural)
ifProvisionedOnDemandCapacity = Lens.lens (provisionedOnDemandCapacity :: InstanceFleet -> Lude.Maybe Lude.Natural) (\s a -> s {provisionedOnDemandCapacity = a} :: InstanceFleet)
{-# DEPRECATED ifProvisionedOnDemandCapacity "Use generic-lens or generic-optics with 'provisionedOnDemandCapacity' instead." #-}

-- | The target capacity of Spot units for the instance fleet, which determines how many Spot instances to provision. When the instance fleet launches, Amazon EMR tries to provision Spot instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When a Spot instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units. You can use 'InstanceFleet$ProvisionedSpotCapacity' to determine the Spot capacity units that have been provisioned for the instance fleet.
--
-- /Note:/ Consider using 'targetSpotCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifTargetSpotCapacity :: Lens.Lens' InstanceFleet (Lude.Maybe Lude.Natural)
ifTargetSpotCapacity = Lens.lens (targetSpotCapacity :: InstanceFleet -> Lude.Maybe Lude.Natural) (\s a -> s {targetSpotCapacity = a} :: InstanceFleet)
{-# DEPRECATED ifTargetSpotCapacity "Use generic-lens or generic-optics with 'targetSpotCapacity' instead." #-}

-- | The unique identifier of the instance fleet.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifId :: Lens.Lens' InstanceFleet (Lude.Maybe Lude.Text)
ifId = Lens.lens (id :: InstanceFleet -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: InstanceFleet)
{-# DEPRECATED ifId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Describes the launch specification for an instance fleet.
--
-- /Note:/ Consider using 'launchSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifLaunchSpecifications :: Lens.Lens' InstanceFleet (Lude.Maybe InstanceFleetProvisioningSpecifications)
ifLaunchSpecifications = Lens.lens (launchSpecifications :: InstanceFleet -> Lude.Maybe InstanceFleetProvisioningSpecifications) (\s a -> s {launchSpecifications = a} :: InstanceFleet)
{-# DEPRECATED ifLaunchSpecifications "Use generic-lens or generic-optics with 'launchSpecifications' instead." #-}

instance Lude.FromJSON InstanceFleet where
  parseJSON =
    Lude.withObject
      "InstanceFleet"
      ( \x ->
          InstanceFleet'
            Lude.<$> (x Lude..:? "ProvisionedSpotCapacity")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "TargetOnDemandCapacity")
            Lude.<*> (x Lude..:? "InstanceFleetType")
            Lude.<*> (x Lude..:? "InstanceTypeSpecifications" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "ProvisionedOnDemandCapacity")
            Lude.<*> (x Lude..:? "TargetSpotCapacity")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "LaunchSpecifications")
      )
