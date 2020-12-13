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
    ifcInstanceTypeConfigs,
    ifcTargetOnDemandCapacity,
    ifcInstanceFleetType,
    ifcName,
    ifcTargetSpotCapacity,
    ifcLaunchSpecifications,
  )
where

import Network.AWS.EMR.Types.InstanceFleetProvisioningSpecifications
import Network.AWS.EMR.Types.InstanceFleetType
import Network.AWS.EMR.Types.InstanceTypeConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The configuration that defines an instance fleet.
--
-- /See:/ 'mkInstanceFleetConfig' smart constructor.
data InstanceFleetConfig = InstanceFleetConfig'
  { -- | The instance type configurations that define the EC2 instances in the instance fleet.
    instanceTypeConfigs :: Lude.Maybe [InstanceTypeConfig],
    -- | The target capacity of On-Demand units for the instance fleet, which determines how many On-Demand Instances to provision. When the instance fleet launches, Amazon EMR tries to provision On-Demand Instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When an On-Demand Instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units.
    targetOnDemandCapacity :: Lude.Maybe Lude.Natural,
    -- | The node type that the instance fleet hosts. Valid values are MASTER,CORE,and TASK.
    instanceFleetType :: InstanceFleetType,
    -- | The friendly name of the instance fleet.
    name :: Lude.Maybe Lude.Text,
    -- | The target capacity of Spot units for the instance fleet, which determines how many Spot Instances to provision. When the instance fleet launches, Amazon EMR tries to provision Spot Instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When a Spot Instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units.
    targetSpotCapacity :: Lude.Maybe Lude.Natural,
    -- | The launch specification for the instance fleet.
    launchSpecifications :: Lude.Maybe InstanceFleetProvisioningSpecifications
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceFleetConfig' with the minimum fields required to make a request.
--
-- * 'instanceTypeConfigs' - The instance type configurations that define the EC2 instances in the instance fleet.
-- * 'targetOnDemandCapacity' - The target capacity of On-Demand units for the instance fleet, which determines how many On-Demand Instances to provision. When the instance fleet launches, Amazon EMR tries to provision On-Demand Instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When an On-Demand Instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units.
-- * 'instanceFleetType' - The node type that the instance fleet hosts. Valid values are MASTER,CORE,and TASK.
-- * 'name' - The friendly name of the instance fleet.
-- * 'targetSpotCapacity' - The target capacity of Spot units for the instance fleet, which determines how many Spot Instances to provision. When the instance fleet launches, Amazon EMR tries to provision Spot Instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When a Spot Instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units.
-- * 'launchSpecifications' - The launch specification for the instance fleet.
mkInstanceFleetConfig ::
  -- | 'instanceFleetType'
  InstanceFleetType ->
  InstanceFleetConfig
mkInstanceFleetConfig pInstanceFleetType_ =
  InstanceFleetConfig'
    { instanceTypeConfigs = Lude.Nothing,
      targetOnDemandCapacity = Lude.Nothing,
      instanceFleetType = pInstanceFleetType_,
      name = Lude.Nothing,
      targetSpotCapacity = Lude.Nothing,
      launchSpecifications = Lude.Nothing
    }

-- | The instance type configurations that define the EC2 instances in the instance fleet.
--
-- /Note:/ Consider using 'instanceTypeConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifcInstanceTypeConfigs :: Lens.Lens' InstanceFleetConfig (Lude.Maybe [InstanceTypeConfig])
ifcInstanceTypeConfigs = Lens.lens (instanceTypeConfigs :: InstanceFleetConfig -> Lude.Maybe [InstanceTypeConfig]) (\s a -> s {instanceTypeConfigs = a} :: InstanceFleetConfig)
{-# DEPRECATED ifcInstanceTypeConfigs "Use generic-lens or generic-optics with 'instanceTypeConfigs' instead." #-}

-- | The target capacity of On-Demand units for the instance fleet, which determines how many On-Demand Instances to provision. When the instance fleet launches, Amazon EMR tries to provision On-Demand Instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When an On-Demand Instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units.
--
-- /Note:/ Consider using 'targetOnDemandCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifcTargetOnDemandCapacity :: Lens.Lens' InstanceFleetConfig (Lude.Maybe Lude.Natural)
ifcTargetOnDemandCapacity = Lens.lens (targetOnDemandCapacity :: InstanceFleetConfig -> Lude.Maybe Lude.Natural) (\s a -> s {targetOnDemandCapacity = a} :: InstanceFleetConfig)
{-# DEPRECATED ifcTargetOnDemandCapacity "Use generic-lens or generic-optics with 'targetOnDemandCapacity' instead." #-}

-- | The node type that the instance fleet hosts. Valid values are MASTER,CORE,and TASK.
--
-- /Note:/ Consider using 'instanceFleetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifcInstanceFleetType :: Lens.Lens' InstanceFleetConfig InstanceFleetType
ifcInstanceFleetType = Lens.lens (instanceFleetType :: InstanceFleetConfig -> InstanceFleetType) (\s a -> s {instanceFleetType = a} :: InstanceFleetConfig)
{-# DEPRECATED ifcInstanceFleetType "Use generic-lens or generic-optics with 'instanceFleetType' instead." #-}

-- | The friendly name of the instance fleet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifcName :: Lens.Lens' InstanceFleetConfig (Lude.Maybe Lude.Text)
ifcName = Lens.lens (name :: InstanceFleetConfig -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: InstanceFleetConfig)
{-# DEPRECATED ifcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The target capacity of Spot units for the instance fleet, which determines how many Spot Instances to provision. When the instance fleet launches, Amazon EMR tries to provision Spot Instances as specified by 'InstanceTypeConfig' . Each instance configuration has a specified @WeightedCapacity@ . When a Spot Instance is provisioned, the @WeightedCapacity@ units count toward the target capacity. Amazon EMR provisions instances until the target capacity is totally fulfilled, even if this results in an overage. For example, if there are 2 units remaining to fulfill capacity, and Amazon EMR can only provision an instance with a @WeightedCapacity@ of 5 units, the instance is provisioned, and the target capacity is exceeded by 3 units.
--
-- /Note:/ Consider using 'targetSpotCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifcTargetSpotCapacity :: Lens.Lens' InstanceFleetConfig (Lude.Maybe Lude.Natural)
ifcTargetSpotCapacity = Lens.lens (targetSpotCapacity :: InstanceFleetConfig -> Lude.Maybe Lude.Natural) (\s a -> s {targetSpotCapacity = a} :: InstanceFleetConfig)
{-# DEPRECATED ifcTargetSpotCapacity "Use generic-lens or generic-optics with 'targetSpotCapacity' instead." #-}

-- | The launch specification for the instance fleet.
--
-- /Note:/ Consider using 'launchSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifcLaunchSpecifications :: Lens.Lens' InstanceFleetConfig (Lude.Maybe InstanceFleetProvisioningSpecifications)
ifcLaunchSpecifications = Lens.lens (launchSpecifications :: InstanceFleetConfig -> Lude.Maybe InstanceFleetProvisioningSpecifications) (\s a -> s {launchSpecifications = a} :: InstanceFleetConfig)
{-# DEPRECATED ifcLaunchSpecifications "Use generic-lens or generic-optics with 'launchSpecifications' instead." #-}

instance Lude.ToJSON InstanceFleetConfig where
  toJSON InstanceFleetConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InstanceTypeConfigs" Lude..=) Lude.<$> instanceTypeConfigs,
            ("TargetOnDemandCapacity" Lude..=) Lude.<$> targetOnDemandCapacity,
            Lude.Just ("InstanceFleetType" Lude..= instanceFleetType),
            ("Name" Lude..=) Lude.<$> name,
            ("TargetSpotCapacity" Lude..=) Lude.<$> targetSpotCapacity,
            ("LaunchSpecifications" Lude..=) Lude.<$> launchSpecifications
          ]
      )
