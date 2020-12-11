-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.FleetCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.FleetCapacity
  ( FleetCapacity (..),

    -- * Smart constructor
    mkFleetCapacity,

    -- * Lenses
    fcInstanceType,
    fcFleetId,
    fcInstanceCounts,
  )
where

import Network.AWS.GameLift.Types.EC2InstanceCounts
import Network.AWS.GameLift.Types.EC2InstanceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the fleet's capacity. Fleet capacity is measured in EC2 instances. By default, new fleets have a capacity of one instance, but can be updated as needed. The maximum number of instances for a fleet is determined by the fleet's instance type.
--
--
--     * 'CreateFleet'
--
--
--     * 'ListFleets'
--
--
--     * 'DeleteFleet'
--
--
--     * 'DescribeFleetAttributes'
--
--
--     * 'UpdateFleetAttributes'
--
--
--     * 'StartFleetActions' or 'StopFleetActions'
--
--
--
-- /See:/ 'mkFleetCapacity' smart constructor.
data FleetCapacity = FleetCapacity'
  { instanceType ::
      Lude.Maybe EC2InstanceType,
    fleetId :: Lude.Maybe Lude.Text,
    instanceCounts :: Lude.Maybe EC2InstanceCounts
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FleetCapacity' with the minimum fields required to make a request.
--
-- * 'fleetId' - A unique identifier for a fleet.
-- * 'instanceCounts' - Current status of fleet capacity.
-- * 'instanceType' - Name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
mkFleetCapacity ::
  FleetCapacity
mkFleetCapacity =
  FleetCapacity'
    { instanceType = Lude.Nothing,
      fleetId = Lude.Nothing,
      instanceCounts = Lude.Nothing
    }

-- | Name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcInstanceType :: Lens.Lens' FleetCapacity (Lude.Maybe EC2InstanceType)
fcInstanceType = Lens.lens (instanceType :: FleetCapacity -> Lude.Maybe EC2InstanceType) (\s a -> s {instanceType = a} :: FleetCapacity)
{-# DEPRECATED fcInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | A unique identifier for a fleet.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcFleetId :: Lens.Lens' FleetCapacity (Lude.Maybe Lude.Text)
fcFleetId = Lens.lens (fleetId :: FleetCapacity -> Lude.Maybe Lude.Text) (\s a -> s {fleetId = a} :: FleetCapacity)
{-# DEPRECATED fcFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | Current status of fleet capacity.
--
-- /Note:/ Consider using 'instanceCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcInstanceCounts :: Lens.Lens' FleetCapacity (Lude.Maybe EC2InstanceCounts)
fcInstanceCounts = Lens.lens (instanceCounts :: FleetCapacity -> Lude.Maybe EC2InstanceCounts) (\s a -> s {instanceCounts = a} :: FleetCapacity)
{-# DEPRECATED fcInstanceCounts "Use generic-lens or generic-optics with 'instanceCounts' instead." #-}

instance Lude.FromJSON FleetCapacity where
  parseJSON =
    Lude.withObject
      "FleetCapacity"
      ( \x ->
          FleetCapacity'
            Lude.<$> (x Lude..:? "InstanceType")
            Lude.<*> (x Lude..:? "FleetId")
            Lude.<*> (x Lude..:? "InstanceCounts")
      )
