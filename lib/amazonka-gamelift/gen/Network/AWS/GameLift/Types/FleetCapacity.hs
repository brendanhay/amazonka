{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.FleetCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.FleetCapacity
  ( FleetCapacity (..)
  -- * Smart constructor
  , mkFleetCapacity
  -- * Lenses
  , fcFleetId
  , fcInstanceCounts
  , fcInstanceType
  ) where

import qualified Network.AWS.GameLift.Types.EC2InstanceCounts as Types
import qualified Network.AWS.GameLift.Types.EC2InstanceType as Types
import qualified Network.AWS.GameLift.Types.FleetId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

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
  { fleetId :: Core.Maybe Types.FleetId
    -- ^ A unique identifier for a fleet.
  , instanceCounts :: Core.Maybe Types.EC2InstanceCounts
    -- ^ Current status of fleet capacity.
  , instanceType :: Core.Maybe Types.EC2InstanceType
    -- ^ Name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FleetCapacity' value with any optional fields omitted.
mkFleetCapacity
    :: FleetCapacity
mkFleetCapacity
  = FleetCapacity'{fleetId = Core.Nothing,
                   instanceCounts = Core.Nothing, instanceType = Core.Nothing}

-- | A unique identifier for a fleet.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcFleetId :: Lens.Lens' FleetCapacity (Core.Maybe Types.FleetId)
fcFleetId = Lens.field @"fleetId"
{-# INLINEABLE fcFleetId #-}
{-# DEPRECATED fleetId "Use generic-lens or generic-optics with 'fleetId' instead"  #-}

-- | Current status of fleet capacity.
--
-- /Note:/ Consider using 'instanceCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcInstanceCounts :: Lens.Lens' FleetCapacity (Core.Maybe Types.EC2InstanceCounts)
fcInstanceCounts = Lens.field @"instanceCounts"
{-# INLINEABLE fcInstanceCounts #-}
{-# DEPRECATED instanceCounts "Use generic-lens or generic-optics with 'instanceCounts' instead"  #-}

-- | Name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcInstanceType :: Lens.Lens' FleetCapacity (Core.Maybe Types.EC2InstanceType)
fcInstanceType = Lens.field @"instanceType"
{-# INLINEABLE fcInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

instance Core.FromJSON FleetCapacity where
        parseJSON
          = Core.withObject "FleetCapacity" Core.$
              \ x ->
                FleetCapacity' Core.<$>
                  (x Core..:? "FleetId") Core.<*> x Core..:? "InstanceCounts"
                    Core.<*> x Core..:? "InstanceType"
