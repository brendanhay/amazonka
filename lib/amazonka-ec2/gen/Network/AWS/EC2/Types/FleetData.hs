{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.FleetData
  ( FleetData (..)
  -- * Smart constructor
  , mkFleetData
  -- * Lenses
  , fdActivityStatus
  , fdClientToken
  , fdCreateTime
  , fdErrors
  , fdExcessCapacityTerminationPolicy
  , fdFleetId
  , fdFleetState
  , fdFulfilledCapacity
  , fdFulfilledOnDemandCapacity
  , fdInstances
  , fdLaunchTemplateConfigs
  , fdOnDemandOptions
  , fdReplaceUnhealthyInstances
  , fdSpotOptions
  , fdTags
  , fdTargetCapacitySpecification
  , fdTerminateInstancesWithExpiration
  , fdType
  , fdValidFrom
  , fdValidUntil
  ) where

import qualified Network.AWS.EC2.Types.DescribeFleetError as Types
import qualified Network.AWS.EC2.Types.DescribeFleetsInstances as Types
import qualified Network.AWS.EC2.Types.FleetActivityStatus as Types
import qualified Network.AWS.EC2.Types.FleetExcessCapacityTerminationPolicy as Types
import qualified Network.AWS.EC2.Types.FleetId as Types
import qualified Network.AWS.EC2.Types.FleetLaunchTemplateConfig as Types
import qualified Network.AWS.EC2.Types.FleetStateCode as Types
import qualified Network.AWS.EC2.Types.FleetType as Types
import qualified Network.AWS.EC2.Types.OnDemandOptions as Types
import qualified Network.AWS.EC2.Types.SpotOptions as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.TargetCapacitySpecification as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an EC2 Fleet.
--
-- /See:/ 'mkFleetData' smart constructor.
data FleetData = FleetData'
  { activityStatus :: Core.Maybe Types.FleetActivityStatus
    -- ^ The progress of the EC2 Fleet. If there is an error, the status is @error@ . After all requests are placed, the status is @pending_fulfillment@ . If the size of the EC2 Fleet is equal to or greater than its target capacity, the status is @fulfilled@ . If the size of the EC2 Fleet is decreased, the status is @pending_termination@ while instances are terminating.
  , clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- Constraints: Maximum 64 ASCII characters
  , createTime :: Core.Maybe Core.UTCTime
    -- ^ The creation date and time of the EC2 Fleet.
  , errors :: Core.Maybe [Types.DescribeFleetError]
    -- ^ Information about the instances that could not be launched by the fleet. Valid only when __Type__ is set to @instant@ .
  , excessCapacityTerminationPolicy :: Core.Maybe Types.FleetExcessCapacityTerminationPolicy
    -- ^ Indicates whether running instances should be terminated if the target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
  , fleetId :: Core.Maybe Types.FleetId
    -- ^ The ID of the EC2 Fleet.
  , fleetState :: Core.Maybe Types.FleetStateCode
    -- ^ The state of the EC2 Fleet.
  , fulfilledCapacity :: Core.Maybe Core.Double
    -- ^ The number of units fulfilled by this request compared to the set target capacity.
  , fulfilledOnDemandCapacity :: Core.Maybe Core.Double
    -- ^ The number of units fulfilled by this request compared to the set target On-Demand capacity.
  , instances :: Core.Maybe [Types.DescribeFleetsInstances]
    -- ^ Information about the instances that were launched by the fleet. Valid only when __Type__ is set to @instant@ .
  , launchTemplateConfigs :: Core.Maybe [Types.FleetLaunchTemplateConfig]
    -- ^ The launch template and overrides.
  , onDemandOptions :: Core.Maybe Types.OnDemandOptions
    -- ^ The allocation strategy of On-Demand Instances in an EC2 Fleet.
  , replaceUnhealthyInstances :: Core.Maybe Core.Bool
    -- ^ Indicates whether EC2 Fleet should replace unhealthy instances.
  , spotOptions :: Core.Maybe Types.SpotOptions
    -- ^ The configuration of Spot Instances in an EC2 Fleet.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags for an EC2 Fleet resource.
  , targetCapacitySpecification :: Core.Maybe Types.TargetCapacitySpecification
    -- ^ The number of units to request. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
  , terminateInstancesWithExpiration :: Core.Maybe Core.Bool
    -- ^ Indicates whether running instances should be terminated when the EC2 Fleet expires. 
  , type' :: Core.Maybe Types.FleetType
    -- ^ The type of request. Indicates whether the EC2 Fleet only @requests@ the target capacity, or also attempts to @maintain@ it. If you request a certain target capacity, EC2 Fleet only places the required requests; it does not attempt to replenish instances if capacity is diminished, and it does not submit requests in alternative capacity pools if capacity is unavailable. To maintain a certain target capacity, EC2 Fleet places the required requests to meet this target capacity. It also automatically replenishes any interrupted Spot Instances. Default: @maintain@ .
  , validFrom :: Core.Maybe Core.UTCTime
    -- ^ The start date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The default is to start fulfilling the request immediately. 
  , validUntil :: Core.Maybe Core.UTCTime
    -- ^ The end date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). At this point, no new instance requests are placed or able to fulfill the request. The default end date is 7 days from the current date. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'FleetData' value with any optional fields omitted.
mkFleetData
    :: FleetData
mkFleetData
  = FleetData'{activityStatus = Core.Nothing,
               clientToken = Core.Nothing, createTime = Core.Nothing,
               errors = Core.Nothing,
               excessCapacityTerminationPolicy = Core.Nothing,
               fleetId = Core.Nothing, fleetState = Core.Nothing,
               fulfilledCapacity = Core.Nothing,
               fulfilledOnDemandCapacity = Core.Nothing, instances = Core.Nothing,
               launchTemplateConfigs = Core.Nothing,
               onDemandOptions = Core.Nothing,
               replaceUnhealthyInstances = Core.Nothing,
               spotOptions = Core.Nothing, tags = Core.Nothing,
               targetCapacitySpecification = Core.Nothing,
               terminateInstancesWithExpiration = Core.Nothing,
               type' = Core.Nothing, validFrom = Core.Nothing,
               validUntil = Core.Nothing}

-- | The progress of the EC2 Fleet. If there is an error, the status is @error@ . After all requests are placed, the status is @pending_fulfillment@ . If the size of the EC2 Fleet is equal to or greater than its target capacity, the status is @fulfilled@ . If the size of the EC2 Fleet is decreased, the status is @pending_termination@ while instances are terminating.
--
-- /Note:/ Consider using 'activityStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdActivityStatus :: Lens.Lens' FleetData (Core.Maybe Types.FleetActivityStatus)
fdActivityStatus = Lens.field @"activityStatus"
{-# INLINEABLE fdActivityStatus #-}
{-# DEPRECATED activityStatus "Use generic-lens or generic-optics with 'activityStatus' instead"  #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- Constraints: Maximum 64 ASCII characters
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdClientToken :: Lens.Lens' FleetData (Core.Maybe Core.Text)
fdClientToken = Lens.field @"clientToken"
{-# INLINEABLE fdClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The creation date and time of the EC2 Fleet.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdCreateTime :: Lens.Lens' FleetData (Core.Maybe Core.UTCTime)
fdCreateTime = Lens.field @"createTime"
{-# INLINEABLE fdCreateTime #-}
{-# DEPRECATED createTime "Use generic-lens or generic-optics with 'createTime' instead"  #-}

-- | Information about the instances that could not be launched by the fleet. Valid only when __Type__ is set to @instant@ .
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdErrors :: Lens.Lens' FleetData (Core.Maybe [Types.DescribeFleetError])
fdErrors = Lens.field @"errors"
{-# INLINEABLE fdErrors #-}
{-# DEPRECATED errors "Use generic-lens or generic-optics with 'errors' instead"  #-}

-- | Indicates whether running instances should be terminated if the target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
--
-- /Note:/ Consider using 'excessCapacityTerminationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdExcessCapacityTerminationPolicy :: Lens.Lens' FleetData (Core.Maybe Types.FleetExcessCapacityTerminationPolicy)
fdExcessCapacityTerminationPolicy = Lens.field @"excessCapacityTerminationPolicy"
{-# INLINEABLE fdExcessCapacityTerminationPolicy #-}
{-# DEPRECATED excessCapacityTerminationPolicy "Use generic-lens or generic-optics with 'excessCapacityTerminationPolicy' instead"  #-}

-- | The ID of the EC2 Fleet.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdFleetId :: Lens.Lens' FleetData (Core.Maybe Types.FleetId)
fdFleetId = Lens.field @"fleetId"
{-# INLINEABLE fdFleetId #-}
{-# DEPRECATED fleetId "Use generic-lens or generic-optics with 'fleetId' instead"  #-}

-- | The state of the EC2 Fleet.
--
-- /Note:/ Consider using 'fleetState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdFleetState :: Lens.Lens' FleetData (Core.Maybe Types.FleetStateCode)
fdFleetState = Lens.field @"fleetState"
{-# INLINEABLE fdFleetState #-}
{-# DEPRECATED fleetState "Use generic-lens or generic-optics with 'fleetState' instead"  #-}

-- | The number of units fulfilled by this request compared to the set target capacity.
--
-- /Note:/ Consider using 'fulfilledCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdFulfilledCapacity :: Lens.Lens' FleetData (Core.Maybe Core.Double)
fdFulfilledCapacity = Lens.field @"fulfilledCapacity"
{-# INLINEABLE fdFulfilledCapacity #-}
{-# DEPRECATED fulfilledCapacity "Use generic-lens or generic-optics with 'fulfilledCapacity' instead"  #-}

-- | The number of units fulfilled by this request compared to the set target On-Demand capacity.
--
-- /Note:/ Consider using 'fulfilledOnDemandCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdFulfilledOnDemandCapacity :: Lens.Lens' FleetData (Core.Maybe Core.Double)
fdFulfilledOnDemandCapacity = Lens.field @"fulfilledOnDemandCapacity"
{-# INLINEABLE fdFulfilledOnDemandCapacity #-}
{-# DEPRECATED fulfilledOnDemandCapacity "Use generic-lens or generic-optics with 'fulfilledOnDemandCapacity' instead"  #-}

-- | Information about the instances that were launched by the fleet. Valid only when __Type__ is set to @instant@ .
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdInstances :: Lens.Lens' FleetData (Core.Maybe [Types.DescribeFleetsInstances])
fdInstances = Lens.field @"instances"
{-# INLINEABLE fdInstances #-}
{-# DEPRECATED instances "Use generic-lens or generic-optics with 'instances' instead"  #-}

-- | The launch template and overrides.
--
-- /Note:/ Consider using 'launchTemplateConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdLaunchTemplateConfigs :: Lens.Lens' FleetData (Core.Maybe [Types.FleetLaunchTemplateConfig])
fdLaunchTemplateConfigs = Lens.field @"launchTemplateConfigs"
{-# INLINEABLE fdLaunchTemplateConfigs #-}
{-# DEPRECATED launchTemplateConfigs "Use generic-lens or generic-optics with 'launchTemplateConfigs' instead"  #-}

-- | The allocation strategy of On-Demand Instances in an EC2 Fleet.
--
-- /Note:/ Consider using 'onDemandOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdOnDemandOptions :: Lens.Lens' FleetData (Core.Maybe Types.OnDemandOptions)
fdOnDemandOptions = Lens.field @"onDemandOptions"
{-# INLINEABLE fdOnDemandOptions #-}
{-# DEPRECATED onDemandOptions "Use generic-lens or generic-optics with 'onDemandOptions' instead"  #-}

-- | Indicates whether EC2 Fleet should replace unhealthy instances.
--
-- /Note:/ Consider using 'replaceUnhealthyInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdReplaceUnhealthyInstances :: Lens.Lens' FleetData (Core.Maybe Core.Bool)
fdReplaceUnhealthyInstances = Lens.field @"replaceUnhealthyInstances"
{-# INLINEABLE fdReplaceUnhealthyInstances #-}
{-# DEPRECATED replaceUnhealthyInstances "Use generic-lens or generic-optics with 'replaceUnhealthyInstances' instead"  #-}

-- | The configuration of Spot Instances in an EC2 Fleet.
--
-- /Note:/ Consider using 'spotOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdSpotOptions :: Lens.Lens' FleetData (Core.Maybe Types.SpotOptions)
fdSpotOptions = Lens.field @"spotOptions"
{-# INLINEABLE fdSpotOptions #-}
{-# DEPRECATED spotOptions "Use generic-lens or generic-optics with 'spotOptions' instead"  #-}

-- | The tags for an EC2 Fleet resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdTags :: Lens.Lens' FleetData (Core.Maybe [Types.Tag])
fdTags = Lens.field @"tags"
{-# INLINEABLE fdTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The number of units to request. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
--
-- /Note:/ Consider using 'targetCapacitySpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdTargetCapacitySpecification :: Lens.Lens' FleetData (Core.Maybe Types.TargetCapacitySpecification)
fdTargetCapacitySpecification = Lens.field @"targetCapacitySpecification"
{-# INLINEABLE fdTargetCapacitySpecification #-}
{-# DEPRECATED targetCapacitySpecification "Use generic-lens or generic-optics with 'targetCapacitySpecification' instead"  #-}

-- | Indicates whether running instances should be terminated when the EC2 Fleet expires. 
--
-- /Note:/ Consider using 'terminateInstancesWithExpiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdTerminateInstancesWithExpiration :: Lens.Lens' FleetData (Core.Maybe Core.Bool)
fdTerminateInstancesWithExpiration = Lens.field @"terminateInstancesWithExpiration"
{-# INLINEABLE fdTerminateInstancesWithExpiration #-}
{-# DEPRECATED terminateInstancesWithExpiration "Use generic-lens or generic-optics with 'terminateInstancesWithExpiration' instead"  #-}

-- | The type of request. Indicates whether the EC2 Fleet only @requests@ the target capacity, or also attempts to @maintain@ it. If you request a certain target capacity, EC2 Fleet only places the required requests; it does not attempt to replenish instances if capacity is diminished, and it does not submit requests in alternative capacity pools if capacity is unavailable. To maintain a certain target capacity, EC2 Fleet places the required requests to meet this target capacity. It also automatically replenishes any interrupted Spot Instances. Default: @maintain@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdType :: Lens.Lens' FleetData (Core.Maybe Types.FleetType)
fdType = Lens.field @"type'"
{-# INLINEABLE fdType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The start date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The default is to start fulfilling the request immediately. 
--
-- /Note:/ Consider using 'validFrom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdValidFrom :: Lens.Lens' FleetData (Core.Maybe Core.UTCTime)
fdValidFrom = Lens.field @"validFrom"
{-# INLINEABLE fdValidFrom #-}
{-# DEPRECATED validFrom "Use generic-lens or generic-optics with 'validFrom' instead"  #-}

-- | The end date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). At this point, no new instance requests are placed or able to fulfill the request. The default end date is 7 days from the current date. 
--
-- /Note:/ Consider using 'validUntil' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdValidUntil :: Lens.Lens' FleetData (Core.Maybe Core.UTCTime)
fdValidUntil = Lens.field @"validUntil"
{-# INLINEABLE fdValidUntil #-}
{-# DEPRECATED validUntil "Use generic-lens or generic-optics with 'validUntil' instead"  #-}

instance Core.FromXML FleetData where
        parseXML x
          = FleetData' Core.<$>
              (x Core..@? "activityStatus") Core.<*> x Core..@? "clientToken"
                Core.<*> x Core..@? "createTime"
                Core.<*> x Core..@? "errorSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "excessCapacityTerminationPolicy"
                Core.<*> x Core..@? "fleetId"
                Core.<*> x Core..@? "fleetState"
                Core.<*> x Core..@? "fulfilledCapacity"
                Core.<*> x Core..@? "fulfilledOnDemandCapacity"
                Core.<*>
                x Core..@? "fleetInstanceSet" Core..<@> Core.parseXMLList "item"
                Core.<*>
                x Core..@? "launchTemplateConfigs" Core..<@>
                  Core.parseXMLList "item"
                Core.<*> x Core..@? "onDemandOptions"
                Core.<*> x Core..@? "replaceUnhealthyInstances"
                Core.<*> x Core..@? "spotOptions"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "targetCapacitySpecification"
                Core.<*> x Core..@? "terminateInstancesWithExpiration"
                Core.<*> x Core..@? "type"
                Core.<*> x Core..@? "validFrom"
                Core.<*> x Core..@? "validUntil"
