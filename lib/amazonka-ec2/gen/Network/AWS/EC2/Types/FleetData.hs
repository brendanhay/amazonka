{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetData
  ( FleetData (..),

    -- * Smart constructor
    mkFleetData,

    -- * Lenses
    fdClientToken,
    fdTargetCapacitySpecification,
    fdSpotOptions,
    fdExcessCapacityTerminationPolicy,
    fdOnDemandOptions,
    fdFleetState,
    fdLaunchTemplateConfigs,
    fdValidUntil,
    fdTerminateInstancesWithExpiration,
    fdInstances,
    fdFulfilledCapacity,
    fdType,
    fdValidFrom,
    fdReplaceUnhealthyInstances,
    fdFulfilledOnDemandCapacity,
    fdFleetId,
    fdErrors,
    fdCreateTime,
    fdTags,
    fdActivityStatus,
  )
where

import Network.AWS.EC2.Types.DescribeFleetError
import Network.AWS.EC2.Types.DescribeFleetsInstances
import Network.AWS.EC2.Types.FleetActivityStatus
import Network.AWS.EC2.Types.FleetExcessCapacityTerminationPolicy
import Network.AWS.EC2.Types.FleetLaunchTemplateConfig
import Network.AWS.EC2.Types.FleetStateCode
import Network.AWS.EC2.Types.FleetType
import Network.AWS.EC2.Types.OnDemandOptions
import Network.AWS.EC2.Types.SpotOptions
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TargetCapacitySpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an EC2 Fleet.
--
-- /See:/ 'mkFleetData' smart constructor.
data FleetData = FleetData'
  { clientToken :: Lude.Maybe Lude.Text,
    targetCapacitySpecification ::
      Lude.Maybe TargetCapacitySpecification,
    spotOptions :: Lude.Maybe SpotOptions,
    excessCapacityTerminationPolicy ::
      Lude.Maybe FleetExcessCapacityTerminationPolicy,
    onDemandOptions :: Lude.Maybe OnDemandOptions,
    fleetState :: Lude.Maybe FleetStateCode,
    launchTemplateConfigs :: Lude.Maybe [FleetLaunchTemplateConfig],
    validUntil :: Lude.Maybe Lude.DateTime,
    terminateInstancesWithExpiration :: Lude.Maybe Lude.Bool,
    instances :: Lude.Maybe [DescribeFleetsInstances],
    fulfilledCapacity :: Lude.Maybe Lude.Double,
    type' :: Lude.Maybe FleetType,
    validFrom :: Lude.Maybe Lude.DateTime,
    replaceUnhealthyInstances :: Lude.Maybe Lude.Bool,
    fulfilledOnDemandCapacity :: Lude.Maybe Lude.Double,
    fleetId :: Lude.Maybe Lude.Text,
    errors :: Lude.Maybe [DescribeFleetError],
    createTime :: Lude.Maybe Lude.DateTime,
    tags :: Lude.Maybe [Tag],
    activityStatus :: Lude.Maybe FleetActivityStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FleetData' with the minimum fields required to make a request.
--
-- * 'activityStatus' - The progress of the EC2 Fleet. If there is an error, the status is @error@ . After all requests are placed, the status is @pending_fulfillment@ . If the size of the EC2 Fleet is equal to or greater than its target capacity, the status is @fulfilled@ . If the size of the EC2 Fleet is decreased, the status is @pending_termination@ while instances are terminating.
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- Constraints: Maximum 64 ASCII characters
-- * 'createTime' - The creation date and time of the EC2 Fleet.
-- * 'errors' - Information about the instances that could not be launched by the fleet. Valid only when __Type__ is set to @instant@ .
-- * 'excessCapacityTerminationPolicy' - Indicates whether running instances should be terminated if the target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
-- * 'fleetId' - The ID of the EC2 Fleet.
-- * 'fleetState' - The state of the EC2 Fleet.
-- * 'fulfilledCapacity' - The number of units fulfilled by this request compared to the set target capacity.
-- * 'fulfilledOnDemandCapacity' - The number of units fulfilled by this request compared to the set target On-Demand capacity.
-- * 'instances' - Information about the instances that were launched by the fleet. Valid only when __Type__ is set to @instant@ .
-- * 'launchTemplateConfigs' - The launch template and overrides.
-- * 'onDemandOptions' - The allocation strategy of On-Demand Instances in an EC2 Fleet.
-- * 'replaceUnhealthyInstances' - Indicates whether EC2 Fleet should replace unhealthy instances.
-- * 'spotOptions' - The configuration of Spot Instances in an EC2 Fleet.
-- * 'tags' - The tags for an EC2 Fleet resource.
-- * 'targetCapacitySpecification' - The number of units to request. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
-- * 'terminateInstancesWithExpiration' - Indicates whether running instances should be terminated when the EC2 Fleet expires.
-- * 'type'' - The type of request. Indicates whether the EC2 Fleet only @requests@ the target capacity, or also attempts to @maintain@ it. If you request a certain target capacity, EC2 Fleet only places the required requests; it does not attempt to replenish instances if capacity is diminished, and it does not submit requests in alternative capacity pools if capacity is unavailable. To maintain a certain target capacity, EC2 Fleet places the required requests to meet this target capacity. It also automatically replenishes any interrupted Spot Instances. Default: @maintain@ .
-- * 'validFrom' - The start date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The default is to start fulfilling the request immediately.
-- * 'validUntil' - The end date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). At this point, no new instance requests are placed or able to fulfill the request. The default end date is 7 days from the current date.
mkFleetData ::
  FleetData
mkFleetData =
  FleetData'
    { clientToken = Lude.Nothing,
      targetCapacitySpecification = Lude.Nothing,
      spotOptions = Lude.Nothing,
      excessCapacityTerminationPolicy = Lude.Nothing,
      onDemandOptions = Lude.Nothing,
      fleetState = Lude.Nothing,
      launchTemplateConfigs = Lude.Nothing,
      validUntil = Lude.Nothing,
      terminateInstancesWithExpiration = Lude.Nothing,
      instances = Lude.Nothing,
      fulfilledCapacity = Lude.Nothing,
      type' = Lude.Nothing,
      validFrom = Lude.Nothing,
      replaceUnhealthyInstances = Lude.Nothing,
      fulfilledOnDemandCapacity = Lude.Nothing,
      fleetId = Lude.Nothing,
      errors = Lude.Nothing,
      createTime = Lude.Nothing,
      tags = Lude.Nothing,
      activityStatus = Lude.Nothing
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- Constraints: Maximum 64 ASCII characters
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdClientToken :: Lens.Lens' FleetData (Lude.Maybe Lude.Text)
fdClientToken = Lens.lens (clientToken :: FleetData -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: FleetData)
{-# DEPRECATED fdClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The number of units to request. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
--
-- /Note:/ Consider using 'targetCapacitySpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdTargetCapacitySpecification :: Lens.Lens' FleetData (Lude.Maybe TargetCapacitySpecification)
fdTargetCapacitySpecification = Lens.lens (targetCapacitySpecification :: FleetData -> Lude.Maybe TargetCapacitySpecification) (\s a -> s {targetCapacitySpecification = a} :: FleetData)
{-# DEPRECATED fdTargetCapacitySpecification "Use generic-lens or generic-optics with 'targetCapacitySpecification' instead." #-}

-- | The configuration of Spot Instances in an EC2 Fleet.
--
-- /Note:/ Consider using 'spotOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdSpotOptions :: Lens.Lens' FleetData (Lude.Maybe SpotOptions)
fdSpotOptions = Lens.lens (spotOptions :: FleetData -> Lude.Maybe SpotOptions) (\s a -> s {spotOptions = a} :: FleetData)
{-# DEPRECATED fdSpotOptions "Use generic-lens or generic-optics with 'spotOptions' instead." #-}

-- | Indicates whether running instances should be terminated if the target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
--
-- /Note:/ Consider using 'excessCapacityTerminationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdExcessCapacityTerminationPolicy :: Lens.Lens' FleetData (Lude.Maybe FleetExcessCapacityTerminationPolicy)
fdExcessCapacityTerminationPolicy = Lens.lens (excessCapacityTerminationPolicy :: FleetData -> Lude.Maybe FleetExcessCapacityTerminationPolicy) (\s a -> s {excessCapacityTerminationPolicy = a} :: FleetData)
{-# DEPRECATED fdExcessCapacityTerminationPolicy "Use generic-lens or generic-optics with 'excessCapacityTerminationPolicy' instead." #-}

-- | The allocation strategy of On-Demand Instances in an EC2 Fleet.
--
-- /Note:/ Consider using 'onDemandOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdOnDemandOptions :: Lens.Lens' FleetData (Lude.Maybe OnDemandOptions)
fdOnDemandOptions = Lens.lens (onDemandOptions :: FleetData -> Lude.Maybe OnDemandOptions) (\s a -> s {onDemandOptions = a} :: FleetData)
{-# DEPRECATED fdOnDemandOptions "Use generic-lens or generic-optics with 'onDemandOptions' instead." #-}

-- | The state of the EC2 Fleet.
--
-- /Note:/ Consider using 'fleetState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdFleetState :: Lens.Lens' FleetData (Lude.Maybe FleetStateCode)
fdFleetState = Lens.lens (fleetState :: FleetData -> Lude.Maybe FleetStateCode) (\s a -> s {fleetState = a} :: FleetData)
{-# DEPRECATED fdFleetState "Use generic-lens or generic-optics with 'fleetState' instead." #-}

-- | The launch template and overrides.
--
-- /Note:/ Consider using 'launchTemplateConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdLaunchTemplateConfigs :: Lens.Lens' FleetData (Lude.Maybe [FleetLaunchTemplateConfig])
fdLaunchTemplateConfigs = Lens.lens (launchTemplateConfigs :: FleetData -> Lude.Maybe [FleetLaunchTemplateConfig]) (\s a -> s {launchTemplateConfigs = a} :: FleetData)
{-# DEPRECATED fdLaunchTemplateConfigs "Use generic-lens or generic-optics with 'launchTemplateConfigs' instead." #-}

-- | The end date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). At this point, no new instance requests are placed or able to fulfill the request. The default end date is 7 days from the current date.
--
-- /Note:/ Consider using 'validUntil' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdValidUntil :: Lens.Lens' FleetData (Lude.Maybe Lude.DateTime)
fdValidUntil = Lens.lens (validUntil :: FleetData -> Lude.Maybe Lude.DateTime) (\s a -> s {validUntil = a} :: FleetData)
{-# DEPRECATED fdValidUntil "Use generic-lens or generic-optics with 'validUntil' instead." #-}

-- | Indicates whether running instances should be terminated when the EC2 Fleet expires.
--
-- /Note:/ Consider using 'terminateInstancesWithExpiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdTerminateInstancesWithExpiration :: Lens.Lens' FleetData (Lude.Maybe Lude.Bool)
fdTerminateInstancesWithExpiration = Lens.lens (terminateInstancesWithExpiration :: FleetData -> Lude.Maybe Lude.Bool) (\s a -> s {terminateInstancesWithExpiration = a} :: FleetData)
{-# DEPRECATED fdTerminateInstancesWithExpiration "Use generic-lens or generic-optics with 'terminateInstancesWithExpiration' instead." #-}

-- | Information about the instances that were launched by the fleet. Valid only when __Type__ is set to @instant@ .
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdInstances :: Lens.Lens' FleetData (Lude.Maybe [DescribeFleetsInstances])
fdInstances = Lens.lens (instances :: FleetData -> Lude.Maybe [DescribeFleetsInstances]) (\s a -> s {instances = a} :: FleetData)
{-# DEPRECATED fdInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The number of units fulfilled by this request compared to the set target capacity.
--
-- /Note:/ Consider using 'fulfilledCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdFulfilledCapacity :: Lens.Lens' FleetData (Lude.Maybe Lude.Double)
fdFulfilledCapacity = Lens.lens (fulfilledCapacity :: FleetData -> Lude.Maybe Lude.Double) (\s a -> s {fulfilledCapacity = a} :: FleetData)
{-# DEPRECATED fdFulfilledCapacity "Use generic-lens or generic-optics with 'fulfilledCapacity' instead." #-}

-- | The type of request. Indicates whether the EC2 Fleet only @requests@ the target capacity, or also attempts to @maintain@ it. If you request a certain target capacity, EC2 Fleet only places the required requests; it does not attempt to replenish instances if capacity is diminished, and it does not submit requests in alternative capacity pools if capacity is unavailable. To maintain a certain target capacity, EC2 Fleet places the required requests to meet this target capacity. It also automatically replenishes any interrupted Spot Instances. Default: @maintain@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdType :: Lens.Lens' FleetData (Lude.Maybe FleetType)
fdType = Lens.lens (type' :: FleetData -> Lude.Maybe FleetType) (\s a -> s {type' = a} :: FleetData)
{-# DEPRECATED fdType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The start date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The default is to start fulfilling the request immediately.
--
-- /Note:/ Consider using 'validFrom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdValidFrom :: Lens.Lens' FleetData (Lude.Maybe Lude.DateTime)
fdValidFrom = Lens.lens (validFrom :: FleetData -> Lude.Maybe Lude.DateTime) (\s a -> s {validFrom = a} :: FleetData)
{-# DEPRECATED fdValidFrom "Use generic-lens or generic-optics with 'validFrom' instead." #-}

-- | Indicates whether EC2 Fleet should replace unhealthy instances.
--
-- /Note:/ Consider using 'replaceUnhealthyInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdReplaceUnhealthyInstances :: Lens.Lens' FleetData (Lude.Maybe Lude.Bool)
fdReplaceUnhealthyInstances = Lens.lens (replaceUnhealthyInstances :: FleetData -> Lude.Maybe Lude.Bool) (\s a -> s {replaceUnhealthyInstances = a} :: FleetData)
{-# DEPRECATED fdReplaceUnhealthyInstances "Use generic-lens or generic-optics with 'replaceUnhealthyInstances' instead." #-}

-- | The number of units fulfilled by this request compared to the set target On-Demand capacity.
--
-- /Note:/ Consider using 'fulfilledOnDemandCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdFulfilledOnDemandCapacity :: Lens.Lens' FleetData (Lude.Maybe Lude.Double)
fdFulfilledOnDemandCapacity = Lens.lens (fulfilledOnDemandCapacity :: FleetData -> Lude.Maybe Lude.Double) (\s a -> s {fulfilledOnDemandCapacity = a} :: FleetData)
{-# DEPRECATED fdFulfilledOnDemandCapacity "Use generic-lens or generic-optics with 'fulfilledOnDemandCapacity' instead." #-}

-- | The ID of the EC2 Fleet.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdFleetId :: Lens.Lens' FleetData (Lude.Maybe Lude.Text)
fdFleetId = Lens.lens (fleetId :: FleetData -> Lude.Maybe Lude.Text) (\s a -> s {fleetId = a} :: FleetData)
{-# DEPRECATED fdFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | Information about the instances that could not be launched by the fleet. Valid only when __Type__ is set to @instant@ .
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdErrors :: Lens.Lens' FleetData (Lude.Maybe [DescribeFleetError])
fdErrors = Lens.lens (errors :: FleetData -> Lude.Maybe [DescribeFleetError]) (\s a -> s {errors = a} :: FleetData)
{-# DEPRECATED fdErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The creation date and time of the EC2 Fleet.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdCreateTime :: Lens.Lens' FleetData (Lude.Maybe Lude.DateTime)
fdCreateTime = Lens.lens (createTime :: FleetData -> Lude.Maybe Lude.DateTime) (\s a -> s {createTime = a} :: FleetData)
{-# DEPRECATED fdCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | The tags for an EC2 Fleet resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdTags :: Lens.Lens' FleetData (Lude.Maybe [Tag])
fdTags = Lens.lens (tags :: FleetData -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: FleetData)
{-# DEPRECATED fdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The progress of the EC2 Fleet. If there is an error, the status is @error@ . After all requests are placed, the status is @pending_fulfillment@ . If the size of the EC2 Fleet is equal to or greater than its target capacity, the status is @fulfilled@ . If the size of the EC2 Fleet is decreased, the status is @pending_termination@ while instances are terminating.
--
-- /Note:/ Consider using 'activityStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdActivityStatus :: Lens.Lens' FleetData (Lude.Maybe FleetActivityStatus)
fdActivityStatus = Lens.lens (activityStatus :: FleetData -> Lude.Maybe FleetActivityStatus) (\s a -> s {activityStatus = a} :: FleetData)
{-# DEPRECATED fdActivityStatus "Use generic-lens or generic-optics with 'activityStatus' instead." #-}

instance Lude.FromXML FleetData where
  parseXML x =
    FleetData'
      Lude.<$> (x Lude..@? "clientToken")
      Lude.<*> (x Lude..@? "targetCapacitySpecification")
      Lude.<*> (x Lude..@? "spotOptions")
      Lude.<*> (x Lude..@? "excessCapacityTerminationPolicy")
      Lude.<*> (x Lude..@? "onDemandOptions")
      Lude.<*> (x Lude..@? "fleetState")
      Lude.<*> ( x Lude..@? "launchTemplateConfigs" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "validUntil")
      Lude.<*> (x Lude..@? "terminateInstancesWithExpiration")
      Lude.<*> ( x Lude..@? "fleetInstanceSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "fulfilledCapacity")
      Lude.<*> (x Lude..@? "type")
      Lude.<*> (x Lude..@? "validFrom")
      Lude.<*> (x Lude..@? "replaceUnhealthyInstances")
      Lude.<*> (x Lude..@? "fulfilledOnDemandCapacity")
      Lude.<*> (x Lude..@? "fleetId")
      Lude.<*> ( x Lude..@? "errorSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "createTime")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "activityStatus")
