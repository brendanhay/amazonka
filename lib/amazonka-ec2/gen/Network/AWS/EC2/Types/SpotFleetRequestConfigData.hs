{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotFleetRequestConfigData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotFleetRequestConfigData where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AllocationStrategy
import Network.AWS.EC2.Types.ExcessCapacityTerminationPolicy
import Network.AWS.EC2.Types.FleetType
import Network.AWS.EC2.Types.InstanceInterruptionBehavior
import Network.AWS.EC2.Types.LaunchTemplateConfig
import Network.AWS.EC2.Types.LoadBalancersConfig
import Network.AWS.EC2.Types.OnDemandAllocationStrategy
import Network.AWS.EC2.Types.SpotFleetLaunchSpecification
import Network.AWS.EC2.Types.SpotMaintenanceStrategies
import Network.AWS.EC2.Types.TagSpecification
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the configuration of a Spot Fleet request.
--
--
--
-- /See:/ 'spotFleetRequestConfigData' smart constructor.
data SpotFleetRequestConfigData = SpotFleetRequestConfigData'
  { _sfrcdClientToken ::
      !(Maybe Text),
    _sfrcdInstanceInterruptionBehavior ::
      !(Maybe InstanceInterruptionBehavior),
    _sfrcdOnDemandMaxTotalPrice ::
      !(Maybe Text),
    _sfrcdSpotPrice :: !(Maybe Text),
    _sfrcdSpotMaintenanceStrategies ::
      !(Maybe SpotMaintenanceStrategies),
    _sfrcdLoadBalancersConfig ::
      !(Maybe LoadBalancersConfig),
    _sfrcdExcessCapacityTerminationPolicy ::
      !( Maybe
           ExcessCapacityTerminationPolicy
       ),
    _sfrcdOnDemandTargetCapacity ::
      !(Maybe Int),
    _sfrcdLaunchTemplateConfigs ::
      !(Maybe [LaunchTemplateConfig]),
    _sfrcdTagSpecifications ::
      !(Maybe [TagSpecification]),
    _sfrcdValidUntil :: !(Maybe ISO8601),
    _sfrcdTerminateInstancesWithExpiration ::
      !(Maybe Bool),
    _sfrcdOnDemandAllocationStrategy ::
      !(Maybe OnDemandAllocationStrategy),
    _sfrcdInstancePoolsToUseCount ::
      !(Maybe Int),
    _sfrcdFulfilledCapacity ::
      !(Maybe Double),
    _sfrcdType :: !(Maybe FleetType),
    _sfrcdValidFrom :: !(Maybe ISO8601),
    _sfrcdReplaceUnhealthyInstances ::
      !(Maybe Bool),
    _sfrcdLaunchSpecifications ::
      !( Maybe
           [SpotFleetLaunchSpecification]
       ),
    _sfrcdOnDemandFulfilledCapacity ::
      !(Maybe Double),
    _sfrcdSpotMaxTotalPrice ::
      !(Maybe Text),
    _sfrcdAllocationStrategy ::
      !(Maybe AllocationStrategy),
    _sfrcdIAMFleetRole :: !Text,
    _sfrcdTargetCapacity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SpotFleetRequestConfigData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfrcdClientToken' - A unique, case-sensitive identifier that you provide to ensure the idempotency of your listings. This helps to avoid duplicate listings. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- * 'sfrcdInstanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is @terminate@ .
--
-- * 'sfrcdOnDemandMaxTotalPrice' - The maximum amount per hour for On-Demand Instances that you're willing to pay. You can use the @onDemandMaxTotalPrice@ parameter, the @spotMaxTotalPrice@ parameter, or both parameters to ensure that your fleet cost does not exceed your budget. If you set a maximum price per hour for the On-Demand Instances and Spot Instances in your request, Spot Fleet will launch instances until it reaches the maximum amount you're willing to pay. When the maximum amount you're willing to pay is reached, the fleet stops launching instances even if it hasn’t met the target capacity.
--
-- * 'sfrcdSpotPrice' - The maximum price per unit hour that you are willing to pay for a Spot Instance. The default is the On-Demand price.
--
-- * 'sfrcdSpotMaintenanceStrategies' - The strategies for managing your Spot Instances that are at an elevated risk of being interrupted.
--
-- * 'sfrcdLoadBalancersConfig' - One or more Classic Load Balancers and target groups to attach to the Spot Fleet request. Spot Fleet registers the running Spot Instances with the specified Classic Load Balancers and target groups. With Network Load Balancers, Spot Fleet cannot register instances that have the following instance types: C1, CC1, CC2, CG1, CG2, CR1, CS1, G1, G2, HI1, HS1, M1, M2, M3, and T1.
--
-- * 'sfrcdExcessCapacityTerminationPolicy' - Indicates whether running Spot Instances should be terminated if you decrease the target capacity of the Spot Fleet request below the current size of the Spot Fleet.
--
-- * 'sfrcdOnDemandTargetCapacity' - The number of On-Demand units to request. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
--
-- * 'sfrcdLaunchTemplateConfigs' - The launch template and overrides. If you specify @LaunchTemplateConfigs@ , you can't specify @LaunchSpecifications@ . If you include On-Demand capacity in your request, you must use @LaunchTemplateConfigs@ .
--
-- * 'sfrcdTagSpecifications' - The key-value pair for tagging the Spot Fleet request on creation. The value for @ResourceType@ must be @spot-fleet-request@ , otherwise the Spot Fleet request fails. To tag instances at launch, specify the tags in the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#create-launch-template launch template> (valid only if you use @LaunchTemplateConfigs@ ) or in the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotFleetTagSpecification.html @SpotFleetTagSpecification@ > (valid only if you use @LaunchSpecifications@ ). For information about tagging after launch, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-resources Tagging Your Resources> .
--
-- * 'sfrcdValidUntil' - The end date and time of the request, in UTC format (/YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). After the end date and time, no new Spot Instance requests are placed or able to fulfill the request. If no value is specified, the Spot Fleet request remains until you cancel it.
--
-- * 'sfrcdTerminateInstancesWithExpiration' - Indicates whether running Spot Instances are terminated when the Spot Fleet request expires.
--
-- * 'sfrcdOnDemandAllocationStrategy' - The order of the launch template overrides to use in fulfilling On-Demand capacity. If you specify @lowestPrice@ , Spot Fleet uses price to determine the order, launching the lowest price first. If you specify @prioritized@ , Spot Fleet uses the priority that you assign to each Spot Fleet launch template override, launching the highest priority first. If you do not specify a value, Spot Fleet defaults to @lowestPrice@ .
--
-- * 'sfrcdInstancePoolsToUseCount' - The number of Spot pools across which to allocate your target Spot capacity. Valid only when Spot __AllocationStrategy__ is set to @lowest-price@ . Spot Fleet selects the cheapest Spot pools and evenly allocates your target Spot capacity across the number of Spot pools that you specify.
--
-- * 'sfrcdFulfilledCapacity' - The number of units fulfilled by this request compared to the set target capacity. You cannot set this value.
--
-- * 'sfrcdType' - The type of request. Indicates whether the Spot Fleet only requests the target capacity or also attempts to maintain it. When this value is @request@ , the Spot Fleet only places the required requests. It does not attempt to replenish Spot Instances if capacity is diminished, nor does it submit requests in alternative Spot pools if capacity is not available. When this value is @maintain@ , the Spot Fleet maintains the target capacity. The Spot Fleet places the required requests to meet capacity and automatically replenishes any interrupted instances. Default: @maintain@ . @instant@ is listed but is not used by Spot Fleet.
--
-- * 'sfrcdValidFrom' - The start date and time of the request, in UTC format (/YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). By default, Amazon EC2 starts fulfilling the request immediately.
--
-- * 'sfrcdReplaceUnhealthyInstances' - Indicates whether Spot Fleet should replace unhealthy instances.
--
-- * 'sfrcdLaunchSpecifications' - The launch specifications for the Spot Fleet request. If you specify @LaunchSpecifications@ , you can't specify @LaunchTemplateConfigs@ . If you include On-Demand capacity in your request, you must use @LaunchTemplateConfigs@ .
--
-- * 'sfrcdOnDemandFulfilledCapacity' - The number of On-Demand units fulfilled by this request compared to the set target On-Demand capacity.
--
-- * 'sfrcdSpotMaxTotalPrice' - The maximum amount per hour for Spot Instances that you're willing to pay. You can use the @spotdMaxTotalPrice@ parameter, the @onDemandMaxTotalPrice@ parameter, or both parameters to ensure that your fleet cost does not exceed your budget. If you set a maximum price per hour for the On-Demand Instances and Spot Instances in your request, Spot Fleet will launch instances until it reaches the maximum amount you're willing to pay. When the maximum amount you're willing to pay is reached, the fleet stops launching instances even if it hasn’t met the target capacity.
--
-- * 'sfrcdAllocationStrategy' - Indicates how to allocate the target Spot Instance capacity across the Spot Instance pools specified by the Spot Fleet request. If the allocation strategy is @lowestPrice@ , Spot Fleet launches instances from the Spot Instance pools with the lowest price. This is the default allocation strategy. If the allocation strategy is @diversified@ , Spot Fleet launches instances from all the Spot Instance pools that you specify. If the allocation strategy is @capacityOptimized@ , Spot Fleet launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching.
--
-- * 'sfrcdIAMFleetRole' - The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that grants the Spot Fleet the permission to request, launch, terminate, and tag instances on your behalf. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-requests.html#spot-fleet-prerequisites Spot Fleet prerequisites> in the /Amazon EC2 User Guide for Linux Instances/ . Spot Fleet can terminate Spot Instances on your behalf when you cancel its Spot Fleet request using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CancelSpotFleetRequests CancelSpotFleetRequests> or when the Spot Fleet request expires, if you set @TerminateInstancesWithExpiration@ .
--
-- * 'sfrcdTargetCapacity' - The number of units to request for the Spot Fleet. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
spotFleetRequestConfigData ::
  -- | 'sfrcdIAMFleetRole'
  Text ->
  -- | 'sfrcdTargetCapacity'
  Int ->
  SpotFleetRequestConfigData
spotFleetRequestConfigData pIAMFleetRole_ pTargetCapacity_ =
  SpotFleetRequestConfigData'
    { _sfrcdClientToken = Nothing,
      _sfrcdInstanceInterruptionBehavior = Nothing,
      _sfrcdOnDemandMaxTotalPrice = Nothing,
      _sfrcdSpotPrice = Nothing,
      _sfrcdSpotMaintenanceStrategies = Nothing,
      _sfrcdLoadBalancersConfig = Nothing,
      _sfrcdExcessCapacityTerminationPolicy = Nothing,
      _sfrcdOnDemandTargetCapacity = Nothing,
      _sfrcdLaunchTemplateConfigs = Nothing,
      _sfrcdTagSpecifications = Nothing,
      _sfrcdValidUntil = Nothing,
      _sfrcdTerminateInstancesWithExpiration = Nothing,
      _sfrcdOnDemandAllocationStrategy = Nothing,
      _sfrcdInstancePoolsToUseCount = Nothing,
      _sfrcdFulfilledCapacity = Nothing,
      _sfrcdType = Nothing,
      _sfrcdValidFrom = Nothing,
      _sfrcdReplaceUnhealthyInstances = Nothing,
      _sfrcdLaunchSpecifications = Nothing,
      _sfrcdOnDemandFulfilledCapacity = Nothing,
      _sfrcdSpotMaxTotalPrice = Nothing,
      _sfrcdAllocationStrategy = Nothing,
      _sfrcdIAMFleetRole = pIAMFleetRole_,
      _sfrcdTargetCapacity = pTargetCapacity_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the idempotency of your listings. This helps to avoid duplicate listings. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
sfrcdClientToken :: Lens' SpotFleetRequestConfigData (Maybe Text)
sfrcdClientToken = lens _sfrcdClientToken (\s a -> s {_sfrcdClientToken = a})

-- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
sfrcdInstanceInterruptionBehavior :: Lens' SpotFleetRequestConfigData (Maybe InstanceInterruptionBehavior)
sfrcdInstanceInterruptionBehavior = lens _sfrcdInstanceInterruptionBehavior (\s a -> s {_sfrcdInstanceInterruptionBehavior = a})

-- | The maximum amount per hour for On-Demand Instances that you're willing to pay. You can use the @onDemandMaxTotalPrice@ parameter, the @spotMaxTotalPrice@ parameter, or both parameters to ensure that your fleet cost does not exceed your budget. If you set a maximum price per hour for the On-Demand Instances and Spot Instances in your request, Spot Fleet will launch instances until it reaches the maximum amount you're willing to pay. When the maximum amount you're willing to pay is reached, the fleet stops launching instances even if it hasn’t met the target capacity.
sfrcdOnDemandMaxTotalPrice :: Lens' SpotFleetRequestConfigData (Maybe Text)
sfrcdOnDemandMaxTotalPrice = lens _sfrcdOnDemandMaxTotalPrice (\s a -> s {_sfrcdOnDemandMaxTotalPrice = a})

-- | The maximum price per unit hour that you are willing to pay for a Spot Instance. The default is the On-Demand price.
sfrcdSpotPrice :: Lens' SpotFleetRequestConfigData (Maybe Text)
sfrcdSpotPrice = lens _sfrcdSpotPrice (\s a -> s {_sfrcdSpotPrice = a})

-- | The strategies for managing your Spot Instances that are at an elevated risk of being interrupted.
sfrcdSpotMaintenanceStrategies :: Lens' SpotFleetRequestConfigData (Maybe SpotMaintenanceStrategies)
sfrcdSpotMaintenanceStrategies = lens _sfrcdSpotMaintenanceStrategies (\s a -> s {_sfrcdSpotMaintenanceStrategies = a})

-- | One or more Classic Load Balancers and target groups to attach to the Spot Fleet request. Spot Fleet registers the running Spot Instances with the specified Classic Load Balancers and target groups. With Network Load Balancers, Spot Fleet cannot register instances that have the following instance types: C1, CC1, CC2, CG1, CG2, CR1, CS1, G1, G2, HI1, HS1, M1, M2, M3, and T1.
sfrcdLoadBalancersConfig :: Lens' SpotFleetRequestConfigData (Maybe LoadBalancersConfig)
sfrcdLoadBalancersConfig = lens _sfrcdLoadBalancersConfig (\s a -> s {_sfrcdLoadBalancersConfig = a})

-- | Indicates whether running Spot Instances should be terminated if you decrease the target capacity of the Spot Fleet request below the current size of the Spot Fleet.
sfrcdExcessCapacityTerminationPolicy :: Lens' SpotFleetRequestConfigData (Maybe ExcessCapacityTerminationPolicy)
sfrcdExcessCapacityTerminationPolicy = lens _sfrcdExcessCapacityTerminationPolicy (\s a -> s {_sfrcdExcessCapacityTerminationPolicy = a})

-- | The number of On-Demand units to request. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
sfrcdOnDemandTargetCapacity :: Lens' SpotFleetRequestConfigData (Maybe Int)
sfrcdOnDemandTargetCapacity = lens _sfrcdOnDemandTargetCapacity (\s a -> s {_sfrcdOnDemandTargetCapacity = a})

-- | The launch template and overrides. If you specify @LaunchTemplateConfigs@ , you can't specify @LaunchSpecifications@ . If you include On-Demand capacity in your request, you must use @LaunchTemplateConfigs@ .
sfrcdLaunchTemplateConfigs :: Lens' SpotFleetRequestConfigData [LaunchTemplateConfig]
sfrcdLaunchTemplateConfigs = lens _sfrcdLaunchTemplateConfigs (\s a -> s {_sfrcdLaunchTemplateConfigs = a}) . _Default . _Coerce

-- | The key-value pair for tagging the Spot Fleet request on creation. The value for @ResourceType@ must be @spot-fleet-request@ , otherwise the Spot Fleet request fails. To tag instances at launch, specify the tags in the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#create-launch-template launch template> (valid only if you use @LaunchTemplateConfigs@ ) or in the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotFleetTagSpecification.html @SpotFleetTagSpecification@ > (valid only if you use @LaunchSpecifications@ ). For information about tagging after launch, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-resources Tagging Your Resources> .
sfrcdTagSpecifications :: Lens' SpotFleetRequestConfigData [TagSpecification]
sfrcdTagSpecifications = lens _sfrcdTagSpecifications (\s a -> s {_sfrcdTagSpecifications = a}) . _Default . _Coerce

-- | The end date and time of the request, in UTC format (/YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). After the end date and time, no new Spot Instance requests are placed or able to fulfill the request. If no value is specified, the Spot Fleet request remains until you cancel it.
sfrcdValidUntil :: Lens' SpotFleetRequestConfigData (Maybe UTCTime)
sfrcdValidUntil = lens _sfrcdValidUntil (\s a -> s {_sfrcdValidUntil = a}) . mapping _Time

-- | Indicates whether running Spot Instances are terminated when the Spot Fleet request expires.
sfrcdTerminateInstancesWithExpiration :: Lens' SpotFleetRequestConfigData (Maybe Bool)
sfrcdTerminateInstancesWithExpiration = lens _sfrcdTerminateInstancesWithExpiration (\s a -> s {_sfrcdTerminateInstancesWithExpiration = a})

-- | The order of the launch template overrides to use in fulfilling On-Demand capacity. If you specify @lowestPrice@ , Spot Fleet uses price to determine the order, launching the lowest price first. If you specify @prioritized@ , Spot Fleet uses the priority that you assign to each Spot Fleet launch template override, launching the highest priority first. If you do not specify a value, Spot Fleet defaults to @lowestPrice@ .
sfrcdOnDemandAllocationStrategy :: Lens' SpotFleetRequestConfigData (Maybe OnDemandAllocationStrategy)
sfrcdOnDemandAllocationStrategy = lens _sfrcdOnDemandAllocationStrategy (\s a -> s {_sfrcdOnDemandAllocationStrategy = a})

-- | The number of Spot pools across which to allocate your target Spot capacity. Valid only when Spot __AllocationStrategy__ is set to @lowest-price@ . Spot Fleet selects the cheapest Spot pools and evenly allocates your target Spot capacity across the number of Spot pools that you specify.
sfrcdInstancePoolsToUseCount :: Lens' SpotFleetRequestConfigData (Maybe Int)
sfrcdInstancePoolsToUseCount = lens _sfrcdInstancePoolsToUseCount (\s a -> s {_sfrcdInstancePoolsToUseCount = a})

-- | The number of units fulfilled by this request compared to the set target capacity. You cannot set this value.
sfrcdFulfilledCapacity :: Lens' SpotFleetRequestConfigData (Maybe Double)
sfrcdFulfilledCapacity = lens _sfrcdFulfilledCapacity (\s a -> s {_sfrcdFulfilledCapacity = a})

-- | The type of request. Indicates whether the Spot Fleet only requests the target capacity or also attempts to maintain it. When this value is @request@ , the Spot Fleet only places the required requests. It does not attempt to replenish Spot Instances if capacity is diminished, nor does it submit requests in alternative Spot pools if capacity is not available. When this value is @maintain@ , the Spot Fleet maintains the target capacity. The Spot Fleet places the required requests to meet capacity and automatically replenishes any interrupted instances. Default: @maintain@ . @instant@ is listed but is not used by Spot Fleet.
sfrcdType :: Lens' SpotFleetRequestConfigData (Maybe FleetType)
sfrcdType = lens _sfrcdType (\s a -> s {_sfrcdType = a})

-- | The start date and time of the request, in UTC format (/YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). By default, Amazon EC2 starts fulfilling the request immediately.
sfrcdValidFrom :: Lens' SpotFleetRequestConfigData (Maybe UTCTime)
sfrcdValidFrom = lens _sfrcdValidFrom (\s a -> s {_sfrcdValidFrom = a}) . mapping _Time

-- | Indicates whether Spot Fleet should replace unhealthy instances.
sfrcdReplaceUnhealthyInstances :: Lens' SpotFleetRequestConfigData (Maybe Bool)
sfrcdReplaceUnhealthyInstances = lens _sfrcdReplaceUnhealthyInstances (\s a -> s {_sfrcdReplaceUnhealthyInstances = a})

-- | The launch specifications for the Spot Fleet request. If you specify @LaunchSpecifications@ , you can't specify @LaunchTemplateConfigs@ . If you include On-Demand capacity in your request, you must use @LaunchTemplateConfigs@ .
sfrcdLaunchSpecifications :: Lens' SpotFleetRequestConfigData [SpotFleetLaunchSpecification]
sfrcdLaunchSpecifications = lens _sfrcdLaunchSpecifications (\s a -> s {_sfrcdLaunchSpecifications = a}) . _Default . _Coerce

-- | The number of On-Demand units fulfilled by this request compared to the set target On-Demand capacity.
sfrcdOnDemandFulfilledCapacity :: Lens' SpotFleetRequestConfigData (Maybe Double)
sfrcdOnDemandFulfilledCapacity = lens _sfrcdOnDemandFulfilledCapacity (\s a -> s {_sfrcdOnDemandFulfilledCapacity = a})

-- | The maximum amount per hour for Spot Instances that you're willing to pay. You can use the @spotdMaxTotalPrice@ parameter, the @onDemandMaxTotalPrice@ parameter, or both parameters to ensure that your fleet cost does not exceed your budget. If you set a maximum price per hour for the On-Demand Instances and Spot Instances in your request, Spot Fleet will launch instances until it reaches the maximum amount you're willing to pay. When the maximum amount you're willing to pay is reached, the fleet stops launching instances even if it hasn’t met the target capacity.
sfrcdSpotMaxTotalPrice :: Lens' SpotFleetRequestConfigData (Maybe Text)
sfrcdSpotMaxTotalPrice = lens _sfrcdSpotMaxTotalPrice (\s a -> s {_sfrcdSpotMaxTotalPrice = a})

-- | Indicates how to allocate the target Spot Instance capacity across the Spot Instance pools specified by the Spot Fleet request. If the allocation strategy is @lowestPrice@ , Spot Fleet launches instances from the Spot Instance pools with the lowest price. This is the default allocation strategy. If the allocation strategy is @diversified@ , Spot Fleet launches instances from all the Spot Instance pools that you specify. If the allocation strategy is @capacityOptimized@ , Spot Fleet launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching.
sfrcdAllocationStrategy :: Lens' SpotFleetRequestConfigData (Maybe AllocationStrategy)
sfrcdAllocationStrategy = lens _sfrcdAllocationStrategy (\s a -> s {_sfrcdAllocationStrategy = a})

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that grants the Spot Fleet the permission to request, launch, terminate, and tag instances on your behalf. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-requests.html#spot-fleet-prerequisites Spot Fleet prerequisites> in the /Amazon EC2 User Guide for Linux Instances/ . Spot Fleet can terminate Spot Instances on your behalf when you cancel its Spot Fleet request using <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CancelSpotFleetRequests CancelSpotFleetRequests> or when the Spot Fleet request expires, if you set @TerminateInstancesWithExpiration@ .
sfrcdIAMFleetRole :: Lens' SpotFleetRequestConfigData Text
sfrcdIAMFleetRole = lens _sfrcdIAMFleetRole (\s a -> s {_sfrcdIAMFleetRole = a})

-- | The number of units to request for the Spot Fleet. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
sfrcdTargetCapacity :: Lens' SpotFleetRequestConfigData Int
sfrcdTargetCapacity = lens _sfrcdTargetCapacity (\s a -> s {_sfrcdTargetCapacity = a})

instance FromXML SpotFleetRequestConfigData where
  parseXML x =
    SpotFleetRequestConfigData'
      <$> (x .@? "clientToken")
      <*> (x .@? "instanceInterruptionBehavior")
      <*> (x .@? "onDemandMaxTotalPrice")
      <*> (x .@? "spotPrice")
      <*> (x .@? "spotMaintenanceStrategies")
      <*> (x .@? "loadBalancersConfig")
      <*> (x .@? "excessCapacityTerminationPolicy")
      <*> (x .@? "onDemandTargetCapacity")
      <*> ( x .@? "launchTemplateConfigs" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "TagSpecification" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "validUntil")
      <*> (x .@? "terminateInstancesWithExpiration")
      <*> (x .@? "onDemandAllocationStrategy")
      <*> (x .@? "instancePoolsToUseCount")
      <*> (x .@? "fulfilledCapacity")
      <*> (x .@? "type")
      <*> (x .@? "validFrom")
      <*> (x .@? "replaceUnhealthyInstances")
      <*> ( x .@? "launchSpecifications" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "onDemandFulfilledCapacity")
      <*> (x .@? "spotMaxTotalPrice")
      <*> (x .@? "allocationStrategy")
      <*> (x .@ "iamFleetRole")
      <*> (x .@ "targetCapacity")

instance Hashable SpotFleetRequestConfigData

instance NFData SpotFleetRequestConfigData

instance ToQuery SpotFleetRequestConfigData where
  toQuery SpotFleetRequestConfigData' {..} =
    mconcat
      [ "ClientToken" =: _sfrcdClientToken,
        "InstanceInterruptionBehavior"
          =: _sfrcdInstanceInterruptionBehavior,
        "OnDemandMaxTotalPrice" =: _sfrcdOnDemandMaxTotalPrice,
        "SpotPrice" =: _sfrcdSpotPrice,
        "SpotMaintenanceStrategies" =: _sfrcdSpotMaintenanceStrategies,
        "LoadBalancersConfig" =: _sfrcdLoadBalancersConfig,
        "ExcessCapacityTerminationPolicy"
          =: _sfrcdExcessCapacityTerminationPolicy,
        "OnDemandTargetCapacity" =: _sfrcdOnDemandTargetCapacity,
        toQuery
          ( toQueryList "LaunchTemplateConfigs"
              <$> _sfrcdLaunchTemplateConfigs
          ),
        toQuery
          (toQueryList "TagSpecification" <$> _sfrcdTagSpecifications),
        "ValidUntil" =: _sfrcdValidUntil,
        "TerminateInstancesWithExpiration"
          =: _sfrcdTerminateInstancesWithExpiration,
        "OnDemandAllocationStrategy" =: _sfrcdOnDemandAllocationStrategy,
        "InstancePoolsToUseCount" =: _sfrcdInstancePoolsToUseCount,
        "FulfilledCapacity" =: _sfrcdFulfilledCapacity,
        "Type" =: _sfrcdType,
        "ValidFrom" =: _sfrcdValidFrom,
        "ReplaceUnhealthyInstances" =: _sfrcdReplaceUnhealthyInstances,
        toQuery
          ( toQueryList "LaunchSpecifications"
              <$> _sfrcdLaunchSpecifications
          ),
        "OnDemandFulfilledCapacity" =: _sfrcdOnDemandFulfilledCapacity,
        "SpotMaxTotalPrice" =: _sfrcdSpotMaxTotalPrice,
        "AllocationStrategy" =: _sfrcdAllocationStrategy,
        "IamFleetRole" =: _sfrcdIAMFleetRole,
        "TargetCapacity" =: _sfrcdTargetCapacity
      ]
