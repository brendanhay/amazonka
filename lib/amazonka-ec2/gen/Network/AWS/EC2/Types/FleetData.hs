{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetData where

import Network.AWS.EC2.Internal
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
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an EC2 Fleet.
--
--
--
-- /See:/ 'fleetData' smart constructor.
data FleetData = FleetData'
  { _fdClientToken :: !(Maybe Text),
    _fdTargetCapacitySpecification ::
      !(Maybe TargetCapacitySpecification),
    _fdSpotOptions :: !(Maybe SpotOptions),
    _fdExcessCapacityTerminationPolicy ::
      !(Maybe FleetExcessCapacityTerminationPolicy),
    _fdOnDemandOptions :: !(Maybe OnDemandOptions),
    _fdFleetState :: !(Maybe FleetStateCode),
    _fdLaunchTemplateConfigs :: !(Maybe [FleetLaunchTemplateConfig]),
    _fdValidUntil :: !(Maybe ISO8601),
    _fdTerminateInstancesWithExpiration :: !(Maybe Bool),
    _fdInstances :: !(Maybe [DescribeFleetsInstances]),
    _fdFulfilledCapacity :: !(Maybe Double),
    _fdType :: !(Maybe FleetType),
    _fdValidFrom :: !(Maybe ISO8601),
    _fdReplaceUnhealthyInstances :: !(Maybe Bool),
    _fdFulfilledOnDemandCapacity :: !(Maybe Double),
    _fdFleetId :: !(Maybe Text),
    _fdErrors :: !(Maybe [DescribeFleetError]),
    _fdCreateTime :: !(Maybe ISO8601),
    _fdTags :: !(Maybe [Tag]),
    _fdActivityStatus :: !(Maybe FleetActivityStatus)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FleetData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> . Constraints: Maximum 64 ASCII characters
--
-- * 'fdTargetCapacitySpecification' - The number of units to request. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
--
-- * 'fdSpotOptions' - The configuration of Spot Instances in an EC2 Fleet.
--
-- * 'fdExcessCapacityTerminationPolicy' - Indicates whether running instances should be terminated if the target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
--
-- * 'fdOnDemandOptions' - The allocation strategy of On-Demand Instances in an EC2 Fleet.
--
-- * 'fdFleetState' - The state of the EC2 Fleet.
--
-- * 'fdLaunchTemplateConfigs' - The launch template and overrides.
--
-- * 'fdValidUntil' - The end date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). At this point, no new instance requests are placed or able to fulfill the request. The default end date is 7 days from the current date.
--
-- * 'fdTerminateInstancesWithExpiration' - Indicates whether running instances should be terminated when the EC2 Fleet expires.
--
-- * 'fdInstances' - Information about the instances that were launched by the fleet. Valid only when __Type__ is set to @instant@ .
--
-- * 'fdFulfilledCapacity' - The number of units fulfilled by this request compared to the set target capacity.
--
-- * 'fdType' - The type of request. Indicates whether the EC2 Fleet only @requests@ the target capacity, or also attempts to @maintain@ it. If you request a certain target capacity, EC2 Fleet only places the required requests; it does not attempt to replenish instances if capacity is diminished, and it does not submit requests in alternative capacity pools if capacity is unavailable. To maintain a certain target capacity, EC2 Fleet places the required requests to meet this target capacity. It also automatically replenishes any interrupted Spot Instances. Default: @maintain@ .
--
-- * 'fdValidFrom' - The start date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The default is to start fulfilling the request immediately.
--
-- * 'fdReplaceUnhealthyInstances' - Indicates whether EC2 Fleet should replace unhealthy instances.
--
-- * 'fdFulfilledOnDemandCapacity' - The number of units fulfilled by this request compared to the set target On-Demand capacity.
--
-- * 'fdFleetId' - The ID of the EC2 Fleet.
--
-- * 'fdErrors' - Information about the instances that could not be launched by the fleet. Valid only when __Type__ is set to @instant@ .
--
-- * 'fdCreateTime' - The creation date and time of the EC2 Fleet.
--
-- * 'fdTags' - The tags for an EC2 Fleet resource.
--
-- * 'fdActivityStatus' - The progress of the EC2 Fleet. If there is an error, the status is @error@ . After all requests are placed, the status is @pending_fulfillment@ . If the size of the EC2 Fleet is equal to or greater than its target capacity, the status is @fulfilled@ . If the size of the EC2 Fleet is decreased, the status is @pending_termination@ while instances are terminating.
fleetData ::
  FleetData
fleetData =
  FleetData'
    { _fdClientToken = Nothing,
      _fdTargetCapacitySpecification = Nothing,
      _fdSpotOptions = Nothing,
      _fdExcessCapacityTerminationPolicy = Nothing,
      _fdOnDemandOptions = Nothing,
      _fdFleetState = Nothing,
      _fdLaunchTemplateConfigs = Nothing,
      _fdValidUntil = Nothing,
      _fdTerminateInstancesWithExpiration = Nothing,
      _fdInstances = Nothing,
      _fdFulfilledCapacity = Nothing,
      _fdType = Nothing,
      _fdValidFrom = Nothing,
      _fdReplaceUnhealthyInstances = Nothing,
      _fdFulfilledOnDemandCapacity = Nothing,
      _fdFleetId = Nothing,
      _fdErrors = Nothing,
      _fdCreateTime = Nothing,
      _fdTags = Nothing,
      _fdActivityStatus = Nothing
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> . Constraints: Maximum 64 ASCII characters
fdClientToken :: Lens' FleetData (Maybe Text)
fdClientToken = lens _fdClientToken (\s a -> s {_fdClientToken = a})

-- | The number of units to request. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
fdTargetCapacitySpecification :: Lens' FleetData (Maybe TargetCapacitySpecification)
fdTargetCapacitySpecification = lens _fdTargetCapacitySpecification (\s a -> s {_fdTargetCapacitySpecification = a})

-- | The configuration of Spot Instances in an EC2 Fleet.
fdSpotOptions :: Lens' FleetData (Maybe SpotOptions)
fdSpotOptions = lens _fdSpotOptions (\s a -> s {_fdSpotOptions = a})

-- | Indicates whether running instances should be terminated if the target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
fdExcessCapacityTerminationPolicy :: Lens' FleetData (Maybe FleetExcessCapacityTerminationPolicy)
fdExcessCapacityTerminationPolicy = lens _fdExcessCapacityTerminationPolicy (\s a -> s {_fdExcessCapacityTerminationPolicy = a})

-- | The allocation strategy of On-Demand Instances in an EC2 Fleet.
fdOnDemandOptions :: Lens' FleetData (Maybe OnDemandOptions)
fdOnDemandOptions = lens _fdOnDemandOptions (\s a -> s {_fdOnDemandOptions = a})

-- | The state of the EC2 Fleet.
fdFleetState :: Lens' FleetData (Maybe FleetStateCode)
fdFleetState = lens _fdFleetState (\s a -> s {_fdFleetState = a})

-- | The launch template and overrides.
fdLaunchTemplateConfigs :: Lens' FleetData [FleetLaunchTemplateConfig]
fdLaunchTemplateConfigs = lens _fdLaunchTemplateConfigs (\s a -> s {_fdLaunchTemplateConfigs = a}) . _Default . _Coerce

-- | The end date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). At this point, no new instance requests are placed or able to fulfill the request. The default end date is 7 days from the current date.
fdValidUntil :: Lens' FleetData (Maybe UTCTime)
fdValidUntil = lens _fdValidUntil (\s a -> s {_fdValidUntil = a}) . mapping _Time

-- | Indicates whether running instances should be terminated when the EC2 Fleet expires.
fdTerminateInstancesWithExpiration :: Lens' FleetData (Maybe Bool)
fdTerminateInstancesWithExpiration = lens _fdTerminateInstancesWithExpiration (\s a -> s {_fdTerminateInstancesWithExpiration = a})

-- | Information about the instances that were launched by the fleet. Valid only when __Type__ is set to @instant@ .
fdInstances :: Lens' FleetData [DescribeFleetsInstances]
fdInstances = lens _fdInstances (\s a -> s {_fdInstances = a}) . _Default . _Coerce

-- | The number of units fulfilled by this request compared to the set target capacity.
fdFulfilledCapacity :: Lens' FleetData (Maybe Double)
fdFulfilledCapacity = lens _fdFulfilledCapacity (\s a -> s {_fdFulfilledCapacity = a})

-- | The type of request. Indicates whether the EC2 Fleet only @requests@ the target capacity, or also attempts to @maintain@ it. If you request a certain target capacity, EC2 Fleet only places the required requests; it does not attempt to replenish instances if capacity is diminished, and it does not submit requests in alternative capacity pools if capacity is unavailable. To maintain a certain target capacity, EC2 Fleet places the required requests to meet this target capacity. It also automatically replenishes any interrupted Spot Instances. Default: @maintain@ .
fdType :: Lens' FleetData (Maybe FleetType)
fdType = lens _fdType (\s a -> s {_fdType = a})

-- | The start date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The default is to start fulfilling the request immediately.
fdValidFrom :: Lens' FleetData (Maybe UTCTime)
fdValidFrom = lens _fdValidFrom (\s a -> s {_fdValidFrom = a}) . mapping _Time

-- | Indicates whether EC2 Fleet should replace unhealthy instances.
fdReplaceUnhealthyInstances :: Lens' FleetData (Maybe Bool)
fdReplaceUnhealthyInstances = lens _fdReplaceUnhealthyInstances (\s a -> s {_fdReplaceUnhealthyInstances = a})

-- | The number of units fulfilled by this request compared to the set target On-Demand capacity.
fdFulfilledOnDemandCapacity :: Lens' FleetData (Maybe Double)
fdFulfilledOnDemandCapacity = lens _fdFulfilledOnDemandCapacity (\s a -> s {_fdFulfilledOnDemandCapacity = a})

-- | The ID of the EC2 Fleet.
fdFleetId :: Lens' FleetData (Maybe Text)
fdFleetId = lens _fdFleetId (\s a -> s {_fdFleetId = a})

-- | Information about the instances that could not be launched by the fleet. Valid only when __Type__ is set to @instant@ .
fdErrors :: Lens' FleetData [DescribeFleetError]
fdErrors = lens _fdErrors (\s a -> s {_fdErrors = a}) . _Default . _Coerce

-- | The creation date and time of the EC2 Fleet.
fdCreateTime :: Lens' FleetData (Maybe UTCTime)
fdCreateTime = lens _fdCreateTime (\s a -> s {_fdCreateTime = a}) . mapping _Time

-- | The tags for an EC2 Fleet resource.
fdTags :: Lens' FleetData [Tag]
fdTags = lens _fdTags (\s a -> s {_fdTags = a}) . _Default . _Coerce

-- | The progress of the EC2 Fleet. If there is an error, the status is @error@ . After all requests are placed, the status is @pending_fulfillment@ . If the size of the EC2 Fleet is equal to or greater than its target capacity, the status is @fulfilled@ . If the size of the EC2 Fleet is decreased, the status is @pending_termination@ while instances are terminating.
fdActivityStatus :: Lens' FleetData (Maybe FleetActivityStatus)
fdActivityStatus = lens _fdActivityStatus (\s a -> s {_fdActivityStatus = a})

instance FromXML FleetData where
  parseXML x =
    FleetData'
      <$> (x .@? "clientToken")
      <*> (x .@? "targetCapacitySpecification")
      <*> (x .@? "spotOptions")
      <*> (x .@? "excessCapacityTerminationPolicy")
      <*> (x .@? "onDemandOptions")
      <*> (x .@? "fleetState")
      <*> ( x .@? "launchTemplateConfigs" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "validUntil")
      <*> (x .@? "terminateInstancesWithExpiration")
      <*> (x .@? "fleetInstanceSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "fulfilledCapacity")
      <*> (x .@? "type")
      <*> (x .@? "validFrom")
      <*> (x .@? "replaceUnhealthyInstances")
      <*> (x .@? "fulfilledOnDemandCapacity")
      <*> (x .@? "fleetId")
      <*> (x .@? "errorSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "createTime")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "activityStatus")

instance Hashable FleetData

instance NFData FleetData
