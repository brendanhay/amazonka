{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateFleet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches an EC2 Fleet.
--
--
-- You can create a single EC2 Fleet that includes multiple launch specifications that vary by instance type, AMI, Availability Zone, or subnet.
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet.html Launching an EC2 Fleet> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.CreateFleet
    (
    -- * Creating a Request
      createFleet
    , CreateFleet
    -- * Request Lenses
    , cfClientToken
    , cfSpotOptions
    , cfExcessCapacityTerminationPolicy
    , cfTagSpecifications
    , cfValidUntil
    , cfTerminateInstancesWithExpiration
    , cfType
    , cfValidFrom
    , cfReplaceUnhealthyInstances
    , cfDryRun
    , cfLaunchTemplateConfigs
    , cfTargetCapacitySpecification

    -- * Destructuring the Response
    , createFleetResponse
    , CreateFleetResponse
    -- * Response Lenses
    , cfrsFleetId
    , cfrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createFleet' smart constructor.
data CreateFleet = CreateFleet'
  { _cfClientToken :: !(Maybe Text)
  , _cfSpotOptions :: !(Maybe SpotOptionsRequest)
  , _cfExcessCapacityTerminationPolicy :: !(Maybe FleetExcessCapacityTerminationPolicy)
  , _cfTagSpecifications :: !(Maybe [TagSpecification])
  , _cfValidUntil :: !(Maybe ISO8601)
  , _cfTerminateInstancesWithExpiration :: !(Maybe Bool)
  , _cfType :: !(Maybe FleetType)
  , _cfValidFrom :: !(Maybe ISO8601)
  , _cfReplaceUnhealthyInstances :: !(Maybe Bool)
  , _cfDryRun :: !(Maybe Bool)
  , _cfLaunchTemplateConfigs :: ![FleetLaunchTemplateConfigRequest]
  , _cfTargetCapacitySpecification :: !TargetCapacitySpecificationRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateFleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfClientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- * 'cfSpotOptions' - Includes @SpotAllocationStrategy@ and @SpotInstanceInterruptionBehavior@ inside this structure.
--
-- * 'cfExcessCapacityTerminationPolicy' - Indicates whether running instances should be terminated if the total target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
--
-- * 'cfTagSpecifications' - The tags for an EC2 Fleet resource.
--
-- * 'cfValidUntil' - The end date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). At this point, no new EC2 Fleet requests are placed or able to fulfill the request. The default end date is 7 days from the current date.
--
-- * 'cfTerminateInstancesWithExpiration' - Indicates whether running instances should be terminated when the EC2 Fleet expires.
--
-- * 'cfType' - The type of request. Indicates whether the EC2 Fleet only @requests@ the target capacity, or also attempts to @maintain@ it. If you request a certain target capacity, EC2 Fleet only places the required requests. It does not attempt to replenish instances if capacity is diminished, and does not submit requests in alternative capacity pools if capacity is unavailable. To maintain a certain target capacity, EC2 Fleet places the required requests to meet this target capacity. It also automatically replenishes any interrupted Spot Instances. Default: @maintain@ .
--
-- * 'cfValidFrom' - The start date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The default is to start fulfilling the request immediately.
--
-- * 'cfReplaceUnhealthyInstances' - Indicates whether EC2 Fleet should replace unhealthy instances.
--
-- * 'cfDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cfLaunchTemplateConfigs' - The configuration for the EC2 Fleet.
--
-- * 'cfTargetCapacitySpecification' - The @TotalTargetCapacity@ , @OnDemandTargetCapacity@ , @SpotTargetCapacity@ , and @DefaultCapacityType@ structure.
createFleet
    :: TargetCapacitySpecificationRequest -- ^ 'cfTargetCapacitySpecification'
    -> CreateFleet
createFleet pTargetCapacitySpecification_ =
  CreateFleet'
    { _cfClientToken = Nothing
    , _cfSpotOptions = Nothing
    , _cfExcessCapacityTerminationPolicy = Nothing
    , _cfTagSpecifications = Nothing
    , _cfValidUntil = Nothing
    , _cfTerminateInstancesWithExpiration = Nothing
    , _cfType = Nothing
    , _cfValidFrom = Nothing
    , _cfReplaceUnhealthyInstances = Nothing
    , _cfDryRun = Nothing
    , _cfLaunchTemplateConfigs = mempty
    , _cfTargetCapacitySpecification = pTargetCapacitySpecification_
    }


-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
cfClientToken :: Lens' CreateFleet (Maybe Text)
cfClientToken = lens _cfClientToken (\ s a -> s{_cfClientToken = a})

-- | Includes @SpotAllocationStrategy@ and @SpotInstanceInterruptionBehavior@ inside this structure.
cfSpotOptions :: Lens' CreateFleet (Maybe SpotOptionsRequest)
cfSpotOptions = lens _cfSpotOptions (\ s a -> s{_cfSpotOptions = a})

-- | Indicates whether running instances should be terminated if the total target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
cfExcessCapacityTerminationPolicy :: Lens' CreateFleet (Maybe FleetExcessCapacityTerminationPolicy)
cfExcessCapacityTerminationPolicy = lens _cfExcessCapacityTerminationPolicy (\ s a -> s{_cfExcessCapacityTerminationPolicy = a})

-- | The tags for an EC2 Fleet resource.
cfTagSpecifications :: Lens' CreateFleet [TagSpecification]
cfTagSpecifications = lens _cfTagSpecifications (\ s a -> s{_cfTagSpecifications = a}) . _Default . _Coerce

-- | The end date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). At this point, no new EC2 Fleet requests are placed or able to fulfill the request. The default end date is 7 days from the current date.
cfValidUntil :: Lens' CreateFleet (Maybe UTCTime)
cfValidUntil = lens _cfValidUntil (\ s a -> s{_cfValidUntil = a}) . mapping _Time

-- | Indicates whether running instances should be terminated when the EC2 Fleet expires.
cfTerminateInstancesWithExpiration :: Lens' CreateFleet (Maybe Bool)
cfTerminateInstancesWithExpiration = lens _cfTerminateInstancesWithExpiration (\ s a -> s{_cfTerminateInstancesWithExpiration = a})

-- | The type of request. Indicates whether the EC2 Fleet only @requests@ the target capacity, or also attempts to @maintain@ it. If you request a certain target capacity, EC2 Fleet only places the required requests. It does not attempt to replenish instances if capacity is diminished, and does not submit requests in alternative capacity pools if capacity is unavailable. To maintain a certain target capacity, EC2 Fleet places the required requests to meet this target capacity. It also automatically replenishes any interrupted Spot Instances. Default: @maintain@ .
cfType :: Lens' CreateFleet (Maybe FleetType)
cfType = lens _cfType (\ s a -> s{_cfType = a})

-- | The start date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). The default is to start fulfilling the request immediately.
cfValidFrom :: Lens' CreateFleet (Maybe UTCTime)
cfValidFrom = lens _cfValidFrom (\ s a -> s{_cfValidFrom = a}) . mapping _Time

-- | Indicates whether EC2 Fleet should replace unhealthy instances.
cfReplaceUnhealthyInstances :: Lens' CreateFleet (Maybe Bool)
cfReplaceUnhealthyInstances = lens _cfReplaceUnhealthyInstances (\ s a -> s{_cfReplaceUnhealthyInstances = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cfDryRun :: Lens' CreateFleet (Maybe Bool)
cfDryRun = lens _cfDryRun (\ s a -> s{_cfDryRun = a})

-- | The configuration for the EC2 Fleet.
cfLaunchTemplateConfigs :: Lens' CreateFleet [FleetLaunchTemplateConfigRequest]
cfLaunchTemplateConfigs = lens _cfLaunchTemplateConfigs (\ s a -> s{_cfLaunchTemplateConfigs = a}) . _Coerce

-- | The @TotalTargetCapacity@ , @OnDemandTargetCapacity@ , @SpotTargetCapacity@ , and @DefaultCapacityType@ structure.
cfTargetCapacitySpecification :: Lens' CreateFleet TargetCapacitySpecificationRequest
cfTargetCapacitySpecification = lens _cfTargetCapacitySpecification (\ s a -> s{_cfTargetCapacitySpecification = a})

instance AWSRequest CreateFleet where
        type Rs CreateFleet = CreateFleetResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateFleetResponse' <$>
                   (x .@? "fleetId") <*> (pure (fromEnum s)))

instance Hashable CreateFleet where

instance NFData CreateFleet where

instance ToHeaders CreateFleet where
        toHeaders = const mempty

instance ToPath CreateFleet where
        toPath = const "/"

instance ToQuery CreateFleet where
        toQuery CreateFleet'{..}
          = mconcat
              ["Action" =: ("CreateFleet" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "ClientToken" =: _cfClientToken,
               "SpotOptions" =: _cfSpotOptions,
               "ExcessCapacityTerminationPolicy" =:
                 _cfExcessCapacityTerminationPolicy,
               toQuery
                 (toQueryList "TagSpecification" <$>
                    _cfTagSpecifications),
               "ValidUntil" =: _cfValidUntil,
               "TerminateInstancesWithExpiration" =:
                 _cfTerminateInstancesWithExpiration,
               "Type" =: _cfType, "ValidFrom" =: _cfValidFrom,
               "ReplaceUnhealthyInstances" =:
                 _cfReplaceUnhealthyInstances,
               "DryRun" =: _cfDryRun,
               toQueryList "LaunchTemplateConfigs"
                 _cfLaunchTemplateConfigs,
               "TargetCapacitySpecification" =:
                 _cfTargetCapacitySpecification]

-- | /See:/ 'createFleetResponse' smart constructor.
data CreateFleetResponse = CreateFleetResponse'
  { _cfrsFleetId        :: !(Maybe Text)
  , _cfrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateFleetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfrsFleetId' - The ID of the EC2 Fleet.
--
-- * 'cfrsResponseStatus' - -- | The response status code.
createFleetResponse
    :: Int -- ^ 'cfrsResponseStatus'
    -> CreateFleetResponse
createFleetResponse pResponseStatus_ =
  CreateFleetResponse'
    {_cfrsFleetId = Nothing, _cfrsResponseStatus = pResponseStatus_}


-- | The ID of the EC2 Fleet.
cfrsFleetId :: Lens' CreateFleetResponse (Maybe Text)
cfrsFleetId = lens _cfrsFleetId (\ s a -> s{_cfrsFleetId = a})

-- | -- | The response status code.
cfrsResponseStatus :: Lens' CreateFleetResponse Int
cfrsResponseStatus = lens _cfrsResponseStatus (\ s a -> s{_cfrsResponseStatus = a})

instance NFData CreateFleetResponse where
