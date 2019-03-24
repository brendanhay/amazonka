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
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet.html Launching an EC2 Fleet> in the /Amazon Elastic Compute Cloud User Guide/ .
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
    , cfOnDemandOptions
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
    , cfrsInstances
    , cfrsFleetId
    , cfrsErrors
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
  , _cfOnDemandOptions :: !(Maybe OnDemandOptionsRequest)
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
-- * 'cfClientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- * 'cfSpotOptions' - Describes the configuration of Spot Instances in an EC2 Fleet.
--
-- * 'cfExcessCapacityTerminationPolicy' - Indicates whether running instances should be terminated if the total target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
--
-- * 'cfOnDemandOptions' - The allocation strategy of On-Demand Instances in an EC2 Fleet.
--
-- * 'cfTagSpecifications' - The key-value pair for tagging the EC2 Fleet request on creation. The value for @ResourceType@ must be @fleet@ , otherwise the fleet request fails. To tag instances at launch, specify the tags in the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#create-launch-template launch template> . For information about tagging after launch, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-resources Tagging Your Resources> .
--
-- * 'cfValidUntil' - The end date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). At this point, no new EC2 Fleet requests are placed or able to fulfill the request. If no value is specified, the request remains until you cancel it.
--
-- * 'cfTerminateInstancesWithExpiration' - Indicates whether running instances should be terminated when the EC2 Fleet expires.
--
-- * 'cfType' - The type of the request. By default, the EC2 Fleet places an asynchronous request for your desired capacity, and maintains it by replenishing interrupted Spot Instances (@maintain@ ). A value of @instant@ places a synchronous one-time request, and returns errors for any instances that could not be launched. A value of @request@ places an asynchronous one-time request without maintaining capacity or submitting requests in alternative capacity pools if capacity is unavailable. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-configuration-strategies.html#ec2-fleet-request-type EC2 Fleet Request Types> in the /Amazon Elastic Compute Cloud User Guide/ .
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
    , _cfOnDemandOptions = Nothing
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


-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
cfClientToken :: Lens' CreateFleet (Maybe Text)
cfClientToken = lens _cfClientToken (\ s a -> s{_cfClientToken = a})

-- | Describes the configuration of Spot Instances in an EC2 Fleet.
cfSpotOptions :: Lens' CreateFleet (Maybe SpotOptionsRequest)
cfSpotOptions = lens _cfSpotOptions (\ s a -> s{_cfSpotOptions = a})

-- | Indicates whether running instances should be terminated if the total target capacity of the EC2 Fleet is decreased below the current size of the EC2 Fleet.
cfExcessCapacityTerminationPolicy :: Lens' CreateFleet (Maybe FleetExcessCapacityTerminationPolicy)
cfExcessCapacityTerminationPolicy = lens _cfExcessCapacityTerminationPolicy (\ s a -> s{_cfExcessCapacityTerminationPolicy = a})

-- | The allocation strategy of On-Demand Instances in an EC2 Fleet.
cfOnDemandOptions :: Lens' CreateFleet (Maybe OnDemandOptionsRequest)
cfOnDemandOptions = lens _cfOnDemandOptions (\ s a -> s{_cfOnDemandOptions = a})

-- | The key-value pair for tagging the EC2 Fleet request on creation. The value for @ResourceType@ must be @fleet@ , otherwise the fleet request fails. To tag instances at launch, specify the tags in the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#create-launch-template launch template> . For information about tagging after launch, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-resources Tagging Your Resources> .
cfTagSpecifications :: Lens' CreateFleet [TagSpecification]
cfTagSpecifications = lens _cfTagSpecifications (\ s a -> s{_cfTagSpecifications = a}) . _Default . _Coerce

-- | The end date and time of the request, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). At this point, no new EC2 Fleet requests are placed or able to fulfill the request. If no value is specified, the request remains until you cancel it.
cfValidUntil :: Lens' CreateFleet (Maybe UTCTime)
cfValidUntil = lens _cfValidUntil (\ s a -> s{_cfValidUntil = a}) . mapping _Time

-- | Indicates whether running instances should be terminated when the EC2 Fleet expires.
cfTerminateInstancesWithExpiration :: Lens' CreateFleet (Maybe Bool)
cfTerminateInstancesWithExpiration = lens _cfTerminateInstancesWithExpiration (\ s a -> s{_cfTerminateInstancesWithExpiration = a})

-- | The type of the request. By default, the EC2 Fleet places an asynchronous request for your desired capacity, and maintains it by replenishing interrupted Spot Instances (@maintain@ ). A value of @instant@ places a synchronous one-time request, and returns errors for any instances that could not be launched. A value of @request@ places an asynchronous one-time request without maintaining capacity or submitting requests in alternative capacity pools if capacity is unavailable. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-configuration-strategies.html#ec2-fleet-request-type EC2 Fleet Request Types> in the /Amazon Elastic Compute Cloud User Guide/ .
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
                   (x .@? "fleetInstanceSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "fleetId")
                     <*>
                     (x .@? "errorSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

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
               "OnDemandOptions" =: _cfOnDemandOptions,
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
  { _cfrsInstances      :: !(Maybe [CreateFleetInstance])
  , _cfrsFleetId        :: !(Maybe Text)
  , _cfrsErrors         :: !(Maybe [CreateFleetError])
  , _cfrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateFleetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfrsInstances' - Information about the instances that were launched by the fleet. Valid only when __Type__ is set to @instant@ .
--
-- * 'cfrsFleetId' - The ID of the EC2 Fleet.
--
-- * 'cfrsErrors' - Information about the instances that could not be launched by the fleet. Valid only when __Type__ is set to @instant@ .
--
-- * 'cfrsResponseStatus' - -- | The response status code.
createFleetResponse
    :: Int -- ^ 'cfrsResponseStatus'
    -> CreateFleetResponse
createFleetResponse pResponseStatus_ =
  CreateFleetResponse'
    { _cfrsInstances = Nothing
    , _cfrsFleetId = Nothing
    , _cfrsErrors = Nothing
    , _cfrsResponseStatus = pResponseStatus_
    }


-- | Information about the instances that were launched by the fleet. Valid only when __Type__ is set to @instant@ .
cfrsInstances :: Lens' CreateFleetResponse [CreateFleetInstance]
cfrsInstances = lens _cfrsInstances (\ s a -> s{_cfrsInstances = a}) . _Default . _Coerce

-- | The ID of the EC2 Fleet.
cfrsFleetId :: Lens' CreateFleetResponse (Maybe Text)
cfrsFleetId = lens _cfrsFleetId (\ s a -> s{_cfrsFleetId = a})

-- | Information about the instances that could not be launched by the fleet. Valid only when __Type__ is set to @instant@ .
cfrsErrors :: Lens' CreateFleetResponse [CreateFleetError]
cfrsErrors = lens _cfrsErrors (\ s a -> s{_cfrsErrors = a}) . _Default . _Coerce

-- | -- | The response status code.
cfrsResponseStatus :: Lens' CreateFleetResponse Int
cfrsResponseStatus = lens _cfrsResponseStatus (\ s a -> s{_cfrsResponseStatus = a})

instance NFData CreateFleetResponse where
