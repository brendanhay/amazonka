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
-- Module      : Network.AWS.GameLift.CreateFleet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new fleet to run your game servers. A fleet is a set of Amazon Elastic Compute Cloud (Amazon EC2) instances, each of which can run multiple server processes to host game sessions. You set up a fleet to use instances with certain hardware specifications (see <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for more information), and deploy your game build to run on each instance.
--
--
-- To create a new fleet, you must specify the following: (1) a fleet name, (2) the build ID of a successfully uploaded game build, (3) an EC2 instance type, and (4) a run-time configuration, which describes the server processes to run on each instance in the fleet. If you don't specify a fleet type (on-demand or spot), the new fleet uses on-demand instances by default.
--
-- You can also configure the new fleet with the following settings:
--
--     * Fleet description
--
--     * Access permissions for inbound traffic
--
--     * Fleet-wide game session protection
--
--     * Resource usage limits
--
--
--
--     * VPC peering connection (see <http://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> )
--
--
--
-- If you use Amazon CloudWatch for metrics, you can add the new fleet to a metric group. By adding multiple fleets to a metric group, you can view aggregated metrics for all the fleets in the group.
--
-- If the @CreateFleet@ call is successful, Amazon GameLift performs the following tasks. You can track the process of a fleet by checking the fleet status or by monitoring fleet creation events:
--
--     * Creates a fleet record. Status: @NEW@ .
--
--     * Begins writing events to the fleet event log, which can be accessed in the Amazon GameLift console.
--
-- Sets the fleet's target capacity to 1 (desired instances), which triggers Amazon GameLift to start one new EC2 instance.
--
--     * Downloads the game build to the new instance and installs it. Statuses: @DOWNLOADING@ , @VALIDATING@ , @BUILDING@ .
--
--     * Starts launching server processes on the instance. If the fleet is configured to run multiple server processes per instance, Amazon GameLift staggers each launch by a few seconds. Status: @ACTIVATING@ .
--
--     * Sets the fleet's status to @ACTIVE@ as soon as one server process is ready to host a game session.
--
--
--
-- Fleet-related operations include:
--
--     * 'CreateFleet'
--
--     * 'ListFleets'
--
--     * 'DeleteFleet'
--
--     * Describe fleets:
--
--     * 'DescribeFleetAttributes'
--
--     * 'DescribeFleetCapacity'
--
--     * 'DescribeFleetPortSettings'
--
--     * 'DescribeFleetUtilization'
--
--     * 'DescribeRuntimeConfiguration'
--
--     * 'DescribeEC2InstanceLimits'
--
--     * 'DescribeFleetEvents'
--
--
--
--     * Update fleets:
--
--     * 'UpdateFleetAttributes'
--
--     * 'UpdateFleetCapacity'
--
--     * 'UpdateFleetPortSettings'
--
--     * 'UpdateRuntimeConfiguration'
--
--
--
--     * Manage fleet actions:
--
--     * 'StartFleetActions'
--
--     * 'StopFleetActions'
--
--
--
--
--
module Network.AWS.GameLift.CreateFleet
    (
    -- * Creating a Request
      createFleet
    , CreateFleet
    -- * Request Lenses
    , cfServerLaunchParameters
    , cfLogPaths
    , cfPeerVPCId
    , cfFleetType
    , cfPeerVPCAWSAccountId
    , cfEC2InboundPermissions
    , cfRuntimeConfiguration
    , cfNewGameSessionProtectionPolicy
    , cfServerLaunchPath
    , cfMetricGroups
    , cfDescription
    , cfResourceCreationLimitPolicy
    , cfName
    , cfBuildId
    , cfEC2InstanceType

    -- * Destructuring the Response
    , createFleetResponse
    , CreateFleetResponse
    -- * Response Lenses
    , cfrsFleetAttributes
    , cfrsResponseStatus
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request action.
--
--
--
-- /See:/ 'createFleet' smart constructor.
data CreateFleet = CreateFleet'
  { _cfServerLaunchParameters         :: !(Maybe Text)
  , _cfLogPaths                       :: !(Maybe [Text])
  , _cfPeerVPCId                      :: !(Maybe Text)
  , _cfFleetType                      :: !(Maybe FleetType)
  , _cfPeerVPCAWSAccountId            :: !(Maybe Text)
  , _cfEC2InboundPermissions          :: !(Maybe [IPPermission])
  , _cfRuntimeConfiguration           :: !(Maybe RuntimeConfiguration)
  , _cfNewGameSessionProtectionPolicy :: !(Maybe ProtectionPolicy)
  , _cfServerLaunchPath               :: !(Maybe Text)
  , _cfMetricGroups                   :: !(Maybe [Text])
  , _cfDescription                    :: !(Maybe Text)
  , _cfResourceCreationLimitPolicy    :: !(Maybe ResourceCreationLimitPolicy)
  , _cfName                           :: !Text
  , _cfBuildId                        :: !Text
  , _cfEC2InstanceType                :: !EC2InstanceType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateFleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfServerLaunchParameters' - This parameter is no longer used. Instead, specify server launch parameters in the @RuntimeConfiguration@ parameter. (Requests that specify a server launch path and launch parameters instead of a run-time configuration will continue to work.)
--
-- * 'cfLogPaths' - This parameter is no longer used. Instead, to specify where Amazon GameLift should store log files once a server process shuts down, use the Amazon GameLift server API @ProcessReady()@ and specify one or more directory paths in @logParameters@ . See more information in the <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api-ref.html#gamelift-sdk-server-api-ref-dataypes-process Server API Reference> .
--
-- * 'cfPeerVPCId' - Unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same region where your fleet is deployed. To get VPC information, including IDs, use the Virtual Private Cloud service tools, including the VPC Dashboard in the AWS Management Console.
--
-- * 'cfFleetType' - Indicates whether to use on-demand instances or spot instances for this fleet. If empty, the default is ON_DEMAND. Both categories of instances use identical hardware and configurations, based on the instance type selected for this fleet. You can acquire on-demand instances at any time for a fixed price and keep them as long as you need them. Spot instances have lower prices, but spot pricing is variable, and while in use they can be interrupted (with a two-minute notification). Learn more about Amazon GameLift spot instances with at <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-ec2-instances.html Choose Computing Resources> .
--
-- * 'cfPeerVPCAWSAccountId' - Unique identifier for the AWS account with the VPC that you want to peer your Amazon GameLift fleet with. You can find your Account ID in the AWS Management Console under account settings.
--
-- * 'cfEC2InboundPermissions' - Range of IP addresses and port settings that permit inbound traffic to access server processes running on the fleet. If no inbound permissions are set, including both IP address range and port range, the server processes in the fleet cannot accept connections. You can specify one or more sets of permissions for a fleet.
--
-- * 'cfRuntimeConfiguration' - Instructions for launching server processes on each instance in the fleet. The run-time configuration for a fleet has a collection of server process configurations, one for each type of server process to run on an instance. A server process configuration specifies the location of the server executable, launch parameters, and the number of concurrent processes with that configuration to maintain on each instance. A CreateFleet request must include a run-time configuration with at least one server process configuration; otherwise the request fails with an invalid request exception. (This parameter replaces the parameters @ServerLaunchPath@ and @ServerLaunchParameters@ ; requests that contain values for these parameters instead of a run-time configuration will continue to work.)
--
-- * 'cfNewGameSessionProtectionPolicy' - Game session protection policy to apply to all instances in this fleet. If this parameter is not set, instances in this fleet default to no protection. You can change a fleet's protection policy using 'UpdateFleetAttributes' , but this change will only affect sessions created after the policy change. You can also set protection for individual instances using 'UpdateGameSession' .     * __NoProtection__ -- The game session can be terminated during a scale-down event.     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
--
-- * 'cfServerLaunchPath' - This parameter is no longer used. Instead, specify a server launch path using the @RuntimeConfiguration@ parameter. (Requests that specify a server launch path and launch parameters instead of a run-time configuration will continue to work.)
--
-- * 'cfMetricGroups' - Name of a metric group to add this fleet to. A metric group tracks metrics across all fleets in the group. Use an existing metric group name to add this fleet to the group, or use a new name to create a new metric group. A fleet can only be included in one metric group at a time.
--
-- * 'cfDescription' - Human-readable description of a fleet.
--
-- * 'cfResourceCreationLimitPolicy' - Policy that limits the number of game sessions an individual player can create over a span of time for this fleet.
--
-- * 'cfName' - Descriptive label that is associated with a fleet. Fleet names do not need to be unique.
--
-- * 'cfBuildId' - Unique identifier for a build to be deployed on the new fleet. The build must have been successfully uploaded to Amazon GameLift and be in a @READY@ status. This fleet setting cannot be changed once the fleet is created.
--
-- * 'cfEC2InstanceType' - Name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
createFleet
    :: Text -- ^ 'cfName'
    -> Text -- ^ 'cfBuildId'
    -> EC2InstanceType -- ^ 'cfEC2InstanceType'
    -> CreateFleet
createFleet pName_ pBuildId_ pEC2InstanceType_ =
  CreateFleet'
    { _cfServerLaunchParameters = Nothing
    , _cfLogPaths = Nothing
    , _cfPeerVPCId = Nothing
    , _cfFleetType = Nothing
    , _cfPeerVPCAWSAccountId = Nothing
    , _cfEC2InboundPermissions = Nothing
    , _cfRuntimeConfiguration = Nothing
    , _cfNewGameSessionProtectionPolicy = Nothing
    , _cfServerLaunchPath = Nothing
    , _cfMetricGroups = Nothing
    , _cfDescription = Nothing
    , _cfResourceCreationLimitPolicy = Nothing
    , _cfName = pName_
    , _cfBuildId = pBuildId_
    , _cfEC2InstanceType = pEC2InstanceType_
    }


-- | This parameter is no longer used. Instead, specify server launch parameters in the @RuntimeConfiguration@ parameter. (Requests that specify a server launch path and launch parameters instead of a run-time configuration will continue to work.)
cfServerLaunchParameters :: Lens' CreateFleet (Maybe Text)
cfServerLaunchParameters = lens _cfServerLaunchParameters (\ s a -> s{_cfServerLaunchParameters = a})

-- | This parameter is no longer used. Instead, to specify where Amazon GameLift should store log files once a server process shuts down, use the Amazon GameLift server API @ProcessReady()@ and specify one or more directory paths in @logParameters@ . See more information in the <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api-ref.html#gamelift-sdk-server-api-ref-dataypes-process Server API Reference> .
cfLogPaths :: Lens' CreateFleet [Text]
cfLogPaths = lens _cfLogPaths (\ s a -> s{_cfLogPaths = a}) . _Default . _Coerce

-- | Unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same region where your fleet is deployed. To get VPC information, including IDs, use the Virtual Private Cloud service tools, including the VPC Dashboard in the AWS Management Console.
cfPeerVPCId :: Lens' CreateFleet (Maybe Text)
cfPeerVPCId = lens _cfPeerVPCId (\ s a -> s{_cfPeerVPCId = a})

-- | Indicates whether to use on-demand instances or spot instances for this fleet. If empty, the default is ON_DEMAND. Both categories of instances use identical hardware and configurations, based on the instance type selected for this fleet. You can acquire on-demand instances at any time for a fixed price and keep them as long as you need them. Spot instances have lower prices, but spot pricing is variable, and while in use they can be interrupted (with a two-minute notification). Learn more about Amazon GameLift spot instances with at <http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-ec2-instances.html Choose Computing Resources> .
cfFleetType :: Lens' CreateFleet (Maybe FleetType)
cfFleetType = lens _cfFleetType (\ s a -> s{_cfFleetType = a})

-- | Unique identifier for the AWS account with the VPC that you want to peer your Amazon GameLift fleet with. You can find your Account ID in the AWS Management Console under account settings.
cfPeerVPCAWSAccountId :: Lens' CreateFleet (Maybe Text)
cfPeerVPCAWSAccountId = lens _cfPeerVPCAWSAccountId (\ s a -> s{_cfPeerVPCAWSAccountId = a})

-- | Range of IP addresses and port settings that permit inbound traffic to access server processes running on the fleet. If no inbound permissions are set, including both IP address range and port range, the server processes in the fleet cannot accept connections. You can specify one or more sets of permissions for a fleet.
cfEC2InboundPermissions :: Lens' CreateFleet [IPPermission]
cfEC2InboundPermissions = lens _cfEC2InboundPermissions (\ s a -> s{_cfEC2InboundPermissions = a}) . _Default . _Coerce

-- | Instructions for launching server processes on each instance in the fleet. The run-time configuration for a fleet has a collection of server process configurations, one for each type of server process to run on an instance. A server process configuration specifies the location of the server executable, launch parameters, and the number of concurrent processes with that configuration to maintain on each instance. A CreateFleet request must include a run-time configuration with at least one server process configuration; otherwise the request fails with an invalid request exception. (This parameter replaces the parameters @ServerLaunchPath@ and @ServerLaunchParameters@ ; requests that contain values for these parameters instead of a run-time configuration will continue to work.)
cfRuntimeConfiguration :: Lens' CreateFleet (Maybe RuntimeConfiguration)
cfRuntimeConfiguration = lens _cfRuntimeConfiguration (\ s a -> s{_cfRuntimeConfiguration = a})

-- | Game session protection policy to apply to all instances in this fleet. If this parameter is not set, instances in this fleet default to no protection. You can change a fleet's protection policy using 'UpdateFleetAttributes' , but this change will only affect sessions created after the policy change. You can also set protection for individual instances using 'UpdateGameSession' .     * __NoProtection__ -- The game session can be terminated during a scale-down event.     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
cfNewGameSessionProtectionPolicy :: Lens' CreateFleet (Maybe ProtectionPolicy)
cfNewGameSessionProtectionPolicy = lens _cfNewGameSessionProtectionPolicy (\ s a -> s{_cfNewGameSessionProtectionPolicy = a})

-- | This parameter is no longer used. Instead, specify a server launch path using the @RuntimeConfiguration@ parameter. (Requests that specify a server launch path and launch parameters instead of a run-time configuration will continue to work.)
cfServerLaunchPath :: Lens' CreateFleet (Maybe Text)
cfServerLaunchPath = lens _cfServerLaunchPath (\ s a -> s{_cfServerLaunchPath = a})

-- | Name of a metric group to add this fleet to. A metric group tracks metrics across all fleets in the group. Use an existing metric group name to add this fleet to the group, or use a new name to create a new metric group. A fleet can only be included in one metric group at a time.
cfMetricGroups :: Lens' CreateFleet [Text]
cfMetricGroups = lens _cfMetricGroups (\ s a -> s{_cfMetricGroups = a}) . _Default . _Coerce

-- | Human-readable description of a fleet.
cfDescription :: Lens' CreateFleet (Maybe Text)
cfDescription = lens _cfDescription (\ s a -> s{_cfDescription = a})

-- | Policy that limits the number of game sessions an individual player can create over a span of time for this fleet.
cfResourceCreationLimitPolicy :: Lens' CreateFleet (Maybe ResourceCreationLimitPolicy)
cfResourceCreationLimitPolicy = lens _cfResourceCreationLimitPolicy (\ s a -> s{_cfResourceCreationLimitPolicy = a})

-- | Descriptive label that is associated with a fleet. Fleet names do not need to be unique.
cfName :: Lens' CreateFleet Text
cfName = lens _cfName (\ s a -> s{_cfName = a})

-- | Unique identifier for a build to be deployed on the new fleet. The build must have been successfully uploaded to Amazon GameLift and be in a @READY@ status. This fleet setting cannot be changed once the fleet is created.
cfBuildId :: Lens' CreateFleet Text
cfBuildId = lens _cfBuildId (\ s a -> s{_cfBuildId = a})

-- | Name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
cfEC2InstanceType :: Lens' CreateFleet EC2InstanceType
cfEC2InstanceType = lens _cfEC2InstanceType (\ s a -> s{_cfEC2InstanceType = a})

instance AWSRequest CreateFleet where
        type Rs CreateFleet = CreateFleetResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 CreateFleetResponse' <$>
                   (x .?> "FleetAttributes") <*> (pure (fromEnum s)))

instance Hashable CreateFleet where

instance NFData CreateFleet where

instance ToHeaders CreateFleet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.CreateFleet" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateFleet where
        toJSON CreateFleet'{..}
          = object
              (catMaybes
                 [("ServerLaunchParameters" .=) <$>
                    _cfServerLaunchParameters,
                  ("LogPaths" .=) <$> _cfLogPaths,
                  ("PeerVpcId" .=) <$> _cfPeerVPCId,
                  ("FleetType" .=) <$> _cfFleetType,
                  ("PeerVpcAwsAccountId" .=) <$>
                    _cfPeerVPCAWSAccountId,
                  ("EC2InboundPermissions" .=) <$>
                    _cfEC2InboundPermissions,
                  ("RuntimeConfiguration" .=) <$>
                    _cfRuntimeConfiguration,
                  ("NewGameSessionProtectionPolicy" .=) <$>
                    _cfNewGameSessionProtectionPolicy,
                  ("ServerLaunchPath" .=) <$> _cfServerLaunchPath,
                  ("MetricGroups" .=) <$> _cfMetricGroups,
                  ("Description" .=) <$> _cfDescription,
                  ("ResourceCreationLimitPolicy" .=) <$>
                    _cfResourceCreationLimitPolicy,
                  Just ("Name" .= _cfName),
                  Just ("BuildId" .= _cfBuildId),
                  Just ("EC2InstanceType" .= _cfEC2InstanceType)])

instance ToPath CreateFleet where
        toPath = const "/"

instance ToQuery CreateFleet where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'createFleetResponse' smart constructor.
data CreateFleetResponse = CreateFleetResponse'
  { _cfrsFleetAttributes :: !(Maybe FleetAttributes)
  , _cfrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateFleetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfrsFleetAttributes' - Properties for the newly created fleet.
--
-- * 'cfrsResponseStatus' - -- | The response status code.
createFleetResponse
    :: Int -- ^ 'cfrsResponseStatus'
    -> CreateFleetResponse
createFleetResponse pResponseStatus_ =
  CreateFleetResponse'
    {_cfrsFleetAttributes = Nothing, _cfrsResponseStatus = pResponseStatus_}


-- | Properties for the newly created fleet.
cfrsFleetAttributes :: Lens' CreateFleetResponse (Maybe FleetAttributes)
cfrsFleetAttributes = lens _cfrsFleetAttributes (\ s a -> s{_cfrsFleetAttributes = a})

-- | -- | The response status code.
cfrsResponseStatus :: Lens' CreateFleetResponse Int
cfrsResponseStatus = lens _cfrsResponseStatus (\ s a -> s{_cfrsResponseStatus = a})

instance NFData CreateFleetResponse where
