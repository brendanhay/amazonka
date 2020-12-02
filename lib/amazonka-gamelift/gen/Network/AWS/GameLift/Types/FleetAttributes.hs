{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.FleetAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.FleetAttributes where

import Network.AWS.GameLift.Types.CertificateConfiguration
import Network.AWS.GameLift.Types.EC2InstanceType
import Network.AWS.GameLift.Types.FleetAction
import Network.AWS.GameLift.Types.FleetStatus
import Network.AWS.GameLift.Types.FleetType
import Network.AWS.GameLift.Types.OperatingSystem
import Network.AWS.GameLift.Types.ProtectionPolicy
import Network.AWS.GameLift.Types.ResourceCreationLimitPolicy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | General properties describing a fleet.
--
--
--     * 'CreateFleet'
--
--     * 'ListFleets'
--
--     * 'DeleteFleet'
--
--     * 'DescribeFleetAttributes'
--
--     * 'UpdateFleetAttributes'
--
--     * 'StartFleetActions' or 'StopFleetActions'
--
--
--
--
-- /See:/ 'fleetAttributes' smart constructor.
data FleetAttributes = FleetAttributes'
  { _faCreationTime ::
      !(Maybe POSIX),
    _faStatus :: !(Maybe FleetStatus),
    _faServerLaunchParameters :: !(Maybe Text),
    _faLogPaths :: !(Maybe [Text]),
    _faOperatingSystem :: !(Maybe OperatingSystem),
    _faBuildId :: !(Maybe Text),
    _faFleetARN :: !(Maybe Text),
    _faFleetType :: !(Maybe FleetType),
    _faTerminationTime :: !(Maybe POSIX),
    _faInstanceType :: !(Maybe EC2InstanceType),
    _faStoppedActions :: !(Maybe (List1 FleetAction)),
    _faNewGameSessionProtectionPolicy ::
      !(Maybe ProtectionPolicy),
    _faName :: !(Maybe Text),
    _faScriptId :: !(Maybe Text),
    _faScriptARN :: !(Maybe Text),
    _faCertificateConfiguration ::
      !(Maybe CertificateConfiguration),
    _faServerLaunchPath :: !(Maybe Text),
    _faInstanceRoleARN :: !(Maybe Text),
    _faMetricGroups :: !(Maybe [Text]),
    _faBuildARN :: !(Maybe Text),
    _faFleetId :: !(Maybe Text),
    _faDescription :: !(Maybe Text),
    _faResourceCreationLimitPolicy ::
      !(Maybe ResourceCreationLimitPolicy)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FleetAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'faCreationTime' - Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'faStatus' - Current status of the fleet. Possible fleet statuses include the following:     * __NEW__ -- A new fleet has been defined and desired instances is set to 1.      * __DOWNLOADING/VALIDATING/BUILDING/ACTIVATING__ -- Amazon GameLift is setting up the new fleet, creating new instances with the game build or Realtime script and starting server processes.     * __ACTIVE__ -- Hosts can now accept game sessions.     * __ERROR__ -- An error occurred when downloading, validating, building, or activating the fleet.     * __DELETING__ -- Hosts are responding to a delete fleet request.     * __TERMINATED__ -- The fleet no longer exists.
--
-- * 'faServerLaunchParameters' - Game server launch parameters specified for fleets created before 2016-08-04 (or AWS SDK v. 0.12.16). Server launch parameters for fleets created after this date are specified in the fleet's 'RuntimeConfiguration' .
--
-- * 'faLogPaths' - Location of default log files. When a server process is shut down, Amazon GameLift captures and stores any log files in this location. These logs are in addition to game session logs; see more on game session logs in the <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-api-server-code Amazon GameLift Developer Guide> . If no default log path for a fleet is specified, Amazon GameLift automatically uploads logs that are stored on each instance at @C:\game\logs@ (for Windows) or @/local/game/logs@ (for Linux). Use the Amazon GameLift console to access stored logs.
--
-- * 'faOperatingSystem' - Operating system of the fleet's computing resources. A fleet's operating system depends on the OS specified for the build that is deployed on this fleet.
--
-- * 'faBuildId' - A unique identifier for a build.
--
-- * 'faFleetARN' - The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift fleet resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift fleet ARN, the resource ID matches the /FleetId/ value.
--
-- * 'faFleetType' - Indicates whether the fleet uses on-demand or spot instances. A spot instance in use may be interrupted with a two-minute notification.
--
-- * 'faTerminationTime' - Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'faInstanceType' - EC2 instance type indicating the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
--
-- * 'faStoppedActions' - List of fleet activity that have been suspended using 'StopFleetActions' . This includes auto-scaling.
--
-- * 'faNewGameSessionProtectionPolicy' - The type of game session protection to set for all new instances started in the fleet.     * __NoProtection__ -- The game session can be terminated during a scale-down event.     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
--
-- * 'faName' - A descriptive label that is associated with a fleet. Fleet names do not need to be unique.
--
-- * 'faScriptId' - A unique identifier for a Realtime script.
--
-- * 'faScriptARN' - The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift script resource that is deployed on instances in this fleet. In a GameLift script ARN, the resource ID matches the /ScriptId/ value.
--
-- * 'faCertificateConfiguration' - Indicates whether a TLS/SSL certificate was generated for the fleet.
--
-- * 'faServerLaunchPath' - Path to a game server executable in the fleet's build, specified for fleets created before 2016-08-04 (or AWS SDK v. 0.12.16). Server launch paths for fleets created after this date are specified in the fleet's 'RuntimeConfiguration' .
--
-- * 'faInstanceRoleARN' - A unique identifier for an AWS IAM role that manages access to your AWS services.
--
-- * 'faMetricGroups' - Names of metric groups that this fleet is included in. In Amazon CloudWatch, you can view metrics for an individual fleet or aggregated metrics for fleets that are in a fleet metric group. A fleet can be included in only one metric group at a time.
--
-- * 'faBuildARN' - The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift build resource that is deployed on instances in this fleet. In a GameLift build ARN, the resource ID matches the /BuildId/ value.
--
-- * 'faFleetId' - A unique identifier for a fleet.
--
-- * 'faDescription' - Human-readable description of the fleet.
--
-- * 'faResourceCreationLimitPolicy' - Fleet policy to limit the number of game sessions an individual player can create over a span of time.
fleetAttributes ::
  FleetAttributes
fleetAttributes =
  FleetAttributes'
    { _faCreationTime = Nothing,
      _faStatus = Nothing,
      _faServerLaunchParameters = Nothing,
      _faLogPaths = Nothing,
      _faOperatingSystem = Nothing,
      _faBuildId = Nothing,
      _faFleetARN = Nothing,
      _faFleetType = Nothing,
      _faTerminationTime = Nothing,
      _faInstanceType = Nothing,
      _faStoppedActions = Nothing,
      _faNewGameSessionProtectionPolicy = Nothing,
      _faName = Nothing,
      _faScriptId = Nothing,
      _faScriptARN = Nothing,
      _faCertificateConfiguration = Nothing,
      _faServerLaunchPath = Nothing,
      _faInstanceRoleARN = Nothing,
      _faMetricGroups = Nothing,
      _faBuildARN = Nothing,
      _faFleetId = Nothing,
      _faDescription = Nothing,
      _faResourceCreationLimitPolicy = Nothing
    }

-- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
faCreationTime :: Lens' FleetAttributes (Maybe UTCTime)
faCreationTime = lens _faCreationTime (\s a -> s {_faCreationTime = a}) . mapping _Time

-- | Current status of the fleet. Possible fleet statuses include the following:     * __NEW__ -- A new fleet has been defined and desired instances is set to 1.      * __DOWNLOADING/VALIDATING/BUILDING/ACTIVATING__ -- Amazon GameLift is setting up the new fleet, creating new instances with the game build or Realtime script and starting server processes.     * __ACTIVE__ -- Hosts can now accept game sessions.     * __ERROR__ -- An error occurred when downloading, validating, building, or activating the fleet.     * __DELETING__ -- Hosts are responding to a delete fleet request.     * __TERMINATED__ -- The fleet no longer exists.
faStatus :: Lens' FleetAttributes (Maybe FleetStatus)
faStatus = lens _faStatus (\s a -> s {_faStatus = a})

-- | Game server launch parameters specified for fleets created before 2016-08-04 (or AWS SDK v. 0.12.16). Server launch parameters for fleets created after this date are specified in the fleet's 'RuntimeConfiguration' .
faServerLaunchParameters :: Lens' FleetAttributes (Maybe Text)
faServerLaunchParameters = lens _faServerLaunchParameters (\s a -> s {_faServerLaunchParameters = a})

-- | Location of default log files. When a server process is shut down, Amazon GameLift captures and stores any log files in this location. These logs are in addition to game session logs; see more on game session logs in the <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-api-server-code Amazon GameLift Developer Guide> . If no default log path for a fleet is specified, Amazon GameLift automatically uploads logs that are stored on each instance at @C:\game\logs@ (for Windows) or @/local/game/logs@ (for Linux). Use the Amazon GameLift console to access stored logs.
faLogPaths :: Lens' FleetAttributes [Text]
faLogPaths = lens _faLogPaths (\s a -> s {_faLogPaths = a}) . _Default . _Coerce

-- | Operating system of the fleet's computing resources. A fleet's operating system depends on the OS specified for the build that is deployed on this fleet.
faOperatingSystem :: Lens' FleetAttributes (Maybe OperatingSystem)
faOperatingSystem = lens _faOperatingSystem (\s a -> s {_faOperatingSystem = a})

-- | A unique identifier for a build.
faBuildId :: Lens' FleetAttributes (Maybe Text)
faBuildId = lens _faBuildId (\s a -> s {_faBuildId = a})

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift fleet resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift fleet ARN, the resource ID matches the /FleetId/ value.
faFleetARN :: Lens' FleetAttributes (Maybe Text)
faFleetARN = lens _faFleetARN (\s a -> s {_faFleetARN = a})

-- | Indicates whether the fleet uses on-demand or spot instances. A spot instance in use may be interrupted with a two-minute notification.
faFleetType :: Lens' FleetAttributes (Maybe FleetType)
faFleetType = lens _faFleetType (\s a -> s {_faFleetType = a})

-- | Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
faTerminationTime :: Lens' FleetAttributes (Maybe UTCTime)
faTerminationTime = lens _faTerminationTime (\s a -> s {_faTerminationTime = a}) . mapping _Time

-- | EC2 instance type indicating the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
faInstanceType :: Lens' FleetAttributes (Maybe EC2InstanceType)
faInstanceType = lens _faInstanceType (\s a -> s {_faInstanceType = a})

-- | List of fleet activity that have been suspended using 'StopFleetActions' . This includes auto-scaling.
faStoppedActions :: Lens' FleetAttributes (Maybe (NonEmpty FleetAction))
faStoppedActions = lens _faStoppedActions (\s a -> s {_faStoppedActions = a}) . mapping _List1

-- | The type of game session protection to set for all new instances started in the fleet.     * __NoProtection__ -- The game session can be terminated during a scale-down event.     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
faNewGameSessionProtectionPolicy :: Lens' FleetAttributes (Maybe ProtectionPolicy)
faNewGameSessionProtectionPolicy = lens _faNewGameSessionProtectionPolicy (\s a -> s {_faNewGameSessionProtectionPolicy = a})

-- | A descriptive label that is associated with a fleet. Fleet names do not need to be unique.
faName :: Lens' FleetAttributes (Maybe Text)
faName = lens _faName (\s a -> s {_faName = a})

-- | A unique identifier for a Realtime script.
faScriptId :: Lens' FleetAttributes (Maybe Text)
faScriptId = lens _faScriptId (\s a -> s {_faScriptId = a})

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift script resource that is deployed on instances in this fleet. In a GameLift script ARN, the resource ID matches the /ScriptId/ value.
faScriptARN :: Lens' FleetAttributes (Maybe Text)
faScriptARN = lens _faScriptARN (\s a -> s {_faScriptARN = a})

-- | Indicates whether a TLS/SSL certificate was generated for the fleet.
faCertificateConfiguration :: Lens' FleetAttributes (Maybe CertificateConfiguration)
faCertificateConfiguration = lens _faCertificateConfiguration (\s a -> s {_faCertificateConfiguration = a})

-- | Path to a game server executable in the fleet's build, specified for fleets created before 2016-08-04 (or AWS SDK v. 0.12.16). Server launch paths for fleets created after this date are specified in the fleet's 'RuntimeConfiguration' .
faServerLaunchPath :: Lens' FleetAttributes (Maybe Text)
faServerLaunchPath = lens _faServerLaunchPath (\s a -> s {_faServerLaunchPath = a})

-- | A unique identifier for an AWS IAM role that manages access to your AWS services.
faInstanceRoleARN :: Lens' FleetAttributes (Maybe Text)
faInstanceRoleARN = lens _faInstanceRoleARN (\s a -> s {_faInstanceRoleARN = a})

-- | Names of metric groups that this fleet is included in. In Amazon CloudWatch, you can view metrics for an individual fleet or aggregated metrics for fleets that are in a fleet metric group. A fleet can be included in only one metric group at a time.
faMetricGroups :: Lens' FleetAttributes [Text]
faMetricGroups = lens _faMetricGroups (\s a -> s {_faMetricGroups = a}) . _Default . _Coerce

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift build resource that is deployed on instances in this fleet. In a GameLift build ARN, the resource ID matches the /BuildId/ value.
faBuildARN :: Lens' FleetAttributes (Maybe Text)
faBuildARN = lens _faBuildARN (\s a -> s {_faBuildARN = a})

-- | A unique identifier for a fleet.
faFleetId :: Lens' FleetAttributes (Maybe Text)
faFleetId = lens _faFleetId (\s a -> s {_faFleetId = a})

-- | Human-readable description of the fleet.
faDescription :: Lens' FleetAttributes (Maybe Text)
faDescription = lens _faDescription (\s a -> s {_faDescription = a})

-- | Fleet policy to limit the number of game sessions an individual player can create over a span of time.
faResourceCreationLimitPolicy :: Lens' FleetAttributes (Maybe ResourceCreationLimitPolicy)
faResourceCreationLimitPolicy = lens _faResourceCreationLimitPolicy (\s a -> s {_faResourceCreationLimitPolicy = a})

instance FromJSON FleetAttributes where
  parseJSON =
    withObject
      "FleetAttributes"
      ( \x ->
          FleetAttributes'
            <$> (x .:? "CreationTime")
            <*> (x .:? "Status")
            <*> (x .:? "ServerLaunchParameters")
            <*> (x .:? "LogPaths" .!= mempty)
            <*> (x .:? "OperatingSystem")
            <*> (x .:? "BuildId")
            <*> (x .:? "FleetArn")
            <*> (x .:? "FleetType")
            <*> (x .:? "TerminationTime")
            <*> (x .:? "InstanceType")
            <*> (x .:? "StoppedActions")
            <*> (x .:? "NewGameSessionProtectionPolicy")
            <*> (x .:? "Name")
            <*> (x .:? "ScriptId")
            <*> (x .:? "ScriptArn")
            <*> (x .:? "CertificateConfiguration")
            <*> (x .:? "ServerLaunchPath")
            <*> (x .:? "InstanceRoleArn")
            <*> (x .:? "MetricGroups" .!= mempty)
            <*> (x .:? "BuildArn")
            <*> (x .:? "FleetId")
            <*> (x .:? "Description")
            <*> (x .:? "ResourceCreationLimitPolicy")
      )

instance Hashable FleetAttributes

instance NFData FleetAttributes
