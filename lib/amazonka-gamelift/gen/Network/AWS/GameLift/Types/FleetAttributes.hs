{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.FleetAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.FleetAttributes
  ( FleetAttributes (..),

    -- * Smart constructor
    mkFleetAttributes,

    -- * Lenses
    faCreationTime,
    faStatus,
    faServerLaunchParameters,
    faLogPaths,
    faOperatingSystem,
    faBuildId,
    faFleetARN,
    faFleetType,
    faTerminationTime,
    faInstanceType,
    faStoppedActions,
    faNewGameSessionProtectionPolicy,
    faName,
    faScriptId,
    faScriptARN,
    faCertificateConfiguration,
    faServerLaunchPath,
    faInstanceRoleARN,
    faMetricGroups,
    faBuildARN,
    faFleetId,
    faDescription,
    faResourceCreationLimitPolicy,
  )
where

import Network.AWS.GameLift.Types.CertificateConfiguration
import Network.AWS.GameLift.Types.EC2InstanceType
import Network.AWS.GameLift.Types.FleetAction
import Network.AWS.GameLift.Types.FleetStatus
import Network.AWS.GameLift.Types.FleetType
import Network.AWS.GameLift.Types.OperatingSystem
import Network.AWS.GameLift.Types.ProtectionPolicy
import Network.AWS.GameLift.Types.ResourceCreationLimitPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | General properties describing a fleet.
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
-- /See:/ 'mkFleetAttributes' smart constructor.
data FleetAttributes = FleetAttributes'
  { -- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | Current status of the fleet.
    --
    -- Possible fleet statuses include the following:
    --
    --     * __NEW__ -- A new fleet has been defined and desired instances is set to 1.
    --
    --
    --     * __DOWNLOADING/VALIDATING/BUILDING/ACTIVATING__ -- Amazon GameLift is setting up the new fleet, creating new instances with the game build or Realtime script and starting server processes.
    --
    --
    --     * __ACTIVE__ -- Hosts can now accept game sessions.
    --
    --
    --     * __ERROR__ -- An error occurred when downloading, validating, building, or activating the fleet.
    --
    --
    --     * __DELETING__ -- Hosts are responding to a delete fleet request.
    --
    --
    --     * __TERMINATED__ -- The fleet no longer exists.
    status :: Lude.Maybe FleetStatus,
    -- | Game server launch parameters specified for fleets created before 2016-08-04 (or AWS SDK v. 0.12.16). Server launch parameters for fleets created after this date are specified in the fleet's 'RuntimeConfiguration' .
    serverLaunchParameters :: Lude.Maybe Lude.Text,
    -- | Location of default log files. When a server process is shut down, Amazon GameLift captures and stores any log files in this location. These logs are in addition to game session logs; see more on game session logs in the <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-api-server-code Amazon GameLift Developer Guide> . If no default log path for a fleet is specified, Amazon GameLift automatically uploads logs that are stored on each instance at @C:\game\logs@ (for Windows) or @/local/game/logs@ (for Linux). Use the Amazon GameLift console to access stored logs.
    logPaths :: Lude.Maybe [Lude.Text],
    -- | Operating system of the fleet's computing resources. A fleet's operating system depends on the OS specified for the build that is deployed on this fleet.
    operatingSystem :: Lude.Maybe OperatingSystem,
    -- | A unique identifier for a build.
    buildId :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift fleet resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift fleet ARN, the resource ID matches the /FleetId/ value.
    fleetARN :: Lude.Maybe Lude.Text,
    -- | Indicates whether the fleet uses on-demand or spot instances. A spot instance in use may be interrupted with a two-minute notification.
    fleetType :: Lude.Maybe FleetType,
    -- | Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
    terminationTime :: Lude.Maybe Lude.Timestamp,
    -- | EC2 instance type indicating the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
    instanceType :: Lude.Maybe EC2InstanceType,
    -- | List of fleet activity that have been suspended using 'StopFleetActions' . This includes auto-scaling.
    stoppedActions :: Lude.Maybe (Lude.NonEmpty FleetAction),
    -- | The type of game session protection to set for all new instances started in the fleet.
    --
    --
    --     * __NoProtection__ -- The game session can be terminated during a scale-down event.
    --
    --
    --     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
    newGameSessionProtectionPolicy :: Lude.Maybe ProtectionPolicy,
    -- | A descriptive label that is associated with a fleet. Fleet names do not need to be unique.
    name :: Lude.Maybe Lude.Text,
    -- | A unique identifier for a Realtime script.
    scriptId :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift script resource that is deployed on instances in this fleet. In a GameLift script ARN, the resource ID matches the /ScriptId/ value.
    scriptARN :: Lude.Maybe Lude.Text,
    -- | Indicates whether a TLS/SSL certificate was generated for the fleet.
    certificateConfiguration :: Lude.Maybe CertificateConfiguration,
    -- | Path to a game server executable in the fleet's build, specified for fleets created before 2016-08-04 (or AWS SDK v. 0.12.16). Server launch paths for fleets created after this date are specified in the fleet's 'RuntimeConfiguration' .
    serverLaunchPath :: Lude.Maybe Lude.Text,
    -- | A unique identifier for an AWS IAM role that manages access to your AWS services.
    instanceRoleARN :: Lude.Maybe Lude.Text,
    -- | Names of metric groups that this fleet is included in. In Amazon CloudWatch, you can view metrics for an individual fleet or aggregated metrics for fleets that are in a fleet metric group. A fleet can be included in only one metric group at a time.
    metricGroups :: Lude.Maybe [Lude.Text],
    -- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift build resource that is deployed on instances in this fleet. In a GameLift build ARN, the resource ID matches the /BuildId/ value.
    buildARN :: Lude.Maybe Lude.Text,
    -- | A unique identifier for a fleet.
    fleetId :: Lude.Maybe Lude.Text,
    -- | Human-readable description of the fleet.
    description :: Lude.Maybe Lude.Text,
    -- | Fleet policy to limit the number of game sessions an individual player can create over a span of time.
    resourceCreationLimitPolicy :: Lude.Maybe ResourceCreationLimitPolicy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FleetAttributes' with the minimum fields required to make a request.
--
-- * 'creationTime' - Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
-- * 'status' - Current status of the fleet.
--
-- Possible fleet statuses include the following:
--
--     * __NEW__ -- A new fleet has been defined and desired instances is set to 1.
--
--
--     * __DOWNLOADING/VALIDATING/BUILDING/ACTIVATING__ -- Amazon GameLift is setting up the new fleet, creating new instances with the game build or Realtime script and starting server processes.
--
--
--     * __ACTIVE__ -- Hosts can now accept game sessions.
--
--
--     * __ERROR__ -- An error occurred when downloading, validating, building, or activating the fleet.
--
--
--     * __DELETING__ -- Hosts are responding to a delete fleet request.
--
--
--     * __TERMINATED__ -- The fleet no longer exists.
--
--
-- * 'serverLaunchParameters' - Game server launch parameters specified for fleets created before 2016-08-04 (or AWS SDK v. 0.12.16). Server launch parameters for fleets created after this date are specified in the fleet's 'RuntimeConfiguration' .
-- * 'logPaths' - Location of default log files. When a server process is shut down, Amazon GameLift captures and stores any log files in this location. These logs are in addition to game session logs; see more on game session logs in the <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-api-server-code Amazon GameLift Developer Guide> . If no default log path for a fleet is specified, Amazon GameLift automatically uploads logs that are stored on each instance at @C:\game\logs@ (for Windows) or @/local/game/logs@ (for Linux). Use the Amazon GameLift console to access stored logs.
-- * 'operatingSystem' - Operating system of the fleet's computing resources. A fleet's operating system depends on the OS specified for the build that is deployed on this fleet.
-- * 'buildId' - A unique identifier for a build.
-- * 'fleetARN' - The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift fleet resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift fleet ARN, the resource ID matches the /FleetId/ value.
-- * 'fleetType' - Indicates whether the fleet uses on-demand or spot instances. A spot instance in use may be interrupted with a two-minute notification.
-- * 'terminationTime' - Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
-- * 'instanceType' - EC2 instance type indicating the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
-- * 'stoppedActions' - List of fleet activity that have been suspended using 'StopFleetActions' . This includes auto-scaling.
-- * 'newGameSessionProtectionPolicy' - The type of game session protection to set for all new instances started in the fleet.
--
--
--     * __NoProtection__ -- The game session can be terminated during a scale-down event.
--
--
--     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
--
--
-- * 'name' - A descriptive label that is associated with a fleet. Fleet names do not need to be unique.
-- * 'scriptId' - A unique identifier for a Realtime script.
-- * 'scriptARN' - The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift script resource that is deployed on instances in this fleet. In a GameLift script ARN, the resource ID matches the /ScriptId/ value.
-- * 'certificateConfiguration' - Indicates whether a TLS/SSL certificate was generated for the fleet.
-- * 'serverLaunchPath' - Path to a game server executable in the fleet's build, specified for fleets created before 2016-08-04 (or AWS SDK v. 0.12.16). Server launch paths for fleets created after this date are specified in the fleet's 'RuntimeConfiguration' .
-- * 'instanceRoleARN' - A unique identifier for an AWS IAM role that manages access to your AWS services.
-- * 'metricGroups' - Names of metric groups that this fleet is included in. In Amazon CloudWatch, you can view metrics for an individual fleet or aggregated metrics for fleets that are in a fleet metric group. A fleet can be included in only one metric group at a time.
-- * 'buildARN' - The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift build resource that is deployed on instances in this fleet. In a GameLift build ARN, the resource ID matches the /BuildId/ value.
-- * 'fleetId' - A unique identifier for a fleet.
-- * 'description' - Human-readable description of the fleet.
-- * 'resourceCreationLimitPolicy' - Fleet policy to limit the number of game sessions an individual player can create over a span of time.
mkFleetAttributes ::
  FleetAttributes
mkFleetAttributes =
  FleetAttributes'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      serverLaunchParameters = Lude.Nothing,
      logPaths = Lude.Nothing,
      operatingSystem = Lude.Nothing,
      buildId = Lude.Nothing,
      fleetARN = Lude.Nothing,
      fleetType = Lude.Nothing,
      terminationTime = Lude.Nothing,
      instanceType = Lude.Nothing,
      stoppedActions = Lude.Nothing,
      newGameSessionProtectionPolicy = Lude.Nothing,
      name = Lude.Nothing,
      scriptId = Lude.Nothing,
      scriptARN = Lude.Nothing,
      certificateConfiguration = Lude.Nothing,
      serverLaunchPath = Lude.Nothing,
      instanceRoleARN = Lude.Nothing,
      metricGroups = Lude.Nothing,
      buildARN = Lude.Nothing,
      fleetId = Lude.Nothing,
      description = Lude.Nothing,
      resourceCreationLimitPolicy = Lude.Nothing
    }

-- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faCreationTime :: Lens.Lens' FleetAttributes (Lude.Maybe Lude.Timestamp)
faCreationTime = Lens.lens (creationTime :: FleetAttributes -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: FleetAttributes)
{-# DEPRECATED faCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Current status of the fleet.
--
-- Possible fleet statuses include the following:
--
--     * __NEW__ -- A new fleet has been defined and desired instances is set to 1.
--
--
--     * __DOWNLOADING/VALIDATING/BUILDING/ACTIVATING__ -- Amazon GameLift is setting up the new fleet, creating new instances with the game build or Realtime script and starting server processes.
--
--
--     * __ACTIVE__ -- Hosts can now accept game sessions.
--
--
--     * __ERROR__ -- An error occurred when downloading, validating, building, or activating the fleet.
--
--
--     * __DELETING__ -- Hosts are responding to a delete fleet request.
--
--
--     * __TERMINATED__ -- The fleet no longer exists.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faStatus :: Lens.Lens' FleetAttributes (Lude.Maybe FleetStatus)
faStatus = Lens.lens (status :: FleetAttributes -> Lude.Maybe FleetStatus) (\s a -> s {status = a} :: FleetAttributes)
{-# DEPRECATED faStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Game server launch parameters specified for fleets created before 2016-08-04 (or AWS SDK v. 0.12.16). Server launch parameters for fleets created after this date are specified in the fleet's 'RuntimeConfiguration' .
--
-- /Note:/ Consider using 'serverLaunchParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faServerLaunchParameters :: Lens.Lens' FleetAttributes (Lude.Maybe Lude.Text)
faServerLaunchParameters = Lens.lens (serverLaunchParameters :: FleetAttributes -> Lude.Maybe Lude.Text) (\s a -> s {serverLaunchParameters = a} :: FleetAttributes)
{-# DEPRECATED faServerLaunchParameters "Use generic-lens or generic-optics with 'serverLaunchParameters' instead." #-}

-- | Location of default log files. When a server process is shut down, Amazon GameLift captures and stores any log files in this location. These logs are in addition to game session logs; see more on game session logs in the <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-api-server-code Amazon GameLift Developer Guide> . If no default log path for a fleet is specified, Amazon GameLift automatically uploads logs that are stored on each instance at @C:\game\logs@ (for Windows) or @/local/game/logs@ (for Linux). Use the Amazon GameLift console to access stored logs.
--
-- /Note:/ Consider using 'logPaths' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faLogPaths :: Lens.Lens' FleetAttributes (Lude.Maybe [Lude.Text])
faLogPaths = Lens.lens (logPaths :: FleetAttributes -> Lude.Maybe [Lude.Text]) (\s a -> s {logPaths = a} :: FleetAttributes)
{-# DEPRECATED faLogPaths "Use generic-lens or generic-optics with 'logPaths' instead." #-}

-- | Operating system of the fleet's computing resources. A fleet's operating system depends on the OS specified for the build that is deployed on this fleet.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faOperatingSystem :: Lens.Lens' FleetAttributes (Lude.Maybe OperatingSystem)
faOperatingSystem = Lens.lens (operatingSystem :: FleetAttributes -> Lude.Maybe OperatingSystem) (\s a -> s {operatingSystem = a} :: FleetAttributes)
{-# DEPRECATED faOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

-- | A unique identifier for a build.
--
-- /Note:/ Consider using 'buildId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faBuildId :: Lens.Lens' FleetAttributes (Lude.Maybe Lude.Text)
faBuildId = Lens.lens (buildId :: FleetAttributes -> Lude.Maybe Lude.Text) (\s a -> s {buildId = a} :: FleetAttributes)
{-# DEPRECATED faBuildId "Use generic-lens or generic-optics with 'buildId' instead." #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift fleet resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift fleet ARN, the resource ID matches the /FleetId/ value.
--
-- /Note:/ Consider using 'fleetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faFleetARN :: Lens.Lens' FleetAttributes (Lude.Maybe Lude.Text)
faFleetARN = Lens.lens (fleetARN :: FleetAttributes -> Lude.Maybe Lude.Text) (\s a -> s {fleetARN = a} :: FleetAttributes)
{-# DEPRECATED faFleetARN "Use generic-lens or generic-optics with 'fleetARN' instead." #-}

-- | Indicates whether the fleet uses on-demand or spot instances. A spot instance in use may be interrupted with a two-minute notification.
--
-- /Note:/ Consider using 'fleetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faFleetType :: Lens.Lens' FleetAttributes (Lude.Maybe FleetType)
faFleetType = Lens.lens (fleetType :: FleetAttributes -> Lude.Maybe FleetType) (\s a -> s {fleetType = a} :: FleetAttributes)
{-# DEPRECATED faFleetType "Use generic-lens or generic-optics with 'fleetType' instead." #-}

-- | Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'terminationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faTerminationTime :: Lens.Lens' FleetAttributes (Lude.Maybe Lude.Timestamp)
faTerminationTime = Lens.lens (terminationTime :: FleetAttributes -> Lude.Maybe Lude.Timestamp) (\s a -> s {terminationTime = a} :: FleetAttributes)
{-# DEPRECATED faTerminationTime "Use generic-lens or generic-optics with 'terminationTime' instead." #-}

-- | EC2 instance type indicating the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faInstanceType :: Lens.Lens' FleetAttributes (Lude.Maybe EC2InstanceType)
faInstanceType = Lens.lens (instanceType :: FleetAttributes -> Lude.Maybe EC2InstanceType) (\s a -> s {instanceType = a} :: FleetAttributes)
{-# DEPRECATED faInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | List of fleet activity that have been suspended using 'StopFleetActions' . This includes auto-scaling.
--
-- /Note:/ Consider using 'stoppedActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faStoppedActions :: Lens.Lens' FleetAttributes (Lude.Maybe (Lude.NonEmpty FleetAction))
faStoppedActions = Lens.lens (stoppedActions :: FleetAttributes -> Lude.Maybe (Lude.NonEmpty FleetAction)) (\s a -> s {stoppedActions = a} :: FleetAttributes)
{-# DEPRECATED faStoppedActions "Use generic-lens or generic-optics with 'stoppedActions' instead." #-}

-- | The type of game session protection to set for all new instances started in the fleet.
--
--
--     * __NoProtection__ -- The game session can be terminated during a scale-down event.
--
--
--     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
--
--
--
-- /Note:/ Consider using 'newGameSessionProtectionPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faNewGameSessionProtectionPolicy :: Lens.Lens' FleetAttributes (Lude.Maybe ProtectionPolicy)
faNewGameSessionProtectionPolicy = Lens.lens (newGameSessionProtectionPolicy :: FleetAttributes -> Lude.Maybe ProtectionPolicy) (\s a -> s {newGameSessionProtectionPolicy = a} :: FleetAttributes)
{-# DEPRECATED faNewGameSessionProtectionPolicy "Use generic-lens or generic-optics with 'newGameSessionProtectionPolicy' instead." #-}

-- | A descriptive label that is associated with a fleet. Fleet names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faName :: Lens.Lens' FleetAttributes (Lude.Maybe Lude.Text)
faName = Lens.lens (name :: FleetAttributes -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: FleetAttributes)
{-# DEPRECATED faName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A unique identifier for a Realtime script.
--
-- /Note:/ Consider using 'scriptId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faScriptId :: Lens.Lens' FleetAttributes (Lude.Maybe Lude.Text)
faScriptId = Lens.lens (scriptId :: FleetAttributes -> Lude.Maybe Lude.Text) (\s a -> s {scriptId = a} :: FleetAttributes)
{-# DEPRECATED faScriptId "Use generic-lens or generic-optics with 'scriptId' instead." #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift script resource that is deployed on instances in this fleet. In a GameLift script ARN, the resource ID matches the /ScriptId/ value.
--
-- /Note:/ Consider using 'scriptARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faScriptARN :: Lens.Lens' FleetAttributes (Lude.Maybe Lude.Text)
faScriptARN = Lens.lens (scriptARN :: FleetAttributes -> Lude.Maybe Lude.Text) (\s a -> s {scriptARN = a} :: FleetAttributes)
{-# DEPRECATED faScriptARN "Use generic-lens or generic-optics with 'scriptARN' instead." #-}

-- | Indicates whether a TLS/SSL certificate was generated for the fleet.
--
-- /Note:/ Consider using 'certificateConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faCertificateConfiguration :: Lens.Lens' FleetAttributes (Lude.Maybe CertificateConfiguration)
faCertificateConfiguration = Lens.lens (certificateConfiguration :: FleetAttributes -> Lude.Maybe CertificateConfiguration) (\s a -> s {certificateConfiguration = a} :: FleetAttributes)
{-# DEPRECATED faCertificateConfiguration "Use generic-lens or generic-optics with 'certificateConfiguration' instead." #-}

-- | Path to a game server executable in the fleet's build, specified for fleets created before 2016-08-04 (or AWS SDK v. 0.12.16). Server launch paths for fleets created after this date are specified in the fleet's 'RuntimeConfiguration' .
--
-- /Note:/ Consider using 'serverLaunchPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faServerLaunchPath :: Lens.Lens' FleetAttributes (Lude.Maybe Lude.Text)
faServerLaunchPath = Lens.lens (serverLaunchPath :: FleetAttributes -> Lude.Maybe Lude.Text) (\s a -> s {serverLaunchPath = a} :: FleetAttributes)
{-# DEPRECATED faServerLaunchPath "Use generic-lens or generic-optics with 'serverLaunchPath' instead." #-}

-- | A unique identifier for an AWS IAM role that manages access to your AWS services.
--
-- /Note:/ Consider using 'instanceRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faInstanceRoleARN :: Lens.Lens' FleetAttributes (Lude.Maybe Lude.Text)
faInstanceRoleARN = Lens.lens (instanceRoleARN :: FleetAttributes -> Lude.Maybe Lude.Text) (\s a -> s {instanceRoleARN = a} :: FleetAttributes)
{-# DEPRECATED faInstanceRoleARN "Use generic-lens or generic-optics with 'instanceRoleARN' instead." #-}

-- | Names of metric groups that this fleet is included in. In Amazon CloudWatch, you can view metrics for an individual fleet or aggregated metrics for fleets that are in a fleet metric group. A fleet can be included in only one metric group at a time.
--
-- /Note:/ Consider using 'metricGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faMetricGroups :: Lens.Lens' FleetAttributes (Lude.Maybe [Lude.Text])
faMetricGroups = Lens.lens (metricGroups :: FleetAttributes -> Lude.Maybe [Lude.Text]) (\s a -> s {metricGroups = a} :: FleetAttributes)
{-# DEPRECATED faMetricGroups "Use generic-lens or generic-optics with 'metricGroups' instead." #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift build resource that is deployed on instances in this fleet. In a GameLift build ARN, the resource ID matches the /BuildId/ value.
--
-- /Note:/ Consider using 'buildARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faBuildARN :: Lens.Lens' FleetAttributes (Lude.Maybe Lude.Text)
faBuildARN = Lens.lens (buildARN :: FleetAttributes -> Lude.Maybe Lude.Text) (\s a -> s {buildARN = a} :: FleetAttributes)
{-# DEPRECATED faBuildARN "Use generic-lens or generic-optics with 'buildARN' instead." #-}

-- | A unique identifier for a fleet.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faFleetId :: Lens.Lens' FleetAttributes (Lude.Maybe Lude.Text)
faFleetId = Lens.lens (fleetId :: FleetAttributes -> Lude.Maybe Lude.Text) (\s a -> s {fleetId = a} :: FleetAttributes)
{-# DEPRECATED faFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | Human-readable description of the fleet.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faDescription :: Lens.Lens' FleetAttributes (Lude.Maybe Lude.Text)
faDescription = Lens.lens (description :: FleetAttributes -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: FleetAttributes)
{-# DEPRECATED faDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Fleet policy to limit the number of game sessions an individual player can create over a span of time.
--
-- /Note:/ Consider using 'resourceCreationLimitPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faResourceCreationLimitPolicy :: Lens.Lens' FleetAttributes (Lude.Maybe ResourceCreationLimitPolicy)
faResourceCreationLimitPolicy = Lens.lens (resourceCreationLimitPolicy :: FleetAttributes -> Lude.Maybe ResourceCreationLimitPolicy) (\s a -> s {resourceCreationLimitPolicy = a} :: FleetAttributes)
{-# DEPRECATED faResourceCreationLimitPolicy "Use generic-lens or generic-optics with 'resourceCreationLimitPolicy' instead." #-}

instance Lude.FromJSON FleetAttributes where
  parseJSON =
    Lude.withObject
      "FleetAttributes"
      ( \x ->
          FleetAttributes'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "ServerLaunchParameters")
            Lude.<*> (x Lude..:? "LogPaths" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "OperatingSystem")
            Lude.<*> (x Lude..:? "BuildId")
            Lude.<*> (x Lude..:? "FleetArn")
            Lude.<*> (x Lude..:? "FleetType")
            Lude.<*> (x Lude..:? "TerminationTime")
            Lude.<*> (x Lude..:? "InstanceType")
            Lude.<*> (x Lude..:? "StoppedActions")
            Lude.<*> (x Lude..:? "NewGameSessionProtectionPolicy")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "ScriptId")
            Lude.<*> (x Lude..:? "ScriptArn")
            Lude.<*> (x Lude..:? "CertificateConfiguration")
            Lude.<*> (x Lude..:? "ServerLaunchPath")
            Lude.<*> (x Lude..:? "InstanceRoleArn")
            Lude.<*> (x Lude..:? "MetricGroups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "BuildArn")
            Lude.<*> (x Lude..:? "FleetId")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "ResourceCreationLimitPolicy")
      )
