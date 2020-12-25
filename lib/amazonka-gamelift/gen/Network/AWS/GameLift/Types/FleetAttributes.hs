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
    faBuildArn,
    faBuildId,
    faCertificateConfiguration,
    faCreationTime,
    faDescription,
    faFleetArn,
    faFleetId,
    faFleetType,
    faInstanceRoleArn,
    faInstanceType,
    faLogPaths,
    faMetricGroups,
    faName,
    faNewGameSessionProtectionPolicy,
    faOperatingSystem,
    faResourceCreationLimitPolicy,
    faScriptArn,
    faScriptId,
    faServerLaunchParameters,
    faServerLaunchPath,
    faStatus,
    faStoppedActions,
    faTerminationTime,
  )
where

import qualified Network.AWS.GameLift.Types.BuildArn as Types
import qualified Network.AWS.GameLift.Types.BuildId as Types
import qualified Network.AWS.GameLift.Types.CertificateConfiguration as Types
import qualified Network.AWS.GameLift.Types.EC2InstanceType as Types
import qualified Network.AWS.GameLift.Types.FleetAction as Types
import qualified Network.AWS.GameLift.Types.FleetArn as Types
import qualified Network.AWS.GameLift.Types.FleetId as Types
import qualified Network.AWS.GameLift.Types.FleetStatus as Types
import qualified Network.AWS.GameLift.Types.FleetType as Types
import qualified Network.AWS.GameLift.Types.MetricGroup as Types
import qualified Network.AWS.GameLift.Types.NonEmptyString as Types
import qualified Network.AWS.GameLift.Types.NonZeroAndMaxString as Types
import qualified Network.AWS.GameLift.Types.OperatingSystem as Types
import qualified Network.AWS.GameLift.Types.ProtectionPolicy as Types
import qualified Network.AWS.GameLift.Types.ResourceCreationLimitPolicy as Types
import qualified Network.AWS.GameLift.Types.ScriptArn as Types
import qualified Network.AWS.GameLift.Types.ScriptId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

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
  { -- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift build resource that is deployed on instances in this fleet. In a GameLift build ARN, the resource ID matches the /BuildId/ value.
    buildArn :: Core.Maybe Types.BuildArn,
    -- | A unique identifier for a build.
    buildId :: Core.Maybe Types.BuildId,
    -- | Indicates whether a TLS/SSL certificate was generated for the fleet.
    certificateConfiguration :: Core.Maybe Types.CertificateConfiguration,
    -- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | Human-readable description of the fleet.
    description :: Core.Maybe Types.NonZeroAndMaxString,
    -- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift fleet resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift fleet ARN, the resource ID matches the /FleetId/ value.
    fleetArn :: Core.Maybe Types.FleetArn,
    -- | A unique identifier for a fleet.
    fleetId :: Core.Maybe Types.FleetId,
    -- | Indicates whether the fleet uses on-demand or spot instances. A spot instance in use may be interrupted with a two-minute notification.
    fleetType :: Core.Maybe Types.FleetType,
    -- | A unique identifier for an AWS IAM role that manages access to your AWS services.
    instanceRoleArn :: Core.Maybe Types.NonEmptyString,
    -- | EC2 instance type indicating the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
    instanceType :: Core.Maybe Types.EC2InstanceType,
    -- | Location of default log files. When a server process is shut down, Amazon GameLift captures and stores any log files in this location. These logs are in addition to game session logs; see more on game session logs in the <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-api-server-code Amazon GameLift Developer Guide> . If no default log path for a fleet is specified, Amazon GameLift automatically uploads logs that are stored on each instance at @C:\game\logs@ (for Windows) or @/local/game/logs@ (for Linux). Use the Amazon GameLift console to access stored logs.
    logPaths :: Core.Maybe [Types.NonZeroAndMaxString],
    -- | Names of metric groups that this fleet is included in. In Amazon CloudWatch, you can view metrics for an individual fleet or aggregated metrics for fleets that are in a fleet metric group. A fleet can be included in only one metric group at a time.
    metricGroups :: Core.Maybe [Types.MetricGroup],
    -- | A descriptive label that is associated with a fleet. Fleet names do not need to be unique.
    name :: Core.Maybe Types.NonZeroAndMaxString,
    -- | The type of game session protection to set for all new instances started in the fleet.
    --
    --
    --     * __NoProtection__ -- The game session can be terminated during a scale-down event.
    --
    --
    --     * __FullProtection__ -- If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
    newGameSessionProtectionPolicy :: Core.Maybe Types.ProtectionPolicy,
    -- | Operating system of the fleet's computing resources. A fleet's operating system depends on the OS specified for the build that is deployed on this fleet.
    operatingSystem :: Core.Maybe Types.OperatingSystem,
    -- | Fleet policy to limit the number of game sessions an individual player can create over a span of time.
    resourceCreationLimitPolicy :: Core.Maybe Types.ResourceCreationLimitPolicy,
    -- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift script resource that is deployed on instances in this fleet. In a GameLift script ARN, the resource ID matches the /ScriptId/ value.
    scriptArn :: Core.Maybe Types.ScriptArn,
    -- | A unique identifier for a Realtime script.
    scriptId :: Core.Maybe Types.ScriptId,
    -- | Game server launch parameters specified for fleets created before 2016-08-04 (or AWS SDK v. 0.12.16). Server launch parameters for fleets created after this date are specified in the fleet's 'RuntimeConfiguration' .
    serverLaunchParameters :: Core.Maybe Types.NonZeroAndMaxString,
    -- | Path to a game server executable in the fleet's build, specified for fleets created before 2016-08-04 (or AWS SDK v. 0.12.16). Server launch paths for fleets created after this date are specified in the fleet's 'RuntimeConfiguration' .
    serverLaunchPath :: Core.Maybe Types.NonZeroAndMaxString,
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
    status :: Core.Maybe Types.FleetStatus,
    -- | List of fleet activity that have been suspended using 'StopFleetActions' . This includes auto-scaling.
    stoppedActions :: Core.Maybe (Core.NonEmpty Types.FleetAction),
    -- | Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
    terminationTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'FleetAttributes' value with any optional fields omitted.
mkFleetAttributes ::
  FleetAttributes
mkFleetAttributes =
  FleetAttributes'
    { buildArn = Core.Nothing,
      buildId = Core.Nothing,
      certificateConfiguration = Core.Nothing,
      creationTime = Core.Nothing,
      description = Core.Nothing,
      fleetArn = Core.Nothing,
      fleetId = Core.Nothing,
      fleetType = Core.Nothing,
      instanceRoleArn = Core.Nothing,
      instanceType = Core.Nothing,
      logPaths = Core.Nothing,
      metricGroups = Core.Nothing,
      name = Core.Nothing,
      newGameSessionProtectionPolicy = Core.Nothing,
      operatingSystem = Core.Nothing,
      resourceCreationLimitPolicy = Core.Nothing,
      scriptArn = Core.Nothing,
      scriptId = Core.Nothing,
      serverLaunchParameters = Core.Nothing,
      serverLaunchPath = Core.Nothing,
      status = Core.Nothing,
      stoppedActions = Core.Nothing,
      terminationTime = Core.Nothing
    }

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift build resource that is deployed on instances in this fleet. In a GameLift build ARN, the resource ID matches the /BuildId/ value.
--
-- /Note:/ Consider using 'buildArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faBuildArn :: Lens.Lens' FleetAttributes (Core.Maybe Types.BuildArn)
faBuildArn = Lens.field @"buildArn"
{-# DEPRECATED faBuildArn "Use generic-lens or generic-optics with 'buildArn' instead." #-}

-- | A unique identifier for a build.
--
-- /Note:/ Consider using 'buildId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faBuildId :: Lens.Lens' FleetAttributes (Core.Maybe Types.BuildId)
faBuildId = Lens.field @"buildId"
{-# DEPRECATED faBuildId "Use generic-lens or generic-optics with 'buildId' instead." #-}

-- | Indicates whether a TLS/SSL certificate was generated for the fleet.
--
-- /Note:/ Consider using 'certificateConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faCertificateConfiguration :: Lens.Lens' FleetAttributes (Core.Maybe Types.CertificateConfiguration)
faCertificateConfiguration = Lens.field @"certificateConfiguration"
{-# DEPRECATED faCertificateConfiguration "Use generic-lens or generic-optics with 'certificateConfiguration' instead." #-}

-- | Time stamp indicating when this data object was created. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faCreationTime :: Lens.Lens' FleetAttributes (Core.Maybe Core.NominalDiffTime)
faCreationTime = Lens.field @"creationTime"
{-# DEPRECATED faCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Human-readable description of the fleet.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faDescription :: Lens.Lens' FleetAttributes (Core.Maybe Types.NonZeroAndMaxString)
faDescription = Lens.field @"description"
{-# DEPRECATED faDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift fleet resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift fleet ARN, the resource ID matches the /FleetId/ value.
--
-- /Note:/ Consider using 'fleetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faFleetArn :: Lens.Lens' FleetAttributes (Core.Maybe Types.FleetArn)
faFleetArn = Lens.field @"fleetArn"
{-# DEPRECATED faFleetArn "Use generic-lens or generic-optics with 'fleetArn' instead." #-}

-- | A unique identifier for a fleet.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faFleetId :: Lens.Lens' FleetAttributes (Core.Maybe Types.FleetId)
faFleetId = Lens.field @"fleetId"
{-# DEPRECATED faFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | Indicates whether the fleet uses on-demand or spot instances. A spot instance in use may be interrupted with a two-minute notification.
--
-- /Note:/ Consider using 'fleetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faFleetType :: Lens.Lens' FleetAttributes (Core.Maybe Types.FleetType)
faFleetType = Lens.field @"fleetType"
{-# DEPRECATED faFleetType "Use generic-lens or generic-optics with 'fleetType' instead." #-}

-- | A unique identifier for an AWS IAM role that manages access to your AWS services.
--
-- /Note:/ Consider using 'instanceRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faInstanceRoleArn :: Lens.Lens' FleetAttributes (Core.Maybe Types.NonEmptyString)
faInstanceRoleArn = Lens.field @"instanceRoleArn"
{-# DEPRECATED faInstanceRoleArn "Use generic-lens or generic-optics with 'instanceRoleArn' instead." #-}

-- | EC2 instance type indicating the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faInstanceType :: Lens.Lens' FleetAttributes (Core.Maybe Types.EC2InstanceType)
faInstanceType = Lens.field @"instanceType"
{-# DEPRECATED faInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Location of default log files. When a server process is shut down, Amazon GameLift captures and stores any log files in this location. These logs are in addition to game session logs; see more on game session logs in the <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-api-server-code Amazon GameLift Developer Guide> . If no default log path for a fleet is specified, Amazon GameLift automatically uploads logs that are stored on each instance at @C:\game\logs@ (for Windows) or @/local/game/logs@ (for Linux). Use the Amazon GameLift console to access stored logs.
--
-- /Note:/ Consider using 'logPaths' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faLogPaths :: Lens.Lens' FleetAttributes (Core.Maybe [Types.NonZeroAndMaxString])
faLogPaths = Lens.field @"logPaths"
{-# DEPRECATED faLogPaths "Use generic-lens or generic-optics with 'logPaths' instead." #-}

-- | Names of metric groups that this fleet is included in. In Amazon CloudWatch, you can view metrics for an individual fleet or aggregated metrics for fleets that are in a fleet metric group. A fleet can be included in only one metric group at a time.
--
-- /Note:/ Consider using 'metricGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faMetricGroups :: Lens.Lens' FleetAttributes (Core.Maybe [Types.MetricGroup])
faMetricGroups = Lens.field @"metricGroups"
{-# DEPRECATED faMetricGroups "Use generic-lens or generic-optics with 'metricGroups' instead." #-}

-- | A descriptive label that is associated with a fleet. Fleet names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faName :: Lens.Lens' FleetAttributes (Core.Maybe Types.NonZeroAndMaxString)
faName = Lens.field @"name"
{-# DEPRECATED faName "Use generic-lens or generic-optics with 'name' instead." #-}

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
faNewGameSessionProtectionPolicy :: Lens.Lens' FleetAttributes (Core.Maybe Types.ProtectionPolicy)
faNewGameSessionProtectionPolicy = Lens.field @"newGameSessionProtectionPolicy"
{-# DEPRECATED faNewGameSessionProtectionPolicy "Use generic-lens or generic-optics with 'newGameSessionProtectionPolicy' instead." #-}

-- | Operating system of the fleet's computing resources. A fleet's operating system depends on the OS specified for the build that is deployed on this fleet.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faOperatingSystem :: Lens.Lens' FleetAttributes (Core.Maybe Types.OperatingSystem)
faOperatingSystem = Lens.field @"operatingSystem"
{-# DEPRECATED faOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

-- | Fleet policy to limit the number of game sessions an individual player can create over a span of time.
--
-- /Note:/ Consider using 'resourceCreationLimitPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faResourceCreationLimitPolicy :: Lens.Lens' FleetAttributes (Core.Maybe Types.ResourceCreationLimitPolicy)
faResourceCreationLimitPolicy = Lens.field @"resourceCreationLimitPolicy"
{-# DEPRECATED faResourceCreationLimitPolicy "Use generic-lens or generic-optics with 'resourceCreationLimitPolicy' instead." #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift script resource that is deployed on instances in this fleet. In a GameLift script ARN, the resource ID matches the /ScriptId/ value.
--
-- /Note:/ Consider using 'scriptArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faScriptArn :: Lens.Lens' FleetAttributes (Core.Maybe Types.ScriptArn)
faScriptArn = Lens.field @"scriptArn"
{-# DEPRECATED faScriptArn "Use generic-lens or generic-optics with 'scriptArn' instead." #-}

-- | A unique identifier for a Realtime script.
--
-- /Note:/ Consider using 'scriptId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faScriptId :: Lens.Lens' FleetAttributes (Core.Maybe Types.ScriptId)
faScriptId = Lens.field @"scriptId"
{-# DEPRECATED faScriptId "Use generic-lens or generic-optics with 'scriptId' instead." #-}

-- | Game server launch parameters specified for fleets created before 2016-08-04 (or AWS SDK v. 0.12.16). Server launch parameters for fleets created after this date are specified in the fleet's 'RuntimeConfiguration' .
--
-- /Note:/ Consider using 'serverLaunchParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faServerLaunchParameters :: Lens.Lens' FleetAttributes (Core.Maybe Types.NonZeroAndMaxString)
faServerLaunchParameters = Lens.field @"serverLaunchParameters"
{-# DEPRECATED faServerLaunchParameters "Use generic-lens or generic-optics with 'serverLaunchParameters' instead." #-}

-- | Path to a game server executable in the fleet's build, specified for fleets created before 2016-08-04 (or AWS SDK v. 0.12.16). Server launch paths for fleets created after this date are specified in the fleet's 'RuntimeConfiguration' .
--
-- /Note:/ Consider using 'serverLaunchPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faServerLaunchPath :: Lens.Lens' FleetAttributes (Core.Maybe Types.NonZeroAndMaxString)
faServerLaunchPath = Lens.field @"serverLaunchPath"
{-# DEPRECATED faServerLaunchPath "Use generic-lens or generic-optics with 'serverLaunchPath' instead." #-}

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
faStatus :: Lens.Lens' FleetAttributes (Core.Maybe Types.FleetStatus)
faStatus = Lens.field @"status"
{-# DEPRECATED faStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | List of fleet activity that have been suspended using 'StopFleetActions' . This includes auto-scaling.
--
-- /Note:/ Consider using 'stoppedActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faStoppedActions :: Lens.Lens' FleetAttributes (Core.Maybe (Core.NonEmpty Types.FleetAction))
faStoppedActions = Lens.field @"stoppedActions"
{-# DEPRECATED faStoppedActions "Use generic-lens or generic-optics with 'stoppedActions' instead." #-}

-- | Time stamp indicating when this data object was terminated. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- /Note:/ Consider using 'terminationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faTerminationTime :: Lens.Lens' FleetAttributes (Core.Maybe Core.NominalDiffTime)
faTerminationTime = Lens.field @"terminationTime"
{-# DEPRECATED faTerminationTime "Use generic-lens or generic-optics with 'terminationTime' instead." #-}

instance Core.FromJSON FleetAttributes where
  parseJSON =
    Core.withObject "FleetAttributes" Core.$
      \x ->
        FleetAttributes'
          Core.<$> (x Core..:? "BuildArn")
          Core.<*> (x Core..:? "BuildId")
          Core.<*> (x Core..:? "CertificateConfiguration")
          Core.<*> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "FleetArn")
          Core.<*> (x Core..:? "FleetId")
          Core.<*> (x Core..:? "FleetType")
          Core.<*> (x Core..:? "InstanceRoleArn")
          Core.<*> (x Core..:? "InstanceType")
          Core.<*> (x Core..:? "LogPaths")
          Core.<*> (x Core..:? "MetricGroups")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "NewGameSessionProtectionPolicy")
          Core.<*> (x Core..:? "OperatingSystem")
          Core.<*> (x Core..:? "ResourceCreationLimitPolicy")
          Core.<*> (x Core..:? "ScriptArn")
          Core.<*> (x Core..:? "ScriptId")
          Core.<*> (x Core..:? "ServerLaunchParameters")
          Core.<*> (x Core..:? "ServerLaunchPath")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "StoppedActions")
          Core.<*> (x Core..:? "TerminationTime")
