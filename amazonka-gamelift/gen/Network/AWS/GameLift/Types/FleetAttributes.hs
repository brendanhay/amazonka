{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.FleetAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.FleetAttributes where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types.CertificateConfiguration
import Network.AWS.GameLift.Types.EC2InstanceType
import Network.AWS.GameLift.Types.FleetAction
import Network.AWS.GameLift.Types.FleetStatus
import Network.AWS.GameLift.Types.FleetType
import Network.AWS.GameLift.Types.OperatingSystem
import Network.AWS.GameLift.Types.ProtectionPolicy
import Network.AWS.GameLift.Types.ResourceCreationLimitPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | General properties describing a fleet.
--
-- -   CreateFleet
--
-- -   ListFleets
--
-- -   DeleteFleet
--
-- -   DescribeFleetAttributes
--
-- -   UpdateFleetAttributes
--
-- -   StartFleetActions or StopFleetActions
--
-- /See:/ 'newFleetAttributes' smart constructor.
data FleetAttributes = FleetAttributes'
  { -- | Current status of the fleet.
    --
    -- Possible fleet statuses include the following:
    --
    -- -   __NEW__ -- A new fleet has been defined and desired instances is set
    --     to 1.
    --
    -- -   __DOWNLOADING\/VALIDATING\/BUILDING\/ACTIVATING__ -- Amazon GameLift
    --     is setting up the new fleet, creating new instances with the game
    --     build or Realtime script and starting server processes.
    --
    -- -   __ACTIVE__ -- Hosts can now accept game sessions.
    --
    -- -   __ERROR__ -- An error occurred when downloading, validating,
    --     building, or activating the fleet.
    --
    -- -   __DELETING__ -- Hosts are responding to a delete fleet request.
    --
    -- -   __TERMINATED__ -- The fleet no longer exists.
    status :: Prelude.Maybe FleetStatus,
    -- | Time stamp indicating when this data object was created. Format is a
    -- number expressed in Unix time as milliseconds (for example
    -- \"1469498468.057\").
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | EC2 instance type indicating the computing resources of each instance in
    -- the fleet, including CPU, memory, storage, and networking capacity. See
    -- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
    -- for detailed descriptions.
    instanceType :: Prelude.Maybe EC2InstanceType,
    -- | Indicates whether the fleet uses on-demand or spot instances. A spot
    -- instance in use may be interrupted with a two-minute notification.
    fleetType :: Prelude.Maybe FleetType,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
    -- that is assigned to a GameLift fleet resource and uniquely identifies
    -- it. ARNs are unique across all Regions. In a GameLift fleet ARN, the
    -- resource ID matches the /FleetId/ value.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a fleet.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for an AWS IAM role that manages access to your AWS
    -- services.
    instanceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether a TLS\/SSL certificate was generated for the fleet.
    certificateConfiguration :: Prelude.Maybe CertificateConfiguration,
    -- | Path to a game server executable in the fleet\'s build, specified for
    -- fleets created before 2016-08-04 (or AWS SDK v. 0.12.16). Server launch
    -- paths for fleets created after this date are specified in the fleet\'s
    -- RuntimeConfiguration.
    serverLaunchPath :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
    -- associated with the GameLift script resource that is deployed on
    -- instances in this fleet. In a GameLift script ARN, the resource ID
    -- matches the /ScriptId/ value.
    scriptArn :: Prelude.Maybe Prelude.Text,
    -- | Game server launch parameters specified for fleets created before
    -- 2016-08-04 (or AWS SDK v. 0.12.16). Server launch parameters for fleets
    -- created after this date are specified in the fleet\'s
    -- RuntimeConfiguration.
    serverLaunchParameters :: Prelude.Maybe Prelude.Text,
    -- | Location of default log files. When a server process is shut down,
    -- Amazon GameLift captures and stores any log files in this location.
    -- These logs are in addition to game session logs; see more on game
    -- session logs in the
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-api-server-code Amazon GameLift Developer Guide>.
    -- If no default log path for a fleet is specified, Amazon GameLift
    -- automatically uploads logs that are stored on each instance at
    -- @C:\\game\\logs@ (for Windows) or @\/local\/game\/logs@ (for Linux). Use
    -- the Amazon GameLift console to access stored logs.
    logPaths :: Prelude.Maybe [Prelude.Text],
    -- | The type of game session protection to set for all new instances started
    -- in the fleet.
    --
    -- -   __NoProtection__ -- The game session can be terminated during a
    --     scale-down event.
    --
    -- -   __FullProtection__ -- If the game session is in an @ACTIVE@ status,
    --     it cannot be terminated during a scale-down event.
    newGameSessionProtectionPolicy' :: Prelude.Maybe ProtectionPolicy,
    -- | A descriptive label that is associated with a fleet. Fleet names do not
    -- need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | List of fleet activity that have been suspended using StopFleetActions.
    -- This includes auto-scaling.
    stoppedActions :: Prelude.Maybe (Prelude.NonEmpty FleetAction),
    -- | Time stamp indicating when this data object was terminated. Format is a
    -- number expressed in Unix time as milliseconds (for example
    -- \"1469498468.057\").
    terminationTime :: Prelude.Maybe Core.POSIX,
    -- | Human-readable description of the fleet.
    description :: Prelude.Maybe Prelude.Text,
    -- | Fleet policy to limit the number of game sessions an individual player
    -- can create over a span of time.
    resourceCreationLimitPolicy :: Prelude.Maybe ResourceCreationLimitPolicy,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
    -- associated with the GameLift build resource that is deployed on
    -- instances in this fleet. In a GameLift build ARN, the resource ID
    -- matches the /BuildId/ value.
    buildArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a build.
    buildId :: Prelude.Maybe Prelude.Text,
    -- | Operating system of the fleet\'s computing resources. A fleet\'s
    -- operating system depends on the OS specified for the build that is
    -- deployed on this fleet.
    operatingSystem :: Prelude.Maybe OperatingSystem,
    -- | Names of metric groups that this fleet is included in. In Amazon
    -- CloudWatch, you can view metrics for an individual fleet or aggregated
    -- metrics for fleets that are in a fleet metric group. A fleet can be
    -- included in only one metric group at a time.
    metricGroups :: Prelude.Maybe [Prelude.Text],
    -- | A unique identifier for a Realtime script.
    scriptId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FleetAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'fleetAttributes_status' - Current status of the fleet.
--
-- Possible fleet statuses include the following:
--
-- -   __NEW__ -- A new fleet has been defined and desired instances is set
--     to 1.
--
-- -   __DOWNLOADING\/VALIDATING\/BUILDING\/ACTIVATING__ -- Amazon GameLift
--     is setting up the new fleet, creating new instances with the game
--     build or Realtime script and starting server processes.
--
-- -   __ACTIVE__ -- Hosts can now accept game sessions.
--
-- -   __ERROR__ -- An error occurred when downloading, validating,
--     building, or activating the fleet.
--
-- -   __DELETING__ -- Hosts are responding to a delete fleet request.
--
-- -   __TERMINATED__ -- The fleet no longer exists.
--
-- 'creationTime', 'fleetAttributes_creationTime' - Time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
--
-- 'instanceType', 'fleetAttributes_instanceType' - EC2 instance type indicating the computing resources of each instance in
-- the fleet, including CPU, memory, storage, and networking capacity. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
-- for detailed descriptions.
--
-- 'fleetType', 'fleetAttributes_fleetType' - Indicates whether the fleet uses on-demand or spot instances. A spot
-- instance in use may be interrupted with a two-minute notification.
--
-- 'fleetArn', 'fleetAttributes_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. In a GameLift fleet ARN, the
-- resource ID matches the /FleetId/ value.
--
-- 'fleetId', 'fleetAttributes_fleetId' - A unique identifier for a fleet.
--
-- 'instanceRoleArn', 'fleetAttributes_instanceRoleArn' - A unique identifier for an AWS IAM role that manages access to your AWS
-- services.
--
-- 'certificateConfiguration', 'fleetAttributes_certificateConfiguration' - Indicates whether a TLS\/SSL certificate was generated for the fleet.
--
-- 'serverLaunchPath', 'fleetAttributes_serverLaunchPath' - Path to a game server executable in the fleet\'s build, specified for
-- fleets created before 2016-08-04 (or AWS SDK v. 0.12.16). Server launch
-- paths for fleets created after this date are specified in the fleet\'s
-- RuntimeConfiguration.
--
-- 'scriptArn', 'fleetAttributes_scriptArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- associated with the GameLift script resource that is deployed on
-- instances in this fleet. In a GameLift script ARN, the resource ID
-- matches the /ScriptId/ value.
--
-- 'serverLaunchParameters', 'fleetAttributes_serverLaunchParameters' - Game server launch parameters specified for fleets created before
-- 2016-08-04 (or AWS SDK v. 0.12.16). Server launch parameters for fleets
-- created after this date are specified in the fleet\'s
-- RuntimeConfiguration.
--
-- 'logPaths', 'fleetAttributes_logPaths' - Location of default log files. When a server process is shut down,
-- Amazon GameLift captures and stores any log files in this location.
-- These logs are in addition to game session logs; see more on game
-- session logs in the
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-api-server-code Amazon GameLift Developer Guide>.
-- If no default log path for a fleet is specified, Amazon GameLift
-- automatically uploads logs that are stored on each instance at
-- @C:\\game\\logs@ (for Windows) or @\/local\/game\/logs@ (for Linux). Use
-- the Amazon GameLift console to access stored logs.
--
-- 'newGameSessionProtectionPolicy'', 'fleetAttributes_newGameSessionProtectionPolicy' - The type of game session protection to set for all new instances started
-- in the fleet.
--
-- -   __NoProtection__ -- The game session can be terminated during a
--     scale-down event.
--
-- -   __FullProtection__ -- If the game session is in an @ACTIVE@ status,
--     it cannot be terminated during a scale-down event.
--
-- 'name', 'fleetAttributes_name' - A descriptive label that is associated with a fleet. Fleet names do not
-- need to be unique.
--
-- 'stoppedActions', 'fleetAttributes_stoppedActions' - List of fleet activity that have been suspended using StopFleetActions.
-- This includes auto-scaling.
--
-- 'terminationTime', 'fleetAttributes_terminationTime' - Time stamp indicating when this data object was terminated. Format is a
-- number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
--
-- 'description', 'fleetAttributes_description' - Human-readable description of the fleet.
--
-- 'resourceCreationLimitPolicy', 'fleetAttributes_resourceCreationLimitPolicy' - Fleet policy to limit the number of game sessions an individual player
-- can create over a span of time.
--
-- 'buildArn', 'fleetAttributes_buildArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- associated with the GameLift build resource that is deployed on
-- instances in this fleet. In a GameLift build ARN, the resource ID
-- matches the /BuildId/ value.
--
-- 'buildId', 'fleetAttributes_buildId' - A unique identifier for a build.
--
-- 'operatingSystem', 'fleetAttributes_operatingSystem' - Operating system of the fleet\'s computing resources. A fleet\'s
-- operating system depends on the OS specified for the build that is
-- deployed on this fleet.
--
-- 'metricGroups', 'fleetAttributes_metricGroups' - Names of metric groups that this fleet is included in. In Amazon
-- CloudWatch, you can view metrics for an individual fleet or aggregated
-- metrics for fleets that are in a fleet metric group. A fleet can be
-- included in only one metric group at a time.
--
-- 'scriptId', 'fleetAttributes_scriptId' - A unique identifier for a Realtime script.
newFleetAttributes ::
  FleetAttributes
newFleetAttributes =
  FleetAttributes'
    { status = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      fleetType = Prelude.Nothing,
      fleetArn = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      instanceRoleArn = Prelude.Nothing,
      certificateConfiguration = Prelude.Nothing,
      serverLaunchPath = Prelude.Nothing,
      scriptArn = Prelude.Nothing,
      serverLaunchParameters = Prelude.Nothing,
      logPaths = Prelude.Nothing,
      newGameSessionProtectionPolicy' = Prelude.Nothing,
      name = Prelude.Nothing,
      stoppedActions = Prelude.Nothing,
      terminationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      resourceCreationLimitPolicy = Prelude.Nothing,
      buildArn = Prelude.Nothing,
      buildId = Prelude.Nothing,
      operatingSystem = Prelude.Nothing,
      metricGroups = Prelude.Nothing,
      scriptId = Prelude.Nothing
    }

-- | Current status of the fleet.
--
-- Possible fleet statuses include the following:
--
-- -   __NEW__ -- A new fleet has been defined and desired instances is set
--     to 1.
--
-- -   __DOWNLOADING\/VALIDATING\/BUILDING\/ACTIVATING__ -- Amazon GameLift
--     is setting up the new fleet, creating new instances with the game
--     build or Realtime script and starting server processes.
--
-- -   __ACTIVE__ -- Hosts can now accept game sessions.
--
-- -   __ERROR__ -- An error occurred when downloading, validating,
--     building, or activating the fleet.
--
-- -   __DELETING__ -- Hosts are responding to a delete fleet request.
--
-- -   __TERMINATED__ -- The fleet no longer exists.
fleetAttributes_status :: Lens.Lens' FleetAttributes (Prelude.Maybe FleetStatus)
fleetAttributes_status = Lens.lens (\FleetAttributes' {status} -> status) (\s@FleetAttributes' {} a -> s {status = a} :: FleetAttributes)

-- | Time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
fleetAttributes_creationTime :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.UTCTime)
fleetAttributes_creationTime = Lens.lens (\FleetAttributes' {creationTime} -> creationTime) (\s@FleetAttributes' {} a -> s {creationTime = a} :: FleetAttributes) Prelude.. Lens.mapping Core._Time

-- | EC2 instance type indicating the computing resources of each instance in
-- the fleet, including CPU, memory, storage, and networking capacity. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
-- for detailed descriptions.
fleetAttributes_instanceType :: Lens.Lens' FleetAttributes (Prelude.Maybe EC2InstanceType)
fleetAttributes_instanceType = Lens.lens (\FleetAttributes' {instanceType} -> instanceType) (\s@FleetAttributes' {} a -> s {instanceType = a} :: FleetAttributes)

-- | Indicates whether the fleet uses on-demand or spot instances. A spot
-- instance in use may be interrupted with a two-minute notification.
fleetAttributes_fleetType :: Lens.Lens' FleetAttributes (Prelude.Maybe FleetType)
fleetAttributes_fleetType = Lens.lens (\FleetAttributes' {fleetType} -> fleetType) (\s@FleetAttributes' {} a -> s {fleetType = a} :: FleetAttributes)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. In a GameLift fleet ARN, the
-- resource ID matches the /FleetId/ value.
fleetAttributes_fleetArn :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_fleetArn = Lens.lens (\FleetAttributes' {fleetArn} -> fleetArn) (\s@FleetAttributes' {} a -> s {fleetArn = a} :: FleetAttributes)

-- | A unique identifier for a fleet.
fleetAttributes_fleetId :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_fleetId = Lens.lens (\FleetAttributes' {fleetId} -> fleetId) (\s@FleetAttributes' {} a -> s {fleetId = a} :: FleetAttributes)

-- | A unique identifier for an AWS IAM role that manages access to your AWS
-- services.
fleetAttributes_instanceRoleArn :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_instanceRoleArn = Lens.lens (\FleetAttributes' {instanceRoleArn} -> instanceRoleArn) (\s@FleetAttributes' {} a -> s {instanceRoleArn = a} :: FleetAttributes)

-- | Indicates whether a TLS\/SSL certificate was generated for the fleet.
fleetAttributes_certificateConfiguration :: Lens.Lens' FleetAttributes (Prelude.Maybe CertificateConfiguration)
fleetAttributes_certificateConfiguration = Lens.lens (\FleetAttributes' {certificateConfiguration} -> certificateConfiguration) (\s@FleetAttributes' {} a -> s {certificateConfiguration = a} :: FleetAttributes)

-- | Path to a game server executable in the fleet\'s build, specified for
-- fleets created before 2016-08-04 (or AWS SDK v. 0.12.16). Server launch
-- paths for fleets created after this date are specified in the fleet\'s
-- RuntimeConfiguration.
fleetAttributes_serverLaunchPath :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_serverLaunchPath = Lens.lens (\FleetAttributes' {serverLaunchPath} -> serverLaunchPath) (\s@FleetAttributes' {} a -> s {serverLaunchPath = a} :: FleetAttributes)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- associated with the GameLift script resource that is deployed on
-- instances in this fleet. In a GameLift script ARN, the resource ID
-- matches the /ScriptId/ value.
fleetAttributes_scriptArn :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_scriptArn = Lens.lens (\FleetAttributes' {scriptArn} -> scriptArn) (\s@FleetAttributes' {} a -> s {scriptArn = a} :: FleetAttributes)

-- | Game server launch parameters specified for fleets created before
-- 2016-08-04 (or AWS SDK v. 0.12.16). Server launch parameters for fleets
-- created after this date are specified in the fleet\'s
-- RuntimeConfiguration.
fleetAttributes_serverLaunchParameters :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_serverLaunchParameters = Lens.lens (\FleetAttributes' {serverLaunchParameters} -> serverLaunchParameters) (\s@FleetAttributes' {} a -> s {serverLaunchParameters = a} :: FleetAttributes)

-- | Location of default log files. When a server process is shut down,
-- Amazon GameLift captures and stores any log files in this location.
-- These logs are in addition to game session logs; see more on game
-- session logs in the
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-api-server-code Amazon GameLift Developer Guide>.
-- If no default log path for a fleet is specified, Amazon GameLift
-- automatically uploads logs that are stored on each instance at
-- @C:\\game\\logs@ (for Windows) or @\/local\/game\/logs@ (for Linux). Use
-- the Amazon GameLift console to access stored logs.
fleetAttributes_logPaths :: Lens.Lens' FleetAttributes (Prelude.Maybe [Prelude.Text])
fleetAttributes_logPaths = Lens.lens (\FleetAttributes' {logPaths} -> logPaths) (\s@FleetAttributes' {} a -> s {logPaths = a} :: FleetAttributes) Prelude.. Lens.mapping Lens._Coerce

-- | The type of game session protection to set for all new instances started
-- in the fleet.
--
-- -   __NoProtection__ -- The game session can be terminated during a
--     scale-down event.
--
-- -   __FullProtection__ -- If the game session is in an @ACTIVE@ status,
--     it cannot be terminated during a scale-down event.
fleetAttributes_newGameSessionProtectionPolicy :: Lens.Lens' FleetAttributes (Prelude.Maybe ProtectionPolicy)
fleetAttributes_newGameSessionProtectionPolicy = Lens.lens (\FleetAttributes' {newGameSessionProtectionPolicy'} -> newGameSessionProtectionPolicy') (\s@FleetAttributes' {} a -> s {newGameSessionProtectionPolicy' = a} :: FleetAttributes)

-- | A descriptive label that is associated with a fleet. Fleet names do not
-- need to be unique.
fleetAttributes_name :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_name = Lens.lens (\FleetAttributes' {name} -> name) (\s@FleetAttributes' {} a -> s {name = a} :: FleetAttributes)

-- | List of fleet activity that have been suspended using StopFleetActions.
-- This includes auto-scaling.
fleetAttributes_stoppedActions :: Lens.Lens' FleetAttributes (Prelude.Maybe (Prelude.NonEmpty FleetAction))
fleetAttributes_stoppedActions = Lens.lens (\FleetAttributes' {stoppedActions} -> stoppedActions) (\s@FleetAttributes' {} a -> s {stoppedActions = a} :: FleetAttributes) Prelude.. Lens.mapping Lens._Coerce

-- | Time stamp indicating when this data object was terminated. Format is a
-- number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
fleetAttributes_terminationTime :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.UTCTime)
fleetAttributes_terminationTime = Lens.lens (\FleetAttributes' {terminationTime} -> terminationTime) (\s@FleetAttributes' {} a -> s {terminationTime = a} :: FleetAttributes) Prelude.. Lens.mapping Core._Time

-- | Human-readable description of the fleet.
fleetAttributes_description :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_description = Lens.lens (\FleetAttributes' {description} -> description) (\s@FleetAttributes' {} a -> s {description = a} :: FleetAttributes)

-- | Fleet policy to limit the number of game sessions an individual player
-- can create over a span of time.
fleetAttributes_resourceCreationLimitPolicy :: Lens.Lens' FleetAttributes (Prelude.Maybe ResourceCreationLimitPolicy)
fleetAttributes_resourceCreationLimitPolicy = Lens.lens (\FleetAttributes' {resourceCreationLimitPolicy} -> resourceCreationLimitPolicy) (\s@FleetAttributes' {} a -> s {resourceCreationLimitPolicy = a} :: FleetAttributes)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- associated with the GameLift build resource that is deployed on
-- instances in this fleet. In a GameLift build ARN, the resource ID
-- matches the /BuildId/ value.
fleetAttributes_buildArn :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_buildArn = Lens.lens (\FleetAttributes' {buildArn} -> buildArn) (\s@FleetAttributes' {} a -> s {buildArn = a} :: FleetAttributes)

-- | A unique identifier for a build.
fleetAttributes_buildId :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_buildId = Lens.lens (\FleetAttributes' {buildId} -> buildId) (\s@FleetAttributes' {} a -> s {buildId = a} :: FleetAttributes)

-- | Operating system of the fleet\'s computing resources. A fleet\'s
-- operating system depends on the OS specified for the build that is
-- deployed on this fleet.
fleetAttributes_operatingSystem :: Lens.Lens' FleetAttributes (Prelude.Maybe OperatingSystem)
fleetAttributes_operatingSystem = Lens.lens (\FleetAttributes' {operatingSystem} -> operatingSystem) (\s@FleetAttributes' {} a -> s {operatingSystem = a} :: FleetAttributes)

-- | Names of metric groups that this fleet is included in. In Amazon
-- CloudWatch, you can view metrics for an individual fleet or aggregated
-- metrics for fleets that are in a fleet metric group. A fleet can be
-- included in only one metric group at a time.
fleetAttributes_metricGroups :: Lens.Lens' FleetAttributes (Prelude.Maybe [Prelude.Text])
fleetAttributes_metricGroups = Lens.lens (\FleetAttributes' {metricGroups} -> metricGroups) (\s@FleetAttributes' {} a -> s {metricGroups = a} :: FleetAttributes) Prelude.. Lens.mapping Lens._Coerce

-- | A unique identifier for a Realtime script.
fleetAttributes_scriptId :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_scriptId = Lens.lens (\FleetAttributes' {scriptId} -> scriptId) (\s@FleetAttributes' {} a -> s {scriptId = a} :: FleetAttributes)

instance Core.FromJSON FleetAttributes where
  parseJSON =
    Core.withObject
      "FleetAttributes"
      ( \x ->
          FleetAttributes'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "InstanceType")
            Prelude.<*> (x Core..:? "FleetType")
            Prelude.<*> (x Core..:? "FleetArn")
            Prelude.<*> (x Core..:? "FleetId")
            Prelude.<*> (x Core..:? "InstanceRoleArn")
            Prelude.<*> (x Core..:? "CertificateConfiguration")
            Prelude.<*> (x Core..:? "ServerLaunchPath")
            Prelude.<*> (x Core..:? "ScriptArn")
            Prelude.<*> (x Core..:? "ServerLaunchParameters")
            Prelude.<*> (x Core..:? "LogPaths" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "NewGameSessionProtectionPolicy")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "StoppedActions")
            Prelude.<*> (x Core..:? "TerminationTime")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "ResourceCreationLimitPolicy")
            Prelude.<*> (x Core..:? "BuildArn")
            Prelude.<*> (x Core..:? "BuildId")
            Prelude.<*> (x Core..:? "OperatingSystem")
            Prelude.<*> (x Core..:? "MetricGroups" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ScriptId")
      )

instance Prelude.Hashable FleetAttributes

instance Prelude.NFData FleetAttributes
