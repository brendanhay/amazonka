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

-- | Describes a GameLift fleet of game hosting resources.
--
-- __Related actions__
--
-- CreateFleet | DescribeFleetAttributes
--
-- /See:/ 'newFleetAttributes' smart constructor.
data FleetAttributes = FleetAttributes'
  { -- | A time stamp indicating when this data object was created. Format is a
    -- number expressed in Unix time as milliseconds (for example
    -- @\"1469498468.057\"@).
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | Current status of the fleet. Possible fleet statuses include the
    -- following:
    --
    -- -   __NEW__ -- A new fleet has been defined and desired instances is set
    --     to 1.
    --
    -- -   __DOWNLOADING\/VALIDATING\/BUILDING\/ACTIVATING__ -- GameLift is
    --     setting up the new fleet, creating new instances with the game build
    --     or Realtime script and starting server processes.
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
    -- | __This parameter is no longer used.__ Server launch parameters are now
    -- defined using the fleet\'s RuntimeConfiguration parameter. Requests that
    -- use this parameter instead continue to be valid.
    serverLaunchParameters :: Prelude.Maybe Prelude.Text,
    -- | __This parameter is no longer used.__ Game session log paths are now
    -- defined using the GameLift server API @ProcessReady()@ @logParameters@.
    -- See more information in the
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api-ref.html#gamelift-sdk-server-api-ref-dataypes-process Server API Reference>.
    logPaths :: Prelude.Maybe [Prelude.Text],
    -- | The operating system of the fleet\'s computing resources. A fleet\'s
    -- operating system is determined by the OS of the build or script that is
    -- deployed on this fleet.
    operatingSystem :: Prelude.Maybe OperatingSystem,
    -- | A unique identifier for the build resource that is deployed on instances
    -- in this fleet.
    buildId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a GameLift fleet resource and uniquely identifies
    -- it. ARNs are unique across all Regions. Format is
    -- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
    -- In a GameLift fleet ARN, the resource ID matches the @FleetId@ value.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | The kind of instances, On-Demand or Spot, that this fleet uses.
    fleetType :: Prelude.Maybe FleetType,
    -- | A time stamp indicating when this data object was terminated. Format is
    -- a number expressed in Unix time as milliseconds (for example
    -- @\"1469498468.057\"@).
    terminationTime :: Prelude.Maybe Core.POSIX,
    -- | The EC2 instance type that determines the computing resources of each
    -- instance in the fleet. Instance type defines the CPU, memory, storage,
    -- and networking capacity. See
    -- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
    -- for detailed descriptions.
    instanceType :: Prelude.Maybe EC2InstanceType,
    -- | A list of fleet activity that has been suspended using StopFleetActions.
    -- This includes fleet auto-scaling.
    stoppedActions :: Prelude.Maybe (Prelude.NonEmpty FleetAction),
    -- | The type of game session protection to set on all new instances that are
    -- started in the fleet.
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
    -- | A unique identifier for the Realtime script resource that is deployed on
    -- instances in this fleet.
    scriptId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- associated with the GameLift script resource that is deployed on
    -- instances in this fleet. In a GameLift script ARN, the resource ID
    -- matches the @ScriptId@ value.
    scriptArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether a TLS\/SSL certificate was generated for the fleet.
    certificateConfiguration :: Prelude.Maybe CertificateConfiguration,
    -- | __This parameter is no longer used.__ Server launch paths are now
    -- defined using the fleet\'s RuntimeConfiguration parameter. Requests that
    -- use this parameter instead continue to be valid.
    serverLaunchPath :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for an AWS IAM role that manages access to your AWS
    -- services. With an instance role ARN set, any application that runs on an
    -- instance in this fleet can assume the role, including install scripts,
    -- server processes, and daemons (background processes). Create a role or
    -- look up a role\'s ARN by using the
    -- <https://console.aws.amazon.com/iam/ IAM dashboard> in the AWS
    -- Management Console. Learn more about using on-box credentials for your
    -- game servers at
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-resources.html Access external resources from a game server>.
    instanceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Name of a metric group that metrics for this fleet are added to. In
    -- Amazon CloudWatch, you can view aggregated metrics for fleets that are
    -- in a metric group. A fleet can be included in only one metric group at a
    -- time.
    metricGroups :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- associated with the GameLift build resource that is deployed on
    -- instances in this fleet. In a GameLift build ARN, the resource ID
    -- matches the @BuildId@ value.
    buildArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the fleet.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | A human-readable description of the fleet.
    description :: Prelude.Maybe Prelude.Text,
    -- | The fleet policy that limits the number of game sessions an individual
    -- player can create over a span of time.
    resourceCreationLimitPolicy :: Prelude.Maybe ResourceCreationLimitPolicy
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
-- 'creationTime', 'fleetAttributes_creationTime' - A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
--
-- 'status', 'fleetAttributes_status' - Current status of the fleet. Possible fleet statuses include the
-- following:
--
-- -   __NEW__ -- A new fleet has been defined and desired instances is set
--     to 1.
--
-- -   __DOWNLOADING\/VALIDATING\/BUILDING\/ACTIVATING__ -- GameLift is
--     setting up the new fleet, creating new instances with the game build
--     or Realtime script and starting server processes.
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
-- 'serverLaunchParameters', 'fleetAttributes_serverLaunchParameters' - __This parameter is no longer used.__ Server launch parameters are now
-- defined using the fleet\'s RuntimeConfiguration parameter. Requests that
-- use this parameter instead continue to be valid.
--
-- 'logPaths', 'fleetAttributes_logPaths' - __This parameter is no longer used.__ Game session log paths are now
-- defined using the GameLift server API @ProcessReady()@ @logParameters@.
-- See more information in the
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api-ref.html#gamelift-sdk-server-api-ref-dataypes-process Server API Reference>.
--
-- 'operatingSystem', 'fleetAttributes_operatingSystem' - The operating system of the fleet\'s computing resources. A fleet\'s
-- operating system is determined by the OS of the build or script that is
-- deployed on this fleet.
--
-- 'buildId', 'fleetAttributes_buildId' - A unique identifier for the build resource that is deployed on instances
-- in this fleet.
--
-- 'fleetArn', 'fleetAttributes_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
-- In a GameLift fleet ARN, the resource ID matches the @FleetId@ value.
--
-- 'fleetType', 'fleetAttributes_fleetType' - The kind of instances, On-Demand or Spot, that this fleet uses.
--
-- 'terminationTime', 'fleetAttributes_terminationTime' - A time stamp indicating when this data object was terminated. Format is
-- a number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
--
-- 'instanceType', 'fleetAttributes_instanceType' - The EC2 instance type that determines the computing resources of each
-- instance in the fleet. Instance type defines the CPU, memory, storage,
-- and networking capacity. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
-- for detailed descriptions.
--
-- 'stoppedActions', 'fleetAttributes_stoppedActions' - A list of fleet activity that has been suspended using StopFleetActions.
-- This includes fleet auto-scaling.
--
-- 'newGameSessionProtectionPolicy'', 'fleetAttributes_newGameSessionProtectionPolicy' - The type of game session protection to set on all new instances that are
-- started in the fleet.
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
-- 'scriptId', 'fleetAttributes_scriptId' - A unique identifier for the Realtime script resource that is deployed on
-- instances in this fleet.
--
-- 'scriptArn', 'fleetAttributes_scriptArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift script resource that is deployed on
-- instances in this fleet. In a GameLift script ARN, the resource ID
-- matches the @ScriptId@ value.
--
-- 'certificateConfiguration', 'fleetAttributes_certificateConfiguration' - Indicates whether a TLS\/SSL certificate was generated for the fleet.
--
-- 'serverLaunchPath', 'fleetAttributes_serverLaunchPath' - __This parameter is no longer used.__ Server launch paths are now
-- defined using the fleet\'s RuntimeConfiguration parameter. Requests that
-- use this parameter instead continue to be valid.
--
-- 'instanceRoleArn', 'fleetAttributes_instanceRoleArn' - A unique identifier for an AWS IAM role that manages access to your AWS
-- services. With an instance role ARN set, any application that runs on an
-- instance in this fleet can assume the role, including install scripts,
-- server processes, and daemons (background processes). Create a role or
-- look up a role\'s ARN by using the
-- <https://console.aws.amazon.com/iam/ IAM dashboard> in the AWS
-- Management Console. Learn more about using on-box credentials for your
-- game servers at
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-resources.html Access external resources from a game server>.
--
-- 'metricGroups', 'fleetAttributes_metricGroups' - Name of a metric group that metrics for this fleet are added to. In
-- Amazon CloudWatch, you can view aggregated metrics for fleets that are
-- in a metric group. A fleet can be included in only one metric group at a
-- time.
--
-- 'buildArn', 'fleetAttributes_buildArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift build resource that is deployed on
-- instances in this fleet. In a GameLift build ARN, the resource ID
-- matches the @BuildId@ value.
--
-- 'fleetId', 'fleetAttributes_fleetId' - A unique identifier for the fleet.
--
-- 'description', 'fleetAttributes_description' - A human-readable description of the fleet.
--
-- 'resourceCreationLimitPolicy', 'fleetAttributes_resourceCreationLimitPolicy' - The fleet policy that limits the number of game sessions an individual
-- player can create over a span of time.
newFleetAttributes ::
  FleetAttributes
newFleetAttributes =
  FleetAttributes'
    { creationTime = Prelude.Nothing,
      status = Prelude.Nothing,
      serverLaunchParameters = Prelude.Nothing,
      logPaths = Prelude.Nothing,
      operatingSystem = Prelude.Nothing,
      buildId = Prelude.Nothing,
      fleetArn = Prelude.Nothing,
      fleetType = Prelude.Nothing,
      terminationTime = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      stoppedActions = Prelude.Nothing,
      newGameSessionProtectionPolicy' = Prelude.Nothing,
      name = Prelude.Nothing,
      scriptId = Prelude.Nothing,
      scriptArn = Prelude.Nothing,
      certificateConfiguration = Prelude.Nothing,
      serverLaunchPath = Prelude.Nothing,
      instanceRoleArn = Prelude.Nothing,
      metricGroups = Prelude.Nothing,
      buildArn = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      description = Prelude.Nothing,
      resourceCreationLimitPolicy = Prelude.Nothing
    }

-- | A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
fleetAttributes_creationTime :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.UTCTime)
fleetAttributes_creationTime = Lens.lens (\FleetAttributes' {creationTime} -> creationTime) (\s@FleetAttributes' {} a -> s {creationTime = a} :: FleetAttributes) Prelude.. Lens.mapping Core._Time

-- | Current status of the fleet. Possible fleet statuses include the
-- following:
--
-- -   __NEW__ -- A new fleet has been defined and desired instances is set
--     to 1.
--
-- -   __DOWNLOADING\/VALIDATING\/BUILDING\/ACTIVATING__ -- GameLift is
--     setting up the new fleet, creating new instances with the game build
--     or Realtime script and starting server processes.
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

-- | __This parameter is no longer used.__ Server launch parameters are now
-- defined using the fleet\'s RuntimeConfiguration parameter. Requests that
-- use this parameter instead continue to be valid.
fleetAttributes_serverLaunchParameters :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_serverLaunchParameters = Lens.lens (\FleetAttributes' {serverLaunchParameters} -> serverLaunchParameters) (\s@FleetAttributes' {} a -> s {serverLaunchParameters = a} :: FleetAttributes)

-- | __This parameter is no longer used.__ Game session log paths are now
-- defined using the GameLift server API @ProcessReady()@ @logParameters@.
-- See more information in the
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api-ref.html#gamelift-sdk-server-api-ref-dataypes-process Server API Reference>.
fleetAttributes_logPaths :: Lens.Lens' FleetAttributes (Prelude.Maybe [Prelude.Text])
fleetAttributes_logPaths = Lens.lens (\FleetAttributes' {logPaths} -> logPaths) (\s@FleetAttributes' {} a -> s {logPaths = a} :: FleetAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The operating system of the fleet\'s computing resources. A fleet\'s
-- operating system is determined by the OS of the build or script that is
-- deployed on this fleet.
fleetAttributes_operatingSystem :: Lens.Lens' FleetAttributes (Prelude.Maybe OperatingSystem)
fleetAttributes_operatingSystem = Lens.lens (\FleetAttributes' {operatingSystem} -> operatingSystem) (\s@FleetAttributes' {} a -> s {operatingSystem = a} :: FleetAttributes)

-- | A unique identifier for the build resource that is deployed on instances
-- in this fleet.
fleetAttributes_buildId :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_buildId = Lens.lens (\FleetAttributes' {buildId} -> buildId) (\s@FleetAttributes' {} a -> s {buildId = a} :: FleetAttributes)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
-- In a GameLift fleet ARN, the resource ID matches the @FleetId@ value.
fleetAttributes_fleetArn :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_fleetArn = Lens.lens (\FleetAttributes' {fleetArn} -> fleetArn) (\s@FleetAttributes' {} a -> s {fleetArn = a} :: FleetAttributes)

-- | The kind of instances, On-Demand or Spot, that this fleet uses.
fleetAttributes_fleetType :: Lens.Lens' FleetAttributes (Prelude.Maybe FleetType)
fleetAttributes_fleetType = Lens.lens (\FleetAttributes' {fleetType} -> fleetType) (\s@FleetAttributes' {} a -> s {fleetType = a} :: FleetAttributes)

-- | A time stamp indicating when this data object was terminated. Format is
-- a number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
fleetAttributes_terminationTime :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.UTCTime)
fleetAttributes_terminationTime = Lens.lens (\FleetAttributes' {terminationTime} -> terminationTime) (\s@FleetAttributes' {} a -> s {terminationTime = a} :: FleetAttributes) Prelude.. Lens.mapping Core._Time

-- | The EC2 instance type that determines the computing resources of each
-- instance in the fleet. Instance type defines the CPU, memory, storage,
-- and networking capacity. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
-- for detailed descriptions.
fleetAttributes_instanceType :: Lens.Lens' FleetAttributes (Prelude.Maybe EC2InstanceType)
fleetAttributes_instanceType = Lens.lens (\FleetAttributes' {instanceType} -> instanceType) (\s@FleetAttributes' {} a -> s {instanceType = a} :: FleetAttributes)

-- | A list of fleet activity that has been suspended using StopFleetActions.
-- This includes fleet auto-scaling.
fleetAttributes_stoppedActions :: Lens.Lens' FleetAttributes (Prelude.Maybe (Prelude.NonEmpty FleetAction))
fleetAttributes_stoppedActions = Lens.lens (\FleetAttributes' {stoppedActions} -> stoppedActions) (\s@FleetAttributes' {} a -> s {stoppedActions = a} :: FleetAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The type of game session protection to set on all new instances that are
-- started in the fleet.
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

-- | A unique identifier for the Realtime script resource that is deployed on
-- instances in this fleet.
fleetAttributes_scriptId :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_scriptId = Lens.lens (\FleetAttributes' {scriptId} -> scriptId) (\s@FleetAttributes' {} a -> s {scriptId = a} :: FleetAttributes)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift script resource that is deployed on
-- instances in this fleet. In a GameLift script ARN, the resource ID
-- matches the @ScriptId@ value.
fleetAttributes_scriptArn :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_scriptArn = Lens.lens (\FleetAttributes' {scriptArn} -> scriptArn) (\s@FleetAttributes' {} a -> s {scriptArn = a} :: FleetAttributes)

-- | Indicates whether a TLS\/SSL certificate was generated for the fleet.
fleetAttributes_certificateConfiguration :: Lens.Lens' FleetAttributes (Prelude.Maybe CertificateConfiguration)
fleetAttributes_certificateConfiguration = Lens.lens (\FleetAttributes' {certificateConfiguration} -> certificateConfiguration) (\s@FleetAttributes' {} a -> s {certificateConfiguration = a} :: FleetAttributes)

-- | __This parameter is no longer used.__ Server launch paths are now
-- defined using the fleet\'s RuntimeConfiguration parameter. Requests that
-- use this parameter instead continue to be valid.
fleetAttributes_serverLaunchPath :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_serverLaunchPath = Lens.lens (\FleetAttributes' {serverLaunchPath} -> serverLaunchPath) (\s@FleetAttributes' {} a -> s {serverLaunchPath = a} :: FleetAttributes)

-- | A unique identifier for an AWS IAM role that manages access to your AWS
-- services. With an instance role ARN set, any application that runs on an
-- instance in this fleet can assume the role, including install scripts,
-- server processes, and daemons (background processes). Create a role or
-- look up a role\'s ARN by using the
-- <https://console.aws.amazon.com/iam/ IAM dashboard> in the AWS
-- Management Console. Learn more about using on-box credentials for your
-- game servers at
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-resources.html Access external resources from a game server>.
fleetAttributes_instanceRoleArn :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_instanceRoleArn = Lens.lens (\FleetAttributes' {instanceRoleArn} -> instanceRoleArn) (\s@FleetAttributes' {} a -> s {instanceRoleArn = a} :: FleetAttributes)

-- | Name of a metric group that metrics for this fleet are added to. In
-- Amazon CloudWatch, you can view aggregated metrics for fleets that are
-- in a metric group. A fleet can be included in only one metric group at a
-- time.
fleetAttributes_metricGroups :: Lens.Lens' FleetAttributes (Prelude.Maybe [Prelude.Text])
fleetAttributes_metricGroups = Lens.lens (\FleetAttributes' {metricGroups} -> metricGroups) (\s@FleetAttributes' {} a -> s {metricGroups = a} :: FleetAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift build resource that is deployed on
-- instances in this fleet. In a GameLift build ARN, the resource ID
-- matches the @BuildId@ value.
fleetAttributes_buildArn :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_buildArn = Lens.lens (\FleetAttributes' {buildArn} -> buildArn) (\s@FleetAttributes' {} a -> s {buildArn = a} :: FleetAttributes)

-- | A unique identifier for the fleet.
fleetAttributes_fleetId :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_fleetId = Lens.lens (\FleetAttributes' {fleetId} -> fleetId) (\s@FleetAttributes' {} a -> s {fleetId = a} :: FleetAttributes)

-- | A human-readable description of the fleet.
fleetAttributes_description :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_description = Lens.lens (\FleetAttributes' {description} -> description) (\s@FleetAttributes' {} a -> s {description = a} :: FleetAttributes)

-- | The fleet policy that limits the number of game sessions an individual
-- player can create over a span of time.
fleetAttributes_resourceCreationLimitPolicy :: Lens.Lens' FleetAttributes (Prelude.Maybe ResourceCreationLimitPolicy)
fleetAttributes_resourceCreationLimitPolicy = Lens.lens (\FleetAttributes' {resourceCreationLimitPolicy} -> resourceCreationLimitPolicy) (\s@FleetAttributes' {} a -> s {resourceCreationLimitPolicy = a} :: FleetAttributes)

instance Core.FromJSON FleetAttributes where
  parseJSON =
    Core.withObject
      "FleetAttributes"
      ( \x ->
          FleetAttributes'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ServerLaunchParameters")
            Prelude.<*> (x Core..:? "LogPaths" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "OperatingSystem")
            Prelude.<*> (x Core..:? "BuildId")
            Prelude.<*> (x Core..:? "FleetArn")
            Prelude.<*> (x Core..:? "FleetType")
            Prelude.<*> (x Core..:? "TerminationTime")
            Prelude.<*> (x Core..:? "InstanceType")
            Prelude.<*> (x Core..:? "StoppedActions")
            Prelude.<*> (x Core..:? "NewGameSessionProtectionPolicy")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "ScriptId")
            Prelude.<*> (x Core..:? "ScriptArn")
            Prelude.<*> (x Core..:? "CertificateConfiguration")
            Prelude.<*> (x Core..:? "ServerLaunchPath")
            Prelude.<*> (x Core..:? "InstanceRoleArn")
            Prelude.<*> (x Core..:? "MetricGroups" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "BuildArn")
            Prelude.<*> (x Core..:? "FleetId")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "ResourceCreationLimitPolicy")
      )

instance Prelude.Hashable FleetAttributes

instance Prelude.NFData FleetAttributes
