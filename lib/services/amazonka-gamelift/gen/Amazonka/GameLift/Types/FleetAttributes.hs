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
-- Module      : Amazonka.GameLift.Types.FleetAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.FleetAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GameLift.Types.CertificateConfiguration
import Amazonka.GameLift.Types.EC2InstanceType
import Amazonka.GameLift.Types.FleetAction
import Amazonka.GameLift.Types.FleetStatus
import Amazonka.GameLift.Types.FleetType
import Amazonka.GameLift.Types.OperatingSystem
import Amazonka.GameLift.Types.ProtectionPolicy
import Amazonka.GameLift.Types.ResourceCreationLimitPolicy
import qualified Amazonka.Prelude as Prelude

-- | Describes a GameLift fleet of game hosting resources.
--
-- __Related actions__
--
-- CreateFleet | DescribeFleetAttributes
--
-- /See:/ 'newFleetAttributes' smart constructor.
data FleetAttributes = FleetAttributes'
  { -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- associated with the GameLift script resource that is deployed on
    -- instances in this fleet. In a GameLift script ARN, the resource ID
    -- matches the @ScriptId@ value.
    scriptArn :: Prelude.Maybe Prelude.Text,
    -- | The operating system of the fleet\'s computing resources. A fleet\'s
    -- operating system is determined by the OS of the build or script that is
    -- deployed on this fleet.
    operatingSystem :: Prelude.Maybe OperatingSystem,
    -- | __This parameter is no longer used.__ Server launch paths are now
    -- defined using the fleet\'s RuntimeConfiguration parameter. Requests that
    -- use this parameter instead continue to be valid.
    serverLaunchPath :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the fleet.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | A descriptive label that is associated with a fleet. Fleet names do not
    -- need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | The kind of instances, On-Demand or Spot, that this fleet uses.
    fleetType :: Prelude.Maybe FleetType,
    -- | Indicates whether a TLS\/SSL certificate was generated for the fleet.
    certificateConfiguration :: Prelude.Maybe CertificateConfiguration,
    -- | A unique identifier for an IAM role that manages access to your Amazon
    -- Web Services services. With an instance role ARN set, any application
    -- that runs on an instance in this fleet can assume the role, including
    -- install scripts, server processes, and daemons (background processes).
    -- Create a role or look up a role\'s ARN by using the
    -- <https://console.aws.amazon.com/iam/ IAM dashboard> in the Amazon Web
    -- Services Management Console. Learn more about using on-box credentials
    -- for your game servers at
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-resources.html Access external resources from a game server>.
    instanceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the build resource that is deployed on instances
    -- in this fleet.
    buildId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- associated with the GameLift build resource that is deployed on
    -- instances in this fleet. In a GameLift build ARN, the resource ID
    -- matches the @BuildId@ value.
    buildArn :: Prelude.Maybe Prelude.Text,
    -- | The type of game session protection to set on all new instances that are
    -- started in the fleet.
    --
    -- -   __NoProtection__ -- The game session can be terminated during a
    --     scale-down event.
    --
    -- -   __FullProtection__ -- If the game session is in an @ACTIVE@ status,
    --     it cannot be terminated during a scale-down event.
    newGameSessionProtectionPolicy' :: Prelude.Maybe ProtectionPolicy,
    -- | A list of fleet activity that has been suspended using StopFleetActions.
    -- This includes fleet auto-scaling.
    stoppedActions :: Prelude.Maybe (Prelude.NonEmpty FleetAction),
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
    -- | A human-readable description of the fleet.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon EC2 instance type that determines the computing resources of
    -- each instance in the fleet. Instance type defines the CPU, memory,
    -- storage, and networking capacity. See
    -- <http://aws.amazon.com/ec2/instance-types/ Amazon Elastic Compute Cloud Instance Types>
    -- for detailed descriptions.
    instanceType :: Prelude.Maybe EC2InstanceType,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a GameLift fleet resource and uniquely identifies
    -- it. ARNs are unique across all Regions. Format is
    -- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
    -- In a GameLift fleet ARN, the resource ID matches the @FleetId@ value.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | A time stamp indicating when this data object was terminated. Format is
    -- a number expressed in Unix time as milliseconds (for example
    -- @\"1469498468.057\"@).
    terminationTime :: Prelude.Maybe Core.POSIX,
    -- | __This parameter is no longer used.__ Game session log paths are now
    -- defined using the GameLift server API @ProcessReady()@ @logParameters@.
    -- See more information in the
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api-ref.html#gamelift-sdk-server-api-ref-dataypes-process Server API Reference>.
    logPaths :: Prelude.Maybe [Prelude.Text],
    -- | A time stamp indicating when this data object was created. Format is a
    -- number expressed in Unix time as milliseconds (for example
    -- @\"1469498468.057\"@).
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | Name of a metric group that metrics for this fleet are added to. In
    -- Amazon CloudWatch, you can view aggregated metrics for fleets that are
    -- in a metric group. A fleet can be included in only one metric group at a
    -- time.
    metricGroups :: Prelude.Maybe [Prelude.Text],
    -- | __This parameter is no longer used.__ Server launch parameters are now
    -- defined using the fleet\'s RuntimeConfiguration parameter. Requests that
    -- use this parameter instead continue to be valid.
    serverLaunchParameters :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the Realtime script resource that is deployed on
    -- instances in this fleet.
    scriptId :: Prelude.Maybe Prelude.Text,
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
-- 'scriptArn', 'fleetAttributes_scriptArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift script resource that is deployed on
-- instances in this fleet. In a GameLift script ARN, the resource ID
-- matches the @ScriptId@ value.
--
-- 'operatingSystem', 'fleetAttributes_operatingSystem' - The operating system of the fleet\'s computing resources. A fleet\'s
-- operating system is determined by the OS of the build or script that is
-- deployed on this fleet.
--
-- 'serverLaunchPath', 'fleetAttributes_serverLaunchPath' - __This parameter is no longer used.__ Server launch paths are now
-- defined using the fleet\'s RuntimeConfiguration parameter. Requests that
-- use this parameter instead continue to be valid.
--
-- 'fleetId', 'fleetAttributes_fleetId' - A unique identifier for the fleet.
--
-- 'name', 'fleetAttributes_name' - A descriptive label that is associated with a fleet. Fleet names do not
-- need to be unique.
--
-- 'fleetType', 'fleetAttributes_fleetType' - The kind of instances, On-Demand or Spot, that this fleet uses.
--
-- 'certificateConfiguration', 'fleetAttributes_certificateConfiguration' - Indicates whether a TLS\/SSL certificate was generated for the fleet.
--
-- 'instanceRoleArn', 'fleetAttributes_instanceRoleArn' - A unique identifier for an IAM role that manages access to your Amazon
-- Web Services services. With an instance role ARN set, any application
-- that runs on an instance in this fleet can assume the role, including
-- install scripts, server processes, and daemons (background processes).
-- Create a role or look up a role\'s ARN by using the
-- <https://console.aws.amazon.com/iam/ IAM dashboard> in the Amazon Web
-- Services Management Console. Learn more about using on-box credentials
-- for your game servers at
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-resources.html Access external resources from a game server>.
--
-- 'buildId', 'fleetAttributes_buildId' - A unique identifier for the build resource that is deployed on instances
-- in this fleet.
--
-- 'buildArn', 'fleetAttributes_buildArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift build resource that is deployed on
-- instances in this fleet. In a GameLift build ARN, the resource ID
-- matches the @BuildId@ value.
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
-- 'stoppedActions', 'fleetAttributes_stoppedActions' - A list of fleet activity that has been suspended using StopFleetActions.
-- This includes fleet auto-scaling.
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
-- 'description', 'fleetAttributes_description' - A human-readable description of the fleet.
--
-- 'instanceType', 'fleetAttributes_instanceType' - The Amazon EC2 instance type that determines the computing resources of
-- each instance in the fleet. Instance type defines the CPU, memory,
-- storage, and networking capacity. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon Elastic Compute Cloud Instance Types>
-- for detailed descriptions.
--
-- 'fleetArn', 'fleetAttributes_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
-- In a GameLift fleet ARN, the resource ID matches the @FleetId@ value.
--
-- 'terminationTime', 'fleetAttributes_terminationTime' - A time stamp indicating when this data object was terminated. Format is
-- a number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
--
-- 'logPaths', 'fleetAttributes_logPaths' - __This parameter is no longer used.__ Game session log paths are now
-- defined using the GameLift server API @ProcessReady()@ @logParameters@.
-- See more information in the
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api-ref.html#gamelift-sdk-server-api-ref-dataypes-process Server API Reference>.
--
-- 'creationTime', 'fleetAttributes_creationTime' - A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
--
-- 'metricGroups', 'fleetAttributes_metricGroups' - Name of a metric group that metrics for this fleet are added to. In
-- Amazon CloudWatch, you can view aggregated metrics for fleets that are
-- in a metric group. A fleet can be included in only one metric group at a
-- time.
--
-- 'serverLaunchParameters', 'fleetAttributes_serverLaunchParameters' - __This parameter is no longer used.__ Server launch parameters are now
-- defined using the fleet\'s RuntimeConfiguration parameter. Requests that
-- use this parameter instead continue to be valid.
--
-- 'scriptId', 'fleetAttributes_scriptId' - A unique identifier for the Realtime script resource that is deployed on
-- instances in this fleet.
--
-- 'resourceCreationLimitPolicy', 'fleetAttributes_resourceCreationLimitPolicy' - The fleet policy that limits the number of game sessions an individual
-- player can create over a span of time.
newFleetAttributes ::
  FleetAttributes
newFleetAttributes =
  FleetAttributes'
    { scriptArn = Prelude.Nothing,
      operatingSystem = Prelude.Nothing,
      serverLaunchPath = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      name = Prelude.Nothing,
      fleetType = Prelude.Nothing,
      certificateConfiguration = Prelude.Nothing,
      instanceRoleArn = Prelude.Nothing,
      buildId = Prelude.Nothing,
      buildArn = Prelude.Nothing,
      newGameSessionProtectionPolicy' = Prelude.Nothing,
      stoppedActions = Prelude.Nothing,
      status = Prelude.Nothing,
      description = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      fleetArn = Prelude.Nothing,
      terminationTime = Prelude.Nothing,
      logPaths = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      metricGroups = Prelude.Nothing,
      serverLaunchParameters = Prelude.Nothing,
      scriptId = Prelude.Nothing,
      resourceCreationLimitPolicy = Prelude.Nothing
    }

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift script resource that is deployed on
-- instances in this fleet. In a GameLift script ARN, the resource ID
-- matches the @ScriptId@ value.
fleetAttributes_scriptArn :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_scriptArn = Lens.lens (\FleetAttributes' {scriptArn} -> scriptArn) (\s@FleetAttributes' {} a -> s {scriptArn = a} :: FleetAttributes)

-- | The operating system of the fleet\'s computing resources. A fleet\'s
-- operating system is determined by the OS of the build or script that is
-- deployed on this fleet.
fleetAttributes_operatingSystem :: Lens.Lens' FleetAttributes (Prelude.Maybe OperatingSystem)
fleetAttributes_operatingSystem = Lens.lens (\FleetAttributes' {operatingSystem} -> operatingSystem) (\s@FleetAttributes' {} a -> s {operatingSystem = a} :: FleetAttributes)

-- | __This parameter is no longer used.__ Server launch paths are now
-- defined using the fleet\'s RuntimeConfiguration parameter. Requests that
-- use this parameter instead continue to be valid.
fleetAttributes_serverLaunchPath :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_serverLaunchPath = Lens.lens (\FleetAttributes' {serverLaunchPath} -> serverLaunchPath) (\s@FleetAttributes' {} a -> s {serverLaunchPath = a} :: FleetAttributes)

-- | A unique identifier for the fleet.
fleetAttributes_fleetId :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_fleetId = Lens.lens (\FleetAttributes' {fleetId} -> fleetId) (\s@FleetAttributes' {} a -> s {fleetId = a} :: FleetAttributes)

-- | A descriptive label that is associated with a fleet. Fleet names do not
-- need to be unique.
fleetAttributes_name :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_name = Lens.lens (\FleetAttributes' {name} -> name) (\s@FleetAttributes' {} a -> s {name = a} :: FleetAttributes)

-- | The kind of instances, On-Demand or Spot, that this fleet uses.
fleetAttributes_fleetType :: Lens.Lens' FleetAttributes (Prelude.Maybe FleetType)
fleetAttributes_fleetType = Lens.lens (\FleetAttributes' {fleetType} -> fleetType) (\s@FleetAttributes' {} a -> s {fleetType = a} :: FleetAttributes)

-- | Indicates whether a TLS\/SSL certificate was generated for the fleet.
fleetAttributes_certificateConfiguration :: Lens.Lens' FleetAttributes (Prelude.Maybe CertificateConfiguration)
fleetAttributes_certificateConfiguration = Lens.lens (\FleetAttributes' {certificateConfiguration} -> certificateConfiguration) (\s@FleetAttributes' {} a -> s {certificateConfiguration = a} :: FleetAttributes)

-- | A unique identifier for an IAM role that manages access to your Amazon
-- Web Services services. With an instance role ARN set, any application
-- that runs on an instance in this fleet can assume the role, including
-- install scripts, server processes, and daemons (background processes).
-- Create a role or look up a role\'s ARN by using the
-- <https://console.aws.amazon.com/iam/ IAM dashboard> in the Amazon Web
-- Services Management Console. Learn more about using on-box credentials
-- for your game servers at
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-resources.html Access external resources from a game server>.
fleetAttributes_instanceRoleArn :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_instanceRoleArn = Lens.lens (\FleetAttributes' {instanceRoleArn} -> instanceRoleArn) (\s@FleetAttributes' {} a -> s {instanceRoleArn = a} :: FleetAttributes)

-- | A unique identifier for the build resource that is deployed on instances
-- in this fleet.
fleetAttributes_buildId :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_buildId = Lens.lens (\FleetAttributes' {buildId} -> buildId) (\s@FleetAttributes' {} a -> s {buildId = a} :: FleetAttributes)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift build resource that is deployed on
-- instances in this fleet. In a GameLift build ARN, the resource ID
-- matches the @BuildId@ value.
fleetAttributes_buildArn :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_buildArn = Lens.lens (\FleetAttributes' {buildArn} -> buildArn) (\s@FleetAttributes' {} a -> s {buildArn = a} :: FleetAttributes)

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

-- | A list of fleet activity that has been suspended using StopFleetActions.
-- This includes fleet auto-scaling.
fleetAttributes_stoppedActions :: Lens.Lens' FleetAttributes (Prelude.Maybe (Prelude.NonEmpty FleetAction))
fleetAttributes_stoppedActions = Lens.lens (\FleetAttributes' {stoppedActions} -> stoppedActions) (\s@FleetAttributes' {} a -> s {stoppedActions = a} :: FleetAttributes) Prelude.. Lens.mapping Lens.coerced

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

-- | A human-readable description of the fleet.
fleetAttributes_description :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_description = Lens.lens (\FleetAttributes' {description} -> description) (\s@FleetAttributes' {} a -> s {description = a} :: FleetAttributes)

-- | The Amazon EC2 instance type that determines the computing resources of
-- each instance in the fleet. Instance type defines the CPU, memory,
-- storage, and networking capacity. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon Elastic Compute Cloud Instance Types>
-- for detailed descriptions.
fleetAttributes_instanceType :: Lens.Lens' FleetAttributes (Prelude.Maybe EC2InstanceType)
fleetAttributes_instanceType = Lens.lens (\FleetAttributes' {instanceType} -> instanceType) (\s@FleetAttributes' {} a -> s {instanceType = a} :: FleetAttributes)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
-- In a GameLift fleet ARN, the resource ID matches the @FleetId@ value.
fleetAttributes_fleetArn :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_fleetArn = Lens.lens (\FleetAttributes' {fleetArn} -> fleetArn) (\s@FleetAttributes' {} a -> s {fleetArn = a} :: FleetAttributes)

-- | A time stamp indicating when this data object was terminated. Format is
-- a number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
fleetAttributes_terminationTime :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.UTCTime)
fleetAttributes_terminationTime = Lens.lens (\FleetAttributes' {terminationTime} -> terminationTime) (\s@FleetAttributes' {} a -> s {terminationTime = a} :: FleetAttributes) Prelude.. Lens.mapping Core._Time

-- | __This parameter is no longer used.__ Game session log paths are now
-- defined using the GameLift server API @ProcessReady()@ @logParameters@.
-- See more information in the
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api-ref.html#gamelift-sdk-server-api-ref-dataypes-process Server API Reference>.
fleetAttributes_logPaths :: Lens.Lens' FleetAttributes (Prelude.Maybe [Prelude.Text])
fleetAttributes_logPaths = Lens.lens (\FleetAttributes' {logPaths} -> logPaths) (\s@FleetAttributes' {} a -> s {logPaths = a} :: FleetAttributes) Prelude.. Lens.mapping Lens.coerced

-- | A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
fleetAttributes_creationTime :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.UTCTime)
fleetAttributes_creationTime = Lens.lens (\FleetAttributes' {creationTime} -> creationTime) (\s@FleetAttributes' {} a -> s {creationTime = a} :: FleetAttributes) Prelude.. Lens.mapping Core._Time

-- | Name of a metric group that metrics for this fleet are added to. In
-- Amazon CloudWatch, you can view aggregated metrics for fleets that are
-- in a metric group. A fleet can be included in only one metric group at a
-- time.
fleetAttributes_metricGroups :: Lens.Lens' FleetAttributes (Prelude.Maybe [Prelude.Text])
fleetAttributes_metricGroups = Lens.lens (\FleetAttributes' {metricGroups} -> metricGroups) (\s@FleetAttributes' {} a -> s {metricGroups = a} :: FleetAttributes) Prelude.. Lens.mapping Lens.coerced

-- | __This parameter is no longer used.__ Server launch parameters are now
-- defined using the fleet\'s RuntimeConfiguration parameter. Requests that
-- use this parameter instead continue to be valid.
fleetAttributes_serverLaunchParameters :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_serverLaunchParameters = Lens.lens (\FleetAttributes' {serverLaunchParameters} -> serverLaunchParameters) (\s@FleetAttributes' {} a -> s {serverLaunchParameters = a} :: FleetAttributes)

-- | A unique identifier for the Realtime script resource that is deployed on
-- instances in this fleet.
fleetAttributes_scriptId :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_scriptId = Lens.lens (\FleetAttributes' {scriptId} -> scriptId) (\s@FleetAttributes' {} a -> s {scriptId = a} :: FleetAttributes)

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
            Prelude.<$> (x Core..:? "ScriptArn")
            Prelude.<*> (x Core..:? "OperatingSystem")
            Prelude.<*> (x Core..:? "ServerLaunchPath")
            Prelude.<*> (x Core..:? "FleetId")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "FleetType")
            Prelude.<*> (x Core..:? "CertificateConfiguration")
            Prelude.<*> (x Core..:? "InstanceRoleArn")
            Prelude.<*> (x Core..:? "BuildId")
            Prelude.<*> (x Core..:? "BuildArn")
            Prelude.<*> (x Core..:? "NewGameSessionProtectionPolicy")
            Prelude.<*> (x Core..:? "StoppedActions")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "InstanceType")
            Prelude.<*> (x Core..:? "FleetArn")
            Prelude.<*> (x Core..:? "TerminationTime")
            Prelude.<*> (x Core..:? "LogPaths" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "MetricGroups" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ServerLaunchParameters")
            Prelude.<*> (x Core..:? "ScriptId")
            Prelude.<*> (x Core..:? "ResourceCreationLimitPolicy")
      )

instance Prelude.Hashable FleetAttributes where
  hashWithSalt _salt FleetAttributes' {..} =
    _salt `Prelude.hashWithSalt` scriptArn
      `Prelude.hashWithSalt` operatingSystem
      `Prelude.hashWithSalt` serverLaunchPath
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` fleetType
      `Prelude.hashWithSalt` certificateConfiguration
      `Prelude.hashWithSalt` instanceRoleArn
      `Prelude.hashWithSalt` buildId
      `Prelude.hashWithSalt` buildArn
      `Prelude.hashWithSalt` newGameSessionProtectionPolicy'
      `Prelude.hashWithSalt` stoppedActions
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` fleetArn
      `Prelude.hashWithSalt` terminationTime
      `Prelude.hashWithSalt` logPaths
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` metricGroups
      `Prelude.hashWithSalt` serverLaunchParameters
      `Prelude.hashWithSalt` scriptId
      `Prelude.hashWithSalt` resourceCreationLimitPolicy

instance Prelude.NFData FleetAttributes where
  rnf FleetAttributes' {..} =
    Prelude.rnf scriptArn
      `Prelude.seq` Prelude.rnf operatingSystem
      `Prelude.seq` Prelude.rnf serverLaunchPath
      `Prelude.seq` Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf fleetType
      `Prelude.seq` Prelude.rnf certificateConfiguration
      `Prelude.seq` Prelude.rnf instanceRoleArn
      `Prelude.seq` Prelude.rnf buildId
      `Prelude.seq` Prelude.rnf buildArn
      `Prelude.seq` Prelude.rnf newGameSessionProtectionPolicy'
      `Prelude.seq` Prelude.rnf stoppedActions
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf fleetArn
      `Prelude.seq` Prelude.rnf terminationTime
      `Prelude.seq` Prelude.rnf logPaths
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf metricGroups
      `Prelude.seq` Prelude.rnf
        serverLaunchParameters
      `Prelude.seq` Prelude.rnf scriptId
      `Prelude.seq` Prelude.rnf
        resourceCreationLimitPolicy
