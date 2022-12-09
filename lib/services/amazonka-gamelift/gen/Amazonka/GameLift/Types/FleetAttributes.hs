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
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types.AnywhereConfiguration
import Amazonka.GameLift.Types.CertificateConfiguration
import Amazonka.GameLift.Types.ComputeType
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
-- /See:/ 'newFleetAttributes' smart constructor.
data FleetAttributes = FleetAttributes'
  { anywhereConfiguration :: Prelude.Maybe AnywhereConfiguration,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- associated with the GameLift build resource that is deployed on
    -- instances in this fleet. In a GameLift build ARN, the resource ID
    -- matches the @BuildId@ value.
    buildArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the build resource that is deployed on instances
    -- in this fleet.
    buildId :: Prelude.Maybe Prelude.Text,
    certificateConfiguration :: Prelude.Maybe CertificateConfiguration,
    -- | The type of compute resource used to host your game servers. You can use
    -- your own compute resources with GameLift Anywhere or use Amazon EC2
    -- instances with managed GameLift.
    computeType :: Prelude.Maybe ComputeType,
    -- | A time stamp indicating when this data object was created. Format is a
    -- number expressed in Unix time as milliseconds (for example
    -- @\"1469498468.057\"@).
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | A human-readable description of the fleet.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a GameLift fleet resource and uniquely identifies
    -- it. ARNs are unique across all Regions. Format is
    -- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
    -- In a GameLift fleet ARN, the resource ID matches the @FleetId@ value.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the fleet.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to use On-Demand or Spot instances for this fleet. By
    -- default, this property is set to @ON_DEMAND@. Learn more about when to
    -- use
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-ec2-instances.html#gamelift-ec2-instances-spot On-Demand versus Spot Instances>.
    -- This property cannot be changed after the fleet is created.
    fleetType :: Prelude.Maybe FleetType,
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
    -- | The Amazon EC2 instance type that determines the computing resources of
    -- each instance in the fleet. Instance type defines the CPU, memory,
    -- storage, and networking capacity. See
    -- <http://aws.amazon.com/ec2/instance-types/ Amazon Elastic Compute Cloud Instance Types>
    -- for detailed descriptions.
    instanceType :: Prelude.Maybe EC2InstanceType,
    -- | __This parameter is no longer used.__ Game session log paths are now
    -- defined using the GameLift server API @ProcessReady()@ @logParameters@.
    -- See more information in the
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api-ref.html#gamelift-sdk-server-api-ref-dataypes-process Server API Reference>.
    logPaths :: Prelude.Maybe [Prelude.Text],
    -- | Name of a metric group that metrics for this fleet are added to. In
    -- Amazon CloudWatch, you can view aggregated metrics for fleets that are
    -- in a metric group. A fleet can be included in only one metric group at a
    -- time.
    metricGroups :: Prelude.Maybe [Prelude.Text],
    -- | A descriptive label that is associated with a fleet. Fleet names do not
    -- need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of game session protection to set on all new instances that are
    -- started in the fleet.
    --
    -- -   __NoProtection__ -- The game session can be terminated during a
    --     scale-down event.
    --
    -- -   __FullProtection__ -- If the game session is in an @ACTIVE@ status,
    --     it cannot be terminated during a scale-down event.
    newGameSessionProtectionPolicy' :: Prelude.Maybe ProtectionPolicy,
    -- | The operating system of the fleet\'s computing resources. A fleet\'s
    -- operating system is determined by the OS of the build or script that is
    -- deployed on this fleet.
    operatingSystem :: Prelude.Maybe OperatingSystem,
    resourceCreationLimitPolicy :: Prelude.Maybe ResourceCreationLimitPolicy,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- associated with the GameLift script resource that is deployed on
    -- instances in this fleet. In a GameLift script ARN, the resource ID
    -- matches the @ScriptId@ value.
    scriptArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the Realtime script resource that is deployed on
    -- instances in this fleet.
    scriptId :: Prelude.Maybe Prelude.Text,
    -- | __This parameter is no longer used.__ Server launch parameters are now
    -- defined using the fleet\'s runtime configuration . Requests that use
    -- this parameter instead continue to be valid.
    serverLaunchParameters :: Prelude.Maybe Prelude.Text,
    -- | __This parameter is no longer used.__ Server launch paths are now
    -- defined using the fleet\'s
    -- <https://docs.aws.amazon.com/gamelift/latest/apireference/RuntimeConfiguration.html RuntimeConfiguration>
    -- . Requests that use this parameter instead continue to be valid.
    serverLaunchPath :: Prelude.Maybe Prelude.Text,
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
    -- | A list of fleet activity that has been suspended using
    -- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_StopFleetActions.html StopFleetActions>
    -- . This includes fleet auto-scaling.
    stoppedActions :: Prelude.Maybe (Prelude.NonEmpty FleetAction),
    -- | A time stamp indicating when this data object was terminated. Format is
    -- a number expressed in Unix time as milliseconds (for example
    -- @\"1469498468.057\"@).
    terminationTime :: Prelude.Maybe Data.POSIX
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
-- 'anywhereConfiguration', 'fleetAttributes_anywhereConfiguration' - Undocumented member.
--
-- 'buildArn', 'fleetAttributes_buildArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift build resource that is deployed on
-- instances in this fleet. In a GameLift build ARN, the resource ID
-- matches the @BuildId@ value.
--
-- 'buildId', 'fleetAttributes_buildId' - A unique identifier for the build resource that is deployed on instances
-- in this fleet.
--
-- 'certificateConfiguration', 'fleetAttributes_certificateConfiguration' - Undocumented member.
--
-- 'computeType', 'fleetAttributes_computeType' - The type of compute resource used to host your game servers. You can use
-- your own compute resources with GameLift Anywhere or use Amazon EC2
-- instances with managed GameLift.
--
-- 'creationTime', 'fleetAttributes_creationTime' - A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
--
-- 'description', 'fleetAttributes_description' - A human-readable description of the fleet.
--
-- 'fleetArn', 'fleetAttributes_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
-- In a GameLift fleet ARN, the resource ID matches the @FleetId@ value.
--
-- 'fleetId', 'fleetAttributes_fleetId' - A unique identifier for the fleet.
--
-- 'fleetType', 'fleetAttributes_fleetType' - Indicates whether to use On-Demand or Spot instances for this fleet. By
-- default, this property is set to @ON_DEMAND@. Learn more about when to
-- use
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-ec2-instances.html#gamelift-ec2-instances-spot On-Demand versus Spot Instances>.
-- This property cannot be changed after the fleet is created.
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
-- 'instanceType', 'fleetAttributes_instanceType' - The Amazon EC2 instance type that determines the computing resources of
-- each instance in the fleet. Instance type defines the CPU, memory,
-- storage, and networking capacity. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon Elastic Compute Cloud Instance Types>
-- for detailed descriptions.
--
-- 'logPaths', 'fleetAttributes_logPaths' - __This parameter is no longer used.__ Game session log paths are now
-- defined using the GameLift server API @ProcessReady()@ @logParameters@.
-- See more information in the
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api-ref.html#gamelift-sdk-server-api-ref-dataypes-process Server API Reference>.
--
-- 'metricGroups', 'fleetAttributes_metricGroups' - Name of a metric group that metrics for this fleet are added to. In
-- Amazon CloudWatch, you can view aggregated metrics for fleets that are
-- in a metric group. A fleet can be included in only one metric group at a
-- time.
--
-- 'name', 'fleetAttributes_name' - A descriptive label that is associated with a fleet. Fleet names do not
-- need to be unique.
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
-- 'operatingSystem', 'fleetAttributes_operatingSystem' - The operating system of the fleet\'s computing resources. A fleet\'s
-- operating system is determined by the OS of the build or script that is
-- deployed on this fleet.
--
-- 'resourceCreationLimitPolicy', 'fleetAttributes_resourceCreationLimitPolicy' - Undocumented member.
--
-- 'scriptArn', 'fleetAttributes_scriptArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift script resource that is deployed on
-- instances in this fleet. In a GameLift script ARN, the resource ID
-- matches the @ScriptId@ value.
--
-- 'scriptId', 'fleetAttributes_scriptId' - A unique identifier for the Realtime script resource that is deployed on
-- instances in this fleet.
--
-- 'serverLaunchParameters', 'fleetAttributes_serverLaunchParameters' - __This parameter is no longer used.__ Server launch parameters are now
-- defined using the fleet\'s runtime configuration . Requests that use
-- this parameter instead continue to be valid.
--
-- 'serverLaunchPath', 'fleetAttributes_serverLaunchPath' - __This parameter is no longer used.__ Server launch paths are now
-- defined using the fleet\'s
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/RuntimeConfiguration.html RuntimeConfiguration>
-- . Requests that use this parameter instead continue to be valid.
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
-- 'stoppedActions', 'fleetAttributes_stoppedActions' - A list of fleet activity that has been suspended using
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_StopFleetActions.html StopFleetActions>
-- . This includes fleet auto-scaling.
--
-- 'terminationTime', 'fleetAttributes_terminationTime' - A time stamp indicating when this data object was terminated. Format is
-- a number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
newFleetAttributes ::
  FleetAttributes
newFleetAttributes =
  FleetAttributes'
    { anywhereConfiguration =
        Prelude.Nothing,
      buildArn = Prelude.Nothing,
      buildId = Prelude.Nothing,
      certificateConfiguration = Prelude.Nothing,
      computeType = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      fleetArn = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      fleetType = Prelude.Nothing,
      instanceRoleArn = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      logPaths = Prelude.Nothing,
      metricGroups = Prelude.Nothing,
      name = Prelude.Nothing,
      newGameSessionProtectionPolicy' = Prelude.Nothing,
      operatingSystem = Prelude.Nothing,
      resourceCreationLimitPolicy = Prelude.Nothing,
      scriptArn = Prelude.Nothing,
      scriptId = Prelude.Nothing,
      serverLaunchParameters = Prelude.Nothing,
      serverLaunchPath = Prelude.Nothing,
      status = Prelude.Nothing,
      stoppedActions = Prelude.Nothing,
      terminationTime = Prelude.Nothing
    }

-- | Undocumented member.
fleetAttributes_anywhereConfiguration :: Lens.Lens' FleetAttributes (Prelude.Maybe AnywhereConfiguration)
fleetAttributes_anywhereConfiguration = Lens.lens (\FleetAttributes' {anywhereConfiguration} -> anywhereConfiguration) (\s@FleetAttributes' {} a -> s {anywhereConfiguration = a} :: FleetAttributes)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift build resource that is deployed on
-- instances in this fleet. In a GameLift build ARN, the resource ID
-- matches the @BuildId@ value.
fleetAttributes_buildArn :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_buildArn = Lens.lens (\FleetAttributes' {buildArn} -> buildArn) (\s@FleetAttributes' {} a -> s {buildArn = a} :: FleetAttributes)

-- | A unique identifier for the build resource that is deployed on instances
-- in this fleet.
fleetAttributes_buildId :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_buildId = Lens.lens (\FleetAttributes' {buildId} -> buildId) (\s@FleetAttributes' {} a -> s {buildId = a} :: FleetAttributes)

-- | Undocumented member.
fleetAttributes_certificateConfiguration :: Lens.Lens' FleetAttributes (Prelude.Maybe CertificateConfiguration)
fleetAttributes_certificateConfiguration = Lens.lens (\FleetAttributes' {certificateConfiguration} -> certificateConfiguration) (\s@FleetAttributes' {} a -> s {certificateConfiguration = a} :: FleetAttributes)

-- | The type of compute resource used to host your game servers. You can use
-- your own compute resources with GameLift Anywhere or use Amazon EC2
-- instances with managed GameLift.
fleetAttributes_computeType :: Lens.Lens' FleetAttributes (Prelude.Maybe ComputeType)
fleetAttributes_computeType = Lens.lens (\FleetAttributes' {computeType} -> computeType) (\s@FleetAttributes' {} a -> s {computeType = a} :: FleetAttributes)

-- | A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
fleetAttributes_creationTime :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.UTCTime)
fleetAttributes_creationTime = Lens.lens (\FleetAttributes' {creationTime} -> creationTime) (\s@FleetAttributes' {} a -> s {creationTime = a} :: FleetAttributes) Prelude.. Lens.mapping Data._Time

-- | A human-readable description of the fleet.
fleetAttributes_description :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_description = Lens.lens (\FleetAttributes' {description} -> description) (\s@FleetAttributes' {} a -> s {description = a} :: FleetAttributes)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
-- In a GameLift fleet ARN, the resource ID matches the @FleetId@ value.
fleetAttributes_fleetArn :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_fleetArn = Lens.lens (\FleetAttributes' {fleetArn} -> fleetArn) (\s@FleetAttributes' {} a -> s {fleetArn = a} :: FleetAttributes)

-- | A unique identifier for the fleet.
fleetAttributes_fleetId :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_fleetId = Lens.lens (\FleetAttributes' {fleetId} -> fleetId) (\s@FleetAttributes' {} a -> s {fleetId = a} :: FleetAttributes)

-- | Indicates whether to use On-Demand or Spot instances for this fleet. By
-- default, this property is set to @ON_DEMAND@. Learn more about when to
-- use
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-ec2-instances.html#gamelift-ec2-instances-spot On-Demand versus Spot Instances>.
-- This property cannot be changed after the fleet is created.
fleetAttributes_fleetType :: Lens.Lens' FleetAttributes (Prelude.Maybe FleetType)
fleetAttributes_fleetType = Lens.lens (\FleetAttributes' {fleetType} -> fleetType) (\s@FleetAttributes' {} a -> s {fleetType = a} :: FleetAttributes)

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

-- | The Amazon EC2 instance type that determines the computing resources of
-- each instance in the fleet. Instance type defines the CPU, memory,
-- storage, and networking capacity. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon Elastic Compute Cloud Instance Types>
-- for detailed descriptions.
fleetAttributes_instanceType :: Lens.Lens' FleetAttributes (Prelude.Maybe EC2InstanceType)
fleetAttributes_instanceType = Lens.lens (\FleetAttributes' {instanceType} -> instanceType) (\s@FleetAttributes' {} a -> s {instanceType = a} :: FleetAttributes)

-- | __This parameter is no longer used.__ Game session log paths are now
-- defined using the GameLift server API @ProcessReady()@ @logParameters@.
-- See more information in the
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api-ref.html#gamelift-sdk-server-api-ref-dataypes-process Server API Reference>.
fleetAttributes_logPaths :: Lens.Lens' FleetAttributes (Prelude.Maybe [Prelude.Text])
fleetAttributes_logPaths = Lens.lens (\FleetAttributes' {logPaths} -> logPaths) (\s@FleetAttributes' {} a -> s {logPaths = a} :: FleetAttributes) Prelude.. Lens.mapping Lens.coerced

-- | Name of a metric group that metrics for this fleet are added to. In
-- Amazon CloudWatch, you can view aggregated metrics for fleets that are
-- in a metric group. A fleet can be included in only one metric group at a
-- time.
fleetAttributes_metricGroups :: Lens.Lens' FleetAttributes (Prelude.Maybe [Prelude.Text])
fleetAttributes_metricGroups = Lens.lens (\FleetAttributes' {metricGroups} -> metricGroups) (\s@FleetAttributes' {} a -> s {metricGroups = a} :: FleetAttributes) Prelude.. Lens.mapping Lens.coerced

-- | A descriptive label that is associated with a fleet. Fleet names do not
-- need to be unique.
fleetAttributes_name :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_name = Lens.lens (\FleetAttributes' {name} -> name) (\s@FleetAttributes' {} a -> s {name = a} :: FleetAttributes)

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

-- | The operating system of the fleet\'s computing resources. A fleet\'s
-- operating system is determined by the OS of the build or script that is
-- deployed on this fleet.
fleetAttributes_operatingSystem :: Lens.Lens' FleetAttributes (Prelude.Maybe OperatingSystem)
fleetAttributes_operatingSystem = Lens.lens (\FleetAttributes' {operatingSystem} -> operatingSystem) (\s@FleetAttributes' {} a -> s {operatingSystem = a} :: FleetAttributes)

-- | Undocumented member.
fleetAttributes_resourceCreationLimitPolicy :: Lens.Lens' FleetAttributes (Prelude.Maybe ResourceCreationLimitPolicy)
fleetAttributes_resourceCreationLimitPolicy = Lens.lens (\FleetAttributes' {resourceCreationLimitPolicy} -> resourceCreationLimitPolicy) (\s@FleetAttributes' {} a -> s {resourceCreationLimitPolicy = a} :: FleetAttributes)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift script resource that is deployed on
-- instances in this fleet. In a GameLift script ARN, the resource ID
-- matches the @ScriptId@ value.
fleetAttributes_scriptArn :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_scriptArn = Lens.lens (\FleetAttributes' {scriptArn} -> scriptArn) (\s@FleetAttributes' {} a -> s {scriptArn = a} :: FleetAttributes)

-- | A unique identifier for the Realtime script resource that is deployed on
-- instances in this fleet.
fleetAttributes_scriptId :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_scriptId = Lens.lens (\FleetAttributes' {scriptId} -> scriptId) (\s@FleetAttributes' {} a -> s {scriptId = a} :: FleetAttributes)

-- | __This parameter is no longer used.__ Server launch parameters are now
-- defined using the fleet\'s runtime configuration . Requests that use
-- this parameter instead continue to be valid.
fleetAttributes_serverLaunchParameters :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_serverLaunchParameters = Lens.lens (\FleetAttributes' {serverLaunchParameters} -> serverLaunchParameters) (\s@FleetAttributes' {} a -> s {serverLaunchParameters = a} :: FleetAttributes)

-- | __This parameter is no longer used.__ Server launch paths are now
-- defined using the fleet\'s
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/RuntimeConfiguration.html RuntimeConfiguration>
-- . Requests that use this parameter instead continue to be valid.
fleetAttributes_serverLaunchPath :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.Text)
fleetAttributes_serverLaunchPath = Lens.lens (\FleetAttributes' {serverLaunchPath} -> serverLaunchPath) (\s@FleetAttributes' {} a -> s {serverLaunchPath = a} :: FleetAttributes)

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

-- | A list of fleet activity that has been suspended using
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_StopFleetActions.html StopFleetActions>
-- . This includes fleet auto-scaling.
fleetAttributes_stoppedActions :: Lens.Lens' FleetAttributes (Prelude.Maybe (Prelude.NonEmpty FleetAction))
fleetAttributes_stoppedActions = Lens.lens (\FleetAttributes' {stoppedActions} -> stoppedActions) (\s@FleetAttributes' {} a -> s {stoppedActions = a} :: FleetAttributes) Prelude.. Lens.mapping Lens.coerced

-- | A time stamp indicating when this data object was terminated. Format is
-- a number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
fleetAttributes_terminationTime :: Lens.Lens' FleetAttributes (Prelude.Maybe Prelude.UTCTime)
fleetAttributes_terminationTime = Lens.lens (\FleetAttributes' {terminationTime} -> terminationTime) (\s@FleetAttributes' {} a -> s {terminationTime = a} :: FleetAttributes) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON FleetAttributes where
  parseJSON =
    Data.withObject
      "FleetAttributes"
      ( \x ->
          FleetAttributes'
            Prelude.<$> (x Data..:? "AnywhereConfiguration")
            Prelude.<*> (x Data..:? "BuildArn")
            Prelude.<*> (x Data..:? "BuildId")
            Prelude.<*> (x Data..:? "CertificateConfiguration")
            Prelude.<*> (x Data..:? "ComputeType")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "FleetArn")
            Prelude.<*> (x Data..:? "FleetId")
            Prelude.<*> (x Data..:? "FleetType")
            Prelude.<*> (x Data..:? "InstanceRoleArn")
            Prelude.<*> (x Data..:? "InstanceType")
            Prelude.<*> (x Data..:? "LogPaths" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "MetricGroups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "NewGameSessionProtectionPolicy")
            Prelude.<*> (x Data..:? "OperatingSystem")
            Prelude.<*> (x Data..:? "ResourceCreationLimitPolicy")
            Prelude.<*> (x Data..:? "ScriptArn")
            Prelude.<*> (x Data..:? "ScriptId")
            Prelude.<*> (x Data..:? "ServerLaunchParameters")
            Prelude.<*> (x Data..:? "ServerLaunchPath")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StoppedActions")
            Prelude.<*> (x Data..:? "TerminationTime")
      )

instance Prelude.Hashable FleetAttributes where
  hashWithSalt _salt FleetAttributes' {..} =
    _salt `Prelude.hashWithSalt` anywhereConfiguration
      `Prelude.hashWithSalt` buildArn
      `Prelude.hashWithSalt` buildId
      `Prelude.hashWithSalt` certificateConfiguration
      `Prelude.hashWithSalt` computeType
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` fleetArn
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` fleetType
      `Prelude.hashWithSalt` instanceRoleArn
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` logPaths
      `Prelude.hashWithSalt` metricGroups
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` newGameSessionProtectionPolicy'
      `Prelude.hashWithSalt` operatingSystem
      `Prelude.hashWithSalt` resourceCreationLimitPolicy
      `Prelude.hashWithSalt` scriptArn
      `Prelude.hashWithSalt` scriptId
      `Prelude.hashWithSalt` serverLaunchParameters
      `Prelude.hashWithSalt` serverLaunchPath
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` stoppedActions
      `Prelude.hashWithSalt` terminationTime

instance Prelude.NFData FleetAttributes where
  rnf FleetAttributes' {..} =
    Prelude.rnf anywhereConfiguration
      `Prelude.seq` Prelude.rnf buildArn
      `Prelude.seq` Prelude.rnf buildId
      `Prelude.seq` Prelude.rnf certificateConfiguration
      `Prelude.seq` Prelude.rnf computeType
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf fleetArn
      `Prelude.seq` Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf fleetType
      `Prelude.seq` Prelude.rnf instanceRoleArn
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf logPaths
      `Prelude.seq` Prelude.rnf metricGroups
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf
        newGameSessionProtectionPolicy'
      `Prelude.seq` Prelude.rnf operatingSystem
      `Prelude.seq` Prelude.rnf
        resourceCreationLimitPolicy
      `Prelude.seq` Prelude.rnf scriptArn
      `Prelude.seq` Prelude.rnf scriptId
      `Prelude.seq` Prelude.rnf
        serverLaunchParameters
      `Prelude.seq` Prelude.rnf
        serverLaunchPath
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf
        stoppedActions
      `Prelude.seq` Prelude.rnf
        terminationTime
