{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GameLift.CreateFleet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a fleet of Amazon Elastic Compute Cloud (Amazon Elastic Compute
-- Cloud) instances to host your custom game server or Realtime Servers.
-- Use this operation to configure the computing resources for your fleet
-- and provide instructions for running game servers on each instance.
--
-- Most GameLift fleets can deploy instances to multiple locations,
-- including the home Region (where the fleet is created) and an optional
-- set of remote locations. Fleets that are created in the following Amazon
-- Web Services Regions support multiple locations: us-east-1 (N.
-- Virginia), us-west-2 (Oregon), eu-central-1 (Frankfurt), eu-west-1
-- (Ireland), ap-southeast-2 (Sydney), ap-northeast-1 (Tokyo), and
-- ap-northeast-2 (Seoul). Fleets that are created in other GameLift
-- Regions can deploy instances in the fleet\'s home Region only. All fleet
-- instances use the same configuration regardless of location; however,
-- you can adjust capacity settings and turn auto-scaling on\/off for each
-- location.
--
-- To create a fleet, choose the hardware for your instances, specify a
-- game server build or Realtime script to deploy, and provide a runtime
-- configuration to direct GameLift how to start and run game servers on
-- each instance in the fleet. Set permissions for inbound traffic to your
-- game servers, and enable optional features as needed. When creating a
-- multi-location fleet, provide a list of additional remote locations.
--
-- If you need to debug your fleet, fetch logs, view performance metrics or
-- other actions on the fleet, create the development fleet with port
-- 22\/3389 open. As a best practice, we recommend opening ports for remote
-- access only when you need them and closing them when you\'re finished.
--
-- If successful, this operation creates a new Fleet resource and places it
-- in @NEW@ status, which prompts GameLift to initiate the
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-creation-workflow.html fleet creation workflow>.
-- You can track fleet creation by checking fleet status using
-- DescribeFleetAttributes and DescribeFleetLocationAttributes\/, or by
-- monitoring fleet creation events using DescribeFleetEvents. As soon as
-- the fleet status changes to @ACTIVE@, you can enable automatic scaling
-- for the fleet with PutScalingPolicy and set capacity for the home Region
-- with UpdateFleetCapacity. When the status of each remote location
-- reaches @ACTIVE@, you can set capacity by location using
-- UpdateFleetCapacity.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up fleets>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-creating-debug.html#fleets-creating-debug-creation Debug fleet creation issues>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Multi-location fleets>
--
-- __Related actions__
--
-- CreateFleet | UpdateFleetCapacity | PutScalingPolicy |
-- DescribeEC2InstanceLimits | DescribeFleetAttributes |
-- DescribeFleetLocationAttributes | UpdateFleetAttributes |
-- StopFleetActions | DeleteFleet |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.CreateFleet
  ( -- * Creating a Request
    CreateFleet (..),
    newCreateFleet,

    -- * Request Lenses
    createFleet_tags,
    createFleet_serverLaunchPath,
    createFleet_fleetType,
    createFleet_certificateConfiguration,
    createFleet_instanceRoleArn,
    createFleet_buildId,
    createFleet_newGameSessionProtectionPolicy,
    createFleet_description,
    createFleet_logPaths,
    createFleet_runtimeConfiguration,
    createFleet_peerVpcId,
    createFleet_metricGroups,
    createFleet_serverLaunchParameters,
    createFleet_locations,
    createFleet_eC2InboundPermissions,
    createFleet_scriptId,
    createFleet_peerVpcAwsAccountId,
    createFleet_resourceCreationLimitPolicy,
    createFleet_name,
    createFleet_eC2InstanceType,

    -- * Destructuring the Response
    CreateFleetResponse (..),
    newCreateFleetResponse,

    -- * Response Lenses
    createFleetResponse_fleetAttributes,
    createFleetResponse_locationStates,
    createFleetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newCreateFleet' smart constructor.
data CreateFleet = CreateFleet'
  { -- | A list of labels to assign to the new fleet resource. Tags are
    -- developer-defined key-value pairs. Tagging Amazon Web Services resources
    -- are useful for resource management, access management and cost
    -- allocation. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
    -- in the /Amazon Web Services General Reference/. Once the fleet is
    -- created, you can use TagResource, UntagResource, and ListTagsForResource
    -- to add, remove, and view tags. The maximum tag limit may be lower than
    -- stated. See the /Amazon Web Services General Reference/ for actual
    -- tagging limits.
    tags :: Prelude.Maybe [Tag],
    -- | __This parameter is no longer used.__ Specify a server launch path using
    -- the @RuntimeConfiguration@ parameter. Requests that use this parameter
    -- instead continue to be valid.
    serverLaunchPath :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to use On-Demand or Spot instances for this fleet. By
    -- default, this property is set to @ON_DEMAND@. Learn more about when to
    -- use
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-ec2-instances.html#gamelift-ec2-instances-spot On-Demand versus Spot Instances>.
    -- This property cannot be changed after the fleet is created.
    fleetType :: Prelude.Maybe FleetType,
    -- | Prompts GameLift to generate a TLS\/SSL certificate for the fleet. TLS
    -- certificates are used for encrypting traffic between game clients and
    -- the game servers that are running on GameLift. By default, the
    -- @CertificateConfiguration@ is set to @DISABLED@. This property cannot be
    -- changed after the fleet is created.
    --
    -- Note: This feature requires the Amazon Web Services Certificate Manager
    -- (ACM) service, which is not available in all Amazon Web Services
    -- regions. When working in a region that does not support this feature, a
    -- fleet creation request with certificate generation fails with a 4xx
    -- error.
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
    -- This property cannot be changed after the fleet is created.
    instanceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for a custom game server build to be deployed on
    -- fleet instances. You can use either the build ID or ARN. The build must
    -- be uploaded to GameLift and in @READY@ status. This fleet property
    -- cannot be changed later.
    buildId :: Prelude.Maybe Prelude.Text,
    -- | The status of termination protection for active game sessions on the
    -- fleet. By default, this property is set to @NoProtection@. You can also
    -- set game session protection for an individual game session by calling
    -- UpdateGameSession.
    --
    -- -   __NoProtection__ - Game sessions can be terminated during active
    --     gameplay as a result of a scale-down event.
    --
    -- -   __FullProtection__ - Game sessions in @ACTIVE@ status cannot be
    --     terminated during a scale-down event.
    newGameSessionProtectionPolicy' :: Prelude.Maybe ProtectionPolicy,
    -- | A human-readable description of the fleet.
    description :: Prelude.Maybe Prelude.Text,
    -- | __This parameter is no longer used.__ To specify where GameLift should
    -- store log files once a server process shuts down, use the GameLift
    -- server API @ProcessReady()@ and specify one or more directory paths in
    -- @logParameters@. See more information in the
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api-ref.html#gamelift-sdk-server-api-ref-dataypes-process Server API Reference>.
    logPaths :: Prelude.Maybe [Prelude.Text],
    -- | Instructions for how to launch and maintain server processes on
    -- instances in the fleet. The runtime configuration defines one or more
    -- server process configurations, each identifying a build executable or
    -- Realtime script file and the number of processes of that type to run
    -- concurrently.
    --
    -- The @RuntimeConfiguration@ parameter is required unless the fleet is
    -- being configured using the older parameters @ServerLaunchPath@ and
    -- @ServerLaunchParameters@, which are still supported for backward
    -- compatibility.
    runtimeConfiguration :: Prelude.Maybe RuntimeConfiguration,
    -- | A unique identifier for a VPC with resources to be accessed by your
    -- GameLift fleet. The VPC must be in the same Region as your fleet. To
    -- look up a VPC ID, use the
    -- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the Amazon Web
    -- Services Management Console. Learn more about VPC peering in
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with GameLift Fleets>.
    peerVpcId :: Prelude.Maybe Prelude.Text,
    -- | The name of an Amazon Web Services CloudWatch metric group to add this
    -- fleet to. A metric group is used to aggregate the metrics for multiple
    -- fleets. You can specify an existing metric group name or set a new name
    -- to create a new metric group. A fleet can be included in only one metric
    -- group at a time.
    metricGroups :: Prelude.Maybe [Prelude.Text],
    -- | __This parameter is no longer used.__ Specify server launch parameters
    -- using the @RuntimeConfiguration@ parameter. Requests that use this
    -- parameter instead continue to be valid.
    serverLaunchParameters :: Prelude.Maybe Prelude.Text,
    -- | A set of remote locations to deploy additional instances to and manage
    -- as part of the fleet. This parameter can only be used when creating
    -- fleets in Amazon Web Services Regions that support multiple locations.
    -- You can add any GameLift-supported Amazon Web Services Region as a
    -- remote location, in the form of an Amazon Web Services Region code such
    -- as @us-west-2@. To create a fleet with instances in the home Region
    -- only, omit this parameter.
    locations :: Prelude.Maybe (Prelude.NonEmpty LocationConfiguration),
    -- | The allowed IP address ranges and port settings that allow inbound
    -- traffic to access game sessions on this fleet. If the fleet is hosting a
    -- custom game build, this property must be set before players can connect
    -- to game sessions. For Realtime Servers fleets, GameLift automatically
    -- sets TCP and UDP ranges.
    eC2InboundPermissions :: Prelude.Maybe [IpPermission],
    -- | The unique identifier for a Realtime configuration script to be deployed
    -- on fleet instances. You can use either the script ID or ARN. Scripts
    -- must be uploaded to GameLift prior to creating the fleet. This fleet
    -- property cannot be changed later.
    scriptId :: Prelude.Maybe Prelude.Text,
    -- | Used when peering your GameLift fleet with a VPC, the unique identifier
    -- for the Amazon Web Services account that owns the VPC. You can find your
    -- account ID in the Amazon Web Services Management Console under account
    -- settings.
    peerVpcAwsAccountId :: Prelude.Maybe Prelude.Text,
    -- | A policy that limits the number of game sessions that an individual
    -- player can create on instances in this fleet within a specified span of
    -- time.
    resourceCreationLimitPolicy :: Prelude.Maybe ResourceCreationLimitPolicy,
    -- | A descriptive label that is associated with a fleet. Fleet names do not
    -- need to be unique.
    name :: Prelude.Text,
    -- | The GameLift-supported Amazon EC2 instance type to use for all fleet
    -- instances. Instance type determines the computing resources that will be
    -- used to host your game servers, including CPU, memory, storage, and
    -- networking capacity. See
    -- <http://aws.amazon.com/ec2/instance-types/ Amazon Elastic Compute Cloud Instance Types>
    -- for detailed descriptions of Amazon EC2 instance types.
    eC2InstanceType :: EC2InstanceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createFleet_tags' - A list of labels to assign to the new fleet resource. Tags are
-- developer-defined key-value pairs. Tagging Amazon Web Services resources
-- are useful for resource management, access management and cost
-- allocation. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in the /Amazon Web Services General Reference/. Once the fleet is
-- created, you can use TagResource, UntagResource, and ListTagsForResource
-- to add, remove, and view tags. The maximum tag limit may be lower than
-- stated. See the /Amazon Web Services General Reference/ for actual
-- tagging limits.
--
-- 'serverLaunchPath', 'createFleet_serverLaunchPath' - __This parameter is no longer used.__ Specify a server launch path using
-- the @RuntimeConfiguration@ parameter. Requests that use this parameter
-- instead continue to be valid.
--
-- 'fleetType', 'createFleet_fleetType' - Indicates whether to use On-Demand or Spot instances for this fleet. By
-- default, this property is set to @ON_DEMAND@. Learn more about when to
-- use
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-ec2-instances.html#gamelift-ec2-instances-spot On-Demand versus Spot Instances>.
-- This property cannot be changed after the fleet is created.
--
-- 'certificateConfiguration', 'createFleet_certificateConfiguration' - Prompts GameLift to generate a TLS\/SSL certificate for the fleet. TLS
-- certificates are used for encrypting traffic between game clients and
-- the game servers that are running on GameLift. By default, the
-- @CertificateConfiguration@ is set to @DISABLED@. This property cannot be
-- changed after the fleet is created.
--
-- Note: This feature requires the Amazon Web Services Certificate Manager
-- (ACM) service, which is not available in all Amazon Web Services
-- regions. When working in a region that does not support this feature, a
-- fleet creation request with certificate generation fails with a 4xx
-- error.
--
-- 'instanceRoleArn', 'createFleet_instanceRoleArn' - A unique identifier for an IAM role that manages access to your Amazon
-- Web Services services. With an instance role ARN set, any application
-- that runs on an instance in this fleet can assume the role, including
-- install scripts, server processes, and daemons (background processes).
-- Create a role or look up a role\'s ARN by using the
-- <https://console.aws.amazon.com/iam/ IAM dashboard> in the Amazon Web
-- Services Management Console. Learn more about using on-box credentials
-- for your game servers at
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-resources.html Access external resources from a game server>.
-- This property cannot be changed after the fleet is created.
--
-- 'buildId', 'createFleet_buildId' - The unique identifier for a custom game server build to be deployed on
-- fleet instances. You can use either the build ID or ARN. The build must
-- be uploaded to GameLift and in @READY@ status. This fleet property
-- cannot be changed later.
--
-- 'newGameSessionProtectionPolicy'', 'createFleet_newGameSessionProtectionPolicy' - The status of termination protection for active game sessions on the
-- fleet. By default, this property is set to @NoProtection@. You can also
-- set game session protection for an individual game session by calling
-- UpdateGameSession.
--
-- -   __NoProtection__ - Game sessions can be terminated during active
--     gameplay as a result of a scale-down event.
--
-- -   __FullProtection__ - Game sessions in @ACTIVE@ status cannot be
--     terminated during a scale-down event.
--
-- 'description', 'createFleet_description' - A human-readable description of the fleet.
--
-- 'logPaths', 'createFleet_logPaths' - __This parameter is no longer used.__ To specify where GameLift should
-- store log files once a server process shuts down, use the GameLift
-- server API @ProcessReady()@ and specify one or more directory paths in
-- @logParameters@. See more information in the
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api-ref.html#gamelift-sdk-server-api-ref-dataypes-process Server API Reference>.
--
-- 'runtimeConfiguration', 'createFleet_runtimeConfiguration' - Instructions for how to launch and maintain server processes on
-- instances in the fleet. The runtime configuration defines one or more
-- server process configurations, each identifying a build executable or
-- Realtime script file and the number of processes of that type to run
-- concurrently.
--
-- The @RuntimeConfiguration@ parameter is required unless the fleet is
-- being configured using the older parameters @ServerLaunchPath@ and
-- @ServerLaunchParameters@, which are still supported for backward
-- compatibility.
--
-- 'peerVpcId', 'createFleet_peerVpcId' - A unique identifier for a VPC with resources to be accessed by your
-- GameLift fleet. The VPC must be in the same Region as your fleet. To
-- look up a VPC ID, use the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the Amazon Web
-- Services Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with GameLift Fleets>.
--
-- 'metricGroups', 'createFleet_metricGroups' - The name of an Amazon Web Services CloudWatch metric group to add this
-- fleet to. A metric group is used to aggregate the metrics for multiple
-- fleets. You can specify an existing metric group name or set a new name
-- to create a new metric group. A fleet can be included in only one metric
-- group at a time.
--
-- 'serverLaunchParameters', 'createFleet_serverLaunchParameters' - __This parameter is no longer used.__ Specify server launch parameters
-- using the @RuntimeConfiguration@ parameter. Requests that use this
-- parameter instead continue to be valid.
--
-- 'locations', 'createFleet_locations' - A set of remote locations to deploy additional instances to and manage
-- as part of the fleet. This parameter can only be used when creating
-- fleets in Amazon Web Services Regions that support multiple locations.
-- You can add any GameLift-supported Amazon Web Services Region as a
-- remote location, in the form of an Amazon Web Services Region code such
-- as @us-west-2@. To create a fleet with instances in the home Region
-- only, omit this parameter.
--
-- 'eC2InboundPermissions', 'createFleet_eC2InboundPermissions' - The allowed IP address ranges and port settings that allow inbound
-- traffic to access game sessions on this fleet. If the fleet is hosting a
-- custom game build, this property must be set before players can connect
-- to game sessions. For Realtime Servers fleets, GameLift automatically
-- sets TCP and UDP ranges.
--
-- 'scriptId', 'createFleet_scriptId' - The unique identifier for a Realtime configuration script to be deployed
-- on fleet instances. You can use either the script ID or ARN. Scripts
-- must be uploaded to GameLift prior to creating the fleet. This fleet
-- property cannot be changed later.
--
-- 'peerVpcAwsAccountId', 'createFleet_peerVpcAwsAccountId' - Used when peering your GameLift fleet with a VPC, the unique identifier
-- for the Amazon Web Services account that owns the VPC. You can find your
-- account ID in the Amazon Web Services Management Console under account
-- settings.
--
-- 'resourceCreationLimitPolicy', 'createFleet_resourceCreationLimitPolicy' - A policy that limits the number of game sessions that an individual
-- player can create on instances in this fleet within a specified span of
-- time.
--
-- 'name', 'createFleet_name' - A descriptive label that is associated with a fleet. Fleet names do not
-- need to be unique.
--
-- 'eC2InstanceType', 'createFleet_eC2InstanceType' - The GameLift-supported Amazon EC2 instance type to use for all fleet
-- instances. Instance type determines the computing resources that will be
-- used to host your game servers, including CPU, memory, storage, and
-- networking capacity. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon Elastic Compute Cloud Instance Types>
-- for detailed descriptions of Amazon EC2 instance types.
newCreateFleet ::
  -- | 'name'
  Prelude.Text ->
  -- | 'eC2InstanceType'
  EC2InstanceType ->
  CreateFleet
newCreateFleet pName_ pEC2InstanceType_ =
  CreateFleet'
    { tags = Prelude.Nothing,
      serverLaunchPath = Prelude.Nothing,
      fleetType = Prelude.Nothing,
      certificateConfiguration = Prelude.Nothing,
      instanceRoleArn = Prelude.Nothing,
      buildId = Prelude.Nothing,
      newGameSessionProtectionPolicy' = Prelude.Nothing,
      description = Prelude.Nothing,
      logPaths = Prelude.Nothing,
      runtimeConfiguration = Prelude.Nothing,
      peerVpcId = Prelude.Nothing,
      metricGroups = Prelude.Nothing,
      serverLaunchParameters = Prelude.Nothing,
      locations = Prelude.Nothing,
      eC2InboundPermissions = Prelude.Nothing,
      scriptId = Prelude.Nothing,
      peerVpcAwsAccountId = Prelude.Nothing,
      resourceCreationLimitPolicy = Prelude.Nothing,
      name = pName_,
      eC2InstanceType = pEC2InstanceType_
    }

-- | A list of labels to assign to the new fleet resource. Tags are
-- developer-defined key-value pairs. Tagging Amazon Web Services resources
-- are useful for resource management, access management and cost
-- allocation. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in the /Amazon Web Services General Reference/. Once the fleet is
-- created, you can use TagResource, UntagResource, and ListTagsForResource
-- to add, remove, and view tags. The maximum tag limit may be lower than
-- stated. See the /Amazon Web Services General Reference/ for actual
-- tagging limits.
createFleet_tags :: Lens.Lens' CreateFleet (Prelude.Maybe [Tag])
createFleet_tags = Lens.lens (\CreateFleet' {tags} -> tags) (\s@CreateFleet' {} a -> s {tags = a} :: CreateFleet) Prelude.. Lens.mapping Lens.coerced

-- | __This parameter is no longer used.__ Specify a server launch path using
-- the @RuntimeConfiguration@ parameter. Requests that use this parameter
-- instead continue to be valid.
createFleet_serverLaunchPath :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_serverLaunchPath = Lens.lens (\CreateFleet' {serverLaunchPath} -> serverLaunchPath) (\s@CreateFleet' {} a -> s {serverLaunchPath = a} :: CreateFleet)

-- | Indicates whether to use On-Demand or Spot instances for this fleet. By
-- default, this property is set to @ON_DEMAND@. Learn more about when to
-- use
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-ec2-instances.html#gamelift-ec2-instances-spot On-Demand versus Spot Instances>.
-- This property cannot be changed after the fleet is created.
createFleet_fleetType :: Lens.Lens' CreateFleet (Prelude.Maybe FleetType)
createFleet_fleetType = Lens.lens (\CreateFleet' {fleetType} -> fleetType) (\s@CreateFleet' {} a -> s {fleetType = a} :: CreateFleet)

-- | Prompts GameLift to generate a TLS\/SSL certificate for the fleet. TLS
-- certificates are used for encrypting traffic between game clients and
-- the game servers that are running on GameLift. By default, the
-- @CertificateConfiguration@ is set to @DISABLED@. This property cannot be
-- changed after the fleet is created.
--
-- Note: This feature requires the Amazon Web Services Certificate Manager
-- (ACM) service, which is not available in all Amazon Web Services
-- regions. When working in a region that does not support this feature, a
-- fleet creation request with certificate generation fails with a 4xx
-- error.
createFleet_certificateConfiguration :: Lens.Lens' CreateFleet (Prelude.Maybe CertificateConfiguration)
createFleet_certificateConfiguration = Lens.lens (\CreateFleet' {certificateConfiguration} -> certificateConfiguration) (\s@CreateFleet' {} a -> s {certificateConfiguration = a} :: CreateFleet)

-- | A unique identifier for an IAM role that manages access to your Amazon
-- Web Services services. With an instance role ARN set, any application
-- that runs on an instance in this fleet can assume the role, including
-- install scripts, server processes, and daemons (background processes).
-- Create a role or look up a role\'s ARN by using the
-- <https://console.aws.amazon.com/iam/ IAM dashboard> in the Amazon Web
-- Services Management Console. Learn more about using on-box credentials
-- for your game servers at
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-resources.html Access external resources from a game server>.
-- This property cannot be changed after the fleet is created.
createFleet_instanceRoleArn :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_instanceRoleArn = Lens.lens (\CreateFleet' {instanceRoleArn} -> instanceRoleArn) (\s@CreateFleet' {} a -> s {instanceRoleArn = a} :: CreateFleet)

-- | The unique identifier for a custom game server build to be deployed on
-- fleet instances. You can use either the build ID or ARN. The build must
-- be uploaded to GameLift and in @READY@ status. This fleet property
-- cannot be changed later.
createFleet_buildId :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_buildId = Lens.lens (\CreateFleet' {buildId} -> buildId) (\s@CreateFleet' {} a -> s {buildId = a} :: CreateFleet)

-- | The status of termination protection for active game sessions on the
-- fleet. By default, this property is set to @NoProtection@. You can also
-- set game session protection for an individual game session by calling
-- UpdateGameSession.
--
-- -   __NoProtection__ - Game sessions can be terminated during active
--     gameplay as a result of a scale-down event.
--
-- -   __FullProtection__ - Game sessions in @ACTIVE@ status cannot be
--     terminated during a scale-down event.
createFleet_newGameSessionProtectionPolicy :: Lens.Lens' CreateFleet (Prelude.Maybe ProtectionPolicy)
createFleet_newGameSessionProtectionPolicy = Lens.lens (\CreateFleet' {newGameSessionProtectionPolicy'} -> newGameSessionProtectionPolicy') (\s@CreateFleet' {} a -> s {newGameSessionProtectionPolicy' = a} :: CreateFleet)

-- | A human-readable description of the fleet.
createFleet_description :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_description = Lens.lens (\CreateFleet' {description} -> description) (\s@CreateFleet' {} a -> s {description = a} :: CreateFleet)

-- | __This parameter is no longer used.__ To specify where GameLift should
-- store log files once a server process shuts down, use the GameLift
-- server API @ProcessReady()@ and specify one or more directory paths in
-- @logParameters@. See more information in the
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api-ref.html#gamelift-sdk-server-api-ref-dataypes-process Server API Reference>.
createFleet_logPaths :: Lens.Lens' CreateFleet (Prelude.Maybe [Prelude.Text])
createFleet_logPaths = Lens.lens (\CreateFleet' {logPaths} -> logPaths) (\s@CreateFleet' {} a -> s {logPaths = a} :: CreateFleet) Prelude.. Lens.mapping Lens.coerced

-- | Instructions for how to launch and maintain server processes on
-- instances in the fleet. The runtime configuration defines one or more
-- server process configurations, each identifying a build executable or
-- Realtime script file and the number of processes of that type to run
-- concurrently.
--
-- The @RuntimeConfiguration@ parameter is required unless the fleet is
-- being configured using the older parameters @ServerLaunchPath@ and
-- @ServerLaunchParameters@, which are still supported for backward
-- compatibility.
createFleet_runtimeConfiguration :: Lens.Lens' CreateFleet (Prelude.Maybe RuntimeConfiguration)
createFleet_runtimeConfiguration = Lens.lens (\CreateFleet' {runtimeConfiguration} -> runtimeConfiguration) (\s@CreateFleet' {} a -> s {runtimeConfiguration = a} :: CreateFleet)

-- | A unique identifier for a VPC with resources to be accessed by your
-- GameLift fleet. The VPC must be in the same Region as your fleet. To
-- look up a VPC ID, use the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the Amazon Web
-- Services Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with GameLift Fleets>.
createFleet_peerVpcId :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_peerVpcId = Lens.lens (\CreateFleet' {peerVpcId} -> peerVpcId) (\s@CreateFleet' {} a -> s {peerVpcId = a} :: CreateFleet)

-- | The name of an Amazon Web Services CloudWatch metric group to add this
-- fleet to. A metric group is used to aggregate the metrics for multiple
-- fleets. You can specify an existing metric group name or set a new name
-- to create a new metric group. A fleet can be included in only one metric
-- group at a time.
createFleet_metricGroups :: Lens.Lens' CreateFleet (Prelude.Maybe [Prelude.Text])
createFleet_metricGroups = Lens.lens (\CreateFleet' {metricGroups} -> metricGroups) (\s@CreateFleet' {} a -> s {metricGroups = a} :: CreateFleet) Prelude.. Lens.mapping Lens.coerced

-- | __This parameter is no longer used.__ Specify server launch parameters
-- using the @RuntimeConfiguration@ parameter. Requests that use this
-- parameter instead continue to be valid.
createFleet_serverLaunchParameters :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_serverLaunchParameters = Lens.lens (\CreateFleet' {serverLaunchParameters} -> serverLaunchParameters) (\s@CreateFleet' {} a -> s {serverLaunchParameters = a} :: CreateFleet)

-- | A set of remote locations to deploy additional instances to and manage
-- as part of the fleet. This parameter can only be used when creating
-- fleets in Amazon Web Services Regions that support multiple locations.
-- You can add any GameLift-supported Amazon Web Services Region as a
-- remote location, in the form of an Amazon Web Services Region code such
-- as @us-west-2@. To create a fleet with instances in the home Region
-- only, omit this parameter.
createFleet_locations :: Lens.Lens' CreateFleet (Prelude.Maybe (Prelude.NonEmpty LocationConfiguration))
createFleet_locations = Lens.lens (\CreateFleet' {locations} -> locations) (\s@CreateFleet' {} a -> s {locations = a} :: CreateFleet) Prelude.. Lens.mapping Lens.coerced

-- | The allowed IP address ranges and port settings that allow inbound
-- traffic to access game sessions on this fleet. If the fleet is hosting a
-- custom game build, this property must be set before players can connect
-- to game sessions. For Realtime Servers fleets, GameLift automatically
-- sets TCP and UDP ranges.
createFleet_eC2InboundPermissions :: Lens.Lens' CreateFleet (Prelude.Maybe [IpPermission])
createFleet_eC2InboundPermissions = Lens.lens (\CreateFleet' {eC2InboundPermissions} -> eC2InboundPermissions) (\s@CreateFleet' {} a -> s {eC2InboundPermissions = a} :: CreateFleet) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier for a Realtime configuration script to be deployed
-- on fleet instances. You can use either the script ID or ARN. Scripts
-- must be uploaded to GameLift prior to creating the fleet. This fleet
-- property cannot be changed later.
createFleet_scriptId :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_scriptId = Lens.lens (\CreateFleet' {scriptId} -> scriptId) (\s@CreateFleet' {} a -> s {scriptId = a} :: CreateFleet)

-- | Used when peering your GameLift fleet with a VPC, the unique identifier
-- for the Amazon Web Services account that owns the VPC. You can find your
-- account ID in the Amazon Web Services Management Console under account
-- settings.
createFleet_peerVpcAwsAccountId :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_peerVpcAwsAccountId = Lens.lens (\CreateFleet' {peerVpcAwsAccountId} -> peerVpcAwsAccountId) (\s@CreateFleet' {} a -> s {peerVpcAwsAccountId = a} :: CreateFleet)

-- | A policy that limits the number of game sessions that an individual
-- player can create on instances in this fleet within a specified span of
-- time.
createFleet_resourceCreationLimitPolicy :: Lens.Lens' CreateFleet (Prelude.Maybe ResourceCreationLimitPolicy)
createFleet_resourceCreationLimitPolicy = Lens.lens (\CreateFleet' {resourceCreationLimitPolicy} -> resourceCreationLimitPolicy) (\s@CreateFleet' {} a -> s {resourceCreationLimitPolicy = a} :: CreateFleet)

-- | A descriptive label that is associated with a fleet. Fleet names do not
-- need to be unique.
createFleet_name :: Lens.Lens' CreateFleet Prelude.Text
createFleet_name = Lens.lens (\CreateFleet' {name} -> name) (\s@CreateFleet' {} a -> s {name = a} :: CreateFleet)

-- | The GameLift-supported Amazon EC2 instance type to use for all fleet
-- instances. Instance type determines the computing resources that will be
-- used to host your game servers, including CPU, memory, storage, and
-- networking capacity. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon Elastic Compute Cloud Instance Types>
-- for detailed descriptions of Amazon EC2 instance types.
createFleet_eC2InstanceType :: Lens.Lens' CreateFleet EC2InstanceType
createFleet_eC2InstanceType = Lens.lens (\CreateFleet' {eC2InstanceType} -> eC2InstanceType) (\s@CreateFleet' {} a -> s {eC2InstanceType = a} :: CreateFleet)

instance Core.AWSRequest CreateFleet where
  type AWSResponse CreateFleet = CreateFleetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFleetResponse'
            Prelude.<$> (x Data..?> "FleetAttributes")
            Prelude.<*> (x Data..?> "LocationStates" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFleet where
  hashWithSalt _salt CreateFleet' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` serverLaunchPath
      `Prelude.hashWithSalt` fleetType
      `Prelude.hashWithSalt` certificateConfiguration
      `Prelude.hashWithSalt` instanceRoleArn
      `Prelude.hashWithSalt` buildId
      `Prelude.hashWithSalt` newGameSessionProtectionPolicy'
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` logPaths
      `Prelude.hashWithSalt` runtimeConfiguration
      `Prelude.hashWithSalt` peerVpcId
      `Prelude.hashWithSalt` metricGroups
      `Prelude.hashWithSalt` serverLaunchParameters
      `Prelude.hashWithSalt` locations
      `Prelude.hashWithSalt` eC2InboundPermissions
      `Prelude.hashWithSalt` scriptId
      `Prelude.hashWithSalt` peerVpcAwsAccountId
      `Prelude.hashWithSalt` resourceCreationLimitPolicy
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` eC2InstanceType

instance Prelude.NFData CreateFleet where
  rnf CreateFleet' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf serverLaunchPath
      `Prelude.seq` Prelude.rnf fleetType
      `Prelude.seq` Prelude.rnf certificateConfiguration
      `Prelude.seq` Prelude.rnf instanceRoleArn
      `Prelude.seq` Prelude.rnf buildId
      `Prelude.seq` Prelude.rnf newGameSessionProtectionPolicy'
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf logPaths
      `Prelude.seq` Prelude.rnf runtimeConfiguration
      `Prelude.seq` Prelude.rnf peerVpcId
      `Prelude.seq` Prelude.rnf metricGroups
      `Prelude.seq` Prelude.rnf serverLaunchParameters
      `Prelude.seq` Prelude.rnf locations
      `Prelude.seq` Prelude.rnf eC2InboundPermissions
      `Prelude.seq` Prelude.rnf scriptId
      `Prelude.seq` Prelude.rnf peerVpcAwsAccountId
      `Prelude.seq` Prelude.rnf
        resourceCreationLimitPolicy
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf eC2InstanceType

instance Data.ToHeaders CreateFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.CreateFleet" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFleet where
  toJSON CreateFleet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("ServerLaunchPath" Data..=)
              Prelude.<$> serverLaunchPath,
            ("FleetType" Data..=) Prelude.<$> fleetType,
            ("CertificateConfiguration" Data..=)
              Prelude.<$> certificateConfiguration,
            ("InstanceRoleArn" Data..=)
              Prelude.<$> instanceRoleArn,
            ("BuildId" Data..=) Prelude.<$> buildId,
            ("NewGameSessionProtectionPolicy" Data..=)
              Prelude.<$> newGameSessionProtectionPolicy',
            ("Description" Data..=) Prelude.<$> description,
            ("LogPaths" Data..=) Prelude.<$> logPaths,
            ("RuntimeConfiguration" Data..=)
              Prelude.<$> runtimeConfiguration,
            ("PeerVpcId" Data..=) Prelude.<$> peerVpcId,
            ("MetricGroups" Data..=) Prelude.<$> metricGroups,
            ("ServerLaunchParameters" Data..=)
              Prelude.<$> serverLaunchParameters,
            ("Locations" Data..=) Prelude.<$> locations,
            ("EC2InboundPermissions" Data..=)
              Prelude.<$> eC2InboundPermissions,
            ("ScriptId" Data..=) Prelude.<$> scriptId,
            ("PeerVpcAwsAccountId" Data..=)
              Prelude.<$> peerVpcAwsAccountId,
            ("ResourceCreationLimitPolicy" Data..=)
              Prelude.<$> resourceCreationLimitPolicy,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("EC2InstanceType" Data..= eC2InstanceType)
          ]
      )

instance Data.ToPath CreateFleet where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateFleet where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newCreateFleetResponse' smart constructor.
data CreateFleetResponse = CreateFleetResponse'
  { -- | The properties for the new fleet, including the current status. All
    -- fleets are placed in @NEW@ status on creation.
    fleetAttributes :: Prelude.Maybe FleetAttributes,
    -- | The fleet\'s locations and life-cycle status of each location. For new
    -- fleets, the status of all locations is set to @NEW@. During fleet
    -- creation, GameLift updates each location status as instances are
    -- deployed there and prepared for game hosting. This list includes an
    -- entry for the fleet\'s home Region. For fleets with no remote locations,
    -- only one entry, representing the home Region, is returned.
    locationStates :: Prelude.Maybe [LocationState],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetAttributes', 'createFleetResponse_fleetAttributes' - The properties for the new fleet, including the current status. All
-- fleets are placed in @NEW@ status on creation.
--
-- 'locationStates', 'createFleetResponse_locationStates' - The fleet\'s locations and life-cycle status of each location. For new
-- fleets, the status of all locations is set to @NEW@. During fleet
-- creation, GameLift updates each location status as instances are
-- deployed there and prepared for game hosting. This list includes an
-- entry for the fleet\'s home Region. For fleets with no remote locations,
-- only one entry, representing the home Region, is returned.
--
-- 'httpStatus', 'createFleetResponse_httpStatus' - The response's http status code.
newCreateFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFleetResponse
newCreateFleetResponse pHttpStatus_ =
  CreateFleetResponse'
    { fleetAttributes =
        Prelude.Nothing,
      locationStates = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The properties for the new fleet, including the current status. All
-- fleets are placed in @NEW@ status on creation.
createFleetResponse_fleetAttributes :: Lens.Lens' CreateFleetResponse (Prelude.Maybe FleetAttributes)
createFleetResponse_fleetAttributes = Lens.lens (\CreateFleetResponse' {fleetAttributes} -> fleetAttributes) (\s@CreateFleetResponse' {} a -> s {fleetAttributes = a} :: CreateFleetResponse)

-- | The fleet\'s locations and life-cycle status of each location. For new
-- fleets, the status of all locations is set to @NEW@. During fleet
-- creation, GameLift updates each location status as instances are
-- deployed there and prepared for game hosting. This list includes an
-- entry for the fleet\'s home Region. For fleets with no remote locations,
-- only one entry, representing the home Region, is returned.
createFleetResponse_locationStates :: Lens.Lens' CreateFleetResponse (Prelude.Maybe [LocationState])
createFleetResponse_locationStates = Lens.lens (\CreateFleetResponse' {locationStates} -> locationStates) (\s@CreateFleetResponse' {} a -> s {locationStates = a} :: CreateFleetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createFleetResponse_httpStatus :: Lens.Lens' CreateFleetResponse Prelude.Int
createFleetResponse_httpStatus = Lens.lens (\CreateFleetResponse' {httpStatus} -> httpStatus) (\s@CreateFleetResponse' {} a -> s {httpStatus = a} :: CreateFleetResponse)

instance Prelude.NFData CreateFleetResponse where
  rnf CreateFleetResponse' {..} =
    Prelude.rnf fleetAttributes
      `Prelude.seq` Prelude.rnf locationStates
      `Prelude.seq` Prelude.rnf httpStatus
