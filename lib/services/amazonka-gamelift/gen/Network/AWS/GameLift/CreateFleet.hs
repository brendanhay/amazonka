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
-- Module      : Network.AWS.GameLift.CreateFleet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a fleet of Amazon Elastic Compute Cloud (Amazon EC2) instances
-- to host your custom game server or Realtime Servers. Use this operation
-- to configure the computing resources for your fleet and provide
-- instructions for running game servers on each instance.
--
-- Most GameLift fleets can deploy instances to multiple locations,
-- including the home Region (where the fleet is created) and an optional
-- set of remote locations. Fleets that are created in the following AWS
-- Regions support multiple locations: us-east-1 (N. Virginia), us-west-2
-- (Oregon), eu-central-1 (Frankfurt), eu-west-1 (Ireland), ap-southeast-2
-- (Sydney), ap-northeast-1 (Tokyo), and ap-northeast-2 (Seoul). Fleets
-- that are created in other GameLift Regions can deploy instances in the
-- fleet\'s home Region only. All fleet instances use the same
-- configuration regardless of location; however, you can adjust capacity
-- settings and turn auto-scaling on\/off for each location.
--
-- To create a fleet, choose the hardware for your instances, specify a
-- game server build or Realtime script to deploy, and provide a runtime
-- configuration to direct GameLift how to start and run game servers on
-- each instance in the fleet. Set permissions for inbound traffic to your
-- game servers, and enable optional features as needed. When creating a
-- multi-location fleet, provide a list of additional remote locations.
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
module Network.AWS.GameLift.CreateFleet
  ( -- * Creating a Request
    CreateFleet (..),
    newCreateFleet,

    -- * Request Lenses
    createFleet_serverLaunchParameters,
    createFleet_logPaths,
    createFleet_peerVpcId,
    createFleet_buildId,
    createFleet_fleetType,
    createFleet_peerVpcAwsAccountId,
    createFleet_eC2InboundPermissions,
    createFleet_runtimeConfiguration,
    createFleet_newGameSessionProtectionPolicy,
    createFleet_scriptId,
    createFleet_certificateConfiguration,
    createFleet_serverLaunchPath,
    createFleet_instanceRoleArn,
    createFleet_metricGroups,
    createFleet_description,
    createFleet_resourceCreationLimitPolicy,
    createFleet_locations,
    createFleet_tags,
    createFleet_name,
    createFleet_eC2InstanceType,

    -- * Destructuring the Response
    CreateFleetResponse (..),
    newCreateFleetResponse,

    -- * Response Lenses
    createFleetResponse_locationStates,
    createFleetResponse_fleetAttributes,
    createFleetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newCreateFleet' smart constructor.
data CreateFleet = CreateFleet'
  { -- | __This parameter is no longer used.__ Specify server launch parameters
    -- using the @RuntimeConfiguration@ parameter. Requests that use this
    -- parameter instead continue to be valid.
    serverLaunchParameters :: Prelude.Maybe Prelude.Text,
    -- | __This parameter is no longer used.__ To specify where GameLift should
    -- store log files once a server process shuts down, use the GameLift
    -- server API @ProcessReady()@ and specify one or more directory paths in
    -- @logParameters@. See more information in the
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api-ref.html#gamelift-sdk-server-api-ref-dataypes-process Server API Reference>.
    logPaths :: Prelude.Maybe [Prelude.Text],
    -- | A unique identifier for a VPC with resources to be accessed by your
    -- GameLift fleet. The VPC must be in the same Region as your fleet. To
    -- look up a VPC ID, use the
    -- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS
    -- Management Console. Learn more about VPC peering in
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with GameLift Fleets>.
    peerVpcId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for a custom game server build to be deployed on
    -- fleet instances. You can use either the build ID or ARN. The build must
    -- be uploaded to GameLift and in @READY@ status. This fleet property
    -- cannot be changed later.
    buildId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to use On-Demand or Spot instances for this fleet. By
    -- default, this property is set to @ON_DEMAND@. Learn more about when to
    -- use
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-ec2-instances.html#gamelift-ec2-instances-spot On-Demand versus Spot Instances>.
    -- This property cannot be changed after the fleet is created.
    fleetType :: Prelude.Maybe FleetType,
    -- | Used when peering your GameLift fleet with a VPC, the unique identifier
    -- for the AWS account that owns the VPC. You can find your account ID in
    -- the AWS Management Console under account settings.
    peerVpcAwsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The allowed IP address ranges and port settings that allow inbound
    -- traffic to access game sessions on this fleet. If the fleet is hosting a
    -- custom game build, this property must be set before players can connect
    -- to game sessions. For Realtime Servers fleets, GameLift automatically
    -- sets TCP and UDP ranges.
    eC2InboundPermissions :: Prelude.Maybe [IpPermission],
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
    -- | The unique identifier for a Realtime configuration script to be deployed
    -- on fleet instances. You can use either the script ID or ARN. Scripts
    -- must be uploaded to GameLift prior to creating the fleet. This fleet
    -- property cannot be changed later.
    scriptId :: Prelude.Maybe Prelude.Text,
    -- | Prompts GameLift to generate a TLS\/SSL certificate for the fleet. TLS
    -- certificates are used for encrypting traffic between game clients and
    -- the game servers that are running on GameLift. By default, the
    -- @CertificateConfiguration@ is set to @DISABLED@. Learn more at
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-howitworks.html#gamelift-howitworks-security Securing Client\/Server Communication>.
    -- This property cannot be changed after the fleet is created.
    --
    -- Note: This feature requires the AWS Certificate Manager (ACM) service,
    -- which is not available in all AWS regions. When working in a region that
    -- does not support this feature, a fleet creation request with certificate
    -- generation fails with a 4xx error.
    certificateConfiguration :: Prelude.Maybe CertificateConfiguration,
    -- | __This parameter is no longer used.__ Specify a server launch path using
    -- the @RuntimeConfiguration@ parameter. Requests that use this parameter
    -- instead continue to be valid.
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
    -- This property cannot be changed after the fleet is created.
    instanceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of an AWS CloudWatch metric group to add this fleet to. A
    -- metric group is used to aggregate the metrics for multiple fleets. You
    -- can specify an existing metric group name or set a new name to create a
    -- new metric group. A fleet can be included in only one metric group at a
    -- time.
    metricGroups :: Prelude.Maybe [Prelude.Text],
    -- | A human-readable description of the fleet.
    description :: Prelude.Maybe Prelude.Text,
    -- | A policy that limits the number of game sessions that an individual
    -- player can create on instances in this fleet within a specified span of
    -- time.
    resourceCreationLimitPolicy :: Prelude.Maybe ResourceCreationLimitPolicy,
    -- | A set of remote locations to deploy additional instances to and manage
    -- as part of the fleet. This parameter can only be used when creating
    -- fleets in AWS Regions that support multiple locations. You can add any
    -- GameLift-supported AWS Region as a remote location, in the form of an
    -- AWS Region code such as @us-west-2@. To create a fleet with instances in
    -- the home Region only, omit this parameter.
    locations :: Prelude.Maybe (Prelude.NonEmpty LocationConfiguration),
    -- | A list of labels to assign to the new fleet resource. Tags are
    -- developer-defined key-value pairs. Tagging AWS resources are useful for
    -- resource management, access management and cost allocation. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
    -- in the /AWS General Reference/. Once the fleet is created, you can use
    -- TagResource, UntagResource, and ListTagsForResource to add, remove, and
    -- view tags. The maximum tag limit may be lower than stated. See the /AWS
    -- General Reference/ for actual tagging limits.
    tags :: Prelude.Maybe [Tag],
    -- | A descriptive label that is associated with a fleet. Fleet names do not
    -- need to be unique.
    name :: Prelude.Text,
    -- | The GameLift-supported EC2 instance type to use for all fleet instances.
    -- Instance type determines the computing resources that will be used to
    -- host your game servers, including CPU, memory, storage, and networking
    -- capacity. See
    -- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
    -- for detailed descriptions of EC2 instance types.
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
-- 'serverLaunchParameters', 'createFleet_serverLaunchParameters' - __This parameter is no longer used.__ Specify server launch parameters
-- using the @RuntimeConfiguration@ parameter. Requests that use this
-- parameter instead continue to be valid.
--
-- 'logPaths', 'createFleet_logPaths' - __This parameter is no longer used.__ To specify where GameLift should
-- store log files once a server process shuts down, use the GameLift
-- server API @ProcessReady()@ and specify one or more directory paths in
-- @logParameters@. See more information in the
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api-ref.html#gamelift-sdk-server-api-ref-dataypes-process Server API Reference>.
--
-- 'peerVpcId', 'createFleet_peerVpcId' - A unique identifier for a VPC with resources to be accessed by your
-- GameLift fleet. The VPC must be in the same Region as your fleet. To
-- look up a VPC ID, use the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS
-- Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with GameLift Fleets>.
--
-- 'buildId', 'createFleet_buildId' - The unique identifier for a custom game server build to be deployed on
-- fleet instances. You can use either the build ID or ARN. The build must
-- be uploaded to GameLift and in @READY@ status. This fleet property
-- cannot be changed later.
--
-- 'fleetType', 'createFleet_fleetType' - Indicates whether to use On-Demand or Spot instances for this fleet. By
-- default, this property is set to @ON_DEMAND@. Learn more about when to
-- use
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-ec2-instances.html#gamelift-ec2-instances-spot On-Demand versus Spot Instances>.
-- This property cannot be changed after the fleet is created.
--
-- 'peerVpcAwsAccountId', 'createFleet_peerVpcAwsAccountId' - Used when peering your GameLift fleet with a VPC, the unique identifier
-- for the AWS account that owns the VPC. You can find your account ID in
-- the AWS Management Console under account settings.
--
-- 'eC2InboundPermissions', 'createFleet_eC2InboundPermissions' - The allowed IP address ranges and port settings that allow inbound
-- traffic to access game sessions on this fleet. If the fleet is hosting a
-- custom game build, this property must be set before players can connect
-- to game sessions. For Realtime Servers fleets, GameLift automatically
-- sets TCP and UDP ranges.
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
-- 'scriptId', 'createFleet_scriptId' - The unique identifier for a Realtime configuration script to be deployed
-- on fleet instances. You can use either the script ID or ARN. Scripts
-- must be uploaded to GameLift prior to creating the fleet. This fleet
-- property cannot be changed later.
--
-- 'certificateConfiguration', 'createFleet_certificateConfiguration' - Prompts GameLift to generate a TLS\/SSL certificate for the fleet. TLS
-- certificates are used for encrypting traffic between game clients and
-- the game servers that are running on GameLift. By default, the
-- @CertificateConfiguration@ is set to @DISABLED@. Learn more at
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-howitworks.html#gamelift-howitworks-security Securing Client\/Server Communication>.
-- This property cannot be changed after the fleet is created.
--
-- Note: This feature requires the AWS Certificate Manager (ACM) service,
-- which is not available in all AWS regions. When working in a region that
-- does not support this feature, a fleet creation request with certificate
-- generation fails with a 4xx error.
--
-- 'serverLaunchPath', 'createFleet_serverLaunchPath' - __This parameter is no longer used.__ Specify a server launch path using
-- the @RuntimeConfiguration@ parameter. Requests that use this parameter
-- instead continue to be valid.
--
-- 'instanceRoleArn', 'createFleet_instanceRoleArn' - A unique identifier for an AWS IAM role that manages access to your AWS
-- services. With an instance role ARN set, any application that runs on an
-- instance in this fleet can assume the role, including install scripts,
-- server processes, and daemons (background processes). Create a role or
-- look up a role\'s ARN by using the
-- <https://console.aws.amazon.com/iam/ IAM dashboard> in the AWS
-- Management Console. Learn more about using on-box credentials for your
-- game servers at
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-resources.html Access external resources from a game server>.
-- This property cannot be changed after the fleet is created.
--
-- 'metricGroups', 'createFleet_metricGroups' - The name of an AWS CloudWatch metric group to add this fleet to. A
-- metric group is used to aggregate the metrics for multiple fleets. You
-- can specify an existing metric group name or set a new name to create a
-- new metric group. A fleet can be included in only one metric group at a
-- time.
--
-- 'description', 'createFleet_description' - A human-readable description of the fleet.
--
-- 'resourceCreationLimitPolicy', 'createFleet_resourceCreationLimitPolicy' - A policy that limits the number of game sessions that an individual
-- player can create on instances in this fleet within a specified span of
-- time.
--
-- 'locations', 'createFleet_locations' - A set of remote locations to deploy additional instances to and manage
-- as part of the fleet. This parameter can only be used when creating
-- fleets in AWS Regions that support multiple locations. You can add any
-- GameLift-supported AWS Region as a remote location, in the form of an
-- AWS Region code such as @us-west-2@. To create a fleet with instances in
-- the home Region only, omit this parameter.
--
-- 'tags', 'createFleet_tags' - A list of labels to assign to the new fleet resource. Tags are
-- developer-defined key-value pairs. Tagging AWS resources are useful for
-- resource management, access management and cost allocation. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- in the /AWS General Reference/. Once the fleet is created, you can use
-- TagResource, UntagResource, and ListTagsForResource to add, remove, and
-- view tags. The maximum tag limit may be lower than stated. See the /AWS
-- General Reference/ for actual tagging limits.
--
-- 'name', 'createFleet_name' - A descriptive label that is associated with a fleet. Fleet names do not
-- need to be unique.
--
-- 'eC2InstanceType', 'createFleet_eC2InstanceType' - The GameLift-supported EC2 instance type to use for all fleet instances.
-- Instance type determines the computing resources that will be used to
-- host your game servers, including CPU, memory, storage, and networking
-- capacity. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
-- for detailed descriptions of EC2 instance types.
newCreateFleet ::
  -- | 'name'
  Prelude.Text ->
  -- | 'eC2InstanceType'
  EC2InstanceType ->
  CreateFleet
newCreateFleet pName_ pEC2InstanceType_ =
  CreateFleet'
    { serverLaunchParameters =
        Prelude.Nothing,
      logPaths = Prelude.Nothing,
      peerVpcId = Prelude.Nothing,
      buildId = Prelude.Nothing,
      fleetType = Prelude.Nothing,
      peerVpcAwsAccountId = Prelude.Nothing,
      eC2InboundPermissions = Prelude.Nothing,
      runtimeConfiguration = Prelude.Nothing,
      newGameSessionProtectionPolicy' = Prelude.Nothing,
      scriptId = Prelude.Nothing,
      certificateConfiguration = Prelude.Nothing,
      serverLaunchPath = Prelude.Nothing,
      instanceRoleArn = Prelude.Nothing,
      metricGroups = Prelude.Nothing,
      description = Prelude.Nothing,
      resourceCreationLimitPolicy = Prelude.Nothing,
      locations = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      eC2InstanceType = pEC2InstanceType_
    }

-- | __This parameter is no longer used.__ Specify server launch parameters
-- using the @RuntimeConfiguration@ parameter. Requests that use this
-- parameter instead continue to be valid.
createFleet_serverLaunchParameters :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_serverLaunchParameters = Lens.lens (\CreateFleet' {serverLaunchParameters} -> serverLaunchParameters) (\s@CreateFleet' {} a -> s {serverLaunchParameters = a} :: CreateFleet)

-- | __This parameter is no longer used.__ To specify where GameLift should
-- store log files once a server process shuts down, use the GameLift
-- server API @ProcessReady()@ and specify one or more directory paths in
-- @logParameters@. See more information in the
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api-ref.html#gamelift-sdk-server-api-ref-dataypes-process Server API Reference>.
createFleet_logPaths :: Lens.Lens' CreateFleet (Prelude.Maybe [Prelude.Text])
createFleet_logPaths = Lens.lens (\CreateFleet' {logPaths} -> logPaths) (\s@CreateFleet' {} a -> s {logPaths = a} :: CreateFleet) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for a VPC with resources to be accessed by your
-- GameLift fleet. The VPC must be in the same Region as your fleet. To
-- look up a VPC ID, use the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS
-- Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with GameLift Fleets>.
createFleet_peerVpcId :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_peerVpcId = Lens.lens (\CreateFleet' {peerVpcId} -> peerVpcId) (\s@CreateFleet' {} a -> s {peerVpcId = a} :: CreateFleet)

-- | The unique identifier for a custom game server build to be deployed on
-- fleet instances. You can use either the build ID or ARN. The build must
-- be uploaded to GameLift and in @READY@ status. This fleet property
-- cannot be changed later.
createFleet_buildId :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_buildId = Lens.lens (\CreateFleet' {buildId} -> buildId) (\s@CreateFleet' {} a -> s {buildId = a} :: CreateFleet)

-- | Indicates whether to use On-Demand or Spot instances for this fleet. By
-- default, this property is set to @ON_DEMAND@. Learn more about when to
-- use
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-ec2-instances.html#gamelift-ec2-instances-spot On-Demand versus Spot Instances>.
-- This property cannot be changed after the fleet is created.
createFleet_fleetType :: Lens.Lens' CreateFleet (Prelude.Maybe FleetType)
createFleet_fleetType = Lens.lens (\CreateFleet' {fleetType} -> fleetType) (\s@CreateFleet' {} a -> s {fleetType = a} :: CreateFleet)

-- | Used when peering your GameLift fleet with a VPC, the unique identifier
-- for the AWS account that owns the VPC. You can find your account ID in
-- the AWS Management Console under account settings.
createFleet_peerVpcAwsAccountId :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_peerVpcAwsAccountId = Lens.lens (\CreateFleet' {peerVpcAwsAccountId} -> peerVpcAwsAccountId) (\s@CreateFleet' {} a -> s {peerVpcAwsAccountId = a} :: CreateFleet)

-- | The allowed IP address ranges and port settings that allow inbound
-- traffic to access game sessions on this fleet. If the fleet is hosting a
-- custom game build, this property must be set before players can connect
-- to game sessions. For Realtime Servers fleets, GameLift automatically
-- sets TCP and UDP ranges.
createFleet_eC2InboundPermissions :: Lens.Lens' CreateFleet (Prelude.Maybe [IpPermission])
createFleet_eC2InboundPermissions = Lens.lens (\CreateFleet' {eC2InboundPermissions} -> eC2InboundPermissions) (\s@CreateFleet' {} a -> s {eC2InboundPermissions = a} :: CreateFleet) Prelude.. Lens.mapping Lens.coerced

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

-- | The unique identifier for a Realtime configuration script to be deployed
-- on fleet instances. You can use either the script ID or ARN. Scripts
-- must be uploaded to GameLift prior to creating the fleet. This fleet
-- property cannot be changed later.
createFleet_scriptId :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_scriptId = Lens.lens (\CreateFleet' {scriptId} -> scriptId) (\s@CreateFleet' {} a -> s {scriptId = a} :: CreateFleet)

-- | Prompts GameLift to generate a TLS\/SSL certificate for the fleet. TLS
-- certificates are used for encrypting traffic between game clients and
-- the game servers that are running on GameLift. By default, the
-- @CertificateConfiguration@ is set to @DISABLED@. Learn more at
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-howitworks.html#gamelift-howitworks-security Securing Client\/Server Communication>.
-- This property cannot be changed after the fleet is created.
--
-- Note: This feature requires the AWS Certificate Manager (ACM) service,
-- which is not available in all AWS regions. When working in a region that
-- does not support this feature, a fleet creation request with certificate
-- generation fails with a 4xx error.
createFleet_certificateConfiguration :: Lens.Lens' CreateFleet (Prelude.Maybe CertificateConfiguration)
createFleet_certificateConfiguration = Lens.lens (\CreateFleet' {certificateConfiguration} -> certificateConfiguration) (\s@CreateFleet' {} a -> s {certificateConfiguration = a} :: CreateFleet)

-- | __This parameter is no longer used.__ Specify a server launch path using
-- the @RuntimeConfiguration@ parameter. Requests that use this parameter
-- instead continue to be valid.
createFleet_serverLaunchPath :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_serverLaunchPath = Lens.lens (\CreateFleet' {serverLaunchPath} -> serverLaunchPath) (\s@CreateFleet' {} a -> s {serverLaunchPath = a} :: CreateFleet)

-- | A unique identifier for an AWS IAM role that manages access to your AWS
-- services. With an instance role ARN set, any application that runs on an
-- instance in this fleet can assume the role, including install scripts,
-- server processes, and daemons (background processes). Create a role or
-- look up a role\'s ARN by using the
-- <https://console.aws.amazon.com/iam/ IAM dashboard> in the AWS
-- Management Console. Learn more about using on-box credentials for your
-- game servers at
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-resources.html Access external resources from a game server>.
-- This property cannot be changed after the fleet is created.
createFleet_instanceRoleArn :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_instanceRoleArn = Lens.lens (\CreateFleet' {instanceRoleArn} -> instanceRoleArn) (\s@CreateFleet' {} a -> s {instanceRoleArn = a} :: CreateFleet)

-- | The name of an AWS CloudWatch metric group to add this fleet to. A
-- metric group is used to aggregate the metrics for multiple fleets. You
-- can specify an existing metric group name or set a new name to create a
-- new metric group. A fleet can be included in only one metric group at a
-- time.
createFleet_metricGroups :: Lens.Lens' CreateFleet (Prelude.Maybe [Prelude.Text])
createFleet_metricGroups = Lens.lens (\CreateFleet' {metricGroups} -> metricGroups) (\s@CreateFleet' {} a -> s {metricGroups = a} :: CreateFleet) Prelude.. Lens.mapping Lens.coerced

-- | A human-readable description of the fleet.
createFleet_description :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_description = Lens.lens (\CreateFleet' {description} -> description) (\s@CreateFleet' {} a -> s {description = a} :: CreateFleet)

-- | A policy that limits the number of game sessions that an individual
-- player can create on instances in this fleet within a specified span of
-- time.
createFleet_resourceCreationLimitPolicy :: Lens.Lens' CreateFleet (Prelude.Maybe ResourceCreationLimitPolicy)
createFleet_resourceCreationLimitPolicy = Lens.lens (\CreateFleet' {resourceCreationLimitPolicy} -> resourceCreationLimitPolicy) (\s@CreateFleet' {} a -> s {resourceCreationLimitPolicy = a} :: CreateFleet)

-- | A set of remote locations to deploy additional instances to and manage
-- as part of the fleet. This parameter can only be used when creating
-- fleets in AWS Regions that support multiple locations. You can add any
-- GameLift-supported AWS Region as a remote location, in the form of an
-- AWS Region code such as @us-west-2@. To create a fleet with instances in
-- the home Region only, omit this parameter.
createFleet_locations :: Lens.Lens' CreateFleet (Prelude.Maybe (Prelude.NonEmpty LocationConfiguration))
createFleet_locations = Lens.lens (\CreateFleet' {locations} -> locations) (\s@CreateFleet' {} a -> s {locations = a} :: CreateFleet) Prelude.. Lens.mapping Lens.coerced

-- | A list of labels to assign to the new fleet resource. Tags are
-- developer-defined key-value pairs. Tagging AWS resources are useful for
-- resource management, access management and cost allocation. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- in the /AWS General Reference/. Once the fleet is created, you can use
-- TagResource, UntagResource, and ListTagsForResource to add, remove, and
-- view tags. The maximum tag limit may be lower than stated. See the /AWS
-- General Reference/ for actual tagging limits.
createFleet_tags :: Lens.Lens' CreateFleet (Prelude.Maybe [Tag])
createFleet_tags = Lens.lens (\CreateFleet' {tags} -> tags) (\s@CreateFleet' {} a -> s {tags = a} :: CreateFleet) Prelude.. Lens.mapping Lens.coerced

-- | A descriptive label that is associated with a fleet. Fleet names do not
-- need to be unique.
createFleet_name :: Lens.Lens' CreateFleet Prelude.Text
createFleet_name = Lens.lens (\CreateFleet' {name} -> name) (\s@CreateFleet' {} a -> s {name = a} :: CreateFleet)

-- | The GameLift-supported EC2 instance type to use for all fleet instances.
-- Instance type determines the computing resources that will be used to
-- host your game servers, including CPU, memory, storage, and networking
-- capacity. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
-- for detailed descriptions of EC2 instance types.
createFleet_eC2InstanceType :: Lens.Lens' CreateFleet EC2InstanceType
createFleet_eC2InstanceType = Lens.lens (\CreateFleet' {eC2InstanceType} -> eC2InstanceType) (\s@CreateFleet' {} a -> s {eC2InstanceType = a} :: CreateFleet)

instance Core.AWSRequest CreateFleet where
  type AWSResponse CreateFleet = CreateFleetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFleetResponse'
            Prelude.<$> (x Core..?> "LocationStates" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "FleetAttributes")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFleet

instance Prelude.NFData CreateFleet

instance Core.ToHeaders CreateFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.CreateFleet" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateFleet where
  toJSON CreateFleet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ServerLaunchParameters" Core..=)
              Prelude.<$> serverLaunchParameters,
            ("LogPaths" Core..=) Prelude.<$> logPaths,
            ("PeerVpcId" Core..=) Prelude.<$> peerVpcId,
            ("BuildId" Core..=) Prelude.<$> buildId,
            ("FleetType" Core..=) Prelude.<$> fleetType,
            ("PeerVpcAwsAccountId" Core..=)
              Prelude.<$> peerVpcAwsAccountId,
            ("EC2InboundPermissions" Core..=)
              Prelude.<$> eC2InboundPermissions,
            ("RuntimeConfiguration" Core..=)
              Prelude.<$> runtimeConfiguration,
            ("NewGameSessionProtectionPolicy" Core..=)
              Prelude.<$> newGameSessionProtectionPolicy',
            ("ScriptId" Core..=) Prelude.<$> scriptId,
            ("CertificateConfiguration" Core..=)
              Prelude.<$> certificateConfiguration,
            ("ServerLaunchPath" Core..=)
              Prelude.<$> serverLaunchPath,
            ("InstanceRoleArn" Core..=)
              Prelude.<$> instanceRoleArn,
            ("MetricGroups" Core..=) Prelude.<$> metricGroups,
            ("Description" Core..=) Prelude.<$> description,
            ("ResourceCreationLimitPolicy" Core..=)
              Prelude.<$> resourceCreationLimitPolicy,
            ("Locations" Core..=) Prelude.<$> locations,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just
              ("EC2InstanceType" Core..= eC2InstanceType)
          ]
      )

instance Core.ToPath CreateFleet where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateFleet where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newCreateFleetResponse' smart constructor.
data CreateFleetResponse = CreateFleetResponse'
  { -- | The fleet\'s locations and life-cycle status of each location. For new
    -- fleets, the status of all locations is set to @NEW@. During fleet
    -- creation, GameLift updates each location status as instances are
    -- deployed there and prepared for game hosting. This list includes an
    -- entry for the fleet\'s home Region. For fleets with no remote locations,
    -- only one entry, representing the home Region, is returned.
    locationStates :: Prelude.Maybe [LocationState],
    -- | The properties for the new fleet, including the current status. All
    -- fleets are placed in @NEW@ status on creation.
    fleetAttributes :: Prelude.Maybe FleetAttributes,
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
-- 'locationStates', 'createFleetResponse_locationStates' - The fleet\'s locations and life-cycle status of each location. For new
-- fleets, the status of all locations is set to @NEW@. During fleet
-- creation, GameLift updates each location status as instances are
-- deployed there and prepared for game hosting. This list includes an
-- entry for the fleet\'s home Region. For fleets with no remote locations,
-- only one entry, representing the home Region, is returned.
--
-- 'fleetAttributes', 'createFleetResponse_fleetAttributes' - The properties for the new fleet, including the current status. All
-- fleets are placed in @NEW@ status on creation.
--
-- 'httpStatus', 'createFleetResponse_httpStatus' - The response's http status code.
newCreateFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFleetResponse
newCreateFleetResponse pHttpStatus_ =
  CreateFleetResponse'
    { locationStates =
        Prelude.Nothing,
      fleetAttributes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The fleet\'s locations and life-cycle status of each location. For new
-- fleets, the status of all locations is set to @NEW@. During fleet
-- creation, GameLift updates each location status as instances are
-- deployed there and prepared for game hosting. This list includes an
-- entry for the fleet\'s home Region. For fleets with no remote locations,
-- only one entry, representing the home Region, is returned.
createFleetResponse_locationStates :: Lens.Lens' CreateFleetResponse (Prelude.Maybe [LocationState])
createFleetResponse_locationStates = Lens.lens (\CreateFleetResponse' {locationStates} -> locationStates) (\s@CreateFleetResponse' {} a -> s {locationStates = a} :: CreateFleetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The properties for the new fleet, including the current status. All
-- fleets are placed in @NEW@ status on creation.
createFleetResponse_fleetAttributes :: Lens.Lens' CreateFleetResponse (Prelude.Maybe FleetAttributes)
createFleetResponse_fleetAttributes = Lens.lens (\CreateFleetResponse' {fleetAttributes} -> fleetAttributes) (\s@CreateFleetResponse' {} a -> s {fleetAttributes = a} :: CreateFleetResponse)

-- | The response's http status code.
createFleetResponse_httpStatus :: Lens.Lens' CreateFleetResponse Prelude.Int
createFleetResponse_httpStatus = Lens.lens (\CreateFleetResponse' {httpStatus} -> httpStatus) (\s@CreateFleetResponse' {} a -> s {httpStatus = a} :: CreateFleetResponse)

instance Prelude.NFData CreateFleetResponse
