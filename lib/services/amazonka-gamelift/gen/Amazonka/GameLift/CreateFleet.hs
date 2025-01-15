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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up fleets>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-creating-debug.html#fleets-creating-debug-creation Debug fleet creation issues>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Multi-location fleets>
module Amazonka.GameLift.CreateFleet
  ( -- * Creating a Request
    CreateFleet (..),
    newCreateFleet,

    -- * Request Lenses
    createFleet_anywhereConfiguration,
    createFleet_buildId,
    createFleet_certificateConfiguration,
    createFleet_computeType,
    createFleet_description,
    createFleet_eC2InboundPermissions,
    createFleet_eC2InstanceType,
    createFleet_fleetType,
    createFleet_instanceRoleArn,
    createFleet_locations,
    createFleet_logPaths,
    createFleet_metricGroups,
    createFleet_newGameSessionProtectionPolicy,
    createFleet_peerVpcAwsAccountId,
    createFleet_peerVpcId,
    createFleet_resourceCreationLimitPolicy,
    createFleet_runtimeConfiguration,
    createFleet_scriptId,
    createFleet_serverLaunchParameters,
    createFleet_serverLaunchPath,
    createFleet_tags,
    createFleet_name,

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

-- | /See:/ 'newCreateFleet' smart constructor.
data CreateFleet = CreateFleet'
  { -- | GameLift Anywhere configuration options.
    anywhereConfiguration :: Prelude.Maybe AnywhereConfiguration,
    -- | The unique identifier for a custom game server build to be deployed on
    -- fleet instances. You can use either the build ID or ARN. The build must
    -- be uploaded to GameLift and in @READY@ status. This fleet property
    -- cannot be changed later.
    buildId :: Prelude.Maybe Prelude.Text,
    -- | Prompts GameLift to generate a TLS\/SSL certificate for the fleet.
    -- GameLift uses the certificates to encrypt traffic between game clients
    -- and the game servers running on GameLift. By default, the
    -- @CertificateConfiguration@ is @DISABLED@. You can\'t change this
    -- property after you create the fleet.
    --
    -- Certificate Manager (ACM) certificates expire after 13 months.
    -- Certificate expiration can cause fleets to fail, preventing players from
    -- connecting to instances in the fleet. We recommend you replace fleets
    -- before 13 months, consider using fleet aliases for a smooth transition.
    --
    -- ACM isn\'t available in all Amazon Web Services regions. A fleet
    -- creation request with certificate generation enabled in an unsupported
    -- Region, fails with a 4xx error. For more information about the supported
    -- Regions, see
    -- <https://docs.aws.amazon.com/acm/latest/userguide/acm-regions.html Supported Regions>
    -- in the /Certificate Manager User Guide/.
    certificateConfiguration :: Prelude.Maybe CertificateConfiguration,
    -- | The type of compute resource used to host your game servers. You can use
    -- your own compute resources with GameLift Anywhere or use Amazon EC2
    -- instances with managed GameLift.
    computeType :: Prelude.Maybe ComputeType,
    -- | A description for the fleet.
    description :: Prelude.Maybe Prelude.Text,
    -- | The allowed IP address ranges and port settings that allow inbound
    -- traffic to access game sessions on this fleet. If the fleet is hosting a
    -- custom game build, this property must be set before players can connect
    -- to game sessions. For Realtime Servers fleets, GameLift automatically
    -- sets TCP and UDP ranges.
    eC2InboundPermissions :: Prelude.Maybe [IpPermission],
    -- | The GameLift-supported Amazon EC2 instance type to use for all fleet
    -- instances. Instance type determines the computing resources that will be
    -- used to host your game servers, including CPU, memory, storage, and
    -- networking capacity. See
    -- <http://aws.amazon.com/ec2/instance-types/ Amazon Elastic Compute Cloud Instance Types>
    -- for detailed descriptions of Amazon EC2 instance types.
    eC2InstanceType :: Prelude.Maybe EC2InstanceType,
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
    -- This property cannot be changed after the fleet is created.
    instanceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A set of remote locations to deploy additional instances to and manage
    -- as part of the fleet. This parameter can only be used when creating
    -- fleets in Amazon Web Services Regions that support multiple locations.
    -- You can add any GameLift-supported Amazon Web Services Region as a
    -- remote location, in the form of an Amazon Web Services Region code such
    -- as @us-west-2@. To create a fleet with instances in the home Region
    -- only, omit this parameter.
    locations :: Prelude.Maybe (Prelude.NonEmpty LocationConfiguration),
    -- | __This parameter is no longer used.__ To specify where GameLift should
    -- store log files once a server process shuts down, use the GameLift
    -- server API @ProcessReady()@ and specify one or more directory paths in
    -- @logParameters@. For more information, see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-initialize Initialize the server process>
    -- in the /GameLift Developer Guide/.
    logPaths :: Prelude.Maybe [Prelude.Text],
    -- | The name of an Amazon Web Services CloudWatch metric group to add this
    -- fleet to. A metric group is used to aggregate the metrics for multiple
    -- fleets. You can specify an existing metric group name or set a new name
    -- to create a new metric group. A fleet can be included in only one metric
    -- group at a time.
    metricGroups :: Prelude.Maybe [Prelude.Text],
    -- | The status of termination protection for active game sessions on the
    -- fleet. By default, this property is set to @NoProtection@. You can also
    -- set game session protection for an individual game session by calling
    -- <gamelift/latest/apireference/API_UpdateGameSession.html UpdateGameSession>.
    --
    -- -   __NoProtection__ - Game sessions can be terminated during active
    --     gameplay as a result of a scale-down event.
    --
    -- -   __FullProtection__ - Game sessions in @ACTIVE@ status cannot be
    --     terminated during a scale-down event.
    newGameSessionProtectionPolicy' :: Prelude.Maybe ProtectionPolicy,
    -- | Used when peering your GameLift fleet with a VPC, the unique identifier
    -- for the Amazon Web Services account that owns the VPC. You can find your
    -- account ID in the Amazon Web Services Management Console under account
    -- settings.
    peerVpcAwsAccountId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a VPC with resources to be accessed by your
    -- GameLift fleet. The VPC must be in the same Region as your fleet. To
    -- look up a VPC ID, use the
    -- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the Amazon Web
    -- Services Management Console. Learn more about VPC peering in
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with GameLift Fleets>.
    peerVpcId :: Prelude.Maybe Prelude.Text,
    -- | A policy that limits the number of game sessions that an individual
    -- player can create on instances in this fleet within a specified span of
    -- time.
    resourceCreationLimitPolicy :: Prelude.Maybe ResourceCreationLimitPolicy,
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
    -- | The unique identifier for a Realtime configuration script to be deployed
    -- on fleet instances. You can use either the script ID or ARN. Scripts
    -- must be uploaded to GameLift prior to creating the fleet. This fleet
    -- property cannot be changed later.
    scriptId :: Prelude.Maybe Prelude.Text,
    -- | __This parameter is no longer used.__ Specify server launch parameters
    -- using the @RuntimeConfiguration@ parameter. Requests that use this
    -- parameter instead continue to be valid.
    serverLaunchParameters :: Prelude.Maybe Prelude.Text,
    -- | __This parameter is no longer used.__ Specify a server launch path using
    -- the @RuntimeConfiguration@ parameter. Requests that use this parameter
    -- instead continue to be valid.
    serverLaunchPath :: Prelude.Maybe Prelude.Text,
    -- | A list of labels to assign to the new fleet resource. Tags are
    -- developer-defined key-value pairs. Tagging Amazon Web Services resources
    -- are useful for resource management, access management and cost
    -- allocation. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
    -- in the /Amazon Web Services General Reference/.
    tags :: Prelude.Maybe [Tag],
    -- | A descriptive label that is associated with a fleet. Fleet names do not
    -- need to be unique.
    name :: Prelude.Text
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
-- 'anywhereConfiguration', 'createFleet_anywhereConfiguration' - GameLift Anywhere configuration options.
--
-- 'buildId', 'createFleet_buildId' - The unique identifier for a custom game server build to be deployed on
-- fleet instances. You can use either the build ID or ARN. The build must
-- be uploaded to GameLift and in @READY@ status. This fleet property
-- cannot be changed later.
--
-- 'certificateConfiguration', 'createFleet_certificateConfiguration' - Prompts GameLift to generate a TLS\/SSL certificate for the fleet.
-- GameLift uses the certificates to encrypt traffic between game clients
-- and the game servers running on GameLift. By default, the
-- @CertificateConfiguration@ is @DISABLED@. You can\'t change this
-- property after you create the fleet.
--
-- Certificate Manager (ACM) certificates expire after 13 months.
-- Certificate expiration can cause fleets to fail, preventing players from
-- connecting to instances in the fleet. We recommend you replace fleets
-- before 13 months, consider using fleet aliases for a smooth transition.
--
-- ACM isn\'t available in all Amazon Web Services regions. A fleet
-- creation request with certificate generation enabled in an unsupported
-- Region, fails with a 4xx error. For more information about the supported
-- Regions, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/acm-regions.html Supported Regions>
-- in the /Certificate Manager User Guide/.
--
-- 'computeType', 'createFleet_computeType' - The type of compute resource used to host your game servers. You can use
-- your own compute resources with GameLift Anywhere or use Amazon EC2
-- instances with managed GameLift.
--
-- 'description', 'createFleet_description' - A description for the fleet.
--
-- 'eC2InboundPermissions', 'createFleet_eC2InboundPermissions' - The allowed IP address ranges and port settings that allow inbound
-- traffic to access game sessions on this fleet. If the fleet is hosting a
-- custom game build, this property must be set before players can connect
-- to game sessions. For Realtime Servers fleets, GameLift automatically
-- sets TCP and UDP ranges.
--
-- 'eC2InstanceType', 'createFleet_eC2InstanceType' - The GameLift-supported Amazon EC2 instance type to use for all fleet
-- instances. Instance type determines the computing resources that will be
-- used to host your game servers, including CPU, memory, storage, and
-- networking capacity. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon Elastic Compute Cloud Instance Types>
-- for detailed descriptions of Amazon EC2 instance types.
--
-- 'fleetType', 'createFleet_fleetType' - Indicates whether to use On-Demand or Spot instances for this fleet. By
-- default, this property is set to @ON_DEMAND@. Learn more about when to
-- use
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-ec2-instances.html#gamelift-ec2-instances-spot On-Demand versus Spot Instances>.
-- This property cannot be changed after the fleet is created.
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
-- 'locations', 'createFleet_locations' - A set of remote locations to deploy additional instances to and manage
-- as part of the fleet. This parameter can only be used when creating
-- fleets in Amazon Web Services Regions that support multiple locations.
-- You can add any GameLift-supported Amazon Web Services Region as a
-- remote location, in the form of an Amazon Web Services Region code such
-- as @us-west-2@. To create a fleet with instances in the home Region
-- only, omit this parameter.
--
-- 'logPaths', 'createFleet_logPaths' - __This parameter is no longer used.__ To specify where GameLift should
-- store log files once a server process shuts down, use the GameLift
-- server API @ProcessReady()@ and specify one or more directory paths in
-- @logParameters@. For more information, see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-initialize Initialize the server process>
-- in the /GameLift Developer Guide/.
--
-- 'metricGroups', 'createFleet_metricGroups' - The name of an Amazon Web Services CloudWatch metric group to add this
-- fleet to. A metric group is used to aggregate the metrics for multiple
-- fleets. You can specify an existing metric group name or set a new name
-- to create a new metric group. A fleet can be included in only one metric
-- group at a time.
--
-- 'newGameSessionProtectionPolicy'', 'createFleet_newGameSessionProtectionPolicy' - The status of termination protection for active game sessions on the
-- fleet. By default, this property is set to @NoProtection@. You can also
-- set game session protection for an individual game session by calling
-- <gamelift/latest/apireference/API_UpdateGameSession.html UpdateGameSession>.
--
-- -   __NoProtection__ - Game sessions can be terminated during active
--     gameplay as a result of a scale-down event.
--
-- -   __FullProtection__ - Game sessions in @ACTIVE@ status cannot be
--     terminated during a scale-down event.
--
-- 'peerVpcAwsAccountId', 'createFleet_peerVpcAwsAccountId' - Used when peering your GameLift fleet with a VPC, the unique identifier
-- for the Amazon Web Services account that owns the VPC. You can find your
-- account ID in the Amazon Web Services Management Console under account
-- settings.
--
-- 'peerVpcId', 'createFleet_peerVpcId' - A unique identifier for a VPC with resources to be accessed by your
-- GameLift fleet. The VPC must be in the same Region as your fleet. To
-- look up a VPC ID, use the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the Amazon Web
-- Services Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with GameLift Fleets>.
--
-- 'resourceCreationLimitPolicy', 'createFleet_resourceCreationLimitPolicy' - A policy that limits the number of game sessions that an individual
-- player can create on instances in this fleet within a specified span of
-- time.
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
-- 'scriptId', 'createFleet_scriptId' - The unique identifier for a Realtime configuration script to be deployed
-- on fleet instances. You can use either the script ID or ARN. Scripts
-- must be uploaded to GameLift prior to creating the fleet. This fleet
-- property cannot be changed later.
--
-- 'serverLaunchParameters', 'createFleet_serverLaunchParameters' - __This parameter is no longer used.__ Specify server launch parameters
-- using the @RuntimeConfiguration@ parameter. Requests that use this
-- parameter instead continue to be valid.
--
-- 'serverLaunchPath', 'createFleet_serverLaunchPath' - __This parameter is no longer used.__ Specify a server launch path using
-- the @RuntimeConfiguration@ parameter. Requests that use this parameter
-- instead continue to be valid.
--
-- 'tags', 'createFleet_tags' - A list of labels to assign to the new fleet resource. Tags are
-- developer-defined key-value pairs. Tagging Amazon Web Services resources
-- are useful for resource management, access management and cost
-- allocation. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in the /Amazon Web Services General Reference/.
--
-- 'name', 'createFleet_name' - A descriptive label that is associated with a fleet. Fleet names do not
-- need to be unique.
newCreateFleet ::
  -- | 'name'
  Prelude.Text ->
  CreateFleet
newCreateFleet pName_ =
  CreateFleet'
    { anywhereConfiguration =
        Prelude.Nothing,
      buildId = Prelude.Nothing,
      certificateConfiguration = Prelude.Nothing,
      computeType = Prelude.Nothing,
      description = Prelude.Nothing,
      eC2InboundPermissions = Prelude.Nothing,
      eC2InstanceType = Prelude.Nothing,
      fleetType = Prelude.Nothing,
      instanceRoleArn = Prelude.Nothing,
      locations = Prelude.Nothing,
      logPaths = Prelude.Nothing,
      metricGroups = Prelude.Nothing,
      newGameSessionProtectionPolicy' = Prelude.Nothing,
      peerVpcAwsAccountId = Prelude.Nothing,
      peerVpcId = Prelude.Nothing,
      resourceCreationLimitPolicy = Prelude.Nothing,
      runtimeConfiguration = Prelude.Nothing,
      scriptId = Prelude.Nothing,
      serverLaunchParameters = Prelude.Nothing,
      serverLaunchPath = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | GameLift Anywhere configuration options.
createFleet_anywhereConfiguration :: Lens.Lens' CreateFleet (Prelude.Maybe AnywhereConfiguration)
createFleet_anywhereConfiguration = Lens.lens (\CreateFleet' {anywhereConfiguration} -> anywhereConfiguration) (\s@CreateFleet' {} a -> s {anywhereConfiguration = a} :: CreateFleet)

-- | The unique identifier for a custom game server build to be deployed on
-- fleet instances. You can use either the build ID or ARN. The build must
-- be uploaded to GameLift and in @READY@ status. This fleet property
-- cannot be changed later.
createFleet_buildId :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_buildId = Lens.lens (\CreateFleet' {buildId} -> buildId) (\s@CreateFleet' {} a -> s {buildId = a} :: CreateFleet)

-- | Prompts GameLift to generate a TLS\/SSL certificate for the fleet.
-- GameLift uses the certificates to encrypt traffic between game clients
-- and the game servers running on GameLift. By default, the
-- @CertificateConfiguration@ is @DISABLED@. You can\'t change this
-- property after you create the fleet.
--
-- Certificate Manager (ACM) certificates expire after 13 months.
-- Certificate expiration can cause fleets to fail, preventing players from
-- connecting to instances in the fleet. We recommend you replace fleets
-- before 13 months, consider using fleet aliases for a smooth transition.
--
-- ACM isn\'t available in all Amazon Web Services regions. A fleet
-- creation request with certificate generation enabled in an unsupported
-- Region, fails with a 4xx error. For more information about the supported
-- Regions, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/acm-regions.html Supported Regions>
-- in the /Certificate Manager User Guide/.
createFleet_certificateConfiguration :: Lens.Lens' CreateFleet (Prelude.Maybe CertificateConfiguration)
createFleet_certificateConfiguration = Lens.lens (\CreateFleet' {certificateConfiguration} -> certificateConfiguration) (\s@CreateFleet' {} a -> s {certificateConfiguration = a} :: CreateFleet)

-- | The type of compute resource used to host your game servers. You can use
-- your own compute resources with GameLift Anywhere or use Amazon EC2
-- instances with managed GameLift.
createFleet_computeType :: Lens.Lens' CreateFleet (Prelude.Maybe ComputeType)
createFleet_computeType = Lens.lens (\CreateFleet' {computeType} -> computeType) (\s@CreateFleet' {} a -> s {computeType = a} :: CreateFleet)

-- | A description for the fleet.
createFleet_description :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_description = Lens.lens (\CreateFleet' {description} -> description) (\s@CreateFleet' {} a -> s {description = a} :: CreateFleet)

-- | The allowed IP address ranges and port settings that allow inbound
-- traffic to access game sessions on this fleet. If the fleet is hosting a
-- custom game build, this property must be set before players can connect
-- to game sessions. For Realtime Servers fleets, GameLift automatically
-- sets TCP and UDP ranges.
createFleet_eC2InboundPermissions :: Lens.Lens' CreateFleet (Prelude.Maybe [IpPermission])
createFleet_eC2InboundPermissions = Lens.lens (\CreateFleet' {eC2InboundPermissions} -> eC2InboundPermissions) (\s@CreateFleet' {} a -> s {eC2InboundPermissions = a} :: CreateFleet) Prelude.. Lens.mapping Lens.coerced

-- | The GameLift-supported Amazon EC2 instance type to use for all fleet
-- instances. Instance type determines the computing resources that will be
-- used to host your game servers, including CPU, memory, storage, and
-- networking capacity. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon Elastic Compute Cloud Instance Types>
-- for detailed descriptions of Amazon EC2 instance types.
createFleet_eC2InstanceType :: Lens.Lens' CreateFleet (Prelude.Maybe EC2InstanceType)
createFleet_eC2InstanceType = Lens.lens (\CreateFleet' {eC2InstanceType} -> eC2InstanceType) (\s@CreateFleet' {} a -> s {eC2InstanceType = a} :: CreateFleet)

-- | Indicates whether to use On-Demand or Spot instances for this fleet. By
-- default, this property is set to @ON_DEMAND@. Learn more about when to
-- use
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-ec2-instances.html#gamelift-ec2-instances-spot On-Demand versus Spot Instances>.
-- This property cannot be changed after the fleet is created.
createFleet_fleetType :: Lens.Lens' CreateFleet (Prelude.Maybe FleetType)
createFleet_fleetType = Lens.lens (\CreateFleet' {fleetType} -> fleetType) (\s@CreateFleet' {} a -> s {fleetType = a} :: CreateFleet)

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

-- | A set of remote locations to deploy additional instances to and manage
-- as part of the fleet. This parameter can only be used when creating
-- fleets in Amazon Web Services Regions that support multiple locations.
-- You can add any GameLift-supported Amazon Web Services Region as a
-- remote location, in the form of an Amazon Web Services Region code such
-- as @us-west-2@. To create a fleet with instances in the home Region
-- only, omit this parameter.
createFleet_locations :: Lens.Lens' CreateFleet (Prelude.Maybe (Prelude.NonEmpty LocationConfiguration))
createFleet_locations = Lens.lens (\CreateFleet' {locations} -> locations) (\s@CreateFleet' {} a -> s {locations = a} :: CreateFleet) Prelude.. Lens.mapping Lens.coerced

-- | __This parameter is no longer used.__ To specify where GameLift should
-- store log files once a server process shuts down, use the GameLift
-- server API @ProcessReady()@ and specify one or more directory paths in
-- @logParameters@. For more information, see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-initialize Initialize the server process>
-- in the /GameLift Developer Guide/.
createFleet_logPaths :: Lens.Lens' CreateFleet (Prelude.Maybe [Prelude.Text])
createFleet_logPaths = Lens.lens (\CreateFleet' {logPaths} -> logPaths) (\s@CreateFleet' {} a -> s {logPaths = a} :: CreateFleet) Prelude.. Lens.mapping Lens.coerced

-- | The name of an Amazon Web Services CloudWatch metric group to add this
-- fleet to. A metric group is used to aggregate the metrics for multiple
-- fleets. You can specify an existing metric group name or set a new name
-- to create a new metric group. A fleet can be included in only one metric
-- group at a time.
createFleet_metricGroups :: Lens.Lens' CreateFleet (Prelude.Maybe [Prelude.Text])
createFleet_metricGroups = Lens.lens (\CreateFleet' {metricGroups} -> metricGroups) (\s@CreateFleet' {} a -> s {metricGroups = a} :: CreateFleet) Prelude.. Lens.mapping Lens.coerced

-- | The status of termination protection for active game sessions on the
-- fleet. By default, this property is set to @NoProtection@. You can also
-- set game session protection for an individual game session by calling
-- <gamelift/latest/apireference/API_UpdateGameSession.html UpdateGameSession>.
--
-- -   __NoProtection__ - Game sessions can be terminated during active
--     gameplay as a result of a scale-down event.
--
-- -   __FullProtection__ - Game sessions in @ACTIVE@ status cannot be
--     terminated during a scale-down event.
createFleet_newGameSessionProtectionPolicy :: Lens.Lens' CreateFleet (Prelude.Maybe ProtectionPolicy)
createFleet_newGameSessionProtectionPolicy = Lens.lens (\CreateFleet' {newGameSessionProtectionPolicy'} -> newGameSessionProtectionPolicy') (\s@CreateFleet' {} a -> s {newGameSessionProtectionPolicy' = a} :: CreateFleet)

-- | Used when peering your GameLift fleet with a VPC, the unique identifier
-- for the Amazon Web Services account that owns the VPC. You can find your
-- account ID in the Amazon Web Services Management Console under account
-- settings.
createFleet_peerVpcAwsAccountId :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_peerVpcAwsAccountId = Lens.lens (\CreateFleet' {peerVpcAwsAccountId} -> peerVpcAwsAccountId) (\s@CreateFleet' {} a -> s {peerVpcAwsAccountId = a} :: CreateFleet)

-- | A unique identifier for a VPC with resources to be accessed by your
-- GameLift fleet. The VPC must be in the same Region as your fleet. To
-- look up a VPC ID, use the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the Amazon Web
-- Services Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with GameLift Fleets>.
createFleet_peerVpcId :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_peerVpcId = Lens.lens (\CreateFleet' {peerVpcId} -> peerVpcId) (\s@CreateFleet' {} a -> s {peerVpcId = a} :: CreateFleet)

-- | A policy that limits the number of game sessions that an individual
-- player can create on instances in this fleet within a specified span of
-- time.
createFleet_resourceCreationLimitPolicy :: Lens.Lens' CreateFleet (Prelude.Maybe ResourceCreationLimitPolicy)
createFleet_resourceCreationLimitPolicy = Lens.lens (\CreateFleet' {resourceCreationLimitPolicy} -> resourceCreationLimitPolicy) (\s@CreateFleet' {} a -> s {resourceCreationLimitPolicy = a} :: CreateFleet)

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

-- | The unique identifier for a Realtime configuration script to be deployed
-- on fleet instances. You can use either the script ID or ARN. Scripts
-- must be uploaded to GameLift prior to creating the fleet. This fleet
-- property cannot be changed later.
createFleet_scriptId :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_scriptId = Lens.lens (\CreateFleet' {scriptId} -> scriptId) (\s@CreateFleet' {} a -> s {scriptId = a} :: CreateFleet)

-- | __This parameter is no longer used.__ Specify server launch parameters
-- using the @RuntimeConfiguration@ parameter. Requests that use this
-- parameter instead continue to be valid.
createFleet_serverLaunchParameters :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_serverLaunchParameters = Lens.lens (\CreateFleet' {serverLaunchParameters} -> serverLaunchParameters) (\s@CreateFleet' {} a -> s {serverLaunchParameters = a} :: CreateFleet)

-- | __This parameter is no longer used.__ Specify a server launch path using
-- the @RuntimeConfiguration@ parameter. Requests that use this parameter
-- instead continue to be valid.
createFleet_serverLaunchPath :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_serverLaunchPath = Lens.lens (\CreateFleet' {serverLaunchPath} -> serverLaunchPath) (\s@CreateFleet' {} a -> s {serverLaunchPath = a} :: CreateFleet)

-- | A list of labels to assign to the new fleet resource. Tags are
-- developer-defined key-value pairs. Tagging Amazon Web Services resources
-- are useful for resource management, access management and cost
-- allocation. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in the /Amazon Web Services General Reference/.
createFleet_tags :: Lens.Lens' CreateFleet (Prelude.Maybe [Tag])
createFleet_tags = Lens.lens (\CreateFleet' {tags} -> tags) (\s@CreateFleet' {} a -> s {tags = a} :: CreateFleet) Prelude.. Lens.mapping Lens.coerced

-- | A descriptive label that is associated with a fleet. Fleet names do not
-- need to be unique.
createFleet_name :: Lens.Lens' CreateFleet Prelude.Text
createFleet_name = Lens.lens (\CreateFleet' {name} -> name) (\s@CreateFleet' {} a -> s {name = a} :: CreateFleet)

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
    _salt
      `Prelude.hashWithSalt` anywhereConfiguration
      `Prelude.hashWithSalt` buildId
      `Prelude.hashWithSalt` certificateConfiguration
      `Prelude.hashWithSalt` computeType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` eC2InboundPermissions
      `Prelude.hashWithSalt` eC2InstanceType
      `Prelude.hashWithSalt` fleetType
      `Prelude.hashWithSalt` instanceRoleArn
      `Prelude.hashWithSalt` locations
      `Prelude.hashWithSalt` logPaths
      `Prelude.hashWithSalt` metricGroups
      `Prelude.hashWithSalt` newGameSessionProtectionPolicy'
      `Prelude.hashWithSalt` peerVpcAwsAccountId
      `Prelude.hashWithSalt` peerVpcId
      `Prelude.hashWithSalt` resourceCreationLimitPolicy
      `Prelude.hashWithSalt` runtimeConfiguration
      `Prelude.hashWithSalt` scriptId
      `Prelude.hashWithSalt` serverLaunchParameters
      `Prelude.hashWithSalt` serverLaunchPath
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateFleet where
  rnf CreateFleet' {..} =
    Prelude.rnf anywhereConfiguration
      `Prelude.seq` Prelude.rnf buildId
      `Prelude.seq` Prelude.rnf certificateConfiguration
      `Prelude.seq` Prelude.rnf computeType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf eC2InboundPermissions
      `Prelude.seq` Prelude.rnf eC2InstanceType
      `Prelude.seq` Prelude.rnf fleetType
      `Prelude.seq` Prelude.rnf instanceRoleArn
      `Prelude.seq` Prelude.rnf locations
      `Prelude.seq` Prelude.rnf logPaths
      `Prelude.seq` Prelude.rnf metricGroups
      `Prelude.seq` Prelude.rnf newGameSessionProtectionPolicy'
      `Prelude.seq` Prelude.rnf peerVpcAwsAccountId
      `Prelude.seq` Prelude.rnf peerVpcId
      `Prelude.seq` Prelude.rnf
        resourceCreationLimitPolicy
      `Prelude.seq` Prelude.rnf runtimeConfiguration
      `Prelude.seq` Prelude.rnf scriptId
      `Prelude.seq` Prelude.rnf
        serverLaunchParameters
      `Prelude.seq` Prelude.rnf serverLaunchPath
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name

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
          [ ("AnywhereConfiguration" Data..=)
              Prelude.<$> anywhereConfiguration,
            ("BuildId" Data..=) Prelude.<$> buildId,
            ("CertificateConfiguration" Data..=)
              Prelude.<$> certificateConfiguration,
            ("ComputeType" Data..=) Prelude.<$> computeType,
            ("Description" Data..=) Prelude.<$> description,
            ("EC2InboundPermissions" Data..=)
              Prelude.<$> eC2InboundPermissions,
            ("EC2InstanceType" Data..=)
              Prelude.<$> eC2InstanceType,
            ("FleetType" Data..=) Prelude.<$> fleetType,
            ("InstanceRoleArn" Data..=)
              Prelude.<$> instanceRoleArn,
            ("Locations" Data..=) Prelude.<$> locations,
            ("LogPaths" Data..=) Prelude.<$> logPaths,
            ("MetricGroups" Data..=) Prelude.<$> metricGroups,
            ("NewGameSessionProtectionPolicy" Data..=)
              Prelude.<$> newGameSessionProtectionPolicy',
            ("PeerVpcAwsAccountId" Data..=)
              Prelude.<$> peerVpcAwsAccountId,
            ("PeerVpcId" Data..=) Prelude.<$> peerVpcId,
            ("ResourceCreationLimitPolicy" Data..=)
              Prelude.<$> resourceCreationLimitPolicy,
            ("RuntimeConfiguration" Data..=)
              Prelude.<$> runtimeConfiguration,
            ("ScriptId" Data..=) Prelude.<$> scriptId,
            ("ServerLaunchParameters" Data..=)
              Prelude.<$> serverLaunchParameters,
            ("ServerLaunchPath" Data..=)
              Prelude.<$> serverLaunchPath,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateFleet where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFleetResponse' smart constructor.
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
    Prelude.rnf fleetAttributes `Prelude.seq`
      Prelude.rnf locationStates `Prelude.seq`
        Prelude.rnf httpStatus
