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
-- Creates a new fleet to run your game servers. whether they are custom
-- game builds or Realtime Servers with game-specific script. A fleet is a
-- set of Amazon Elastic Compute Cloud (Amazon EC2) instances, each of
-- which can host multiple game sessions. When creating a fleet, you choose
-- the hardware specifications, set some configuration options, and specify
-- the game server to deploy on the new fleet.
--
-- To create a new fleet, provide the following: (1) a fleet name, (2) an
-- EC2 instance type and fleet type (spot or on-demand), (3) the build ID
-- for your game build or script ID if using Realtime Servers, and (4) a
-- runtime configuration, which determines how game servers will run on
-- each instance in the fleet.
--
-- If the @CreateFleet@ call is successful, Amazon GameLift performs the
-- following tasks. You can track the process of a fleet by checking the
-- fleet status or by monitoring fleet creation events:
--
-- -   Creates a fleet resource. Status: @NEW@.
--
-- -   Begins writing events to the fleet event log, which can be accessed
--     in the Amazon GameLift console.
--
-- -   Sets the fleet\'s target capacity to 1 (desired instances), which
--     triggers Amazon GameLift to start one new EC2 instance.
--
-- -   Downloads the game build or Realtime script to the new instance and
--     installs it. Statuses: @DOWNLOADING@, @VALIDATING@, @BUILDING@.
--
-- -   Starts launching server processes on the instance. If the fleet is
--     configured to run multiple server processes per instance, Amazon
--     GameLift staggers each process launch by a few seconds. Status:
--     @ACTIVATING@.
--
-- -   Sets the fleet\'s status to @ACTIVE@ as soon as one server process
--     is ready to host a game session.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting Up Fleets>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-creating-debug.html#fleets-creating-debug-creation Debug Fleet Creation Issues>
--
-- __Related operations__
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
module Network.AWS.GameLift.CreateFleet
  ( -- * Creating a Request
    CreateFleet (..),
    newCreateFleet,

    -- * Request Lenses
    createFleet_fleetType,
    createFleet_peerVpcAwsAccountId,
    createFleet_instanceRoleArn,
    createFleet_certificateConfiguration,
    createFleet_serverLaunchPath,
    createFleet_serverLaunchParameters,
    createFleet_logPaths,
    createFleet_newGameSessionProtectionPolicy,
    createFleet_runtimeConfiguration,
    createFleet_tags,
    createFleet_eC2InboundPermissions,
    createFleet_description,
    createFleet_resourceCreationLimitPolicy,
    createFleet_buildId,
    createFleet_metricGroups,
    createFleet_peerVpcId,
    createFleet_scriptId,
    createFleet_name,
    createFleet_eC2InstanceType,

    -- * Destructuring the Response
    CreateFleetResponse (..),
    newCreateFleetResponse,

    -- * Response Lenses
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
  { -- | Indicates whether to use On-Demand instances or Spot instances for this
    -- fleet. If empty, the default is @ON_DEMAND@. Both categories of
    -- instances use identical hardware and configurations based on the
    -- instance type selected for this fleet. Learn more about
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-ec2-instances.html#gamelift-ec2-instances-spot On-Demand versus Spot Instances>.
    fleetType :: Prelude.Maybe FleetType,
    -- | A unique identifier for the AWS account with the VPC that you want to
    -- peer your Amazon GameLift fleet with. You can find your account ID in
    -- the AWS Management Console under account settings.
    peerVpcAwsAccountId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for an AWS IAM role that manages access to your AWS
    -- services. Fleets with an instance role ARN allow applications that are
    -- running on the fleet\'s instances to assume the role. Learn more about
    -- using on-box credentials for your game servers at
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-resources.html Access external resources from a game server>.
    -- To call this operation with instance role ARN, you must have IAM
    -- PassRole permissions. See
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-iam-policy-examples.html IAM policy examples for GameLift>.
    instanceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to generate a TLS\/SSL certificate for the new fleet.
    -- TLS certificates are used for encrypting traffic between game clients
    -- and game servers running on GameLift. If this parameter is not
    -- specified, the default value, DISABLED, is used. This fleet setting
    -- cannot be changed once the fleet is created. Learn more at
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-howitworks.html#gamelift-howitworks-security Securing Client\/Server Communication>.
    --
    -- Note: This feature requires the AWS Certificate Manager (ACM) service,
    -- which is available in the AWS global partition but not in all other
    -- partitions. When working in a partition that does not support this
    -- feature, a request for a new fleet with certificate generation results
    -- fails with a 4xx unsupported Region error.
    --
    -- Valid values include:
    --
    -- -   __GENERATED__ - Generate a TLS\/SSL certificate for this fleet.
    --
    -- -   __DISABLED__ - (default) Do not generate a TLS\/SSL certificate for
    --     this fleet.
    certificateConfiguration :: Prelude.Maybe CertificateConfiguration,
    -- | This parameter is no longer used. Instead, specify a server launch path
    -- using the @RuntimeConfiguration@ parameter. Requests that specify a
    -- server launch path and launch parameters instead of a runtime
    -- configuration will continue to work.
    serverLaunchPath :: Prelude.Maybe Prelude.Text,
    -- | This parameter is no longer used. Instead, specify server launch
    -- parameters in the @RuntimeConfiguration@ parameter. (Requests that
    -- specify a server launch path and launch parameters instead of a runtime
    -- configuration will continue to work.)
    serverLaunchParameters :: Prelude.Maybe Prelude.Text,
    -- | This parameter is no longer used. Instead, to specify where Amazon
    -- GameLift should store log files once a server process shuts down, use
    -- the Amazon GameLift server API @ProcessReady()@ and specify one or more
    -- directory paths in @logParameters@. See more information in the
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api-ref.html#gamelift-sdk-server-api-ref-dataypes-process Server API Reference>.
    logPaths :: Prelude.Maybe [Prelude.Text],
    -- | A game session protection policy to apply to all instances in this
    -- fleet. If this parameter is not set, instances in this fleet default to
    -- no protection. You can change a fleet\'s protection policy using
    -- UpdateFleetAttributes, but this change will only affect sessions created
    -- after the policy change. You can also set protection for individual
    -- instances using UpdateGameSession.
    --
    -- -   __NoProtection__ - The game session can be terminated during a
    --     scale-down event.
    --
    -- -   __FullProtection__ - If the game session is in an @ACTIVE@ status,
    --     it cannot be terminated during a scale-down event.
    newGameSessionProtectionPolicy' :: Prelude.Maybe ProtectionPolicy,
    -- | Instructions for launching server processes on each instance in the
    -- fleet. Server processes run either a custom game build executable or a
    -- Realtime script. The runtime configuration defines the server
    -- executables or launch script file, launch parameters, and the number of
    -- processes to run concurrently on each instance. When creating a fleet,
    -- the runtime configuration must have at least one server process
    -- configuration; otherwise the request fails with an invalid request
    -- exception. (This parameter replaces the parameters @ServerLaunchPath@
    -- and @ServerLaunchParameters@, although requests that contain values for
    -- these parameters instead of a runtime configuration will continue to
    -- work.) This parameter is required unless the parameters
    -- @ServerLaunchPath@ and @ServerLaunchParameters@ are defined. Runtime
    -- configuration replaced these parameters, but fleets that use them will
    -- continue to work.
    runtimeConfiguration :: Prelude.Maybe RuntimeConfiguration,
    -- | A list of labels to assign to the new fleet resource. Tags are
    -- developer-defined key-value pairs. Tagging AWS resources are useful for
    -- resource management, access management and cost allocation. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
    -- in the /AWS General Reference/. Once the resource is created, you can
    -- use TagResource, UntagResource, and ListTagsForResource to add, remove,
    -- and view tags. The maximum tag limit may be lower than stated. See the
    -- AWS General Reference for actual tagging limits.
    tags :: Prelude.Maybe [Tag],
    -- | Range of IP addresses and port settings that permit inbound traffic to
    -- access game sessions that are running on the fleet. For fleets using a
    -- custom game build, this parameter is required before game sessions
    -- running on the fleet can accept connections. For Realtime Servers
    -- fleets, Amazon GameLift automatically sets TCP and UDP ranges for use by
    -- the Realtime servers. You can specify multiple permission settings or
    -- add more by updating the fleet.
    eC2InboundPermissions :: Prelude.Maybe [IpPermission],
    -- | A human-readable description of a fleet.
    description :: Prelude.Maybe Prelude.Text,
    -- | A policy that limits the number of game sessions an individual player
    -- can create over a span of time for this fleet.
    resourceCreationLimitPolicy :: Prelude.Maybe ResourceCreationLimitPolicy,
    -- | A unique identifier for a build to be deployed on the new fleet. You can
    -- use either the build ID or ARN value. The custom game server build must
    -- have been successfully uploaded to Amazon GameLift and be in a @READY@
    -- status. This fleet setting cannot be changed once the fleet is created.
    buildId :: Prelude.Maybe Prelude.Text,
    -- | The name of an Amazon CloudWatch metric group to add this fleet to. A
    -- metric group aggregates the metrics for all fleets in the group. Specify
    -- an existing metric group name, or provide a new name to create a new
    -- metric group. A fleet can only be included in one metric group at a
    -- time.
    metricGroups :: Prelude.Maybe [Prelude.Text],
    -- | A unique identifier for a VPC with resources to be accessed by your
    -- Amazon GameLift fleet. The VPC must be in the same Region as your fleet.
    -- To look up a VPC ID, use the
    -- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS
    -- Management Console. Learn more about VPC peering in
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets>.
    peerVpcId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a Realtime script to be deployed on the new
    -- fleet. You can use either the script ID or ARN value. The Realtime
    -- script must have been successfully uploaded to Amazon GameLift. This
    -- fleet setting cannot be changed once the fleet is created.
    scriptId :: Prelude.Maybe Prelude.Text,
    -- | A descriptive label that is associated with a fleet. Fleet names do not
    -- need to be unique.
    name :: Prelude.Text,
    -- | The name of an EC2 instance type that is supported in Amazon GameLift. A
    -- fleet instance type determines the computing resources of each instance
    -- in the fleet, including CPU, memory, storage, and networking capacity.
    -- Amazon GameLift supports the following EC2 instance types. See
    -- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
    -- for detailed descriptions.
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
-- 'fleetType', 'createFleet_fleetType' - Indicates whether to use On-Demand instances or Spot instances for this
-- fleet. If empty, the default is @ON_DEMAND@. Both categories of
-- instances use identical hardware and configurations based on the
-- instance type selected for this fleet. Learn more about
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-ec2-instances.html#gamelift-ec2-instances-spot On-Demand versus Spot Instances>.
--
-- 'peerVpcAwsAccountId', 'createFleet_peerVpcAwsAccountId' - A unique identifier for the AWS account with the VPC that you want to
-- peer your Amazon GameLift fleet with. You can find your account ID in
-- the AWS Management Console under account settings.
--
-- 'instanceRoleArn', 'createFleet_instanceRoleArn' - A unique identifier for an AWS IAM role that manages access to your AWS
-- services. Fleets with an instance role ARN allow applications that are
-- running on the fleet\'s instances to assume the role. Learn more about
-- using on-box credentials for your game servers at
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-resources.html Access external resources from a game server>.
-- To call this operation with instance role ARN, you must have IAM
-- PassRole permissions. See
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-iam-policy-examples.html IAM policy examples for GameLift>.
--
-- 'certificateConfiguration', 'createFleet_certificateConfiguration' - Indicates whether to generate a TLS\/SSL certificate for the new fleet.
-- TLS certificates are used for encrypting traffic between game clients
-- and game servers running on GameLift. If this parameter is not
-- specified, the default value, DISABLED, is used. This fleet setting
-- cannot be changed once the fleet is created. Learn more at
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-howitworks.html#gamelift-howitworks-security Securing Client\/Server Communication>.
--
-- Note: This feature requires the AWS Certificate Manager (ACM) service,
-- which is available in the AWS global partition but not in all other
-- partitions. When working in a partition that does not support this
-- feature, a request for a new fleet with certificate generation results
-- fails with a 4xx unsupported Region error.
--
-- Valid values include:
--
-- -   __GENERATED__ - Generate a TLS\/SSL certificate for this fleet.
--
-- -   __DISABLED__ - (default) Do not generate a TLS\/SSL certificate for
--     this fleet.
--
-- 'serverLaunchPath', 'createFleet_serverLaunchPath' - This parameter is no longer used. Instead, specify a server launch path
-- using the @RuntimeConfiguration@ parameter. Requests that specify a
-- server launch path and launch parameters instead of a runtime
-- configuration will continue to work.
--
-- 'serverLaunchParameters', 'createFleet_serverLaunchParameters' - This parameter is no longer used. Instead, specify server launch
-- parameters in the @RuntimeConfiguration@ parameter. (Requests that
-- specify a server launch path and launch parameters instead of a runtime
-- configuration will continue to work.)
--
-- 'logPaths', 'createFleet_logPaths' - This parameter is no longer used. Instead, to specify where Amazon
-- GameLift should store log files once a server process shuts down, use
-- the Amazon GameLift server API @ProcessReady()@ and specify one or more
-- directory paths in @logParameters@. See more information in the
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api-ref.html#gamelift-sdk-server-api-ref-dataypes-process Server API Reference>.
--
-- 'newGameSessionProtectionPolicy'', 'createFleet_newGameSessionProtectionPolicy' - A game session protection policy to apply to all instances in this
-- fleet. If this parameter is not set, instances in this fleet default to
-- no protection. You can change a fleet\'s protection policy using
-- UpdateFleetAttributes, but this change will only affect sessions created
-- after the policy change. You can also set protection for individual
-- instances using UpdateGameSession.
--
-- -   __NoProtection__ - The game session can be terminated during a
--     scale-down event.
--
-- -   __FullProtection__ - If the game session is in an @ACTIVE@ status,
--     it cannot be terminated during a scale-down event.
--
-- 'runtimeConfiguration', 'createFleet_runtimeConfiguration' - Instructions for launching server processes on each instance in the
-- fleet. Server processes run either a custom game build executable or a
-- Realtime script. The runtime configuration defines the server
-- executables or launch script file, launch parameters, and the number of
-- processes to run concurrently on each instance. When creating a fleet,
-- the runtime configuration must have at least one server process
-- configuration; otherwise the request fails with an invalid request
-- exception. (This parameter replaces the parameters @ServerLaunchPath@
-- and @ServerLaunchParameters@, although requests that contain values for
-- these parameters instead of a runtime configuration will continue to
-- work.) This parameter is required unless the parameters
-- @ServerLaunchPath@ and @ServerLaunchParameters@ are defined. Runtime
-- configuration replaced these parameters, but fleets that use them will
-- continue to work.
--
-- 'tags', 'createFleet_tags' - A list of labels to assign to the new fleet resource. Tags are
-- developer-defined key-value pairs. Tagging AWS resources are useful for
-- resource management, access management and cost allocation. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- in the /AWS General Reference/. Once the resource is created, you can
-- use TagResource, UntagResource, and ListTagsForResource to add, remove,
-- and view tags. The maximum tag limit may be lower than stated. See the
-- AWS General Reference for actual tagging limits.
--
-- 'eC2InboundPermissions', 'createFleet_eC2InboundPermissions' - Range of IP addresses and port settings that permit inbound traffic to
-- access game sessions that are running on the fleet. For fleets using a
-- custom game build, this parameter is required before game sessions
-- running on the fleet can accept connections. For Realtime Servers
-- fleets, Amazon GameLift automatically sets TCP and UDP ranges for use by
-- the Realtime servers. You can specify multiple permission settings or
-- add more by updating the fleet.
--
-- 'description', 'createFleet_description' - A human-readable description of a fleet.
--
-- 'resourceCreationLimitPolicy', 'createFleet_resourceCreationLimitPolicy' - A policy that limits the number of game sessions an individual player
-- can create over a span of time for this fleet.
--
-- 'buildId', 'createFleet_buildId' - A unique identifier for a build to be deployed on the new fleet. You can
-- use either the build ID or ARN value. The custom game server build must
-- have been successfully uploaded to Amazon GameLift and be in a @READY@
-- status. This fleet setting cannot be changed once the fleet is created.
--
-- 'metricGroups', 'createFleet_metricGroups' - The name of an Amazon CloudWatch metric group to add this fleet to. A
-- metric group aggregates the metrics for all fleets in the group. Specify
-- an existing metric group name, or provide a new name to create a new
-- metric group. A fleet can only be included in one metric group at a
-- time.
--
-- 'peerVpcId', 'createFleet_peerVpcId' - A unique identifier for a VPC with resources to be accessed by your
-- Amazon GameLift fleet. The VPC must be in the same Region as your fleet.
-- To look up a VPC ID, use the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS
-- Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets>.
--
-- 'scriptId', 'createFleet_scriptId' - A unique identifier for a Realtime script to be deployed on the new
-- fleet. You can use either the script ID or ARN value. The Realtime
-- script must have been successfully uploaded to Amazon GameLift. This
-- fleet setting cannot be changed once the fleet is created.
--
-- 'name', 'createFleet_name' - A descriptive label that is associated with a fleet. Fleet names do not
-- need to be unique.
--
-- 'eC2InstanceType', 'createFleet_eC2InstanceType' - The name of an EC2 instance type that is supported in Amazon GameLift. A
-- fleet instance type determines the computing resources of each instance
-- in the fleet, including CPU, memory, storage, and networking capacity.
-- Amazon GameLift supports the following EC2 instance types. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
-- for detailed descriptions.
newCreateFleet ::
  -- | 'name'
  Prelude.Text ->
  -- | 'eC2InstanceType'
  EC2InstanceType ->
  CreateFleet
newCreateFleet pName_ pEC2InstanceType_ =
  CreateFleet'
    { fleetType = Prelude.Nothing,
      peerVpcAwsAccountId = Prelude.Nothing,
      instanceRoleArn = Prelude.Nothing,
      certificateConfiguration = Prelude.Nothing,
      serverLaunchPath = Prelude.Nothing,
      serverLaunchParameters = Prelude.Nothing,
      logPaths = Prelude.Nothing,
      newGameSessionProtectionPolicy' = Prelude.Nothing,
      runtimeConfiguration = Prelude.Nothing,
      tags = Prelude.Nothing,
      eC2InboundPermissions = Prelude.Nothing,
      description = Prelude.Nothing,
      resourceCreationLimitPolicy = Prelude.Nothing,
      buildId = Prelude.Nothing,
      metricGroups = Prelude.Nothing,
      peerVpcId = Prelude.Nothing,
      scriptId = Prelude.Nothing,
      name = pName_,
      eC2InstanceType = pEC2InstanceType_
    }

-- | Indicates whether to use On-Demand instances or Spot instances for this
-- fleet. If empty, the default is @ON_DEMAND@. Both categories of
-- instances use identical hardware and configurations based on the
-- instance type selected for this fleet. Learn more about
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-ec2-instances.html#gamelift-ec2-instances-spot On-Demand versus Spot Instances>.
createFleet_fleetType :: Lens.Lens' CreateFleet (Prelude.Maybe FleetType)
createFleet_fleetType = Lens.lens (\CreateFleet' {fleetType} -> fleetType) (\s@CreateFleet' {} a -> s {fleetType = a} :: CreateFleet)

-- | A unique identifier for the AWS account with the VPC that you want to
-- peer your Amazon GameLift fleet with. You can find your account ID in
-- the AWS Management Console under account settings.
createFleet_peerVpcAwsAccountId :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_peerVpcAwsAccountId = Lens.lens (\CreateFleet' {peerVpcAwsAccountId} -> peerVpcAwsAccountId) (\s@CreateFleet' {} a -> s {peerVpcAwsAccountId = a} :: CreateFleet)

-- | A unique identifier for an AWS IAM role that manages access to your AWS
-- services. Fleets with an instance role ARN allow applications that are
-- running on the fleet\'s instances to assume the role. Learn more about
-- using on-box credentials for your game servers at
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-resources.html Access external resources from a game server>.
-- To call this operation with instance role ARN, you must have IAM
-- PassRole permissions. See
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-iam-policy-examples.html IAM policy examples for GameLift>.
createFleet_instanceRoleArn :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_instanceRoleArn = Lens.lens (\CreateFleet' {instanceRoleArn} -> instanceRoleArn) (\s@CreateFleet' {} a -> s {instanceRoleArn = a} :: CreateFleet)

-- | Indicates whether to generate a TLS\/SSL certificate for the new fleet.
-- TLS certificates are used for encrypting traffic between game clients
-- and game servers running on GameLift. If this parameter is not
-- specified, the default value, DISABLED, is used. This fleet setting
-- cannot be changed once the fleet is created. Learn more at
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-howitworks.html#gamelift-howitworks-security Securing Client\/Server Communication>.
--
-- Note: This feature requires the AWS Certificate Manager (ACM) service,
-- which is available in the AWS global partition but not in all other
-- partitions. When working in a partition that does not support this
-- feature, a request for a new fleet with certificate generation results
-- fails with a 4xx unsupported Region error.
--
-- Valid values include:
--
-- -   __GENERATED__ - Generate a TLS\/SSL certificate for this fleet.
--
-- -   __DISABLED__ - (default) Do not generate a TLS\/SSL certificate for
--     this fleet.
createFleet_certificateConfiguration :: Lens.Lens' CreateFleet (Prelude.Maybe CertificateConfiguration)
createFleet_certificateConfiguration = Lens.lens (\CreateFleet' {certificateConfiguration} -> certificateConfiguration) (\s@CreateFleet' {} a -> s {certificateConfiguration = a} :: CreateFleet)

-- | This parameter is no longer used. Instead, specify a server launch path
-- using the @RuntimeConfiguration@ parameter. Requests that specify a
-- server launch path and launch parameters instead of a runtime
-- configuration will continue to work.
createFleet_serverLaunchPath :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_serverLaunchPath = Lens.lens (\CreateFleet' {serverLaunchPath} -> serverLaunchPath) (\s@CreateFleet' {} a -> s {serverLaunchPath = a} :: CreateFleet)

-- | This parameter is no longer used. Instead, specify server launch
-- parameters in the @RuntimeConfiguration@ parameter. (Requests that
-- specify a server launch path and launch parameters instead of a runtime
-- configuration will continue to work.)
createFleet_serverLaunchParameters :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_serverLaunchParameters = Lens.lens (\CreateFleet' {serverLaunchParameters} -> serverLaunchParameters) (\s@CreateFleet' {} a -> s {serverLaunchParameters = a} :: CreateFleet)

-- | This parameter is no longer used. Instead, to specify where Amazon
-- GameLift should store log files once a server process shuts down, use
-- the Amazon GameLift server API @ProcessReady()@ and specify one or more
-- directory paths in @logParameters@. See more information in the
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api-ref.html#gamelift-sdk-server-api-ref-dataypes-process Server API Reference>.
createFleet_logPaths :: Lens.Lens' CreateFleet (Prelude.Maybe [Prelude.Text])
createFleet_logPaths = Lens.lens (\CreateFleet' {logPaths} -> logPaths) (\s@CreateFleet' {} a -> s {logPaths = a} :: CreateFleet) Prelude.. Lens.mapping Lens._Coerce

-- | A game session protection policy to apply to all instances in this
-- fleet. If this parameter is not set, instances in this fleet default to
-- no protection. You can change a fleet\'s protection policy using
-- UpdateFleetAttributes, but this change will only affect sessions created
-- after the policy change. You can also set protection for individual
-- instances using UpdateGameSession.
--
-- -   __NoProtection__ - The game session can be terminated during a
--     scale-down event.
--
-- -   __FullProtection__ - If the game session is in an @ACTIVE@ status,
--     it cannot be terminated during a scale-down event.
createFleet_newGameSessionProtectionPolicy :: Lens.Lens' CreateFleet (Prelude.Maybe ProtectionPolicy)
createFleet_newGameSessionProtectionPolicy = Lens.lens (\CreateFleet' {newGameSessionProtectionPolicy'} -> newGameSessionProtectionPolicy') (\s@CreateFleet' {} a -> s {newGameSessionProtectionPolicy' = a} :: CreateFleet)

-- | Instructions for launching server processes on each instance in the
-- fleet. Server processes run either a custom game build executable or a
-- Realtime script. The runtime configuration defines the server
-- executables or launch script file, launch parameters, and the number of
-- processes to run concurrently on each instance. When creating a fleet,
-- the runtime configuration must have at least one server process
-- configuration; otherwise the request fails with an invalid request
-- exception. (This parameter replaces the parameters @ServerLaunchPath@
-- and @ServerLaunchParameters@, although requests that contain values for
-- these parameters instead of a runtime configuration will continue to
-- work.) This parameter is required unless the parameters
-- @ServerLaunchPath@ and @ServerLaunchParameters@ are defined. Runtime
-- configuration replaced these parameters, but fleets that use them will
-- continue to work.
createFleet_runtimeConfiguration :: Lens.Lens' CreateFleet (Prelude.Maybe RuntimeConfiguration)
createFleet_runtimeConfiguration = Lens.lens (\CreateFleet' {runtimeConfiguration} -> runtimeConfiguration) (\s@CreateFleet' {} a -> s {runtimeConfiguration = a} :: CreateFleet)

-- | A list of labels to assign to the new fleet resource. Tags are
-- developer-defined key-value pairs. Tagging AWS resources are useful for
-- resource management, access management and cost allocation. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- in the /AWS General Reference/. Once the resource is created, you can
-- use TagResource, UntagResource, and ListTagsForResource to add, remove,
-- and view tags. The maximum tag limit may be lower than stated. See the
-- AWS General Reference for actual tagging limits.
createFleet_tags :: Lens.Lens' CreateFleet (Prelude.Maybe [Tag])
createFleet_tags = Lens.lens (\CreateFleet' {tags} -> tags) (\s@CreateFleet' {} a -> s {tags = a} :: CreateFleet) Prelude.. Lens.mapping Lens._Coerce

-- | Range of IP addresses and port settings that permit inbound traffic to
-- access game sessions that are running on the fleet. For fleets using a
-- custom game build, this parameter is required before game sessions
-- running on the fleet can accept connections. For Realtime Servers
-- fleets, Amazon GameLift automatically sets TCP and UDP ranges for use by
-- the Realtime servers. You can specify multiple permission settings or
-- add more by updating the fleet.
createFleet_eC2InboundPermissions :: Lens.Lens' CreateFleet (Prelude.Maybe [IpPermission])
createFleet_eC2InboundPermissions = Lens.lens (\CreateFleet' {eC2InboundPermissions} -> eC2InboundPermissions) (\s@CreateFleet' {} a -> s {eC2InboundPermissions = a} :: CreateFleet) Prelude.. Lens.mapping Lens._Coerce

-- | A human-readable description of a fleet.
createFleet_description :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_description = Lens.lens (\CreateFleet' {description} -> description) (\s@CreateFleet' {} a -> s {description = a} :: CreateFleet)

-- | A policy that limits the number of game sessions an individual player
-- can create over a span of time for this fleet.
createFleet_resourceCreationLimitPolicy :: Lens.Lens' CreateFleet (Prelude.Maybe ResourceCreationLimitPolicy)
createFleet_resourceCreationLimitPolicy = Lens.lens (\CreateFleet' {resourceCreationLimitPolicy} -> resourceCreationLimitPolicy) (\s@CreateFleet' {} a -> s {resourceCreationLimitPolicy = a} :: CreateFleet)

-- | A unique identifier for a build to be deployed on the new fleet. You can
-- use either the build ID or ARN value. The custom game server build must
-- have been successfully uploaded to Amazon GameLift and be in a @READY@
-- status. This fleet setting cannot be changed once the fleet is created.
createFleet_buildId :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_buildId = Lens.lens (\CreateFleet' {buildId} -> buildId) (\s@CreateFleet' {} a -> s {buildId = a} :: CreateFleet)

-- | The name of an Amazon CloudWatch metric group to add this fleet to. A
-- metric group aggregates the metrics for all fleets in the group. Specify
-- an existing metric group name, or provide a new name to create a new
-- metric group. A fleet can only be included in one metric group at a
-- time.
createFleet_metricGroups :: Lens.Lens' CreateFleet (Prelude.Maybe [Prelude.Text])
createFleet_metricGroups = Lens.lens (\CreateFleet' {metricGroups} -> metricGroups) (\s@CreateFleet' {} a -> s {metricGroups = a} :: CreateFleet) Prelude.. Lens.mapping Lens._Coerce

-- | A unique identifier for a VPC with resources to be accessed by your
-- Amazon GameLift fleet. The VPC must be in the same Region as your fleet.
-- To look up a VPC ID, use the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS
-- Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets>.
createFleet_peerVpcId :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_peerVpcId = Lens.lens (\CreateFleet' {peerVpcId} -> peerVpcId) (\s@CreateFleet' {} a -> s {peerVpcId = a} :: CreateFleet)

-- | A unique identifier for a Realtime script to be deployed on the new
-- fleet. You can use either the script ID or ARN value. The Realtime
-- script must have been successfully uploaded to Amazon GameLift. This
-- fleet setting cannot be changed once the fleet is created.
createFleet_scriptId :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_scriptId = Lens.lens (\CreateFleet' {scriptId} -> scriptId) (\s@CreateFleet' {} a -> s {scriptId = a} :: CreateFleet)

-- | A descriptive label that is associated with a fleet. Fleet names do not
-- need to be unique.
createFleet_name :: Lens.Lens' CreateFleet Prelude.Text
createFleet_name = Lens.lens (\CreateFleet' {name} -> name) (\s@CreateFleet' {} a -> s {name = a} :: CreateFleet)

-- | The name of an EC2 instance type that is supported in Amazon GameLift. A
-- fleet instance type determines the computing resources of each instance
-- in the fleet, including CPU, memory, storage, and networking capacity.
-- Amazon GameLift supports the following EC2 instance types. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
-- for detailed descriptions.
createFleet_eC2InstanceType :: Lens.Lens' CreateFleet EC2InstanceType
createFleet_eC2InstanceType = Lens.lens (\CreateFleet' {eC2InstanceType} -> eC2InstanceType) (\s@CreateFleet' {} a -> s {eC2InstanceType = a} :: CreateFleet)

instance Core.AWSRequest CreateFleet where
  type AWSResponse CreateFleet = CreateFleetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFleetResponse'
            Prelude.<$> (x Core..?> "FleetAttributes")
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
          [ ("FleetType" Core..=) Prelude.<$> fleetType,
            ("PeerVpcAwsAccountId" Core..=)
              Prelude.<$> peerVpcAwsAccountId,
            ("InstanceRoleArn" Core..=)
              Prelude.<$> instanceRoleArn,
            ("CertificateConfiguration" Core..=)
              Prelude.<$> certificateConfiguration,
            ("ServerLaunchPath" Core..=)
              Prelude.<$> serverLaunchPath,
            ("ServerLaunchParameters" Core..=)
              Prelude.<$> serverLaunchParameters,
            ("LogPaths" Core..=) Prelude.<$> logPaths,
            ("NewGameSessionProtectionPolicy" Core..=)
              Prelude.<$> newGameSessionProtectionPolicy',
            ("RuntimeConfiguration" Core..=)
              Prelude.<$> runtimeConfiguration,
            ("Tags" Core..=) Prelude.<$> tags,
            ("EC2InboundPermissions" Core..=)
              Prelude.<$> eC2InboundPermissions,
            ("Description" Core..=) Prelude.<$> description,
            ("ResourceCreationLimitPolicy" Core..=)
              Prelude.<$> resourceCreationLimitPolicy,
            ("BuildId" Core..=) Prelude.<$> buildId,
            ("MetricGroups" Core..=) Prelude.<$> metricGroups,
            ("PeerVpcId" Core..=) Prelude.<$> peerVpcId,
            ("ScriptId" Core..=) Prelude.<$> scriptId,
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
  { -- | Properties for the newly created fleet.
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
-- 'fleetAttributes', 'createFleetResponse_fleetAttributes' - Properties for the newly created fleet.
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
      httpStatus = pHttpStatus_
    }

-- | Properties for the newly created fleet.
createFleetResponse_fleetAttributes :: Lens.Lens' CreateFleetResponse (Prelude.Maybe FleetAttributes)
createFleetResponse_fleetAttributes = Lens.lens (\CreateFleetResponse' {fleetAttributes} -> fleetAttributes) (\s@CreateFleetResponse' {} a -> s {fleetAttributes = a} :: CreateFleetResponse)

-- | The response's http status code.
createFleetResponse_httpStatus :: Lens.Lens' CreateFleetResponse Prelude.Int
createFleetResponse_httpStatus = Lens.lens (\CreateFleetResponse' {httpStatus} -> httpStatus) (\s@CreateFleetResponse' {} a -> s {httpStatus = a} :: CreateFleetResponse)

instance Prelude.NFData CreateFleetResponse
