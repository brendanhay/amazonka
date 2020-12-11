{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.CreateFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new fleet to run your game servers. whether they are custom game builds or Realtime Servers with game-specific script. A fleet is a set of Amazon Elastic Compute Cloud (Amazon EC2) instances, each of which can host multiple game sessions. When creating a fleet, you choose the hardware specifications, set some configuration options, and specify the game server to deploy on the new fleet.
--
-- To create a new fleet, provide the following: (1) a fleet name, (2) an EC2 instance type and fleet type (spot or on-demand), (3) the build ID for your game build or script ID if using Realtime Servers, and (4) a runtime configuration, which determines how game servers will run on each instance in the fleet.
-- If the @CreateFleet@ call is successful, Amazon GameLift performs the following tasks. You can track the process of a fleet by checking the fleet status or by monitoring fleet creation events:
--
--     * Creates a fleet resource. Status: @NEW@ .
--
--
--     * Begins writing events to the fleet event log, which can be accessed in the Amazon GameLift console.
--
--
--     * Sets the fleet's target capacity to 1 (desired instances), which triggers Amazon GameLift to start one new EC2 instance.
--
--
--     * Downloads the game build or Realtime script to the new instance and installs it. Statuses: @DOWNLOADING@ , @VALIDATING@ , @BUILDING@ .
--
--
--     * Starts launching server processes on the instance. If the fleet is configured to run multiple server processes per instance, Amazon GameLift staggers each process launch by a few seconds. Status: @ACTIVATING@ .
--
--
--     * Sets the fleet's status to @ACTIVE@ as soon as one server process is ready to host a game session.
--
--
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting Up Fleets>
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-creating-debug.html#fleets-creating-debug-creation Debug Fleet Creation Issues>
-- __Related operations__
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
module Network.AWS.GameLift.CreateFleet
  ( -- * Creating a request
    CreateFleet (..),
    mkCreateFleet,

    -- ** Request lenses
    cfServerLaunchParameters,
    cfLogPaths,
    cfPeerVPCId,
    cfBuildId,
    cfFleetType,
    cfPeerVPCAWSAccountId,
    cfEC2InboundPermissions,
    cfRuntimeConfiguration,
    cfNewGameSessionProtectionPolicy,
    cfScriptId,
    cfCertificateConfiguration,
    cfServerLaunchPath,
    cfInstanceRoleARN,
    cfMetricGroups,
    cfDescription,
    cfResourceCreationLimitPolicy,
    cfTags,
    cfName,
    cfEC2InstanceType,

    -- * Destructuring the response
    CreateFleetResponse (..),
    mkCreateFleetResponse,

    -- ** Response lenses
    cfrsFleetAttributes,
    cfrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkCreateFleet' smart constructor.
data CreateFleet = CreateFleet'
  { serverLaunchParameters ::
      Lude.Maybe Lude.Text,
    logPaths :: Lude.Maybe [Lude.Text],
    peerVPCId :: Lude.Maybe Lude.Text,
    buildId :: Lude.Maybe Lude.Text,
    fleetType :: Lude.Maybe FleetType,
    peerVPCAWSAccountId :: Lude.Maybe Lude.Text,
    ec2InboundPermissions :: Lude.Maybe [IPPermission],
    runtimeConfiguration :: Lude.Maybe RuntimeConfiguration,
    newGameSessionProtectionPolicy :: Lude.Maybe ProtectionPolicy,
    scriptId :: Lude.Maybe Lude.Text,
    certificateConfiguration :: Lude.Maybe CertificateConfiguration,
    serverLaunchPath :: Lude.Maybe Lude.Text,
    instanceRoleARN :: Lude.Maybe Lude.Text,
    metricGroups :: Lude.Maybe [Lude.Text],
    description :: Lude.Maybe Lude.Text,
    resourceCreationLimitPolicy ::
      Lude.Maybe ResourceCreationLimitPolicy,
    tags :: Lude.Maybe [Tag],
    name :: Lude.Text,
    ec2InstanceType :: EC2InstanceType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateFleet' with the minimum fields required to make a request.
--
-- * 'buildId' - A unique identifier for a build to be deployed on the new fleet. You can use either the build ID or ARN value. The custom game server build must have been successfully uploaded to Amazon GameLift and be in a @READY@ status. This fleet setting cannot be changed once the fleet is created.
-- * 'certificateConfiguration' - Indicates whether to generate a TLS/SSL certificate for the new fleet. TLS certificates are used for encrypting traffic between game clients and game servers running on GameLift. If this parameter is not specified, the default value, DISABLED, is used. This fleet setting cannot be changed once the fleet is created. Learn more at <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-howitworks.html#gamelift-howitworks-security Securing Client/Server Communication> .
--
-- Note: This feature requires the AWS Certificate Manager (ACM) service, which is available in the AWS global partition but not in all other partitions. When working in a partition that does not support this feature, a request for a new fleet with certificate generation results fails with a 4xx unsupported Region error.
-- Valid values include:
--
--     * __GENERATED__ - Generate a TLS/SSL certificate for this fleet.
--
--
--     * __DISABLED__ - (default) Do not generate a TLS/SSL certificate for this fleet.
--
--
-- * 'description' - A human-readable description of a fleet.
-- * 'ec2InboundPermissions' - Range of IP addresses and port settings that permit inbound traffic to access game sessions that are running on the fleet. For fleets using a custom game build, this parameter is required before game sessions running on the fleet can accept connections. For Realtime Servers fleets, Amazon GameLift automatically sets TCP and UDP ranges for use by the Realtime servers. You can specify multiple permission settings or add more by updating the fleet.
-- * 'ec2InstanceType' - The name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
-- * 'fleetType' - Indicates whether to use On-Demand instances or Spot instances for this fleet. If empty, the default is @ON_DEMAND@ . Both categories of instances use identical hardware and configurations based on the instance type selected for this fleet. Learn more about <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-ec2-instances.html#gamelift-ec2-instances-spot On-Demand versus Spot Instances> .
-- * 'instanceRoleARN' - A unique identifier for an AWS IAM role that manages access to your AWS services. Fleets with an instance role ARN allow applications that are running on the fleet's instances to assume the role. Learn more about using on-box credentials for your game servers at <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-resources.html Access external resources from a game server> . To call this operation with instance role ARN, you must have IAM PassRole permissions. See <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-iam-policy-examples.html IAM policy examples for GameLift> .
-- * 'logPaths' - This parameter is no longer used. Instead, to specify where Amazon GameLift should store log files once a server process shuts down, use the Amazon GameLift server API @ProcessReady()@ and specify one or more directory paths in @logParameters@ . See more information in the <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api-ref.html#gamelift-sdk-server-api-ref-dataypes-process Server API Reference> .
-- * 'metricGroups' - The name of an Amazon CloudWatch metric group to add this fleet to. A metric group aggregates the metrics for all fleets in the group. Specify an existing metric group name, or provide a new name to create a new metric group. A fleet can only be included in one metric group at a time.
-- * 'name' - A descriptive label that is associated with a fleet. Fleet names do not need to be unique.
-- * 'newGameSessionProtectionPolicy' - A game session protection policy to apply to all instances in this fleet. If this parameter is not set, instances in this fleet default to no protection. You can change a fleet's protection policy using 'UpdateFleetAttributes' , but this change will only affect sessions created after the policy change. You can also set protection for individual instances using 'UpdateGameSession' .
--
--
--     * __NoProtection__ - The game session can be terminated during a scale-down event.
--
--
--     * __FullProtection__ - If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
--
--
-- * 'peerVPCAWSAccountId' - A unique identifier for the AWS account with the VPC that you want to peer your Amazon GameLift fleet with. You can find your account ID in the AWS Management Console under account settings.
-- * 'peerVPCId' - A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region as your fleet. To look up a VPC ID, use the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
-- * 'resourceCreationLimitPolicy' - A policy that limits the number of game sessions an individual player can create over a span of time for this fleet.
-- * 'runtimeConfiguration' - Instructions for launching server processes on each instance in the fleet. Server processes run either a custom game build executable or a Realtime script. The runtime configuration defines the server executables or launch script file, launch parameters, and the number of processes to run concurrently on each instance. When creating a fleet, the runtime configuration must have at least one server process configuration; otherwise the request fails with an invalid request exception. (This parameter replaces the parameters @ServerLaunchPath@ and @ServerLaunchParameters@ , although requests that contain values for these parameters instead of a runtime configuration will continue to work.) This parameter is required unless the parameters @ServerLaunchPath@ and @ServerLaunchParameters@ are defined. Runtime configuration replaced these parameters, but fleets that use them will continue to work.
-- * 'scriptId' - A unique identifier for a Realtime script to be deployed on the new fleet. You can use either the script ID or ARN value. The Realtime script must have been successfully uploaded to Amazon GameLift. This fleet setting cannot be changed once the fleet is created.
-- * 'serverLaunchParameters' - This parameter is no longer used. Instead, specify server launch parameters in the @RuntimeConfiguration@ parameter. (Requests that specify a server launch path and launch parameters instead of a runtime configuration will continue to work.)
-- * 'serverLaunchPath' - This parameter is no longer used. Instead, specify a server launch path using the @RuntimeConfiguration@ parameter. Requests that specify a server launch path and launch parameters instead of a runtime configuration will continue to work.
-- * 'tags' - A list of labels to assign to the new fleet resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
mkCreateFleet ::
  -- | 'name'
  Lude.Text ->
  -- | 'ec2InstanceType'
  EC2InstanceType ->
  CreateFleet
mkCreateFleet pName_ pEC2InstanceType_ =
  CreateFleet'
    { serverLaunchParameters = Lude.Nothing,
      logPaths = Lude.Nothing,
      peerVPCId = Lude.Nothing,
      buildId = Lude.Nothing,
      fleetType = Lude.Nothing,
      peerVPCAWSAccountId = Lude.Nothing,
      ec2InboundPermissions = Lude.Nothing,
      runtimeConfiguration = Lude.Nothing,
      newGameSessionProtectionPolicy = Lude.Nothing,
      scriptId = Lude.Nothing,
      certificateConfiguration = Lude.Nothing,
      serverLaunchPath = Lude.Nothing,
      instanceRoleARN = Lude.Nothing,
      metricGroups = Lude.Nothing,
      description = Lude.Nothing,
      resourceCreationLimitPolicy = Lude.Nothing,
      tags = Lude.Nothing,
      name = pName_,
      ec2InstanceType = pEC2InstanceType_
    }

-- | This parameter is no longer used. Instead, specify server launch parameters in the @RuntimeConfiguration@ parameter. (Requests that specify a server launch path and launch parameters instead of a runtime configuration will continue to work.)
--
-- /Note:/ Consider using 'serverLaunchParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfServerLaunchParameters :: Lens.Lens' CreateFleet (Lude.Maybe Lude.Text)
cfServerLaunchParameters = Lens.lens (serverLaunchParameters :: CreateFleet -> Lude.Maybe Lude.Text) (\s a -> s {serverLaunchParameters = a} :: CreateFleet)
{-# DEPRECATED cfServerLaunchParameters "Use generic-lens or generic-optics with 'serverLaunchParameters' instead." #-}

-- | This parameter is no longer used. Instead, to specify where Amazon GameLift should store log files once a server process shuts down, use the Amazon GameLift server API @ProcessReady()@ and specify one or more directory paths in @logParameters@ . See more information in the <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api-ref.html#gamelift-sdk-server-api-ref-dataypes-process Server API Reference> .
--
-- /Note:/ Consider using 'logPaths' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfLogPaths :: Lens.Lens' CreateFleet (Lude.Maybe [Lude.Text])
cfLogPaths = Lens.lens (logPaths :: CreateFleet -> Lude.Maybe [Lude.Text]) (\s a -> s {logPaths = a} :: CreateFleet)
{-# DEPRECATED cfLogPaths "Use generic-lens or generic-optics with 'logPaths' instead." #-}

-- | A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region as your fleet. To look up a VPC ID, use the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
--
-- /Note:/ Consider using 'peerVPCId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfPeerVPCId :: Lens.Lens' CreateFleet (Lude.Maybe Lude.Text)
cfPeerVPCId = Lens.lens (peerVPCId :: CreateFleet -> Lude.Maybe Lude.Text) (\s a -> s {peerVPCId = a} :: CreateFleet)
{-# DEPRECATED cfPeerVPCId "Use generic-lens or generic-optics with 'peerVPCId' instead." #-}

-- | A unique identifier for a build to be deployed on the new fleet. You can use either the build ID or ARN value. The custom game server build must have been successfully uploaded to Amazon GameLift and be in a @READY@ status. This fleet setting cannot be changed once the fleet is created.
--
-- /Note:/ Consider using 'buildId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfBuildId :: Lens.Lens' CreateFleet (Lude.Maybe Lude.Text)
cfBuildId = Lens.lens (buildId :: CreateFleet -> Lude.Maybe Lude.Text) (\s a -> s {buildId = a} :: CreateFleet)
{-# DEPRECATED cfBuildId "Use generic-lens or generic-optics with 'buildId' instead." #-}

-- | Indicates whether to use On-Demand instances or Spot instances for this fleet. If empty, the default is @ON_DEMAND@ . Both categories of instances use identical hardware and configurations based on the instance type selected for this fleet. Learn more about <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-ec2-instances.html#gamelift-ec2-instances-spot On-Demand versus Spot Instances> .
--
-- /Note:/ Consider using 'fleetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfFleetType :: Lens.Lens' CreateFleet (Lude.Maybe FleetType)
cfFleetType = Lens.lens (fleetType :: CreateFleet -> Lude.Maybe FleetType) (\s a -> s {fleetType = a} :: CreateFleet)
{-# DEPRECATED cfFleetType "Use generic-lens or generic-optics with 'fleetType' instead." #-}

-- | A unique identifier for the AWS account with the VPC that you want to peer your Amazon GameLift fleet with. You can find your account ID in the AWS Management Console under account settings.
--
-- /Note:/ Consider using 'peerVPCAWSAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfPeerVPCAWSAccountId :: Lens.Lens' CreateFleet (Lude.Maybe Lude.Text)
cfPeerVPCAWSAccountId = Lens.lens (peerVPCAWSAccountId :: CreateFleet -> Lude.Maybe Lude.Text) (\s a -> s {peerVPCAWSAccountId = a} :: CreateFleet)
{-# DEPRECATED cfPeerVPCAWSAccountId "Use generic-lens or generic-optics with 'peerVPCAWSAccountId' instead." #-}

-- | Range of IP addresses and port settings that permit inbound traffic to access game sessions that are running on the fleet. For fleets using a custom game build, this parameter is required before game sessions running on the fleet can accept connections. For Realtime Servers fleets, Amazon GameLift automatically sets TCP and UDP ranges for use by the Realtime servers. You can specify multiple permission settings or add more by updating the fleet.
--
-- /Note:/ Consider using 'ec2InboundPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfEC2InboundPermissions :: Lens.Lens' CreateFleet (Lude.Maybe [IPPermission])
cfEC2InboundPermissions = Lens.lens (ec2InboundPermissions :: CreateFleet -> Lude.Maybe [IPPermission]) (\s a -> s {ec2InboundPermissions = a} :: CreateFleet)
{-# DEPRECATED cfEC2InboundPermissions "Use generic-lens or generic-optics with 'ec2InboundPermissions' instead." #-}

-- | Instructions for launching server processes on each instance in the fleet. Server processes run either a custom game build executable or a Realtime script. The runtime configuration defines the server executables or launch script file, launch parameters, and the number of processes to run concurrently on each instance. When creating a fleet, the runtime configuration must have at least one server process configuration; otherwise the request fails with an invalid request exception. (This parameter replaces the parameters @ServerLaunchPath@ and @ServerLaunchParameters@ , although requests that contain values for these parameters instead of a runtime configuration will continue to work.) This parameter is required unless the parameters @ServerLaunchPath@ and @ServerLaunchParameters@ are defined. Runtime configuration replaced these parameters, but fleets that use them will continue to work.
--
-- /Note:/ Consider using 'runtimeConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRuntimeConfiguration :: Lens.Lens' CreateFleet (Lude.Maybe RuntimeConfiguration)
cfRuntimeConfiguration = Lens.lens (runtimeConfiguration :: CreateFleet -> Lude.Maybe RuntimeConfiguration) (\s a -> s {runtimeConfiguration = a} :: CreateFleet)
{-# DEPRECATED cfRuntimeConfiguration "Use generic-lens or generic-optics with 'runtimeConfiguration' instead." #-}

-- | A game session protection policy to apply to all instances in this fleet. If this parameter is not set, instances in this fleet default to no protection. You can change a fleet's protection policy using 'UpdateFleetAttributes' , but this change will only affect sessions created after the policy change. You can also set protection for individual instances using 'UpdateGameSession' .
--
--
--     * __NoProtection__ - The game session can be terminated during a scale-down event.
--
--
--     * __FullProtection__ - If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
--
--
--
-- /Note:/ Consider using 'newGameSessionProtectionPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfNewGameSessionProtectionPolicy :: Lens.Lens' CreateFleet (Lude.Maybe ProtectionPolicy)
cfNewGameSessionProtectionPolicy = Lens.lens (newGameSessionProtectionPolicy :: CreateFleet -> Lude.Maybe ProtectionPolicy) (\s a -> s {newGameSessionProtectionPolicy = a} :: CreateFleet)
{-# DEPRECATED cfNewGameSessionProtectionPolicy "Use generic-lens or generic-optics with 'newGameSessionProtectionPolicy' instead." #-}

-- | A unique identifier for a Realtime script to be deployed on the new fleet. You can use either the script ID or ARN value. The Realtime script must have been successfully uploaded to Amazon GameLift. This fleet setting cannot be changed once the fleet is created.
--
-- /Note:/ Consider using 'scriptId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfScriptId :: Lens.Lens' CreateFleet (Lude.Maybe Lude.Text)
cfScriptId = Lens.lens (scriptId :: CreateFleet -> Lude.Maybe Lude.Text) (\s a -> s {scriptId = a} :: CreateFleet)
{-# DEPRECATED cfScriptId "Use generic-lens or generic-optics with 'scriptId' instead." #-}

-- | Indicates whether to generate a TLS/SSL certificate for the new fleet. TLS certificates are used for encrypting traffic between game clients and game servers running on GameLift. If this parameter is not specified, the default value, DISABLED, is used. This fleet setting cannot be changed once the fleet is created. Learn more at <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-howitworks.html#gamelift-howitworks-security Securing Client/Server Communication> .
--
-- Note: This feature requires the AWS Certificate Manager (ACM) service, which is available in the AWS global partition but not in all other partitions. When working in a partition that does not support this feature, a request for a new fleet with certificate generation results fails with a 4xx unsupported Region error.
-- Valid values include:
--
--     * __GENERATED__ - Generate a TLS/SSL certificate for this fleet.
--
--
--     * __DISABLED__ - (default) Do not generate a TLS/SSL certificate for this fleet.
--
--
--
-- /Note:/ Consider using 'certificateConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfCertificateConfiguration :: Lens.Lens' CreateFleet (Lude.Maybe CertificateConfiguration)
cfCertificateConfiguration = Lens.lens (certificateConfiguration :: CreateFleet -> Lude.Maybe CertificateConfiguration) (\s a -> s {certificateConfiguration = a} :: CreateFleet)
{-# DEPRECATED cfCertificateConfiguration "Use generic-lens or generic-optics with 'certificateConfiguration' instead." #-}

-- | This parameter is no longer used. Instead, specify a server launch path using the @RuntimeConfiguration@ parameter. Requests that specify a server launch path and launch parameters instead of a runtime configuration will continue to work.
--
-- /Note:/ Consider using 'serverLaunchPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfServerLaunchPath :: Lens.Lens' CreateFleet (Lude.Maybe Lude.Text)
cfServerLaunchPath = Lens.lens (serverLaunchPath :: CreateFleet -> Lude.Maybe Lude.Text) (\s a -> s {serverLaunchPath = a} :: CreateFleet)
{-# DEPRECATED cfServerLaunchPath "Use generic-lens or generic-optics with 'serverLaunchPath' instead." #-}

-- | A unique identifier for an AWS IAM role that manages access to your AWS services. Fleets with an instance role ARN allow applications that are running on the fleet's instances to assume the role. Learn more about using on-box credentials for your game servers at <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-resources.html Access external resources from a game server> . To call this operation with instance role ARN, you must have IAM PassRole permissions. See <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-iam-policy-examples.html IAM policy examples for GameLift> .
--
-- /Note:/ Consider using 'instanceRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfInstanceRoleARN :: Lens.Lens' CreateFleet (Lude.Maybe Lude.Text)
cfInstanceRoleARN = Lens.lens (instanceRoleARN :: CreateFleet -> Lude.Maybe Lude.Text) (\s a -> s {instanceRoleARN = a} :: CreateFleet)
{-# DEPRECATED cfInstanceRoleARN "Use generic-lens or generic-optics with 'instanceRoleARN' instead." #-}

-- | The name of an Amazon CloudWatch metric group to add this fleet to. A metric group aggregates the metrics for all fleets in the group. Specify an existing metric group name, or provide a new name to create a new metric group. A fleet can only be included in one metric group at a time.
--
-- /Note:/ Consider using 'metricGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfMetricGroups :: Lens.Lens' CreateFleet (Lude.Maybe [Lude.Text])
cfMetricGroups = Lens.lens (metricGroups :: CreateFleet -> Lude.Maybe [Lude.Text]) (\s a -> s {metricGroups = a} :: CreateFleet)
{-# DEPRECATED cfMetricGroups "Use generic-lens or generic-optics with 'metricGroups' instead." #-}

-- | A human-readable description of a fleet.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDescription :: Lens.Lens' CreateFleet (Lude.Maybe Lude.Text)
cfDescription = Lens.lens (description :: CreateFleet -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateFleet)
{-# DEPRECATED cfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A policy that limits the number of game sessions an individual player can create over a span of time for this fleet.
--
-- /Note:/ Consider using 'resourceCreationLimitPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfResourceCreationLimitPolicy :: Lens.Lens' CreateFleet (Lude.Maybe ResourceCreationLimitPolicy)
cfResourceCreationLimitPolicy = Lens.lens (resourceCreationLimitPolicy :: CreateFleet -> Lude.Maybe ResourceCreationLimitPolicy) (\s a -> s {resourceCreationLimitPolicy = a} :: CreateFleet)
{-# DEPRECATED cfResourceCreationLimitPolicy "Use generic-lens or generic-optics with 'resourceCreationLimitPolicy' instead." #-}

-- | A list of labels to assign to the new fleet resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTags :: Lens.Lens' CreateFleet (Lude.Maybe [Tag])
cfTags = Lens.lens (tags :: CreateFleet -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateFleet)
{-# DEPRECATED cfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A descriptive label that is associated with a fleet. Fleet names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfName :: Lens.Lens' CreateFleet Lude.Text
cfName = Lens.lens (name :: CreateFleet -> Lude.Text) (\s a -> s {name = a} :: CreateFleet)
{-# DEPRECATED cfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
--
-- /Note:/ Consider using 'ec2InstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfEC2InstanceType :: Lens.Lens' CreateFleet EC2InstanceType
cfEC2InstanceType = Lens.lens (ec2InstanceType :: CreateFleet -> EC2InstanceType) (\s a -> s {ec2InstanceType = a} :: CreateFleet)
{-# DEPRECATED cfEC2InstanceType "Use generic-lens or generic-optics with 'ec2InstanceType' instead." #-}

instance Lude.AWSRequest CreateFleet where
  type Rs CreateFleet = CreateFleetResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateFleetResponse'
            Lude.<$> (x Lude..?> "FleetAttributes")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateFleet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.CreateFleet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateFleet where
  toJSON CreateFleet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ServerLaunchParameters" Lude..=)
              Lude.<$> serverLaunchParameters,
            ("LogPaths" Lude..=) Lude.<$> logPaths,
            ("PeerVpcId" Lude..=) Lude.<$> peerVPCId,
            ("BuildId" Lude..=) Lude.<$> buildId,
            ("FleetType" Lude..=) Lude.<$> fleetType,
            ("PeerVpcAwsAccountId" Lude..=) Lude.<$> peerVPCAWSAccountId,
            ("EC2InboundPermissions" Lude..=) Lude.<$> ec2InboundPermissions,
            ("RuntimeConfiguration" Lude..=) Lude.<$> runtimeConfiguration,
            ("NewGameSessionProtectionPolicy" Lude..=)
              Lude.<$> newGameSessionProtectionPolicy,
            ("ScriptId" Lude..=) Lude.<$> scriptId,
            ("CertificateConfiguration" Lude..=)
              Lude.<$> certificateConfiguration,
            ("ServerLaunchPath" Lude..=) Lude.<$> serverLaunchPath,
            ("InstanceRoleArn" Lude..=) Lude.<$> instanceRoleARN,
            ("MetricGroups" Lude..=) Lude.<$> metricGroups,
            ("Description" Lude..=) Lude.<$> description,
            ("ResourceCreationLimitPolicy" Lude..=)
              Lude.<$> resourceCreationLimitPolicy,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("EC2InstanceType" Lude..= ec2InstanceType)
          ]
      )

instance Lude.ToPath CreateFleet where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateFleet where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkCreateFleetResponse' smart constructor.
data CreateFleetResponse = CreateFleetResponse'
  { fleetAttributes ::
      Lude.Maybe FleetAttributes,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateFleetResponse' with the minimum fields required to make a request.
--
-- * 'fleetAttributes' - Properties for the newly created fleet.
-- * 'responseStatus' - The response status code.
mkCreateFleetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateFleetResponse
mkCreateFleetResponse pResponseStatus_ =
  CreateFleetResponse'
    { fleetAttributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Properties for the newly created fleet.
--
-- /Note:/ Consider using 'fleetAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrsFleetAttributes :: Lens.Lens' CreateFleetResponse (Lude.Maybe FleetAttributes)
cfrsFleetAttributes = Lens.lens (fleetAttributes :: CreateFleetResponse -> Lude.Maybe FleetAttributes) (\s a -> s {fleetAttributes = a} :: CreateFleetResponse)
{-# DEPRECATED cfrsFleetAttributes "Use generic-lens or generic-optics with 'fleetAttributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrsResponseStatus :: Lens.Lens' CreateFleetResponse Lude.Int
cfrsResponseStatus = Lens.lens (responseStatus :: CreateFleetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateFleetResponse)
{-# DEPRECATED cfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
