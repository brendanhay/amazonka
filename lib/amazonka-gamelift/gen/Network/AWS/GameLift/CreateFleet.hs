{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.GameLift.CreateFleet
    (
    -- * Creating a request
      CreateFleet (..)
    , mkCreateFleet
    -- ** Request lenses
    , cfName
    , cfEC2InstanceType
    , cfBuildId
    , cfCertificateConfiguration
    , cfDescription
    , cfEC2InboundPermissions
    , cfFleetType
    , cfInstanceRoleArn
    , cfLogPaths
    , cfMetricGroups
    , cfNewGameSessionProtectionPolicy
    , cfPeerVpcAwsAccountId
    , cfPeerVpcId
    , cfResourceCreationLimitPolicy
    , cfRuntimeConfiguration
    , cfScriptId
    , cfServerLaunchParameters
    , cfServerLaunchPath
    , cfTags

    -- * Destructuring the response
    , CreateFleetResponse (..)
    , mkCreateFleetResponse
    -- ** Response lenses
    , cfrrsFleetAttributes
    , cfrrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkCreateFleet' smart constructor.
data CreateFleet = CreateFleet'
  { name :: Types.Name
    -- ^ A descriptive label that is associated with a fleet. Fleet names do not need to be unique.
  , eC2InstanceType :: Types.EC2InstanceType
    -- ^ The name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
  , buildId :: Core.Maybe Types.BuildIdOrArn
    -- ^ A unique identifier for a build to be deployed on the new fleet. You can use either the build ID or ARN value. The custom game server build must have been successfully uploaded to Amazon GameLift and be in a @READY@ status. This fleet setting cannot be changed once the fleet is created. 
  , certificateConfiguration :: Core.Maybe Types.CertificateConfiguration
    -- ^ Indicates whether to generate a TLS/SSL certificate for the new fleet. TLS certificates are used for encrypting traffic between game clients and game servers running on GameLift. If this parameter is not specified, the default value, DISABLED, is used. This fleet setting cannot be changed once the fleet is created. Learn more at <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-howitworks.html#gamelift-howitworks-security Securing Client/Server Communication> . 
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
  , description :: Core.Maybe Types.Description
    -- ^ A human-readable description of a fleet.
  , eC2InboundPermissions :: Core.Maybe [Types.IpPermission]
    -- ^ Range of IP addresses and port settings that permit inbound traffic to access game sessions that are running on the fleet. For fleets using a custom game build, this parameter is required before game sessions running on the fleet can accept connections. For Realtime Servers fleets, Amazon GameLift automatically sets TCP and UDP ranges for use by the Realtime servers. You can specify multiple permission settings or add more by updating the fleet.
  , fleetType :: Core.Maybe Types.FleetType
    -- ^ Indicates whether to use On-Demand instances or Spot instances for this fleet. If empty, the default is @ON_DEMAND@ . Both categories of instances use identical hardware and configurations based on the instance type selected for this fleet. Learn more about <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-ec2-instances.html#gamelift-ec2-instances-spot On-Demand versus Spot Instances> . 
  , instanceRoleArn :: Core.Maybe Types.InstanceRoleArn
    -- ^ A unique identifier for an AWS IAM role that manages access to your AWS services. Fleets with an instance role ARN allow applications that are running on the fleet's instances to assume the role. Learn more about using on-box credentials for your game servers at <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-resources.html Access external resources from a game server> . To call this operation with instance role ARN, you must have IAM PassRole permissions. See <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-iam-policy-examples.html IAM policy examples for GameLift> . 
  , logPaths :: Core.Maybe [Types.NonZeroAndMaxString]
    -- ^ This parameter is no longer used. Instead, to specify where Amazon GameLift should store log files once a server process shuts down, use the Amazon GameLift server API @ProcessReady()@ and specify one or more directory paths in @logParameters@ . See more information in the <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api-ref.html#gamelift-sdk-server-api-ref-dataypes-process Server API Reference> . 
  , metricGroups :: Core.Maybe [Types.MetricGroup]
    -- ^ The name of an Amazon CloudWatch metric group to add this fleet to. A metric group aggregates the metrics for all fleets in the group. Specify an existing metric group name, or provide a new name to create a new metric group. A fleet can only be included in one metric group at a time. 
  , newGameSessionProtectionPolicy :: Core.Maybe Types.ProtectionPolicy
    -- ^ A game session protection policy to apply to all instances in this fleet. If this parameter is not set, instances in this fleet default to no protection. You can change a fleet's protection policy using 'UpdateFleetAttributes' , but this change will only affect sessions created after the policy change. You can also set protection for individual instances using 'UpdateGameSession' .
--
--
--     * __NoProtection__ - The game session can be terminated during a scale-down event.
--
--
--     * __FullProtection__ - If the game session is in an @ACTIVE@ status, it cannot be terminated during a scale-down event.
--
--
  , peerVpcAwsAccountId :: Core.Maybe Types.PeerVpcAwsAccountId
    -- ^ A unique identifier for the AWS account with the VPC that you want to peer your Amazon GameLift fleet with. You can find your account ID in the AWS Management Console under account settings. 
  , peerVpcId :: Core.Maybe Types.PeerVpcId
    -- ^ A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region as your fleet. To look up a VPC ID, use the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> . 
  , resourceCreationLimitPolicy :: Core.Maybe Types.ResourceCreationLimitPolicy
    -- ^ A policy that limits the number of game sessions an individual player can create over a span of time for this fleet.
  , runtimeConfiguration :: Core.Maybe Types.RuntimeConfiguration
    -- ^ Instructions for launching server processes on each instance in the fleet. Server processes run either a custom game build executable or a Realtime script. The runtime configuration defines the server executables or launch script file, launch parameters, and the number of processes to run concurrently on each instance. When creating a fleet, the runtime configuration must have at least one server process configuration; otherwise the request fails with an invalid request exception. (This parameter replaces the parameters @ServerLaunchPath@ and @ServerLaunchParameters@ , although requests that contain values for these parameters instead of a runtime configuration will continue to work.) This parameter is required unless the parameters @ServerLaunchPath@ and @ServerLaunchParameters@ are defined. Runtime configuration replaced these parameters, but fleets that use them will continue to work. 
  , scriptId :: Core.Maybe Types.ScriptIdOrArn
    -- ^ A unique identifier for a Realtime script to be deployed on the new fleet. You can use either the script ID or ARN value. The Realtime script must have been successfully uploaded to Amazon GameLift. This fleet setting cannot be changed once the fleet is created.
  , serverLaunchParameters :: Core.Maybe Types.ServerLaunchParameters
    -- ^ This parameter is no longer used. Instead, specify server launch parameters in the @RuntimeConfiguration@ parameter. (Requests that specify a server launch path and launch parameters instead of a runtime configuration will continue to work.)
  , serverLaunchPath :: Core.Maybe Types.ServerLaunchPath
    -- ^ This parameter is no longer used. Instead, specify a server launch path using the @RuntimeConfiguration@ parameter. Requests that specify a server launch path and launch parameters instead of a runtime configuration will continue to work.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of labels to assign to the new fleet resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateFleet' value with any optional fields omitted.
mkCreateFleet
    :: Types.Name -- ^ 'name'
    -> Types.EC2InstanceType -- ^ 'eC2InstanceType'
    -> CreateFleet
mkCreateFleet name eC2InstanceType
  = CreateFleet'{name, eC2InstanceType, buildId = Core.Nothing,
                 certificateConfiguration = Core.Nothing,
                 description = Core.Nothing, eC2InboundPermissions = Core.Nothing,
                 fleetType = Core.Nothing, instanceRoleArn = Core.Nothing,
                 logPaths = Core.Nothing, metricGroups = Core.Nothing,
                 newGameSessionProtectionPolicy = Core.Nothing,
                 peerVpcAwsAccountId = Core.Nothing, peerVpcId = Core.Nothing,
                 resourceCreationLimitPolicy = Core.Nothing,
                 runtimeConfiguration = Core.Nothing, scriptId = Core.Nothing,
                 serverLaunchParameters = Core.Nothing,
                 serverLaunchPath = Core.Nothing, tags = Core.Nothing}

-- | A descriptive label that is associated with a fleet. Fleet names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfName :: Lens.Lens' CreateFleet Types.Name
cfName = Lens.field @"name"
{-# INLINEABLE cfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
--
-- /Note:/ Consider using 'eC2InstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfEC2InstanceType :: Lens.Lens' CreateFleet Types.EC2InstanceType
cfEC2InstanceType = Lens.field @"eC2InstanceType"
{-# INLINEABLE cfEC2InstanceType #-}
{-# DEPRECATED eC2InstanceType "Use generic-lens or generic-optics with 'eC2InstanceType' instead"  #-}

-- | A unique identifier for a build to be deployed on the new fleet. You can use either the build ID or ARN value. The custom game server build must have been successfully uploaded to Amazon GameLift and be in a @READY@ status. This fleet setting cannot be changed once the fleet is created. 
--
-- /Note:/ Consider using 'buildId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfBuildId :: Lens.Lens' CreateFleet (Core.Maybe Types.BuildIdOrArn)
cfBuildId = Lens.field @"buildId"
{-# INLINEABLE cfBuildId #-}
{-# DEPRECATED buildId "Use generic-lens or generic-optics with 'buildId' instead"  #-}

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
cfCertificateConfiguration :: Lens.Lens' CreateFleet (Core.Maybe Types.CertificateConfiguration)
cfCertificateConfiguration = Lens.field @"certificateConfiguration"
{-# INLINEABLE cfCertificateConfiguration #-}
{-# DEPRECATED certificateConfiguration "Use generic-lens or generic-optics with 'certificateConfiguration' instead"  #-}

-- | A human-readable description of a fleet.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDescription :: Lens.Lens' CreateFleet (Core.Maybe Types.Description)
cfDescription = Lens.field @"description"
{-# INLINEABLE cfDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Range of IP addresses and port settings that permit inbound traffic to access game sessions that are running on the fleet. For fleets using a custom game build, this parameter is required before game sessions running on the fleet can accept connections. For Realtime Servers fleets, Amazon GameLift automatically sets TCP and UDP ranges for use by the Realtime servers. You can specify multiple permission settings or add more by updating the fleet.
--
-- /Note:/ Consider using 'eC2InboundPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfEC2InboundPermissions :: Lens.Lens' CreateFleet (Core.Maybe [Types.IpPermission])
cfEC2InboundPermissions = Lens.field @"eC2InboundPermissions"
{-# INLINEABLE cfEC2InboundPermissions #-}
{-# DEPRECATED eC2InboundPermissions "Use generic-lens or generic-optics with 'eC2InboundPermissions' instead"  #-}

-- | Indicates whether to use On-Demand instances or Spot instances for this fleet. If empty, the default is @ON_DEMAND@ . Both categories of instances use identical hardware and configurations based on the instance type selected for this fleet. Learn more about <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-ec2-instances.html#gamelift-ec2-instances-spot On-Demand versus Spot Instances> . 
--
-- /Note:/ Consider using 'fleetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfFleetType :: Lens.Lens' CreateFleet (Core.Maybe Types.FleetType)
cfFleetType = Lens.field @"fleetType"
{-# INLINEABLE cfFleetType #-}
{-# DEPRECATED fleetType "Use generic-lens or generic-optics with 'fleetType' instead"  #-}

-- | A unique identifier for an AWS IAM role that manages access to your AWS services. Fleets with an instance role ARN allow applications that are running on the fleet's instances to assume the role. Learn more about using on-box credentials for your game servers at <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-resources.html Access external resources from a game server> . To call this operation with instance role ARN, you must have IAM PassRole permissions. See <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-iam-policy-examples.html IAM policy examples for GameLift> . 
--
-- /Note:/ Consider using 'instanceRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfInstanceRoleArn :: Lens.Lens' CreateFleet (Core.Maybe Types.InstanceRoleArn)
cfInstanceRoleArn = Lens.field @"instanceRoleArn"
{-# INLINEABLE cfInstanceRoleArn #-}
{-# DEPRECATED instanceRoleArn "Use generic-lens or generic-optics with 'instanceRoleArn' instead"  #-}

-- | This parameter is no longer used. Instead, to specify where Amazon GameLift should store log files once a server process shuts down, use the Amazon GameLift server API @ProcessReady()@ and specify one or more directory paths in @logParameters@ . See more information in the <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api-ref.html#gamelift-sdk-server-api-ref-dataypes-process Server API Reference> . 
--
-- /Note:/ Consider using 'logPaths' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfLogPaths :: Lens.Lens' CreateFleet (Core.Maybe [Types.NonZeroAndMaxString])
cfLogPaths = Lens.field @"logPaths"
{-# INLINEABLE cfLogPaths #-}
{-# DEPRECATED logPaths "Use generic-lens or generic-optics with 'logPaths' instead"  #-}

-- | The name of an Amazon CloudWatch metric group to add this fleet to. A metric group aggregates the metrics for all fleets in the group. Specify an existing metric group name, or provide a new name to create a new metric group. A fleet can only be included in one metric group at a time. 
--
-- /Note:/ Consider using 'metricGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfMetricGroups :: Lens.Lens' CreateFleet (Core.Maybe [Types.MetricGroup])
cfMetricGroups = Lens.field @"metricGroups"
{-# INLINEABLE cfMetricGroups #-}
{-# DEPRECATED metricGroups "Use generic-lens or generic-optics with 'metricGroups' instead"  #-}

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
cfNewGameSessionProtectionPolicy :: Lens.Lens' CreateFleet (Core.Maybe Types.ProtectionPolicy)
cfNewGameSessionProtectionPolicy = Lens.field @"newGameSessionProtectionPolicy"
{-# INLINEABLE cfNewGameSessionProtectionPolicy #-}
{-# DEPRECATED newGameSessionProtectionPolicy "Use generic-lens or generic-optics with 'newGameSessionProtectionPolicy' instead"  #-}

-- | A unique identifier for the AWS account with the VPC that you want to peer your Amazon GameLift fleet with. You can find your account ID in the AWS Management Console under account settings. 
--
-- /Note:/ Consider using 'peerVpcAwsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfPeerVpcAwsAccountId :: Lens.Lens' CreateFleet (Core.Maybe Types.PeerVpcAwsAccountId)
cfPeerVpcAwsAccountId = Lens.field @"peerVpcAwsAccountId"
{-# INLINEABLE cfPeerVpcAwsAccountId #-}
{-# DEPRECATED peerVpcAwsAccountId "Use generic-lens or generic-optics with 'peerVpcAwsAccountId' instead"  #-}

-- | A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region as your fleet. To look up a VPC ID, use the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> . 
--
-- /Note:/ Consider using 'peerVpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfPeerVpcId :: Lens.Lens' CreateFleet (Core.Maybe Types.PeerVpcId)
cfPeerVpcId = Lens.field @"peerVpcId"
{-# INLINEABLE cfPeerVpcId #-}
{-# DEPRECATED peerVpcId "Use generic-lens or generic-optics with 'peerVpcId' instead"  #-}

-- | A policy that limits the number of game sessions an individual player can create over a span of time for this fleet.
--
-- /Note:/ Consider using 'resourceCreationLimitPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfResourceCreationLimitPolicy :: Lens.Lens' CreateFleet (Core.Maybe Types.ResourceCreationLimitPolicy)
cfResourceCreationLimitPolicy = Lens.field @"resourceCreationLimitPolicy"
{-# INLINEABLE cfResourceCreationLimitPolicy #-}
{-# DEPRECATED resourceCreationLimitPolicy "Use generic-lens or generic-optics with 'resourceCreationLimitPolicy' instead"  #-}

-- | Instructions for launching server processes on each instance in the fleet. Server processes run either a custom game build executable or a Realtime script. The runtime configuration defines the server executables or launch script file, launch parameters, and the number of processes to run concurrently on each instance. When creating a fleet, the runtime configuration must have at least one server process configuration; otherwise the request fails with an invalid request exception. (This parameter replaces the parameters @ServerLaunchPath@ and @ServerLaunchParameters@ , although requests that contain values for these parameters instead of a runtime configuration will continue to work.) This parameter is required unless the parameters @ServerLaunchPath@ and @ServerLaunchParameters@ are defined. Runtime configuration replaced these parameters, but fleets that use them will continue to work. 
--
-- /Note:/ Consider using 'runtimeConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRuntimeConfiguration :: Lens.Lens' CreateFleet (Core.Maybe Types.RuntimeConfiguration)
cfRuntimeConfiguration = Lens.field @"runtimeConfiguration"
{-# INLINEABLE cfRuntimeConfiguration #-}
{-# DEPRECATED runtimeConfiguration "Use generic-lens or generic-optics with 'runtimeConfiguration' instead"  #-}

-- | A unique identifier for a Realtime script to be deployed on the new fleet. You can use either the script ID or ARN value. The Realtime script must have been successfully uploaded to Amazon GameLift. This fleet setting cannot be changed once the fleet is created.
--
-- /Note:/ Consider using 'scriptId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfScriptId :: Lens.Lens' CreateFleet (Core.Maybe Types.ScriptIdOrArn)
cfScriptId = Lens.field @"scriptId"
{-# INLINEABLE cfScriptId #-}
{-# DEPRECATED scriptId "Use generic-lens or generic-optics with 'scriptId' instead"  #-}

-- | This parameter is no longer used. Instead, specify server launch parameters in the @RuntimeConfiguration@ parameter. (Requests that specify a server launch path and launch parameters instead of a runtime configuration will continue to work.)
--
-- /Note:/ Consider using 'serverLaunchParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfServerLaunchParameters :: Lens.Lens' CreateFleet (Core.Maybe Types.ServerLaunchParameters)
cfServerLaunchParameters = Lens.field @"serverLaunchParameters"
{-# INLINEABLE cfServerLaunchParameters #-}
{-# DEPRECATED serverLaunchParameters "Use generic-lens or generic-optics with 'serverLaunchParameters' instead"  #-}

-- | This parameter is no longer used. Instead, specify a server launch path using the @RuntimeConfiguration@ parameter. Requests that specify a server launch path and launch parameters instead of a runtime configuration will continue to work.
--
-- /Note:/ Consider using 'serverLaunchPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfServerLaunchPath :: Lens.Lens' CreateFleet (Core.Maybe Types.ServerLaunchPath)
cfServerLaunchPath = Lens.field @"serverLaunchPath"
{-# INLINEABLE cfServerLaunchPath #-}
{-# DEPRECATED serverLaunchPath "Use generic-lens or generic-optics with 'serverLaunchPath' instead"  #-}

-- | A list of labels to assign to the new fleet resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTags :: Lens.Lens' CreateFleet (Core.Maybe [Types.Tag])
cfTags = Lens.field @"tags"
{-# INLINEABLE cfTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateFleet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateFleet where
        toHeaders CreateFleet{..}
          = Core.pure ("X-Amz-Target", "GameLift.CreateFleet") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateFleet where
        toJSON CreateFleet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("EC2InstanceType" Core..= eC2InstanceType),
                  ("BuildId" Core..=) Core.<$> buildId,
                  ("CertificateConfiguration" Core..=) Core.<$>
                    certificateConfiguration,
                  ("Description" Core..=) Core.<$> description,
                  ("EC2InboundPermissions" Core..=) Core.<$> eC2InboundPermissions,
                  ("FleetType" Core..=) Core.<$> fleetType,
                  ("InstanceRoleArn" Core..=) Core.<$> instanceRoleArn,
                  ("LogPaths" Core..=) Core.<$> logPaths,
                  ("MetricGroups" Core..=) Core.<$> metricGroups,
                  ("NewGameSessionProtectionPolicy" Core..=) Core.<$>
                    newGameSessionProtectionPolicy,
                  ("PeerVpcAwsAccountId" Core..=) Core.<$> peerVpcAwsAccountId,
                  ("PeerVpcId" Core..=) Core.<$> peerVpcId,
                  ("ResourceCreationLimitPolicy" Core..=) Core.<$>
                    resourceCreationLimitPolicy,
                  ("RuntimeConfiguration" Core..=) Core.<$> runtimeConfiguration,
                  ("ScriptId" Core..=) Core.<$> scriptId,
                  ("ServerLaunchParameters" Core..=) Core.<$> serverLaunchParameters,
                  ("ServerLaunchPath" Core..=) Core.<$> serverLaunchPath,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateFleet where
        type Rs CreateFleet = CreateFleetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateFleetResponse' Core.<$>
                   (x Core..:? "FleetAttributes") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkCreateFleetResponse' smart constructor.
data CreateFleetResponse = CreateFleetResponse'
  { fleetAttributes :: Core.Maybe Types.FleetAttributes
    -- ^ Properties for the newly created fleet.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateFleetResponse' value with any optional fields omitted.
mkCreateFleetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateFleetResponse
mkCreateFleetResponse responseStatus
  = CreateFleetResponse'{fleetAttributes = Core.Nothing,
                         responseStatus}

-- | Properties for the newly created fleet.
--
-- /Note:/ Consider using 'fleetAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrrsFleetAttributes :: Lens.Lens' CreateFleetResponse (Core.Maybe Types.FleetAttributes)
cfrrsFleetAttributes = Lens.field @"fleetAttributes"
{-# INLINEABLE cfrrsFleetAttributes #-}
{-# DEPRECATED fleetAttributes "Use generic-lens or generic-optics with 'fleetAttributes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrrsResponseStatus :: Lens.Lens' CreateFleetResponse Core.Int
cfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
