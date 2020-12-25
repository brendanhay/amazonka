{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.CreateFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a fleet. A fleet consists of streaming instances that run a specified image.
module Network.AWS.AppStream.CreateFleet
  ( -- * Creating a request
    CreateFleet (..),
    mkCreateFleet,

    -- ** Request lenses
    cfName,
    cfInstanceType,
    cfComputeCapacity,
    cfDescription,
    cfDisconnectTimeoutInSeconds,
    cfDisplayName,
    cfDomainJoinInfo,
    cfEnableDefaultInternetAccess,
    cfFleetType,
    cfIamRoleArn,
    cfIdleDisconnectTimeoutInSeconds,
    cfImageArn,
    cfImageName,
    cfMaxUserDurationInSeconds,
    cfStreamView,
    cfTags,
    cfVpcConfig,

    -- * Destructuring the response
    CreateFleetResponse (..),
    mkCreateFleetResponse,

    -- ** Response lenses
    cfrrsFleet,
    cfrrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateFleet' smart constructor.
data CreateFleet = CreateFleet'
  { -- | A unique name for the fleet.
    name :: Types.Name,
    -- | The instance type to use when launching fleet instances. The following instance types are available:
    --
    --
    --     * stream.standard.medium
    --
    --
    --     * stream.standard.large
    --
    --
    --     * stream.compute.large
    --
    --
    --     * stream.compute.xlarge
    --
    --
    --     * stream.compute.2xlarge
    --
    --
    --     * stream.compute.4xlarge
    --
    --
    --     * stream.compute.8xlarge
    --
    --
    --     * stream.memory.large
    --
    --
    --     * stream.memory.xlarge
    --
    --
    --     * stream.memory.2xlarge
    --
    --
    --     * stream.memory.4xlarge
    --
    --
    --     * stream.memory.8xlarge
    --
    --
    --     * stream.memory.z1d.large
    --
    --
    --     * stream.memory.z1d.xlarge
    --
    --
    --     * stream.memory.z1d.2xlarge
    --
    --
    --     * stream.memory.z1d.3xlarge
    --
    --
    --     * stream.memory.z1d.6xlarge
    --
    --
    --     * stream.memory.z1d.12xlarge
    --
    --
    --     * stream.graphics-design.large
    --
    --
    --     * stream.graphics-design.xlarge
    --
    --
    --     * stream.graphics-design.2xlarge
    --
    --
    --     * stream.graphics-design.4xlarge
    --
    --
    --     * stream.graphics-desktop.2xlarge
    --
    --
    --     * stream.graphics.g4dn.xlarge
    --
    --
    --     * stream.graphics.g4dn.2xlarge
    --
    --
    --     * stream.graphics.g4dn.4xlarge
    --
    --
    --     * stream.graphics.g4dn.8xlarge
    --
    --
    --     * stream.graphics.g4dn.12xlarge
    --
    --
    --     * stream.graphics.g4dn.16xlarge
    --
    --
    --     * stream.graphics-pro.4xlarge
    --
    --
    --     * stream.graphics-pro.8xlarge
    --
    --
    --     * stream.graphics-pro.16xlarge
    instanceType :: Types.String,
    -- | The desired capacity for the fleet.
    computeCapacity :: Types.ComputeCapacity,
    -- | The description to display.
    description :: Core.Maybe Types.Description,
    -- | The amount of time that a streaming session remains active after users disconnect. If users try to reconnect to the streaming session after a disconnection or network interruption within this time interval, they are connected to their previous session. Otherwise, they are connected to a new session with a new streaming instance.
    --
    -- Specify a value between 60 and 360000.
    disconnectTimeoutInSeconds :: Core.Maybe Core.Int,
    -- | The fleet name to display.
    displayName :: Core.Maybe Types.DisplayName,
    -- | The name of the directory and organizational unit (OU) to use to join the fleet to a Microsoft Active Directory domain.
    domainJoinInfo :: Core.Maybe Types.DomainJoinInfo,
    -- | Enables or disables default internet access for the fleet.
    enableDefaultInternetAccess :: Core.Maybe Core.Bool,
    -- | The fleet type.
    --
    --
    --     * ALWAYS_ON
    --
    --     * Provides users with instant-on access to their apps. You are charged for all running instances in your fleet, even if no users are streaming apps.
    --
    --
    --     * ON_DEMAND
    --
    --     * Provide users with access to applications after they connect, which takes one to two minutes. You are charged for instance streaming when users are connected and a small hourly fee for instances that are not streaming apps.
    fleetType :: Core.Maybe Types.FleetType,
    -- | The Amazon Resource Name (ARN) of the IAM role to apply to the fleet. To assume a role, a fleet instance calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance.
    --
    -- For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
    iamRoleArn :: Core.Maybe Types.Arn,
    -- | The amount of time that users can be idle (inactive) before they are disconnected from their streaming session and the @DisconnectTimeoutInSeconds@ time interval begins. Users are notified before they are disconnected due to inactivity. If they try to reconnect to the streaming session before the time interval specified in @DisconnectTimeoutInSeconds@ elapses, they are connected to their previous session. Users are considered idle when they stop providing keyboard or mouse input during their streaming session. File uploads and downloads, audio in, audio out, and pixels changing do not qualify as user activity. If users continue to be idle after the time interval in @IdleDisconnectTimeoutInSeconds@ elapses, they are disconnected.
    --
    -- To prevent users from being disconnected due to inactivity, specify a value of 0. Otherwise, specify a value between 60 and 3600. The default value is 0.
    idleDisconnectTimeoutInSeconds :: Core.Maybe Core.Int,
    -- | The ARN of the public, private, or shared image to use.
    imageArn :: Core.Maybe Types.Arn,
    -- | The name of the image used to create the fleet.
    imageName :: Core.Maybe Types.String,
    -- | The maximum amount of time that a streaming session can remain active, in seconds. If users are still connected to a streaming instance five minutes before this limit is reached, they are prompted to save any open documents before being disconnected. After this time elapses, the instance is terminated and replaced by a new instance.
    --
    -- Specify a value between 600 and 360000.
    maxUserDurationInSeconds :: Core.Maybe Core.Int,
    -- | The AppStream 2.0 view that is displayed to your users when they stream from the fleet. When @APP@ is specified, only the windows of applications opened by users display. When @DESKTOP@ is specified, the standard desktop that is provided by the operating system displays.
    --
    -- The default value is @APP@ .
    streamView :: Core.Maybe Types.StreamView,
    -- | The tags to associate with the fleet. A tag is a key-value pair, and the value is optional. For example, Environment=Test. If you do not specify a value, Environment=.
    --
    -- If you do not specify a value, the value is set to an empty string.
    -- Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following special characters:
    -- _ . : / = + \ - @
    -- For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources> in the /Amazon AppStream 2.0 Administration Guide/ .
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue),
    -- | The VPC configuration for the fleet.
    vpcConfig :: Core.Maybe Types.VpcConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateFleet' value with any optional fields omitted.
mkCreateFleet ::
  -- | 'name'
  Types.Name ->
  -- | 'instanceType'
  Types.String ->
  -- | 'computeCapacity'
  Types.ComputeCapacity ->
  CreateFleet
mkCreateFleet name instanceType computeCapacity =
  CreateFleet'
    { name,
      instanceType,
      computeCapacity,
      description = Core.Nothing,
      disconnectTimeoutInSeconds = Core.Nothing,
      displayName = Core.Nothing,
      domainJoinInfo = Core.Nothing,
      enableDefaultInternetAccess = Core.Nothing,
      fleetType = Core.Nothing,
      iamRoleArn = Core.Nothing,
      idleDisconnectTimeoutInSeconds = Core.Nothing,
      imageArn = Core.Nothing,
      imageName = Core.Nothing,
      maxUserDurationInSeconds = Core.Nothing,
      streamView = Core.Nothing,
      tags = Core.Nothing,
      vpcConfig = Core.Nothing
    }

-- | A unique name for the fleet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfName :: Lens.Lens' CreateFleet Types.Name
cfName = Lens.field @"name"
{-# DEPRECATED cfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The instance type to use when launching fleet instances. The following instance types are available:
--
--
--     * stream.standard.medium
--
--
--     * stream.standard.large
--
--
--     * stream.compute.large
--
--
--     * stream.compute.xlarge
--
--
--     * stream.compute.2xlarge
--
--
--     * stream.compute.4xlarge
--
--
--     * stream.compute.8xlarge
--
--
--     * stream.memory.large
--
--
--     * stream.memory.xlarge
--
--
--     * stream.memory.2xlarge
--
--
--     * stream.memory.4xlarge
--
--
--     * stream.memory.8xlarge
--
--
--     * stream.memory.z1d.large
--
--
--     * stream.memory.z1d.xlarge
--
--
--     * stream.memory.z1d.2xlarge
--
--
--     * stream.memory.z1d.3xlarge
--
--
--     * stream.memory.z1d.6xlarge
--
--
--     * stream.memory.z1d.12xlarge
--
--
--     * stream.graphics-design.large
--
--
--     * stream.graphics-design.xlarge
--
--
--     * stream.graphics-design.2xlarge
--
--
--     * stream.graphics-design.4xlarge
--
--
--     * stream.graphics-desktop.2xlarge
--
--
--     * stream.graphics.g4dn.xlarge
--
--
--     * stream.graphics.g4dn.2xlarge
--
--
--     * stream.graphics.g4dn.4xlarge
--
--
--     * stream.graphics.g4dn.8xlarge
--
--
--     * stream.graphics.g4dn.12xlarge
--
--
--     * stream.graphics.g4dn.16xlarge
--
--
--     * stream.graphics-pro.4xlarge
--
--
--     * stream.graphics-pro.8xlarge
--
--
--     * stream.graphics-pro.16xlarge
--
--
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfInstanceType :: Lens.Lens' CreateFleet Types.String
cfInstanceType = Lens.field @"instanceType"
{-# DEPRECATED cfInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The desired capacity for the fleet.
--
-- /Note:/ Consider using 'computeCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfComputeCapacity :: Lens.Lens' CreateFleet Types.ComputeCapacity
cfComputeCapacity = Lens.field @"computeCapacity"
{-# DEPRECATED cfComputeCapacity "Use generic-lens or generic-optics with 'computeCapacity' instead." #-}

-- | The description to display.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDescription :: Lens.Lens' CreateFleet (Core.Maybe Types.Description)
cfDescription = Lens.field @"description"
{-# DEPRECATED cfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The amount of time that a streaming session remains active after users disconnect. If users try to reconnect to the streaming session after a disconnection or network interruption within this time interval, they are connected to their previous session. Otherwise, they are connected to a new session with a new streaming instance.
--
-- Specify a value between 60 and 360000.
--
-- /Note:/ Consider using 'disconnectTimeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDisconnectTimeoutInSeconds :: Lens.Lens' CreateFleet (Core.Maybe Core.Int)
cfDisconnectTimeoutInSeconds = Lens.field @"disconnectTimeoutInSeconds"
{-# DEPRECATED cfDisconnectTimeoutInSeconds "Use generic-lens or generic-optics with 'disconnectTimeoutInSeconds' instead." #-}

-- | The fleet name to display.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDisplayName :: Lens.Lens' CreateFleet (Core.Maybe Types.DisplayName)
cfDisplayName = Lens.field @"displayName"
{-# DEPRECATED cfDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The name of the directory and organizational unit (OU) to use to join the fleet to a Microsoft Active Directory domain.
--
-- /Note:/ Consider using 'domainJoinInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDomainJoinInfo :: Lens.Lens' CreateFleet (Core.Maybe Types.DomainJoinInfo)
cfDomainJoinInfo = Lens.field @"domainJoinInfo"
{-# DEPRECATED cfDomainJoinInfo "Use generic-lens or generic-optics with 'domainJoinInfo' instead." #-}

-- | Enables or disables default internet access for the fleet.
--
-- /Note:/ Consider using 'enableDefaultInternetAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfEnableDefaultInternetAccess :: Lens.Lens' CreateFleet (Core.Maybe Core.Bool)
cfEnableDefaultInternetAccess = Lens.field @"enableDefaultInternetAccess"
{-# DEPRECATED cfEnableDefaultInternetAccess "Use generic-lens or generic-optics with 'enableDefaultInternetAccess' instead." #-}

-- | The fleet type.
--
--
--     * ALWAYS_ON
--
--     * Provides users with instant-on access to their apps. You are charged for all running instances in your fleet, even if no users are streaming apps.
--
--
--     * ON_DEMAND
--
--     * Provide users with access to applications after they connect, which takes one to two minutes. You are charged for instance streaming when users are connected and a small hourly fee for instances that are not streaming apps.
--
--
--
-- /Note:/ Consider using 'fleetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfFleetType :: Lens.Lens' CreateFleet (Core.Maybe Types.FleetType)
cfFleetType = Lens.field @"fleetType"
{-# DEPRECATED cfFleetType "Use generic-lens or generic-optics with 'fleetType' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role to apply to the fleet. To assume a role, a fleet instance calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance.
--
-- For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
--
-- /Note:/ Consider using 'iamRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfIamRoleArn :: Lens.Lens' CreateFleet (Core.Maybe Types.Arn)
cfIamRoleArn = Lens.field @"iamRoleArn"
{-# DEPRECATED cfIamRoleArn "Use generic-lens or generic-optics with 'iamRoleArn' instead." #-}

-- | The amount of time that users can be idle (inactive) before they are disconnected from their streaming session and the @DisconnectTimeoutInSeconds@ time interval begins. Users are notified before they are disconnected due to inactivity. If they try to reconnect to the streaming session before the time interval specified in @DisconnectTimeoutInSeconds@ elapses, they are connected to their previous session. Users are considered idle when they stop providing keyboard or mouse input during their streaming session. File uploads and downloads, audio in, audio out, and pixels changing do not qualify as user activity. If users continue to be idle after the time interval in @IdleDisconnectTimeoutInSeconds@ elapses, they are disconnected.
--
-- To prevent users from being disconnected due to inactivity, specify a value of 0. Otherwise, specify a value between 60 and 3600. The default value is 0.
--
-- /Note:/ Consider using 'idleDisconnectTimeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfIdleDisconnectTimeoutInSeconds :: Lens.Lens' CreateFleet (Core.Maybe Core.Int)
cfIdleDisconnectTimeoutInSeconds = Lens.field @"idleDisconnectTimeoutInSeconds"
{-# DEPRECATED cfIdleDisconnectTimeoutInSeconds "Use generic-lens or generic-optics with 'idleDisconnectTimeoutInSeconds' instead." #-}

-- | The ARN of the public, private, or shared image to use.
--
-- /Note:/ Consider using 'imageArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfImageArn :: Lens.Lens' CreateFleet (Core.Maybe Types.Arn)
cfImageArn = Lens.field @"imageArn"
{-# DEPRECATED cfImageArn "Use generic-lens or generic-optics with 'imageArn' instead." #-}

-- | The name of the image used to create the fleet.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfImageName :: Lens.Lens' CreateFleet (Core.Maybe Types.String)
cfImageName = Lens.field @"imageName"
{-# DEPRECATED cfImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

-- | The maximum amount of time that a streaming session can remain active, in seconds. If users are still connected to a streaming instance five minutes before this limit is reached, they are prompted to save any open documents before being disconnected. After this time elapses, the instance is terminated and replaced by a new instance.
--
-- Specify a value between 600 and 360000.
--
-- /Note:/ Consider using 'maxUserDurationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfMaxUserDurationInSeconds :: Lens.Lens' CreateFleet (Core.Maybe Core.Int)
cfMaxUserDurationInSeconds = Lens.field @"maxUserDurationInSeconds"
{-# DEPRECATED cfMaxUserDurationInSeconds "Use generic-lens or generic-optics with 'maxUserDurationInSeconds' instead." #-}

-- | The AppStream 2.0 view that is displayed to your users when they stream from the fleet. When @APP@ is specified, only the windows of applications opened by users display. When @DESKTOP@ is specified, the standard desktop that is provided by the operating system displays.
--
-- The default value is @APP@ .
--
-- /Note:/ Consider using 'streamView' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfStreamView :: Lens.Lens' CreateFleet (Core.Maybe Types.StreamView)
cfStreamView = Lens.field @"streamView"
{-# DEPRECATED cfStreamView "Use generic-lens or generic-optics with 'streamView' instead." #-}

-- | The tags to associate with the fleet. A tag is a key-value pair, and the value is optional. For example, Environment=Test. If you do not specify a value, Environment=.
--
-- If you do not specify a value, the value is set to an empty string.
-- Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following special characters:
-- _ . : / = + \ - @
-- For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources> in the /Amazon AppStream 2.0 Administration Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTags :: Lens.Lens' CreateFleet (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
cfTags = Lens.field @"tags"
{-# DEPRECATED cfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The VPC configuration for the fleet.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfVpcConfig :: Lens.Lens' CreateFleet (Core.Maybe Types.VpcConfig)
cfVpcConfig = Lens.field @"vpcConfig"
{-# DEPRECATED cfVpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

instance Core.FromJSON CreateFleet where
  toJSON CreateFleet {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("InstanceType" Core..= instanceType),
            Core.Just ("ComputeCapacity" Core..= computeCapacity),
            ("Description" Core..=) Core.<$> description,
            ("DisconnectTimeoutInSeconds" Core..=)
              Core.<$> disconnectTimeoutInSeconds,
            ("DisplayName" Core..=) Core.<$> displayName,
            ("DomainJoinInfo" Core..=) Core.<$> domainJoinInfo,
            ("EnableDefaultInternetAccess" Core..=)
              Core.<$> enableDefaultInternetAccess,
            ("FleetType" Core..=) Core.<$> fleetType,
            ("IamRoleArn" Core..=) Core.<$> iamRoleArn,
            ("IdleDisconnectTimeoutInSeconds" Core..=)
              Core.<$> idleDisconnectTimeoutInSeconds,
            ("ImageArn" Core..=) Core.<$> imageArn,
            ("ImageName" Core..=) Core.<$> imageName,
            ("MaxUserDurationInSeconds" Core..=)
              Core.<$> maxUserDurationInSeconds,
            ("StreamView" Core..=) Core.<$> streamView,
            ("Tags" Core..=) Core.<$> tags,
            ("VpcConfig" Core..=) Core.<$> vpcConfig
          ]
      )

instance Core.AWSRequest CreateFleet where
  type Rs CreateFleet = CreateFleetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "PhotonAdminProxyService.CreateFleet")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFleetResponse'
            Core.<$> (x Core..:? "Fleet") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateFleetResponse' smart constructor.
data CreateFleetResponse = CreateFleetResponse'
  { -- | Information about the fleet.
    fleet :: Core.Maybe Types.Fleet,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateFleetResponse' value with any optional fields omitted.
mkCreateFleetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateFleetResponse
mkCreateFleetResponse responseStatus =
  CreateFleetResponse' {fleet = Core.Nothing, responseStatus}

-- | Information about the fleet.
--
-- /Note:/ Consider using 'fleet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrrsFleet :: Lens.Lens' CreateFleetResponse (Core.Maybe Types.Fleet)
cfrrsFleet = Lens.field @"fleet"
{-# DEPRECATED cfrrsFleet "Use generic-lens or generic-optics with 'fleet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrrsResponseStatus :: Lens.Lens' CreateFleetResponse Core.Int
cfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
