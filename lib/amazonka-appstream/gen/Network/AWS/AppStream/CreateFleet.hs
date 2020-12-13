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
    cfDomainJoinInfo,
    cfIAMRoleARN,
    cfDisconnectTimeoutInSeconds,
    cfMaxUserDurationInSeconds,
    cfIdleDisconnectTimeoutInSeconds,
    cfFleetType,
    cfInstanceType,
    cfVPCConfig,
    cfName,
    cfImageARN,
    cfDisplayName,
    cfEnableDefaultInternetAccess,
    cfImageName,
    cfDescription,
    cfStreamView,
    cfTags,
    cfComputeCapacity,

    -- * Destructuring the response
    CreateFleetResponse (..),
    mkCreateFleetResponse,

    -- ** Response lenses
    cfrsFleet,
    cfrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateFleet' smart constructor.
data CreateFleet = CreateFleet'
  { -- | The name of the directory and organizational unit (OU) to use to join the fleet to a Microsoft Active Directory domain.
    domainJoinInfo :: Lude.Maybe DomainJoinInfo,
    -- | The Amazon Resource Name (ARN) of the IAM role to apply to the fleet. To assume a role, a fleet instance calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance.
    --
    -- For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
    iamRoleARN :: Lude.Maybe Lude.Text,
    -- | The amount of time that a streaming session remains active after users disconnect. If users try to reconnect to the streaming session after a disconnection or network interruption within this time interval, they are connected to their previous session. Otherwise, they are connected to a new session with a new streaming instance.
    --
    -- Specify a value between 60 and 360000.
    disconnectTimeoutInSeconds :: Lude.Maybe Lude.Int,
    -- | The maximum amount of time that a streaming session can remain active, in seconds. If users are still connected to a streaming instance five minutes before this limit is reached, they are prompted to save any open documents before being disconnected. After this time elapses, the instance is terminated and replaced by a new instance.
    --
    -- Specify a value between 600 and 360000.
    maxUserDurationInSeconds :: Lude.Maybe Lude.Int,
    -- | The amount of time that users can be idle (inactive) before they are disconnected from their streaming session and the @DisconnectTimeoutInSeconds@ time interval begins. Users are notified before they are disconnected due to inactivity. If they try to reconnect to the streaming session before the time interval specified in @DisconnectTimeoutInSeconds@ elapses, they are connected to their previous session. Users are considered idle when they stop providing keyboard or mouse input during their streaming session. File uploads and downloads, audio in, audio out, and pixels changing do not qualify as user activity. If users continue to be idle after the time interval in @IdleDisconnectTimeoutInSeconds@ elapses, they are disconnected.
    --
    -- To prevent users from being disconnected due to inactivity, specify a value of 0. Otherwise, specify a value between 60 and 3600. The default value is 0.
    idleDisconnectTimeoutInSeconds :: Lude.Maybe Lude.Int,
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
    fleetType :: Lude.Maybe FleetType,
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
    instanceType :: Lude.Text,
    -- | The VPC configuration for the fleet.
    vpcConfig :: Lude.Maybe VPCConfig,
    -- | A unique name for the fleet.
    name :: Lude.Text,
    -- | The ARN of the public, private, or shared image to use.
    imageARN :: Lude.Maybe Lude.Text,
    -- | The fleet name to display.
    displayName :: Lude.Maybe Lude.Text,
    -- | Enables or disables default internet access for the fleet.
    enableDefaultInternetAccess :: Lude.Maybe Lude.Bool,
    -- | The name of the image used to create the fleet.
    imageName :: Lude.Maybe Lude.Text,
    -- | The description to display.
    description :: Lude.Maybe Lude.Text,
    -- | The AppStream 2.0 view that is displayed to your users when they stream from the fleet. When @APP@ is specified, only the windows of applications opened by users display. When @DESKTOP@ is specified, the standard desktop that is provided by the operating system displays.
    --
    -- The default value is @APP@ .
    streamView :: Lude.Maybe StreamView,
    -- | The tags to associate with the fleet. A tag is a key-value pair, and the value is optional. For example, Environment=Test. If you do not specify a value, Environment=.
    --
    -- If you do not specify a value, the value is set to an empty string.
    -- Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following special characters:
    -- _ . : / = + \ - @
    -- For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources> in the /Amazon AppStream 2.0 Administration Guide/ .
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The desired capacity for the fleet.
    computeCapacity :: ComputeCapacity
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateFleet' with the minimum fields required to make a request.
--
-- * 'domainJoinInfo' - The name of the directory and organizational unit (OU) to use to join the fleet to a Microsoft Active Directory domain.
-- * 'iamRoleARN' - The Amazon Resource Name (ARN) of the IAM role to apply to the fleet. To assume a role, a fleet instance calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance.
--
-- For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
-- * 'disconnectTimeoutInSeconds' - The amount of time that a streaming session remains active after users disconnect. If users try to reconnect to the streaming session after a disconnection or network interruption within this time interval, they are connected to their previous session. Otherwise, they are connected to a new session with a new streaming instance.
--
-- Specify a value between 60 and 360000.
-- * 'maxUserDurationInSeconds' - The maximum amount of time that a streaming session can remain active, in seconds. If users are still connected to a streaming instance five minutes before this limit is reached, they are prompted to save any open documents before being disconnected. After this time elapses, the instance is terminated and replaced by a new instance.
--
-- Specify a value between 600 and 360000.
-- * 'idleDisconnectTimeoutInSeconds' - The amount of time that users can be idle (inactive) before they are disconnected from their streaming session and the @DisconnectTimeoutInSeconds@ time interval begins. Users are notified before they are disconnected due to inactivity. If they try to reconnect to the streaming session before the time interval specified in @DisconnectTimeoutInSeconds@ elapses, they are connected to their previous session. Users are considered idle when they stop providing keyboard or mouse input during their streaming session. File uploads and downloads, audio in, audio out, and pixels changing do not qualify as user activity. If users continue to be idle after the time interval in @IdleDisconnectTimeoutInSeconds@ elapses, they are disconnected.
--
-- To prevent users from being disconnected due to inactivity, specify a value of 0. Otherwise, specify a value between 60 and 3600. The default value is 0.
-- * 'fleetType' - The fleet type.
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
-- * 'instanceType' - The instance type to use when launching fleet instances. The following instance types are available:
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
-- * 'vpcConfig' - The VPC configuration for the fleet.
-- * 'name' - A unique name for the fleet.
-- * 'imageARN' - The ARN of the public, private, or shared image to use.
-- * 'displayName' - The fleet name to display.
-- * 'enableDefaultInternetAccess' - Enables or disables default internet access for the fleet.
-- * 'imageName' - The name of the image used to create the fleet.
-- * 'description' - The description to display.
-- * 'streamView' - The AppStream 2.0 view that is displayed to your users when they stream from the fleet. When @APP@ is specified, only the windows of applications opened by users display. When @DESKTOP@ is specified, the standard desktop that is provided by the operating system displays.
--
-- The default value is @APP@ .
-- * 'tags' - The tags to associate with the fleet. A tag is a key-value pair, and the value is optional. For example, Environment=Test. If you do not specify a value, Environment=.
--
-- If you do not specify a value, the value is set to an empty string.
-- Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following special characters:
-- _ . : / = + \ - @
-- For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources> in the /Amazon AppStream 2.0 Administration Guide/ .
-- * 'computeCapacity' - The desired capacity for the fleet.
mkCreateFleet ::
  -- | 'instanceType'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'computeCapacity'
  ComputeCapacity ->
  CreateFleet
mkCreateFleet pInstanceType_ pName_ pComputeCapacity_ =
  CreateFleet'
    { domainJoinInfo = Lude.Nothing,
      iamRoleARN = Lude.Nothing,
      disconnectTimeoutInSeconds = Lude.Nothing,
      maxUserDurationInSeconds = Lude.Nothing,
      idleDisconnectTimeoutInSeconds = Lude.Nothing,
      fleetType = Lude.Nothing,
      instanceType = pInstanceType_,
      vpcConfig = Lude.Nothing,
      name = pName_,
      imageARN = Lude.Nothing,
      displayName = Lude.Nothing,
      enableDefaultInternetAccess = Lude.Nothing,
      imageName = Lude.Nothing,
      description = Lude.Nothing,
      streamView = Lude.Nothing,
      tags = Lude.Nothing,
      computeCapacity = pComputeCapacity_
    }

-- | The name of the directory and organizational unit (OU) to use to join the fleet to a Microsoft Active Directory domain.
--
-- /Note:/ Consider using 'domainJoinInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDomainJoinInfo :: Lens.Lens' CreateFleet (Lude.Maybe DomainJoinInfo)
cfDomainJoinInfo = Lens.lens (domainJoinInfo :: CreateFleet -> Lude.Maybe DomainJoinInfo) (\s a -> s {domainJoinInfo = a} :: CreateFleet)
{-# DEPRECATED cfDomainJoinInfo "Use generic-lens or generic-optics with 'domainJoinInfo' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role to apply to the fleet. To assume a role, a fleet instance calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance.
--
-- For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
--
-- /Note:/ Consider using 'iamRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfIAMRoleARN :: Lens.Lens' CreateFleet (Lude.Maybe Lude.Text)
cfIAMRoleARN = Lens.lens (iamRoleARN :: CreateFleet -> Lude.Maybe Lude.Text) (\s a -> s {iamRoleARN = a} :: CreateFleet)
{-# DEPRECATED cfIAMRoleARN "Use generic-lens or generic-optics with 'iamRoleARN' instead." #-}

-- | The amount of time that a streaming session remains active after users disconnect. If users try to reconnect to the streaming session after a disconnection or network interruption within this time interval, they are connected to their previous session. Otherwise, they are connected to a new session with a new streaming instance.
--
-- Specify a value between 60 and 360000.
--
-- /Note:/ Consider using 'disconnectTimeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDisconnectTimeoutInSeconds :: Lens.Lens' CreateFleet (Lude.Maybe Lude.Int)
cfDisconnectTimeoutInSeconds = Lens.lens (disconnectTimeoutInSeconds :: CreateFleet -> Lude.Maybe Lude.Int) (\s a -> s {disconnectTimeoutInSeconds = a} :: CreateFleet)
{-# DEPRECATED cfDisconnectTimeoutInSeconds "Use generic-lens or generic-optics with 'disconnectTimeoutInSeconds' instead." #-}

-- | The maximum amount of time that a streaming session can remain active, in seconds. If users are still connected to a streaming instance five minutes before this limit is reached, they are prompted to save any open documents before being disconnected. After this time elapses, the instance is terminated and replaced by a new instance.
--
-- Specify a value between 600 and 360000.
--
-- /Note:/ Consider using 'maxUserDurationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfMaxUserDurationInSeconds :: Lens.Lens' CreateFleet (Lude.Maybe Lude.Int)
cfMaxUserDurationInSeconds = Lens.lens (maxUserDurationInSeconds :: CreateFleet -> Lude.Maybe Lude.Int) (\s a -> s {maxUserDurationInSeconds = a} :: CreateFleet)
{-# DEPRECATED cfMaxUserDurationInSeconds "Use generic-lens or generic-optics with 'maxUserDurationInSeconds' instead." #-}

-- | The amount of time that users can be idle (inactive) before they are disconnected from their streaming session and the @DisconnectTimeoutInSeconds@ time interval begins. Users are notified before they are disconnected due to inactivity. If they try to reconnect to the streaming session before the time interval specified in @DisconnectTimeoutInSeconds@ elapses, they are connected to their previous session. Users are considered idle when they stop providing keyboard or mouse input during their streaming session. File uploads and downloads, audio in, audio out, and pixels changing do not qualify as user activity. If users continue to be idle after the time interval in @IdleDisconnectTimeoutInSeconds@ elapses, they are disconnected.
--
-- To prevent users from being disconnected due to inactivity, specify a value of 0. Otherwise, specify a value between 60 and 3600. The default value is 0.
--
-- /Note:/ Consider using 'idleDisconnectTimeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfIdleDisconnectTimeoutInSeconds :: Lens.Lens' CreateFleet (Lude.Maybe Lude.Int)
cfIdleDisconnectTimeoutInSeconds = Lens.lens (idleDisconnectTimeoutInSeconds :: CreateFleet -> Lude.Maybe Lude.Int) (\s a -> s {idleDisconnectTimeoutInSeconds = a} :: CreateFleet)
{-# DEPRECATED cfIdleDisconnectTimeoutInSeconds "Use generic-lens or generic-optics with 'idleDisconnectTimeoutInSeconds' instead." #-}

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
cfFleetType :: Lens.Lens' CreateFleet (Lude.Maybe FleetType)
cfFleetType = Lens.lens (fleetType :: CreateFleet -> Lude.Maybe FleetType) (\s a -> s {fleetType = a} :: CreateFleet)
{-# DEPRECATED cfFleetType "Use generic-lens or generic-optics with 'fleetType' instead." #-}

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
cfInstanceType :: Lens.Lens' CreateFleet Lude.Text
cfInstanceType = Lens.lens (instanceType :: CreateFleet -> Lude.Text) (\s a -> s {instanceType = a} :: CreateFleet)
{-# DEPRECATED cfInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The VPC configuration for the fleet.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfVPCConfig :: Lens.Lens' CreateFleet (Lude.Maybe VPCConfig)
cfVPCConfig = Lens.lens (vpcConfig :: CreateFleet -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: CreateFleet)
{-# DEPRECATED cfVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | A unique name for the fleet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfName :: Lens.Lens' CreateFleet Lude.Text
cfName = Lens.lens (name :: CreateFleet -> Lude.Text) (\s a -> s {name = a} :: CreateFleet)
{-# DEPRECATED cfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ARN of the public, private, or shared image to use.
--
-- /Note:/ Consider using 'imageARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfImageARN :: Lens.Lens' CreateFleet (Lude.Maybe Lude.Text)
cfImageARN = Lens.lens (imageARN :: CreateFleet -> Lude.Maybe Lude.Text) (\s a -> s {imageARN = a} :: CreateFleet)
{-# DEPRECATED cfImageARN "Use generic-lens or generic-optics with 'imageARN' instead." #-}

-- | The fleet name to display.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDisplayName :: Lens.Lens' CreateFleet (Lude.Maybe Lude.Text)
cfDisplayName = Lens.lens (displayName :: CreateFleet -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: CreateFleet)
{-# DEPRECATED cfDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | Enables or disables default internet access for the fleet.
--
-- /Note:/ Consider using 'enableDefaultInternetAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfEnableDefaultInternetAccess :: Lens.Lens' CreateFleet (Lude.Maybe Lude.Bool)
cfEnableDefaultInternetAccess = Lens.lens (enableDefaultInternetAccess :: CreateFleet -> Lude.Maybe Lude.Bool) (\s a -> s {enableDefaultInternetAccess = a} :: CreateFleet)
{-# DEPRECATED cfEnableDefaultInternetAccess "Use generic-lens or generic-optics with 'enableDefaultInternetAccess' instead." #-}

-- | The name of the image used to create the fleet.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfImageName :: Lens.Lens' CreateFleet (Lude.Maybe Lude.Text)
cfImageName = Lens.lens (imageName :: CreateFleet -> Lude.Maybe Lude.Text) (\s a -> s {imageName = a} :: CreateFleet)
{-# DEPRECATED cfImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

-- | The description to display.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDescription :: Lens.Lens' CreateFleet (Lude.Maybe Lude.Text)
cfDescription = Lens.lens (description :: CreateFleet -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateFleet)
{-# DEPRECATED cfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The AppStream 2.0 view that is displayed to your users when they stream from the fleet. When @APP@ is specified, only the windows of applications opened by users display. When @DESKTOP@ is specified, the standard desktop that is provided by the operating system displays.
--
-- The default value is @APP@ .
--
-- /Note:/ Consider using 'streamView' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfStreamView :: Lens.Lens' CreateFleet (Lude.Maybe StreamView)
cfStreamView = Lens.lens (streamView :: CreateFleet -> Lude.Maybe StreamView) (\s a -> s {streamView = a} :: CreateFleet)
{-# DEPRECATED cfStreamView "Use generic-lens or generic-optics with 'streamView' instead." #-}

-- | The tags to associate with the fleet. A tag is a key-value pair, and the value is optional. For example, Environment=Test. If you do not specify a value, Environment=.
--
-- If you do not specify a value, the value is set to an empty string.
-- Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following special characters:
-- _ . : / = + \ - @
-- For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources> in the /Amazon AppStream 2.0 Administration Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTags :: Lens.Lens' CreateFleet (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cfTags = Lens.lens (tags :: CreateFleet -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateFleet)
{-# DEPRECATED cfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The desired capacity for the fleet.
--
-- /Note:/ Consider using 'computeCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfComputeCapacity :: Lens.Lens' CreateFleet ComputeCapacity
cfComputeCapacity = Lens.lens (computeCapacity :: CreateFleet -> ComputeCapacity) (\s a -> s {computeCapacity = a} :: CreateFleet)
{-# DEPRECATED cfComputeCapacity "Use generic-lens or generic-optics with 'computeCapacity' instead." #-}

instance Lude.AWSRequest CreateFleet where
  type Rs CreateFleet = CreateFleetResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateFleetResponse'
            Lude.<$> (x Lude..?> "Fleet") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateFleet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.CreateFleet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateFleet where
  toJSON CreateFleet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DomainJoinInfo" Lude..=) Lude.<$> domainJoinInfo,
            ("IamRoleArn" Lude..=) Lude.<$> iamRoleARN,
            ("DisconnectTimeoutInSeconds" Lude..=)
              Lude.<$> disconnectTimeoutInSeconds,
            ("MaxUserDurationInSeconds" Lude..=)
              Lude.<$> maxUserDurationInSeconds,
            ("IdleDisconnectTimeoutInSeconds" Lude..=)
              Lude.<$> idleDisconnectTimeoutInSeconds,
            ("FleetType" Lude..=) Lude.<$> fleetType,
            Lude.Just ("InstanceType" Lude..= instanceType),
            ("VpcConfig" Lude..=) Lude.<$> vpcConfig,
            Lude.Just ("Name" Lude..= name),
            ("ImageArn" Lude..=) Lude.<$> imageARN,
            ("DisplayName" Lude..=) Lude.<$> displayName,
            ("EnableDefaultInternetAccess" Lude..=)
              Lude.<$> enableDefaultInternetAccess,
            ("ImageName" Lude..=) Lude.<$> imageName,
            ("Description" Lude..=) Lude.<$> description,
            ("StreamView" Lude..=) Lude.<$> streamView,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("ComputeCapacity" Lude..= computeCapacity)
          ]
      )

instance Lude.ToPath CreateFleet where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateFleet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateFleetResponse' smart constructor.
data CreateFleetResponse = CreateFleetResponse'
  { -- | Information about the fleet.
    fleet :: Lude.Maybe Fleet,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateFleetResponse' with the minimum fields required to make a request.
--
-- * 'fleet' - Information about the fleet.
-- * 'responseStatus' - The response status code.
mkCreateFleetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateFleetResponse
mkCreateFleetResponse pResponseStatus_ =
  CreateFleetResponse'
    { fleet = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the fleet.
--
-- /Note:/ Consider using 'fleet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrsFleet :: Lens.Lens' CreateFleetResponse (Lude.Maybe Fleet)
cfrsFleet = Lens.lens (fleet :: CreateFleetResponse -> Lude.Maybe Fleet) (\s a -> s {fleet = a} :: CreateFleetResponse)
{-# DEPRECATED cfrsFleet "Use generic-lens or generic-optics with 'fleet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrsResponseStatus :: Lens.Lens' CreateFleetResponse Lude.Int
cfrsResponseStatus = Lens.lens (responseStatus :: CreateFleetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateFleetResponse)
{-# DEPRECATED cfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
