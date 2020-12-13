{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.UpdateFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified fleet.
--
-- If the fleet is in the @STOPPED@ state, you can update any attribute except the fleet name. If the fleet is in the @RUNNING@ state, you can update the @DisplayName@ , @ComputeCapacity@ , @ImageARN@ , @ImageName@ , @IdleDisconnectTimeoutInSeconds@ , and @DisconnectTimeoutInSeconds@ attributes. If the fleet is in the @STARTING@ or @STOPPING@ state, you can't update it.
module Network.AWS.AppStream.UpdateFleet
  ( -- * Creating a request
    UpdateFleet (..),
    mkUpdateFleet,

    -- ** Request lenses
    ufDomainJoinInfo,
    ufIAMRoleARN,
    ufDisconnectTimeoutInSeconds,
    ufMaxUserDurationInSeconds,
    ufAttributesToDelete,
    ufIdleDisconnectTimeoutInSeconds,
    ufDeleteVPCConfig,
    ufInstanceType,
    ufVPCConfig,
    ufName,
    ufImageARN,
    ufDisplayName,
    ufEnableDefaultInternetAccess,
    ufImageName,
    ufDescription,
    ufStreamView,
    ufComputeCapacity,

    -- * Destructuring the response
    UpdateFleetResponse (..),
    mkUpdateFleetResponse,

    -- ** Response lenses
    ufrsFleet,
    ufrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateFleet' smart constructor.
data UpdateFleet = UpdateFleet'
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
    -- | The fleet attributes to delete.
    attributesToDelete :: Lude.Maybe [FleetAttribute],
    -- | The amount of time that users can be idle (inactive) before they are disconnected from their streaming session and the @DisconnectTimeoutInSeconds@ time interval begins. Users are notified before they are disconnected due to inactivity. If users try to reconnect to the streaming session before the time interval specified in @DisconnectTimeoutInSeconds@ elapses, they are connected to their previous session. Users are considered idle when they stop providing keyboard or mouse input during their streaming session. File uploads and downloads, audio in, audio out, and pixels changing do not qualify as user activity. If users continue to be idle after the time interval in @IdleDisconnectTimeoutInSeconds@ elapses, they are disconnected.
    --
    -- To prevent users from being disconnected due to inactivity, specify a value of 0. Otherwise, specify a value between 60 and 3600. The default value is 0.
    idleDisconnectTimeoutInSeconds :: Lude.Maybe Lude.Int,
    -- | Deletes the VPC association for the specified fleet.
    deleteVPCConfig :: Lude.Maybe Lude.Bool,
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
    instanceType :: Lude.Maybe Lude.Text,
    -- | The VPC configuration for the fleet.
    vpcConfig :: Lude.Maybe VPCConfig,
    -- | A unique name for the fleet.
    name :: Lude.Maybe Lude.Text,
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
    -- | The desired capacity for the fleet.
    computeCapacity :: Lude.Maybe ComputeCapacity
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFleet' with the minimum fields required to make a request.
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
-- * 'attributesToDelete' - The fleet attributes to delete.
-- * 'idleDisconnectTimeoutInSeconds' - The amount of time that users can be idle (inactive) before they are disconnected from their streaming session and the @DisconnectTimeoutInSeconds@ time interval begins. Users are notified before they are disconnected due to inactivity. If users try to reconnect to the streaming session before the time interval specified in @DisconnectTimeoutInSeconds@ elapses, they are connected to their previous session. Users are considered idle when they stop providing keyboard or mouse input during their streaming session. File uploads and downloads, audio in, audio out, and pixels changing do not qualify as user activity. If users continue to be idle after the time interval in @IdleDisconnectTimeoutInSeconds@ elapses, they are disconnected.
--
-- To prevent users from being disconnected due to inactivity, specify a value of 0. Otherwise, specify a value between 60 and 3600. The default value is 0.
-- * 'deleteVPCConfig' - Deletes the VPC association for the specified fleet.
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
-- * 'computeCapacity' - The desired capacity for the fleet.
mkUpdateFleet ::
  UpdateFleet
mkUpdateFleet =
  UpdateFleet'
    { domainJoinInfo = Lude.Nothing,
      iamRoleARN = Lude.Nothing,
      disconnectTimeoutInSeconds = Lude.Nothing,
      maxUserDurationInSeconds = Lude.Nothing,
      attributesToDelete = Lude.Nothing,
      idleDisconnectTimeoutInSeconds = Lude.Nothing,
      deleteVPCConfig = Lude.Nothing,
      instanceType = Lude.Nothing,
      vpcConfig = Lude.Nothing,
      name = Lude.Nothing,
      imageARN = Lude.Nothing,
      displayName = Lude.Nothing,
      enableDefaultInternetAccess = Lude.Nothing,
      imageName = Lude.Nothing,
      description = Lude.Nothing,
      streamView = Lude.Nothing,
      computeCapacity = Lude.Nothing
    }

-- | The name of the directory and organizational unit (OU) to use to join the fleet to a Microsoft Active Directory domain.
--
-- /Note:/ Consider using 'domainJoinInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufDomainJoinInfo :: Lens.Lens' UpdateFleet (Lude.Maybe DomainJoinInfo)
ufDomainJoinInfo = Lens.lens (domainJoinInfo :: UpdateFleet -> Lude.Maybe DomainJoinInfo) (\s a -> s {domainJoinInfo = a} :: UpdateFleet)
{-# DEPRECATED ufDomainJoinInfo "Use generic-lens or generic-optics with 'domainJoinInfo' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role to apply to the fleet. To assume a role, a fleet instance calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance.
--
-- For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
--
-- /Note:/ Consider using 'iamRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufIAMRoleARN :: Lens.Lens' UpdateFleet (Lude.Maybe Lude.Text)
ufIAMRoleARN = Lens.lens (iamRoleARN :: UpdateFleet -> Lude.Maybe Lude.Text) (\s a -> s {iamRoleARN = a} :: UpdateFleet)
{-# DEPRECATED ufIAMRoleARN "Use generic-lens or generic-optics with 'iamRoleARN' instead." #-}

-- | The amount of time that a streaming session remains active after users disconnect. If users try to reconnect to the streaming session after a disconnection or network interruption within this time interval, they are connected to their previous session. Otherwise, they are connected to a new session with a new streaming instance.
--
-- Specify a value between 60 and 360000.
--
-- /Note:/ Consider using 'disconnectTimeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufDisconnectTimeoutInSeconds :: Lens.Lens' UpdateFleet (Lude.Maybe Lude.Int)
ufDisconnectTimeoutInSeconds = Lens.lens (disconnectTimeoutInSeconds :: UpdateFleet -> Lude.Maybe Lude.Int) (\s a -> s {disconnectTimeoutInSeconds = a} :: UpdateFleet)
{-# DEPRECATED ufDisconnectTimeoutInSeconds "Use generic-lens or generic-optics with 'disconnectTimeoutInSeconds' instead." #-}

-- | The maximum amount of time that a streaming session can remain active, in seconds. If users are still connected to a streaming instance five minutes before this limit is reached, they are prompted to save any open documents before being disconnected. After this time elapses, the instance is terminated and replaced by a new instance.
--
-- Specify a value between 600 and 360000.
--
-- /Note:/ Consider using 'maxUserDurationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufMaxUserDurationInSeconds :: Lens.Lens' UpdateFleet (Lude.Maybe Lude.Int)
ufMaxUserDurationInSeconds = Lens.lens (maxUserDurationInSeconds :: UpdateFleet -> Lude.Maybe Lude.Int) (\s a -> s {maxUserDurationInSeconds = a} :: UpdateFleet)
{-# DEPRECATED ufMaxUserDurationInSeconds "Use generic-lens or generic-optics with 'maxUserDurationInSeconds' instead." #-}

-- | The fleet attributes to delete.
--
-- /Note:/ Consider using 'attributesToDelete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufAttributesToDelete :: Lens.Lens' UpdateFleet (Lude.Maybe [FleetAttribute])
ufAttributesToDelete = Lens.lens (attributesToDelete :: UpdateFleet -> Lude.Maybe [FleetAttribute]) (\s a -> s {attributesToDelete = a} :: UpdateFleet)
{-# DEPRECATED ufAttributesToDelete "Use generic-lens or generic-optics with 'attributesToDelete' instead." #-}

-- | The amount of time that users can be idle (inactive) before they are disconnected from their streaming session and the @DisconnectTimeoutInSeconds@ time interval begins. Users are notified before they are disconnected due to inactivity. If users try to reconnect to the streaming session before the time interval specified in @DisconnectTimeoutInSeconds@ elapses, they are connected to their previous session. Users are considered idle when they stop providing keyboard or mouse input during their streaming session. File uploads and downloads, audio in, audio out, and pixels changing do not qualify as user activity. If users continue to be idle after the time interval in @IdleDisconnectTimeoutInSeconds@ elapses, they are disconnected.
--
-- To prevent users from being disconnected due to inactivity, specify a value of 0. Otherwise, specify a value between 60 and 3600. The default value is 0.
--
-- /Note:/ Consider using 'idleDisconnectTimeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufIdleDisconnectTimeoutInSeconds :: Lens.Lens' UpdateFleet (Lude.Maybe Lude.Int)
ufIdleDisconnectTimeoutInSeconds = Lens.lens (idleDisconnectTimeoutInSeconds :: UpdateFleet -> Lude.Maybe Lude.Int) (\s a -> s {idleDisconnectTimeoutInSeconds = a} :: UpdateFleet)
{-# DEPRECATED ufIdleDisconnectTimeoutInSeconds "Use generic-lens or generic-optics with 'idleDisconnectTimeoutInSeconds' instead." #-}

-- | Deletes the VPC association for the specified fleet.
--
-- /Note:/ Consider using 'deleteVPCConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufDeleteVPCConfig :: Lens.Lens' UpdateFleet (Lude.Maybe Lude.Bool)
ufDeleteVPCConfig = Lens.lens (deleteVPCConfig :: UpdateFleet -> Lude.Maybe Lude.Bool) (\s a -> s {deleteVPCConfig = a} :: UpdateFleet)
{-# DEPRECATED ufDeleteVPCConfig "Use generic-lens or generic-optics with 'deleteVPCConfig' instead." #-}

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
ufInstanceType :: Lens.Lens' UpdateFleet (Lude.Maybe Lude.Text)
ufInstanceType = Lens.lens (instanceType :: UpdateFleet -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: UpdateFleet)
{-# DEPRECATED ufInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The VPC configuration for the fleet.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufVPCConfig :: Lens.Lens' UpdateFleet (Lude.Maybe VPCConfig)
ufVPCConfig = Lens.lens (vpcConfig :: UpdateFleet -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: UpdateFleet)
{-# DEPRECATED ufVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | A unique name for the fleet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufName :: Lens.Lens' UpdateFleet (Lude.Maybe Lude.Text)
ufName = Lens.lens (name :: UpdateFleet -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateFleet)
{-# DEPRECATED ufName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ARN of the public, private, or shared image to use.
--
-- /Note:/ Consider using 'imageARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufImageARN :: Lens.Lens' UpdateFleet (Lude.Maybe Lude.Text)
ufImageARN = Lens.lens (imageARN :: UpdateFleet -> Lude.Maybe Lude.Text) (\s a -> s {imageARN = a} :: UpdateFleet)
{-# DEPRECATED ufImageARN "Use generic-lens or generic-optics with 'imageARN' instead." #-}

-- | The fleet name to display.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufDisplayName :: Lens.Lens' UpdateFleet (Lude.Maybe Lude.Text)
ufDisplayName = Lens.lens (displayName :: UpdateFleet -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: UpdateFleet)
{-# DEPRECATED ufDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | Enables or disables default internet access for the fleet.
--
-- /Note:/ Consider using 'enableDefaultInternetAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufEnableDefaultInternetAccess :: Lens.Lens' UpdateFleet (Lude.Maybe Lude.Bool)
ufEnableDefaultInternetAccess = Lens.lens (enableDefaultInternetAccess :: UpdateFleet -> Lude.Maybe Lude.Bool) (\s a -> s {enableDefaultInternetAccess = a} :: UpdateFleet)
{-# DEPRECATED ufEnableDefaultInternetAccess "Use generic-lens or generic-optics with 'enableDefaultInternetAccess' instead." #-}

-- | The name of the image used to create the fleet.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufImageName :: Lens.Lens' UpdateFleet (Lude.Maybe Lude.Text)
ufImageName = Lens.lens (imageName :: UpdateFleet -> Lude.Maybe Lude.Text) (\s a -> s {imageName = a} :: UpdateFleet)
{-# DEPRECATED ufImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

-- | The description to display.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufDescription :: Lens.Lens' UpdateFleet (Lude.Maybe Lude.Text)
ufDescription = Lens.lens (description :: UpdateFleet -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateFleet)
{-# DEPRECATED ufDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The AppStream 2.0 view that is displayed to your users when they stream from the fleet. When @APP@ is specified, only the windows of applications opened by users display. When @DESKTOP@ is specified, the standard desktop that is provided by the operating system displays.
--
-- The default value is @APP@ .
--
-- /Note:/ Consider using 'streamView' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufStreamView :: Lens.Lens' UpdateFleet (Lude.Maybe StreamView)
ufStreamView = Lens.lens (streamView :: UpdateFleet -> Lude.Maybe StreamView) (\s a -> s {streamView = a} :: UpdateFleet)
{-# DEPRECATED ufStreamView "Use generic-lens or generic-optics with 'streamView' instead." #-}

-- | The desired capacity for the fleet.
--
-- /Note:/ Consider using 'computeCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufComputeCapacity :: Lens.Lens' UpdateFleet (Lude.Maybe ComputeCapacity)
ufComputeCapacity = Lens.lens (computeCapacity :: UpdateFleet -> Lude.Maybe ComputeCapacity) (\s a -> s {computeCapacity = a} :: UpdateFleet)
{-# DEPRECATED ufComputeCapacity "Use generic-lens or generic-optics with 'computeCapacity' instead." #-}

instance Lude.AWSRequest UpdateFleet where
  type Rs UpdateFleet = UpdateFleetResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateFleetResponse'
            Lude.<$> (x Lude..?> "Fleet") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateFleet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.UpdateFleet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateFleet where
  toJSON UpdateFleet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DomainJoinInfo" Lude..=) Lude.<$> domainJoinInfo,
            ("IamRoleArn" Lude..=) Lude.<$> iamRoleARN,
            ("DisconnectTimeoutInSeconds" Lude..=)
              Lude.<$> disconnectTimeoutInSeconds,
            ("MaxUserDurationInSeconds" Lude..=)
              Lude.<$> maxUserDurationInSeconds,
            ("AttributesToDelete" Lude..=) Lude.<$> attributesToDelete,
            ("IdleDisconnectTimeoutInSeconds" Lude..=)
              Lude.<$> idleDisconnectTimeoutInSeconds,
            ("DeleteVpcConfig" Lude..=) Lude.<$> deleteVPCConfig,
            ("InstanceType" Lude..=) Lude.<$> instanceType,
            ("VpcConfig" Lude..=) Lude.<$> vpcConfig,
            ("Name" Lude..=) Lude.<$> name,
            ("ImageArn" Lude..=) Lude.<$> imageARN,
            ("DisplayName" Lude..=) Lude.<$> displayName,
            ("EnableDefaultInternetAccess" Lude..=)
              Lude.<$> enableDefaultInternetAccess,
            ("ImageName" Lude..=) Lude.<$> imageName,
            ("Description" Lude..=) Lude.<$> description,
            ("StreamView" Lude..=) Lude.<$> streamView,
            ("ComputeCapacity" Lude..=) Lude.<$> computeCapacity
          ]
      )

instance Lude.ToPath UpdateFleet where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateFleet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateFleetResponse' smart constructor.
data UpdateFleetResponse = UpdateFleetResponse'
  { -- | Information about the fleet.
    fleet :: Lude.Maybe Fleet,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFleetResponse' with the minimum fields required to make a request.
--
-- * 'fleet' - Information about the fleet.
-- * 'responseStatus' - The response status code.
mkUpdateFleetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateFleetResponse
mkUpdateFleetResponse pResponseStatus_ =
  UpdateFleetResponse'
    { fleet = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the fleet.
--
-- /Note:/ Consider using 'fleet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufrsFleet :: Lens.Lens' UpdateFleetResponse (Lude.Maybe Fleet)
ufrsFleet = Lens.lens (fleet :: UpdateFleetResponse -> Lude.Maybe Fleet) (\s a -> s {fleet = a} :: UpdateFleetResponse)
{-# DEPRECATED ufrsFleet "Use generic-lens or generic-optics with 'fleet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufrsResponseStatus :: Lens.Lens' UpdateFleetResponse Lude.Int
ufrsResponseStatus = Lens.lens (responseStatus :: UpdateFleetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateFleetResponse)
{-# DEPRECATED ufrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
