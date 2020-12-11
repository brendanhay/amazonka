-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.Fleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.Fleet
  ( Fleet (..),

    -- * Smart constructor
    mkFleet,

    -- * Lenses
    fDomainJoinInfo,
    fIAMRoleARN,
    fDisconnectTimeoutInSeconds,
    fMaxUserDurationInSeconds,
    fCreatedTime,
    fIdleDisconnectTimeoutInSeconds,
    fFleetType,
    fVPCConfig,
    fImageARN,
    fFleetErrors,
    fDisplayName,
    fEnableDefaultInternetAccess,
    fImageName,
    fDescription,
    fStreamView,
    fARN,
    fName,
    fInstanceType,
    fComputeCapacityStatus,
    fState,
  )
where

import Network.AWS.AppStream.Types.ComputeCapacityStatus
import Network.AWS.AppStream.Types.DomainJoinInfo
import Network.AWS.AppStream.Types.FleetError
import Network.AWS.AppStream.Types.FleetState
import Network.AWS.AppStream.Types.FleetType
import Network.AWS.AppStream.Types.StreamView
import Network.AWS.AppStream.Types.VPCConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a fleet.
--
-- /See:/ 'mkFleet' smart constructor.
data Fleet = Fleet'
  { domainJoinInfo :: Lude.Maybe DomainJoinInfo,
    iamRoleARN :: Lude.Maybe Lude.Text,
    disconnectTimeoutInSeconds :: Lude.Maybe Lude.Int,
    maxUserDurationInSeconds :: Lude.Maybe Lude.Int,
    createdTime :: Lude.Maybe Lude.Timestamp,
    idleDisconnectTimeoutInSeconds :: Lude.Maybe Lude.Int,
    fleetType :: Lude.Maybe FleetType,
    vpcConfig :: Lude.Maybe VPCConfig,
    imageARN :: Lude.Maybe Lude.Text,
    fleetErrors :: Lude.Maybe [FleetError],
    displayName :: Lude.Maybe Lude.Text,
    enableDefaultInternetAccess :: Lude.Maybe Lude.Bool,
    imageName :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    streamView :: Lude.Maybe StreamView,
    arn :: Lude.Text,
    name :: Lude.Text,
    instanceType :: Lude.Text,
    computeCapacityStatus :: ComputeCapacityStatus,
    state :: FleetState
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Fleet' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) for the fleet.
-- * 'computeCapacityStatus' - The capacity status for the fleet.
-- * 'createdTime' - The time the fleet was created.
-- * 'description' - The description to display.
-- * 'disconnectTimeoutInSeconds' - The amount of time that a streaming session remains active after users disconnect. If they try to reconnect to the streaming session after a disconnection or network interruption within this time interval, they are connected to their previous session. Otherwise, they are connected to a new session with a new streaming instance.
--
-- Specify a value between 60 and 360000.
-- * 'displayName' - The fleet name to display.
-- * 'domainJoinInfo' - The name of the directory and organizational unit (OU) to use to join the fleet to a Microsoft Active Directory domain.
-- * 'enableDefaultInternetAccess' - Indicates whether default internet access is enabled for the fleet.
-- * 'fleetErrors' - The fleet errors.
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
-- * 'iamRoleARN' - The ARN of the IAM role that is applied to the fleet. To assume a role, the fleet instance calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance.
--
-- For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
-- * 'idleDisconnectTimeoutInSeconds' - The amount of time that users can be idle (inactive) before they are disconnected from their streaming session and the @DisconnectTimeoutInSeconds@ time interval begins. Users are notified before they are disconnected due to inactivity. If users try to reconnect to the streaming session before the time interval specified in @DisconnectTimeoutInSeconds@ elapses, they are connected to their previous session. Users are considered idle when they stop providing keyboard or mouse input during their streaming session. File uploads and downloads, audio in, audio out, and pixels changing do not qualify as user activity. If users continue to be idle after the time interval in @IdleDisconnectTimeoutInSeconds@ elapses, they are disconnected.
--
-- To prevent users from being disconnected due to inactivity, specify a value of 0. Otherwise, specify a value between 60 and 3600. The default value is 0.
-- * 'imageARN' - The ARN for the public, private, or shared image.
-- * 'imageName' - The name of the image used to create the fleet.
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
-- * 'maxUserDurationInSeconds' - The maximum amount of time that a streaming session can remain active, in seconds. If users are still connected to a streaming instance five minutes before this limit is reached, they are prompted to save any open documents before being disconnected. After this time elapses, the instance is terminated and replaced by a new instance.
--
-- Specify a value between 600 and 360000.
-- * 'name' - The name of the fleet.
-- * 'state' - The current state for the fleet.
-- * 'streamView' - The AppStream 2.0 view that is displayed to your users when they stream from the fleet. When @APP@ is specified, only the windows of applications opened by users display. When @DESKTOP@ is specified, the standard desktop that is provided by the operating system displays.
--
-- The default value is @APP@ .
-- * 'vpcConfig' - The VPC configuration for the fleet.
mkFleet ::
  -- | 'arn'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'instanceType'
  Lude.Text ->
  -- | 'computeCapacityStatus'
  ComputeCapacityStatus ->
  -- | 'state'
  FleetState ->
  Fleet
mkFleet pARN_ pName_ pInstanceType_ pComputeCapacityStatus_ pState_ =
  Fleet'
    { domainJoinInfo = Lude.Nothing,
      iamRoleARN = Lude.Nothing,
      disconnectTimeoutInSeconds = Lude.Nothing,
      maxUserDurationInSeconds = Lude.Nothing,
      createdTime = Lude.Nothing,
      idleDisconnectTimeoutInSeconds = Lude.Nothing,
      fleetType = Lude.Nothing,
      vpcConfig = Lude.Nothing,
      imageARN = Lude.Nothing,
      fleetErrors = Lude.Nothing,
      displayName = Lude.Nothing,
      enableDefaultInternetAccess = Lude.Nothing,
      imageName = Lude.Nothing,
      description = Lude.Nothing,
      streamView = Lude.Nothing,
      arn = pARN_,
      name = pName_,
      instanceType = pInstanceType_,
      computeCapacityStatus = pComputeCapacityStatus_,
      state = pState_
    }

-- | The name of the directory and organizational unit (OU) to use to join the fleet to a Microsoft Active Directory domain.
--
-- /Note:/ Consider using 'domainJoinInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fDomainJoinInfo :: Lens.Lens' Fleet (Lude.Maybe DomainJoinInfo)
fDomainJoinInfo = Lens.lens (domainJoinInfo :: Fleet -> Lude.Maybe DomainJoinInfo) (\s a -> s {domainJoinInfo = a} :: Fleet)
{-# DEPRECATED fDomainJoinInfo "Use generic-lens or generic-optics with 'domainJoinInfo' instead." #-}

-- | The ARN of the IAM role that is applied to the fleet. To assume a role, the fleet instance calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance.
--
-- For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
--
-- /Note:/ Consider using 'iamRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fIAMRoleARN :: Lens.Lens' Fleet (Lude.Maybe Lude.Text)
fIAMRoleARN = Lens.lens (iamRoleARN :: Fleet -> Lude.Maybe Lude.Text) (\s a -> s {iamRoleARN = a} :: Fleet)
{-# DEPRECATED fIAMRoleARN "Use generic-lens or generic-optics with 'iamRoleARN' instead." #-}

-- | The amount of time that a streaming session remains active after users disconnect. If they try to reconnect to the streaming session after a disconnection or network interruption within this time interval, they are connected to their previous session. Otherwise, they are connected to a new session with a new streaming instance.
--
-- Specify a value between 60 and 360000.
--
-- /Note:/ Consider using 'disconnectTimeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fDisconnectTimeoutInSeconds :: Lens.Lens' Fleet (Lude.Maybe Lude.Int)
fDisconnectTimeoutInSeconds = Lens.lens (disconnectTimeoutInSeconds :: Fleet -> Lude.Maybe Lude.Int) (\s a -> s {disconnectTimeoutInSeconds = a} :: Fleet)
{-# DEPRECATED fDisconnectTimeoutInSeconds "Use generic-lens or generic-optics with 'disconnectTimeoutInSeconds' instead." #-}

-- | The maximum amount of time that a streaming session can remain active, in seconds. If users are still connected to a streaming instance five minutes before this limit is reached, they are prompted to save any open documents before being disconnected. After this time elapses, the instance is terminated and replaced by a new instance.
--
-- Specify a value between 600 and 360000.
--
-- /Note:/ Consider using 'maxUserDurationInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fMaxUserDurationInSeconds :: Lens.Lens' Fleet (Lude.Maybe Lude.Int)
fMaxUserDurationInSeconds = Lens.lens (maxUserDurationInSeconds :: Fleet -> Lude.Maybe Lude.Int) (\s a -> s {maxUserDurationInSeconds = a} :: Fleet)
{-# DEPRECATED fMaxUserDurationInSeconds "Use generic-lens or generic-optics with 'maxUserDurationInSeconds' instead." #-}

-- | The time the fleet was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fCreatedTime :: Lens.Lens' Fleet (Lude.Maybe Lude.Timestamp)
fCreatedTime = Lens.lens (createdTime :: Fleet -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTime = a} :: Fleet)
{-# DEPRECATED fCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The amount of time that users can be idle (inactive) before they are disconnected from their streaming session and the @DisconnectTimeoutInSeconds@ time interval begins. Users are notified before they are disconnected due to inactivity. If users try to reconnect to the streaming session before the time interval specified in @DisconnectTimeoutInSeconds@ elapses, they are connected to their previous session. Users are considered idle when they stop providing keyboard or mouse input during their streaming session. File uploads and downloads, audio in, audio out, and pixels changing do not qualify as user activity. If users continue to be idle after the time interval in @IdleDisconnectTimeoutInSeconds@ elapses, they are disconnected.
--
-- To prevent users from being disconnected due to inactivity, specify a value of 0. Otherwise, specify a value between 60 and 3600. The default value is 0.
--
-- /Note:/ Consider using 'idleDisconnectTimeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fIdleDisconnectTimeoutInSeconds :: Lens.Lens' Fleet (Lude.Maybe Lude.Int)
fIdleDisconnectTimeoutInSeconds = Lens.lens (idleDisconnectTimeoutInSeconds :: Fleet -> Lude.Maybe Lude.Int) (\s a -> s {idleDisconnectTimeoutInSeconds = a} :: Fleet)
{-# DEPRECATED fIdleDisconnectTimeoutInSeconds "Use generic-lens or generic-optics with 'idleDisconnectTimeoutInSeconds' instead." #-}

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
fFleetType :: Lens.Lens' Fleet (Lude.Maybe FleetType)
fFleetType = Lens.lens (fleetType :: Fleet -> Lude.Maybe FleetType) (\s a -> s {fleetType = a} :: Fleet)
{-# DEPRECATED fFleetType "Use generic-lens or generic-optics with 'fleetType' instead." #-}

-- | The VPC configuration for the fleet.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fVPCConfig :: Lens.Lens' Fleet (Lude.Maybe VPCConfig)
fVPCConfig = Lens.lens (vpcConfig :: Fleet -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: Fleet)
{-# DEPRECATED fVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | The ARN for the public, private, or shared image.
--
-- /Note:/ Consider using 'imageARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fImageARN :: Lens.Lens' Fleet (Lude.Maybe Lude.Text)
fImageARN = Lens.lens (imageARN :: Fleet -> Lude.Maybe Lude.Text) (\s a -> s {imageARN = a} :: Fleet)
{-# DEPRECATED fImageARN "Use generic-lens or generic-optics with 'imageARN' instead." #-}

-- | The fleet errors.
--
-- /Note:/ Consider using 'fleetErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fFleetErrors :: Lens.Lens' Fleet (Lude.Maybe [FleetError])
fFleetErrors = Lens.lens (fleetErrors :: Fleet -> Lude.Maybe [FleetError]) (\s a -> s {fleetErrors = a} :: Fleet)
{-# DEPRECATED fFleetErrors "Use generic-lens or generic-optics with 'fleetErrors' instead." #-}

-- | The fleet name to display.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fDisplayName :: Lens.Lens' Fleet (Lude.Maybe Lude.Text)
fDisplayName = Lens.lens (displayName :: Fleet -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: Fleet)
{-# DEPRECATED fDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | Indicates whether default internet access is enabled for the fleet.
--
-- /Note:/ Consider using 'enableDefaultInternetAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fEnableDefaultInternetAccess :: Lens.Lens' Fleet (Lude.Maybe Lude.Bool)
fEnableDefaultInternetAccess = Lens.lens (enableDefaultInternetAccess :: Fleet -> Lude.Maybe Lude.Bool) (\s a -> s {enableDefaultInternetAccess = a} :: Fleet)
{-# DEPRECATED fEnableDefaultInternetAccess "Use generic-lens or generic-optics with 'enableDefaultInternetAccess' instead." #-}

-- | The name of the image used to create the fleet.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fImageName :: Lens.Lens' Fleet (Lude.Maybe Lude.Text)
fImageName = Lens.lens (imageName :: Fleet -> Lude.Maybe Lude.Text) (\s a -> s {imageName = a} :: Fleet)
{-# DEPRECATED fImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

-- | The description to display.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fDescription :: Lens.Lens' Fleet (Lude.Maybe Lude.Text)
fDescription = Lens.lens (description :: Fleet -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Fleet)
{-# DEPRECATED fDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The AppStream 2.0 view that is displayed to your users when they stream from the fleet. When @APP@ is specified, only the windows of applications opened by users display. When @DESKTOP@ is specified, the standard desktop that is provided by the operating system displays.
--
-- The default value is @APP@ .
--
-- /Note:/ Consider using 'streamView' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fStreamView :: Lens.Lens' Fleet (Lude.Maybe StreamView)
fStreamView = Lens.lens (streamView :: Fleet -> Lude.Maybe StreamView) (\s a -> s {streamView = a} :: Fleet)
{-# DEPRECATED fStreamView "Use generic-lens or generic-optics with 'streamView' instead." #-}

-- | The Amazon Resource Name (ARN) for the fleet.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fARN :: Lens.Lens' Fleet Lude.Text
fARN = Lens.lens (arn :: Fleet -> Lude.Text) (\s a -> s {arn = a} :: Fleet)
{-# DEPRECATED fARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the fleet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fName :: Lens.Lens' Fleet Lude.Text
fName = Lens.lens (name :: Fleet -> Lude.Text) (\s a -> s {name = a} :: Fleet)
{-# DEPRECATED fName "Use generic-lens or generic-optics with 'name' instead." #-}

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
fInstanceType :: Lens.Lens' Fleet Lude.Text
fInstanceType = Lens.lens (instanceType :: Fleet -> Lude.Text) (\s a -> s {instanceType = a} :: Fleet)
{-# DEPRECATED fInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The capacity status for the fleet.
--
-- /Note:/ Consider using 'computeCapacityStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fComputeCapacityStatus :: Lens.Lens' Fleet ComputeCapacityStatus
fComputeCapacityStatus = Lens.lens (computeCapacityStatus :: Fleet -> ComputeCapacityStatus) (\s a -> s {computeCapacityStatus = a} :: Fleet)
{-# DEPRECATED fComputeCapacityStatus "Use generic-lens or generic-optics with 'computeCapacityStatus' instead." #-}

-- | The current state for the fleet.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fState :: Lens.Lens' Fleet FleetState
fState = Lens.lens (state :: Fleet -> FleetState) (\s a -> s {state = a} :: Fleet)
{-# DEPRECATED fState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Lude.FromJSON Fleet where
  parseJSON =
    Lude.withObject
      "Fleet"
      ( \x ->
          Fleet'
            Lude.<$> (x Lude..:? "DomainJoinInfo")
            Lude.<*> (x Lude..:? "IamRoleArn")
            Lude.<*> (x Lude..:? "DisconnectTimeoutInSeconds")
            Lude.<*> (x Lude..:? "MaxUserDurationInSeconds")
            Lude.<*> (x Lude..:? "CreatedTime")
            Lude.<*> (x Lude..:? "IdleDisconnectTimeoutInSeconds")
            Lude.<*> (x Lude..:? "FleetType")
            Lude.<*> (x Lude..:? "VpcConfig")
            Lude.<*> (x Lude..:? "ImageArn")
            Lude.<*> (x Lude..:? "FleetErrors" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "DisplayName")
            Lude.<*> (x Lude..:? "EnableDefaultInternetAccess")
            Lude.<*> (x Lude..:? "ImageName")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "StreamView")
            Lude.<*> (x Lude..: "Arn")
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..: "InstanceType")
            Lude.<*> (x Lude..: "ComputeCapacityStatus")
            Lude.<*> (x Lude..: "State")
      )
