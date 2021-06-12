{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.Fleet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.Fleet where

import Network.AWS.AppStream.Types.ComputeCapacityStatus
import Network.AWS.AppStream.Types.DomainJoinInfo
import Network.AWS.AppStream.Types.FleetError
import Network.AWS.AppStream.Types.FleetState
import Network.AWS.AppStream.Types.FleetType
import Network.AWS.AppStream.Types.StreamView
import Network.AWS.AppStream.Types.VpcConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a fleet.
--
-- /See:/ 'newFleet' smart constructor.
data Fleet = Fleet'
  { -- | The maximum amount of time that a streaming session can remain active,
    -- in seconds. If users are still connected to a streaming instance five
    -- minutes before this limit is reached, they are prompted to save any open
    -- documents before being disconnected. After this time elapses, the
    -- instance is terminated and replaced by a new instance.
    --
    -- Specify a value between 600 and 360000.
    maxUserDurationInSeconds :: Core.Maybe Core.Int,
    -- | The amount of time that a streaming session remains active after users
    -- disconnect. If they try to reconnect to the streaming session after a
    -- disconnection or network interruption within this time interval, they
    -- are connected to their previous session. Otherwise, they are connected
    -- to a new session with a new streaming instance.
    --
    -- Specify a value between 60 and 360000.
    disconnectTimeoutInSeconds :: Core.Maybe Core.Int,
    -- | The VPC configuration for the fleet.
    vpcConfig :: Core.Maybe VpcConfig,
    -- | The ARN of the IAM role that is applied to the fleet. To assume a role,
    -- the fleet instance calls the AWS Security Token Service (STS)
    -- @AssumeRole@ API operation and passes the ARN of the role to use. The
    -- operation creates a new session with temporary credentials. AppStream
    -- 2.0 retrieves the temporary credentials and creates the
    -- __appstream_machine_role__ credential profile on the instance.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances>
    -- in the /Amazon AppStream 2.0 Administration Guide/.
    iamRoleArn :: Core.Maybe Core.Text,
    -- | The name of the directory and organizational unit (OU) to use to join
    -- the fleet to a Microsoft Active Directory domain.
    domainJoinInfo :: Core.Maybe DomainJoinInfo,
    -- | The fleet type.
    --
    -- [ALWAYS_ON]
    --     Provides users with instant-on access to their apps. You are charged
    --     for all running instances in your fleet, even if no users are
    --     streaming apps.
    --
    -- [ON_DEMAND]
    --     Provide users with access to applications after they connect, which
    --     takes one to two minutes. You are charged for instance streaming
    --     when users are connected and a small hourly fee for instances that
    --     are not streaming apps.
    fleetType :: Core.Maybe FleetType,
    -- | The amount of time that users can be idle (inactive) before they are
    -- disconnected from their streaming session and the
    -- @DisconnectTimeoutInSeconds@ time interval begins. Users are notified
    -- before they are disconnected due to inactivity. If users try to
    -- reconnect to the streaming session before the time interval specified in
    -- @DisconnectTimeoutInSeconds@ elapses, they are connected to their
    -- previous session. Users are considered idle when they stop providing
    -- keyboard or mouse input during their streaming session. File uploads and
    -- downloads, audio in, audio out, and pixels changing do not qualify as
    -- user activity. If users continue to be idle after the time interval in
    -- @IdleDisconnectTimeoutInSeconds@ elapses, they are disconnected.
    --
    -- To prevent users from being disconnected due to inactivity, specify a
    -- value of 0. Otherwise, specify a value between 60 and 3600. The default
    -- value is 0.
    --
    -- If you enable this feature, we recommend that you specify a value that
    -- corresponds exactly to a whole number of minutes (for example, 60, 120,
    -- and 180). If you don\'t do this, the value is rounded to the nearest
    -- minute. For example, if you specify a value of 70, users are
    -- disconnected after 1 minute of inactivity. If you specify a value that
    -- is at the midpoint between two different minutes, the value is rounded
    -- up. For example, if you specify a value of 90, users are disconnected
    -- after 2 minutes of inactivity.
    idleDisconnectTimeoutInSeconds :: Core.Maybe Core.Int,
    -- | The name of the image used to create the fleet.
    imageName :: Core.Maybe Core.Text,
    -- | The time the fleet was created.
    createdTime :: Core.Maybe Core.POSIX,
    -- | The AppStream 2.0 view that is displayed to your users when they stream
    -- from the fleet. When @APP@ is specified, only the windows of
    -- applications opened by users display. When @DESKTOP@ is specified, the
    -- standard desktop that is provided by the operating system displays.
    --
    -- The default value is @APP@.
    streamView :: Core.Maybe StreamView,
    -- | The description to display.
    description :: Core.Maybe Core.Text,
    -- | The fleet name to display.
    displayName :: Core.Maybe Core.Text,
    -- | Indicates whether default internet access is enabled for the fleet.
    enableDefaultInternetAccess :: Core.Maybe Core.Bool,
    -- | The fleet errors.
    fleetErrors :: Core.Maybe [FleetError],
    -- | The ARN for the public, private, or shared image.
    imageArn :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) for the fleet.
    arn :: Core.Text,
    -- | The name of the fleet.
    name :: Core.Text,
    -- | The instance type to use when launching fleet instances. The following
    -- instance types are available:
    --
    -- -   stream.standard.small
    --
    -- -   stream.standard.medium
    --
    -- -   stream.standard.large
    --
    -- -   stream.compute.large
    --
    -- -   stream.compute.xlarge
    --
    -- -   stream.compute.2xlarge
    --
    -- -   stream.compute.4xlarge
    --
    -- -   stream.compute.8xlarge
    --
    -- -   stream.memory.large
    --
    -- -   stream.memory.xlarge
    --
    -- -   stream.memory.2xlarge
    --
    -- -   stream.memory.4xlarge
    --
    -- -   stream.memory.8xlarge
    --
    -- -   stream.memory.z1d.large
    --
    -- -   stream.memory.z1d.xlarge
    --
    -- -   stream.memory.z1d.2xlarge
    --
    -- -   stream.memory.z1d.3xlarge
    --
    -- -   stream.memory.z1d.6xlarge
    --
    -- -   stream.memory.z1d.12xlarge
    --
    -- -   stream.graphics-design.large
    --
    -- -   stream.graphics-design.xlarge
    --
    -- -   stream.graphics-design.2xlarge
    --
    -- -   stream.graphics-design.4xlarge
    --
    -- -   stream.graphics-desktop.2xlarge
    --
    -- -   stream.graphics.g4dn.xlarge
    --
    -- -   stream.graphics.g4dn.2xlarge
    --
    -- -   stream.graphics.g4dn.4xlarge
    --
    -- -   stream.graphics.g4dn.8xlarge
    --
    -- -   stream.graphics.g4dn.12xlarge
    --
    -- -   stream.graphics.g4dn.16xlarge
    --
    -- -   stream.graphics-pro.4xlarge
    --
    -- -   stream.graphics-pro.8xlarge
    --
    -- -   stream.graphics-pro.16xlarge
    instanceType :: Core.Text,
    -- | The capacity status for the fleet.
    computeCapacityStatus :: ComputeCapacityStatus,
    -- | The current state for the fleet.
    state :: FleetState
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Fleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxUserDurationInSeconds', 'fleet_maxUserDurationInSeconds' - The maximum amount of time that a streaming session can remain active,
-- in seconds. If users are still connected to a streaming instance five
-- minutes before this limit is reached, they are prompted to save any open
-- documents before being disconnected. After this time elapses, the
-- instance is terminated and replaced by a new instance.
--
-- Specify a value between 600 and 360000.
--
-- 'disconnectTimeoutInSeconds', 'fleet_disconnectTimeoutInSeconds' - The amount of time that a streaming session remains active after users
-- disconnect. If they try to reconnect to the streaming session after a
-- disconnection or network interruption within this time interval, they
-- are connected to their previous session. Otherwise, they are connected
-- to a new session with a new streaming instance.
--
-- Specify a value between 60 and 360000.
--
-- 'vpcConfig', 'fleet_vpcConfig' - The VPC configuration for the fleet.
--
-- 'iamRoleArn', 'fleet_iamRoleArn' - The ARN of the IAM role that is applied to the fleet. To assume a role,
-- the fleet instance calls the AWS Security Token Service (STS)
-- @AssumeRole@ API operation and passes the ARN of the role to use. The
-- operation creates a new session with temporary credentials. AppStream
-- 2.0 retrieves the temporary credentials and creates the
-- __appstream_machine_role__ credential profile on the instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances>
-- in the /Amazon AppStream 2.0 Administration Guide/.
--
-- 'domainJoinInfo', 'fleet_domainJoinInfo' - The name of the directory and organizational unit (OU) to use to join
-- the fleet to a Microsoft Active Directory domain.
--
-- 'fleetType', 'fleet_fleetType' - The fleet type.
--
-- [ALWAYS_ON]
--     Provides users with instant-on access to their apps. You are charged
--     for all running instances in your fleet, even if no users are
--     streaming apps.
--
-- [ON_DEMAND]
--     Provide users with access to applications after they connect, which
--     takes one to two minutes. You are charged for instance streaming
--     when users are connected and a small hourly fee for instances that
--     are not streaming apps.
--
-- 'idleDisconnectTimeoutInSeconds', 'fleet_idleDisconnectTimeoutInSeconds' - The amount of time that users can be idle (inactive) before they are
-- disconnected from their streaming session and the
-- @DisconnectTimeoutInSeconds@ time interval begins. Users are notified
-- before they are disconnected due to inactivity. If users try to
-- reconnect to the streaming session before the time interval specified in
-- @DisconnectTimeoutInSeconds@ elapses, they are connected to their
-- previous session. Users are considered idle when they stop providing
-- keyboard or mouse input during their streaming session. File uploads and
-- downloads, audio in, audio out, and pixels changing do not qualify as
-- user activity. If users continue to be idle after the time interval in
-- @IdleDisconnectTimeoutInSeconds@ elapses, they are disconnected.
--
-- To prevent users from being disconnected due to inactivity, specify a
-- value of 0. Otherwise, specify a value between 60 and 3600. The default
-- value is 0.
--
-- If you enable this feature, we recommend that you specify a value that
-- corresponds exactly to a whole number of minutes (for example, 60, 120,
-- and 180). If you don\'t do this, the value is rounded to the nearest
-- minute. For example, if you specify a value of 70, users are
-- disconnected after 1 minute of inactivity. If you specify a value that
-- is at the midpoint between two different minutes, the value is rounded
-- up. For example, if you specify a value of 90, users are disconnected
-- after 2 minutes of inactivity.
--
-- 'imageName', 'fleet_imageName' - The name of the image used to create the fleet.
--
-- 'createdTime', 'fleet_createdTime' - The time the fleet was created.
--
-- 'streamView', 'fleet_streamView' - The AppStream 2.0 view that is displayed to your users when they stream
-- from the fleet. When @APP@ is specified, only the windows of
-- applications opened by users display. When @DESKTOP@ is specified, the
-- standard desktop that is provided by the operating system displays.
--
-- The default value is @APP@.
--
-- 'description', 'fleet_description' - The description to display.
--
-- 'displayName', 'fleet_displayName' - The fleet name to display.
--
-- 'enableDefaultInternetAccess', 'fleet_enableDefaultInternetAccess' - Indicates whether default internet access is enabled for the fleet.
--
-- 'fleetErrors', 'fleet_fleetErrors' - The fleet errors.
--
-- 'imageArn', 'fleet_imageArn' - The ARN for the public, private, or shared image.
--
-- 'arn', 'fleet_arn' - The Amazon Resource Name (ARN) for the fleet.
--
-- 'name', 'fleet_name' - The name of the fleet.
--
-- 'instanceType', 'fleet_instanceType' - The instance type to use when launching fleet instances. The following
-- instance types are available:
--
-- -   stream.standard.small
--
-- -   stream.standard.medium
--
-- -   stream.standard.large
--
-- -   stream.compute.large
--
-- -   stream.compute.xlarge
--
-- -   stream.compute.2xlarge
--
-- -   stream.compute.4xlarge
--
-- -   stream.compute.8xlarge
--
-- -   stream.memory.large
--
-- -   stream.memory.xlarge
--
-- -   stream.memory.2xlarge
--
-- -   stream.memory.4xlarge
--
-- -   stream.memory.8xlarge
--
-- -   stream.memory.z1d.large
--
-- -   stream.memory.z1d.xlarge
--
-- -   stream.memory.z1d.2xlarge
--
-- -   stream.memory.z1d.3xlarge
--
-- -   stream.memory.z1d.6xlarge
--
-- -   stream.memory.z1d.12xlarge
--
-- -   stream.graphics-design.large
--
-- -   stream.graphics-design.xlarge
--
-- -   stream.graphics-design.2xlarge
--
-- -   stream.graphics-design.4xlarge
--
-- -   stream.graphics-desktop.2xlarge
--
-- -   stream.graphics.g4dn.xlarge
--
-- -   stream.graphics.g4dn.2xlarge
--
-- -   stream.graphics.g4dn.4xlarge
--
-- -   stream.graphics.g4dn.8xlarge
--
-- -   stream.graphics.g4dn.12xlarge
--
-- -   stream.graphics.g4dn.16xlarge
--
-- -   stream.graphics-pro.4xlarge
--
-- -   stream.graphics-pro.8xlarge
--
-- -   stream.graphics-pro.16xlarge
--
-- 'computeCapacityStatus', 'fleet_computeCapacityStatus' - The capacity status for the fleet.
--
-- 'state', 'fleet_state' - The current state for the fleet.
newFleet ::
  -- | 'arn'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  -- | 'instanceType'
  Core.Text ->
  -- | 'computeCapacityStatus'
  ComputeCapacityStatus ->
  -- | 'state'
  FleetState ->
  Fleet
newFleet
  pArn_
  pName_
  pInstanceType_
  pComputeCapacityStatus_
  pState_ =
    Fleet'
      { maxUserDurationInSeconds = Core.Nothing,
        disconnectTimeoutInSeconds = Core.Nothing,
        vpcConfig = Core.Nothing,
        iamRoleArn = Core.Nothing,
        domainJoinInfo = Core.Nothing,
        fleetType = Core.Nothing,
        idleDisconnectTimeoutInSeconds = Core.Nothing,
        imageName = Core.Nothing,
        createdTime = Core.Nothing,
        streamView = Core.Nothing,
        description = Core.Nothing,
        displayName = Core.Nothing,
        enableDefaultInternetAccess = Core.Nothing,
        fleetErrors = Core.Nothing,
        imageArn = Core.Nothing,
        arn = pArn_,
        name = pName_,
        instanceType = pInstanceType_,
        computeCapacityStatus = pComputeCapacityStatus_,
        state = pState_
      }

-- | The maximum amount of time that a streaming session can remain active,
-- in seconds. If users are still connected to a streaming instance five
-- minutes before this limit is reached, they are prompted to save any open
-- documents before being disconnected. After this time elapses, the
-- instance is terminated and replaced by a new instance.
--
-- Specify a value between 600 and 360000.
fleet_maxUserDurationInSeconds :: Lens.Lens' Fleet (Core.Maybe Core.Int)
fleet_maxUserDurationInSeconds = Lens.lens (\Fleet' {maxUserDurationInSeconds} -> maxUserDurationInSeconds) (\s@Fleet' {} a -> s {maxUserDurationInSeconds = a} :: Fleet)

-- | The amount of time that a streaming session remains active after users
-- disconnect. If they try to reconnect to the streaming session after a
-- disconnection or network interruption within this time interval, they
-- are connected to their previous session. Otherwise, they are connected
-- to a new session with a new streaming instance.
--
-- Specify a value between 60 and 360000.
fleet_disconnectTimeoutInSeconds :: Lens.Lens' Fleet (Core.Maybe Core.Int)
fleet_disconnectTimeoutInSeconds = Lens.lens (\Fleet' {disconnectTimeoutInSeconds} -> disconnectTimeoutInSeconds) (\s@Fleet' {} a -> s {disconnectTimeoutInSeconds = a} :: Fleet)

-- | The VPC configuration for the fleet.
fleet_vpcConfig :: Lens.Lens' Fleet (Core.Maybe VpcConfig)
fleet_vpcConfig = Lens.lens (\Fleet' {vpcConfig} -> vpcConfig) (\s@Fleet' {} a -> s {vpcConfig = a} :: Fleet)

-- | The ARN of the IAM role that is applied to the fleet. To assume a role,
-- the fleet instance calls the AWS Security Token Service (STS)
-- @AssumeRole@ API operation and passes the ARN of the role to use. The
-- operation creates a new session with temporary credentials. AppStream
-- 2.0 retrieves the temporary credentials and creates the
-- __appstream_machine_role__ credential profile on the instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances>
-- in the /Amazon AppStream 2.0 Administration Guide/.
fleet_iamRoleArn :: Lens.Lens' Fleet (Core.Maybe Core.Text)
fleet_iamRoleArn = Lens.lens (\Fleet' {iamRoleArn} -> iamRoleArn) (\s@Fleet' {} a -> s {iamRoleArn = a} :: Fleet)

-- | The name of the directory and organizational unit (OU) to use to join
-- the fleet to a Microsoft Active Directory domain.
fleet_domainJoinInfo :: Lens.Lens' Fleet (Core.Maybe DomainJoinInfo)
fleet_domainJoinInfo = Lens.lens (\Fleet' {domainJoinInfo} -> domainJoinInfo) (\s@Fleet' {} a -> s {domainJoinInfo = a} :: Fleet)

-- | The fleet type.
--
-- [ALWAYS_ON]
--     Provides users with instant-on access to their apps. You are charged
--     for all running instances in your fleet, even if no users are
--     streaming apps.
--
-- [ON_DEMAND]
--     Provide users with access to applications after they connect, which
--     takes one to two minutes. You are charged for instance streaming
--     when users are connected and a small hourly fee for instances that
--     are not streaming apps.
fleet_fleetType :: Lens.Lens' Fleet (Core.Maybe FleetType)
fleet_fleetType = Lens.lens (\Fleet' {fleetType} -> fleetType) (\s@Fleet' {} a -> s {fleetType = a} :: Fleet)

-- | The amount of time that users can be idle (inactive) before they are
-- disconnected from their streaming session and the
-- @DisconnectTimeoutInSeconds@ time interval begins. Users are notified
-- before they are disconnected due to inactivity. If users try to
-- reconnect to the streaming session before the time interval specified in
-- @DisconnectTimeoutInSeconds@ elapses, they are connected to their
-- previous session. Users are considered idle when they stop providing
-- keyboard or mouse input during their streaming session. File uploads and
-- downloads, audio in, audio out, and pixels changing do not qualify as
-- user activity. If users continue to be idle after the time interval in
-- @IdleDisconnectTimeoutInSeconds@ elapses, they are disconnected.
--
-- To prevent users from being disconnected due to inactivity, specify a
-- value of 0. Otherwise, specify a value between 60 and 3600. The default
-- value is 0.
--
-- If you enable this feature, we recommend that you specify a value that
-- corresponds exactly to a whole number of minutes (for example, 60, 120,
-- and 180). If you don\'t do this, the value is rounded to the nearest
-- minute. For example, if you specify a value of 70, users are
-- disconnected after 1 minute of inactivity. If you specify a value that
-- is at the midpoint between two different minutes, the value is rounded
-- up. For example, if you specify a value of 90, users are disconnected
-- after 2 minutes of inactivity.
fleet_idleDisconnectTimeoutInSeconds :: Lens.Lens' Fleet (Core.Maybe Core.Int)
fleet_idleDisconnectTimeoutInSeconds = Lens.lens (\Fleet' {idleDisconnectTimeoutInSeconds} -> idleDisconnectTimeoutInSeconds) (\s@Fleet' {} a -> s {idleDisconnectTimeoutInSeconds = a} :: Fleet)

-- | The name of the image used to create the fleet.
fleet_imageName :: Lens.Lens' Fleet (Core.Maybe Core.Text)
fleet_imageName = Lens.lens (\Fleet' {imageName} -> imageName) (\s@Fleet' {} a -> s {imageName = a} :: Fleet)

-- | The time the fleet was created.
fleet_createdTime :: Lens.Lens' Fleet (Core.Maybe Core.UTCTime)
fleet_createdTime = Lens.lens (\Fleet' {createdTime} -> createdTime) (\s@Fleet' {} a -> s {createdTime = a} :: Fleet) Core.. Lens.mapping Core._Time

-- | The AppStream 2.0 view that is displayed to your users when they stream
-- from the fleet. When @APP@ is specified, only the windows of
-- applications opened by users display. When @DESKTOP@ is specified, the
-- standard desktop that is provided by the operating system displays.
--
-- The default value is @APP@.
fleet_streamView :: Lens.Lens' Fleet (Core.Maybe StreamView)
fleet_streamView = Lens.lens (\Fleet' {streamView} -> streamView) (\s@Fleet' {} a -> s {streamView = a} :: Fleet)

-- | The description to display.
fleet_description :: Lens.Lens' Fleet (Core.Maybe Core.Text)
fleet_description = Lens.lens (\Fleet' {description} -> description) (\s@Fleet' {} a -> s {description = a} :: Fleet)

-- | The fleet name to display.
fleet_displayName :: Lens.Lens' Fleet (Core.Maybe Core.Text)
fleet_displayName = Lens.lens (\Fleet' {displayName} -> displayName) (\s@Fleet' {} a -> s {displayName = a} :: Fleet)

-- | Indicates whether default internet access is enabled for the fleet.
fleet_enableDefaultInternetAccess :: Lens.Lens' Fleet (Core.Maybe Core.Bool)
fleet_enableDefaultInternetAccess = Lens.lens (\Fleet' {enableDefaultInternetAccess} -> enableDefaultInternetAccess) (\s@Fleet' {} a -> s {enableDefaultInternetAccess = a} :: Fleet)

-- | The fleet errors.
fleet_fleetErrors :: Lens.Lens' Fleet (Core.Maybe [FleetError])
fleet_fleetErrors = Lens.lens (\Fleet' {fleetErrors} -> fleetErrors) (\s@Fleet' {} a -> s {fleetErrors = a} :: Fleet) Core.. Lens.mapping Lens._Coerce

-- | The ARN for the public, private, or shared image.
fleet_imageArn :: Lens.Lens' Fleet (Core.Maybe Core.Text)
fleet_imageArn = Lens.lens (\Fleet' {imageArn} -> imageArn) (\s@Fleet' {} a -> s {imageArn = a} :: Fleet)

-- | The Amazon Resource Name (ARN) for the fleet.
fleet_arn :: Lens.Lens' Fleet Core.Text
fleet_arn = Lens.lens (\Fleet' {arn} -> arn) (\s@Fleet' {} a -> s {arn = a} :: Fleet)

-- | The name of the fleet.
fleet_name :: Lens.Lens' Fleet Core.Text
fleet_name = Lens.lens (\Fleet' {name} -> name) (\s@Fleet' {} a -> s {name = a} :: Fleet)

-- | The instance type to use when launching fleet instances. The following
-- instance types are available:
--
-- -   stream.standard.small
--
-- -   stream.standard.medium
--
-- -   stream.standard.large
--
-- -   stream.compute.large
--
-- -   stream.compute.xlarge
--
-- -   stream.compute.2xlarge
--
-- -   stream.compute.4xlarge
--
-- -   stream.compute.8xlarge
--
-- -   stream.memory.large
--
-- -   stream.memory.xlarge
--
-- -   stream.memory.2xlarge
--
-- -   stream.memory.4xlarge
--
-- -   stream.memory.8xlarge
--
-- -   stream.memory.z1d.large
--
-- -   stream.memory.z1d.xlarge
--
-- -   stream.memory.z1d.2xlarge
--
-- -   stream.memory.z1d.3xlarge
--
-- -   stream.memory.z1d.6xlarge
--
-- -   stream.memory.z1d.12xlarge
--
-- -   stream.graphics-design.large
--
-- -   stream.graphics-design.xlarge
--
-- -   stream.graphics-design.2xlarge
--
-- -   stream.graphics-design.4xlarge
--
-- -   stream.graphics-desktop.2xlarge
--
-- -   stream.graphics.g4dn.xlarge
--
-- -   stream.graphics.g4dn.2xlarge
--
-- -   stream.graphics.g4dn.4xlarge
--
-- -   stream.graphics.g4dn.8xlarge
--
-- -   stream.graphics.g4dn.12xlarge
--
-- -   stream.graphics.g4dn.16xlarge
--
-- -   stream.graphics-pro.4xlarge
--
-- -   stream.graphics-pro.8xlarge
--
-- -   stream.graphics-pro.16xlarge
fleet_instanceType :: Lens.Lens' Fleet Core.Text
fleet_instanceType = Lens.lens (\Fleet' {instanceType} -> instanceType) (\s@Fleet' {} a -> s {instanceType = a} :: Fleet)

-- | The capacity status for the fleet.
fleet_computeCapacityStatus :: Lens.Lens' Fleet ComputeCapacityStatus
fleet_computeCapacityStatus = Lens.lens (\Fleet' {computeCapacityStatus} -> computeCapacityStatus) (\s@Fleet' {} a -> s {computeCapacityStatus = a} :: Fleet)

-- | The current state for the fleet.
fleet_state :: Lens.Lens' Fleet FleetState
fleet_state = Lens.lens (\Fleet' {state} -> state) (\s@Fleet' {} a -> s {state = a} :: Fleet)

instance Core.FromJSON Fleet where
  parseJSON =
    Core.withObject
      "Fleet"
      ( \x ->
          Fleet'
            Core.<$> (x Core..:? "MaxUserDurationInSeconds")
            Core.<*> (x Core..:? "DisconnectTimeoutInSeconds")
            Core.<*> (x Core..:? "VpcConfig")
            Core.<*> (x Core..:? "IamRoleArn")
            Core.<*> (x Core..:? "DomainJoinInfo")
            Core.<*> (x Core..:? "FleetType")
            Core.<*> (x Core..:? "IdleDisconnectTimeoutInSeconds")
            Core.<*> (x Core..:? "ImageName")
            Core.<*> (x Core..:? "CreatedTime")
            Core.<*> (x Core..:? "StreamView")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "DisplayName")
            Core.<*> (x Core..:? "EnableDefaultInternetAccess")
            Core.<*> (x Core..:? "FleetErrors" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ImageArn")
            Core.<*> (x Core..: "Arn")
            Core.<*> (x Core..: "Name")
            Core.<*> (x Core..: "InstanceType")
            Core.<*> (x Core..: "ComputeCapacityStatus")
            Core.<*> (x Core..: "State")
      )

instance Core.Hashable Fleet

instance Core.NFData Fleet
