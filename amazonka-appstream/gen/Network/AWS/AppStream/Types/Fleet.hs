{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    maxUserDurationInSeconds :: Prelude.Maybe Prelude.Int,
    -- | The amount of time that a streaming session remains active after users
    -- disconnect. If they try to reconnect to the streaming session after a
    -- disconnection or network interruption within this time interval, they
    -- are connected to their previous session. Otherwise, they are connected
    -- to a new session with a new streaming instance.
    --
    -- Specify a value between 60 and 360000.
    disconnectTimeoutInSeconds :: Prelude.Maybe Prelude.Int,
    -- | The VPC configuration for the fleet.
    vpcConfig :: Prelude.Maybe VpcConfig,
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
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the directory and organizational unit (OU) to use to join
    -- the fleet to a Microsoft Active Directory domain.
    domainJoinInfo :: Prelude.Maybe DomainJoinInfo,
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
    fleetType :: Prelude.Maybe FleetType,
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
    idleDisconnectTimeoutInSeconds :: Prelude.Maybe Prelude.Int,
    -- | The name of the image used to create the fleet.
    imageName :: Prelude.Maybe Prelude.Text,
    -- | The time the fleet was created.
    createdTime :: Prelude.Maybe Prelude.POSIX,
    -- | The AppStream 2.0 view that is displayed to your users when they stream
    -- from the fleet. When @APP@ is specified, only the windows of
    -- applications opened by users display. When @DESKTOP@ is specified, the
    -- standard desktop that is provided by the operating system displays.
    --
    -- The default value is @APP@.
    streamView :: Prelude.Maybe StreamView,
    -- | The description to display.
    description :: Prelude.Maybe Prelude.Text,
    -- | The fleet name to display.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether default internet access is enabled for the fleet.
    enableDefaultInternetAccess :: Prelude.Maybe Prelude.Bool,
    -- | The fleet errors.
    fleetErrors :: Prelude.Maybe [FleetError],
    -- | The ARN for the public, private, or shared image.
    imageArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the fleet.
    arn :: Prelude.Text,
    -- | The name of the fleet.
    name :: Prelude.Text,
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
    instanceType :: Prelude.Text,
    -- | The capacity status for the fleet.
    computeCapacityStatus :: ComputeCapacityStatus,
    -- | The current state for the fleet.
    state :: FleetState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'instanceType'
  Prelude.Text ->
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
      { maxUserDurationInSeconds = Prelude.Nothing,
        disconnectTimeoutInSeconds = Prelude.Nothing,
        vpcConfig = Prelude.Nothing,
        iamRoleArn = Prelude.Nothing,
        domainJoinInfo = Prelude.Nothing,
        fleetType = Prelude.Nothing,
        idleDisconnectTimeoutInSeconds = Prelude.Nothing,
        imageName = Prelude.Nothing,
        createdTime = Prelude.Nothing,
        streamView = Prelude.Nothing,
        description = Prelude.Nothing,
        displayName = Prelude.Nothing,
        enableDefaultInternetAccess = Prelude.Nothing,
        fleetErrors = Prelude.Nothing,
        imageArn = Prelude.Nothing,
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
fleet_maxUserDurationInSeconds :: Lens.Lens' Fleet (Prelude.Maybe Prelude.Int)
fleet_maxUserDurationInSeconds = Lens.lens (\Fleet' {maxUserDurationInSeconds} -> maxUserDurationInSeconds) (\s@Fleet' {} a -> s {maxUserDurationInSeconds = a} :: Fleet)

-- | The amount of time that a streaming session remains active after users
-- disconnect. If they try to reconnect to the streaming session after a
-- disconnection or network interruption within this time interval, they
-- are connected to their previous session. Otherwise, they are connected
-- to a new session with a new streaming instance.
--
-- Specify a value between 60 and 360000.
fleet_disconnectTimeoutInSeconds :: Lens.Lens' Fleet (Prelude.Maybe Prelude.Int)
fleet_disconnectTimeoutInSeconds = Lens.lens (\Fleet' {disconnectTimeoutInSeconds} -> disconnectTimeoutInSeconds) (\s@Fleet' {} a -> s {disconnectTimeoutInSeconds = a} :: Fleet)

-- | The VPC configuration for the fleet.
fleet_vpcConfig :: Lens.Lens' Fleet (Prelude.Maybe VpcConfig)
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
fleet_iamRoleArn :: Lens.Lens' Fleet (Prelude.Maybe Prelude.Text)
fleet_iamRoleArn = Lens.lens (\Fleet' {iamRoleArn} -> iamRoleArn) (\s@Fleet' {} a -> s {iamRoleArn = a} :: Fleet)

-- | The name of the directory and organizational unit (OU) to use to join
-- the fleet to a Microsoft Active Directory domain.
fleet_domainJoinInfo :: Lens.Lens' Fleet (Prelude.Maybe DomainJoinInfo)
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
fleet_fleetType :: Lens.Lens' Fleet (Prelude.Maybe FleetType)
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
fleet_idleDisconnectTimeoutInSeconds :: Lens.Lens' Fleet (Prelude.Maybe Prelude.Int)
fleet_idleDisconnectTimeoutInSeconds = Lens.lens (\Fleet' {idleDisconnectTimeoutInSeconds} -> idleDisconnectTimeoutInSeconds) (\s@Fleet' {} a -> s {idleDisconnectTimeoutInSeconds = a} :: Fleet)

-- | The name of the image used to create the fleet.
fleet_imageName :: Lens.Lens' Fleet (Prelude.Maybe Prelude.Text)
fleet_imageName = Lens.lens (\Fleet' {imageName} -> imageName) (\s@Fleet' {} a -> s {imageName = a} :: Fleet)

-- | The time the fleet was created.
fleet_createdTime :: Lens.Lens' Fleet (Prelude.Maybe Prelude.UTCTime)
fleet_createdTime = Lens.lens (\Fleet' {createdTime} -> createdTime) (\s@Fleet' {} a -> s {createdTime = a} :: Fleet) Prelude.. Lens.mapping Prelude._Time

-- | The AppStream 2.0 view that is displayed to your users when they stream
-- from the fleet. When @APP@ is specified, only the windows of
-- applications opened by users display. When @DESKTOP@ is specified, the
-- standard desktop that is provided by the operating system displays.
--
-- The default value is @APP@.
fleet_streamView :: Lens.Lens' Fleet (Prelude.Maybe StreamView)
fleet_streamView = Lens.lens (\Fleet' {streamView} -> streamView) (\s@Fleet' {} a -> s {streamView = a} :: Fleet)

-- | The description to display.
fleet_description :: Lens.Lens' Fleet (Prelude.Maybe Prelude.Text)
fleet_description = Lens.lens (\Fleet' {description} -> description) (\s@Fleet' {} a -> s {description = a} :: Fleet)

-- | The fleet name to display.
fleet_displayName :: Lens.Lens' Fleet (Prelude.Maybe Prelude.Text)
fleet_displayName = Lens.lens (\Fleet' {displayName} -> displayName) (\s@Fleet' {} a -> s {displayName = a} :: Fleet)

-- | Indicates whether default internet access is enabled for the fleet.
fleet_enableDefaultInternetAccess :: Lens.Lens' Fleet (Prelude.Maybe Prelude.Bool)
fleet_enableDefaultInternetAccess = Lens.lens (\Fleet' {enableDefaultInternetAccess} -> enableDefaultInternetAccess) (\s@Fleet' {} a -> s {enableDefaultInternetAccess = a} :: Fleet)

-- | The fleet errors.
fleet_fleetErrors :: Lens.Lens' Fleet (Prelude.Maybe [FleetError])
fleet_fleetErrors = Lens.lens (\Fleet' {fleetErrors} -> fleetErrors) (\s@Fleet' {} a -> s {fleetErrors = a} :: Fleet) Prelude.. Lens.mapping Prelude._Coerce

-- | The ARN for the public, private, or shared image.
fleet_imageArn :: Lens.Lens' Fleet (Prelude.Maybe Prelude.Text)
fleet_imageArn = Lens.lens (\Fleet' {imageArn} -> imageArn) (\s@Fleet' {} a -> s {imageArn = a} :: Fleet)

-- | The Amazon Resource Name (ARN) for the fleet.
fleet_arn :: Lens.Lens' Fleet Prelude.Text
fleet_arn = Lens.lens (\Fleet' {arn} -> arn) (\s@Fleet' {} a -> s {arn = a} :: Fleet)

-- | The name of the fleet.
fleet_name :: Lens.Lens' Fleet Prelude.Text
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
fleet_instanceType :: Lens.Lens' Fleet Prelude.Text
fleet_instanceType = Lens.lens (\Fleet' {instanceType} -> instanceType) (\s@Fleet' {} a -> s {instanceType = a} :: Fleet)

-- | The capacity status for the fleet.
fleet_computeCapacityStatus :: Lens.Lens' Fleet ComputeCapacityStatus
fleet_computeCapacityStatus = Lens.lens (\Fleet' {computeCapacityStatus} -> computeCapacityStatus) (\s@Fleet' {} a -> s {computeCapacityStatus = a} :: Fleet)

-- | The current state for the fleet.
fleet_state :: Lens.Lens' Fleet FleetState
fleet_state = Lens.lens (\Fleet' {state} -> state) (\s@Fleet' {} a -> s {state = a} :: Fleet)

instance Prelude.FromJSON Fleet where
  parseJSON =
    Prelude.withObject
      "Fleet"
      ( \x ->
          Fleet'
            Prelude.<$> (x Prelude..:? "MaxUserDurationInSeconds")
            Prelude.<*> (x Prelude..:? "DisconnectTimeoutInSeconds")
            Prelude.<*> (x Prelude..:? "VpcConfig")
            Prelude.<*> (x Prelude..:? "IamRoleArn")
            Prelude.<*> (x Prelude..:? "DomainJoinInfo")
            Prelude.<*> (x Prelude..:? "FleetType")
            Prelude.<*> (x Prelude..:? "IdleDisconnectTimeoutInSeconds")
            Prelude.<*> (x Prelude..:? "ImageName")
            Prelude.<*> (x Prelude..:? "CreatedTime")
            Prelude.<*> (x Prelude..:? "StreamView")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "DisplayName")
            Prelude.<*> (x Prelude..:? "EnableDefaultInternetAccess")
            Prelude.<*> ( x Prelude..:? "FleetErrors"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "ImageArn")
            Prelude.<*> (x Prelude..: "Arn")
            Prelude.<*> (x Prelude..: "Name")
            Prelude.<*> (x Prelude..: "InstanceType")
            Prelude.<*> (x Prelude..: "ComputeCapacityStatus")
            Prelude.<*> (x Prelude..: "State")
      )

instance Prelude.Hashable Fleet

instance Prelude.NFData Fleet
