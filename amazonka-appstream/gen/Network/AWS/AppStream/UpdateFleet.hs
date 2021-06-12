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
-- Module      : Network.AWS.AppStream.UpdateFleet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified fleet.
--
-- If the fleet is in the @STOPPED@ state, you can update any attribute
-- except the fleet name. If the fleet is in the @RUNNING@ state, you can
-- update the @DisplayName@, @ComputeCapacity@, @ImageARN@, @ImageName@,
-- @IdleDisconnectTimeoutInSeconds@, and @DisconnectTimeoutInSeconds@
-- attributes. If the fleet is in the @STARTING@ or @STOPPING@ state, you
-- can\'t update it.
module Network.AWS.AppStream.UpdateFleet
  ( -- * Creating a Request
    UpdateFleet (..),
    newUpdateFleet,

    -- * Request Lenses
    updateFleet_maxUserDurationInSeconds,
    updateFleet_disconnectTimeoutInSeconds,
    updateFleet_vpcConfig,
    updateFleet_iamRoleArn,
    updateFleet_domainJoinInfo,
    updateFleet_instanceType,
    updateFleet_computeCapacity,
    updateFleet_deleteVpcConfig,
    updateFleet_idleDisconnectTimeoutInSeconds,
    updateFleet_imageName,
    updateFleet_name,
    updateFleet_streamView,
    updateFleet_description,
    updateFleet_displayName,
    updateFleet_enableDefaultInternetAccess,
    updateFleet_attributesToDelete,
    updateFleet_imageArn,

    -- * Destructuring the Response
    UpdateFleetResponse (..),
    newUpdateFleetResponse,

    -- * Response Lenses
    updateFleetResponse_fleet,
    updateFleetResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateFleet' smart constructor.
data UpdateFleet = UpdateFleet'
  { -- | The maximum amount of time that a streaming session can remain active,
    -- in seconds. If users are still connected to a streaming instance five
    -- minutes before this limit is reached, they are prompted to save any open
    -- documents before being disconnected. After this time elapses, the
    -- instance is terminated and replaced by a new instance.
    --
    -- Specify a value between 600 and 360000.
    maxUserDurationInSeconds :: Core.Maybe Core.Int,
    -- | The amount of time that a streaming session remains active after users
    -- disconnect. If users try to reconnect to the streaming session after a
    -- disconnection or network interruption within this time interval, they
    -- are connected to their previous session. Otherwise, they are connected
    -- to a new session with a new streaming instance.
    --
    -- Specify a value between 60 and 360000.
    disconnectTimeoutInSeconds :: Core.Maybe Core.Int,
    -- | The VPC configuration for the fleet.
    vpcConfig :: Core.Maybe VpcConfig,
    -- | The Amazon Resource Name (ARN) of the IAM role to apply to the fleet. To
    -- assume a role, a fleet instance calls the AWS Security Token Service
    -- (STS) @AssumeRole@ API operation and passes the ARN of the role to use.
    -- The operation creates a new session with temporary credentials.
    -- AppStream 2.0 retrieves the temporary credentials and creates the
    -- __appstream_machine_role__ credential profile on the instance.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances>
    -- in the /Amazon AppStream 2.0 Administration Guide/.
    iamRoleArn :: Core.Maybe Core.Text,
    -- | The name of the directory and organizational unit (OU) to use to join
    -- the fleet to a Microsoft Active Directory domain.
    domainJoinInfo :: Core.Maybe DomainJoinInfo,
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
    instanceType :: Core.Maybe Core.Text,
    -- | The desired capacity for the fleet.
    computeCapacity :: Core.Maybe ComputeCapacity,
    -- | Deletes the VPC association for the specified fleet.
    deleteVpcConfig :: Core.Maybe Core.Bool,
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
    -- | A unique name for the fleet.
    name :: Core.Maybe Core.Text,
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
    -- | Enables or disables default internet access for the fleet.
    enableDefaultInternetAccess :: Core.Maybe Core.Bool,
    -- | The fleet attributes to delete.
    attributesToDelete :: Core.Maybe [FleetAttribute],
    -- | The ARN of the public, private, or shared image to use.
    imageArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxUserDurationInSeconds', 'updateFleet_maxUserDurationInSeconds' - The maximum amount of time that a streaming session can remain active,
-- in seconds. If users are still connected to a streaming instance five
-- minutes before this limit is reached, they are prompted to save any open
-- documents before being disconnected. After this time elapses, the
-- instance is terminated and replaced by a new instance.
--
-- Specify a value between 600 and 360000.
--
-- 'disconnectTimeoutInSeconds', 'updateFleet_disconnectTimeoutInSeconds' - The amount of time that a streaming session remains active after users
-- disconnect. If users try to reconnect to the streaming session after a
-- disconnection or network interruption within this time interval, they
-- are connected to their previous session. Otherwise, they are connected
-- to a new session with a new streaming instance.
--
-- Specify a value between 60 and 360000.
--
-- 'vpcConfig', 'updateFleet_vpcConfig' - The VPC configuration for the fleet.
--
-- 'iamRoleArn', 'updateFleet_iamRoleArn' - The Amazon Resource Name (ARN) of the IAM role to apply to the fleet. To
-- assume a role, a fleet instance calls the AWS Security Token Service
-- (STS) @AssumeRole@ API operation and passes the ARN of the role to use.
-- The operation creates a new session with temporary credentials.
-- AppStream 2.0 retrieves the temporary credentials and creates the
-- __appstream_machine_role__ credential profile on the instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances>
-- in the /Amazon AppStream 2.0 Administration Guide/.
--
-- 'domainJoinInfo', 'updateFleet_domainJoinInfo' - The name of the directory and organizational unit (OU) to use to join
-- the fleet to a Microsoft Active Directory domain.
--
-- 'instanceType', 'updateFleet_instanceType' - The instance type to use when launching fleet instances. The following
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
-- 'computeCapacity', 'updateFleet_computeCapacity' - The desired capacity for the fleet.
--
-- 'deleteVpcConfig', 'updateFleet_deleteVpcConfig' - Deletes the VPC association for the specified fleet.
--
-- 'idleDisconnectTimeoutInSeconds', 'updateFleet_idleDisconnectTimeoutInSeconds' - The amount of time that users can be idle (inactive) before they are
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
-- 'imageName', 'updateFleet_imageName' - The name of the image used to create the fleet.
--
-- 'name', 'updateFleet_name' - A unique name for the fleet.
--
-- 'streamView', 'updateFleet_streamView' - The AppStream 2.0 view that is displayed to your users when they stream
-- from the fleet. When @APP@ is specified, only the windows of
-- applications opened by users display. When @DESKTOP@ is specified, the
-- standard desktop that is provided by the operating system displays.
--
-- The default value is @APP@.
--
-- 'description', 'updateFleet_description' - The description to display.
--
-- 'displayName', 'updateFleet_displayName' - The fleet name to display.
--
-- 'enableDefaultInternetAccess', 'updateFleet_enableDefaultInternetAccess' - Enables or disables default internet access for the fleet.
--
-- 'attributesToDelete', 'updateFleet_attributesToDelete' - The fleet attributes to delete.
--
-- 'imageArn', 'updateFleet_imageArn' - The ARN of the public, private, or shared image to use.
newUpdateFleet ::
  UpdateFleet
newUpdateFleet =
  UpdateFleet'
    { maxUserDurationInSeconds =
        Core.Nothing,
      disconnectTimeoutInSeconds = Core.Nothing,
      vpcConfig = Core.Nothing,
      iamRoleArn = Core.Nothing,
      domainJoinInfo = Core.Nothing,
      instanceType = Core.Nothing,
      computeCapacity = Core.Nothing,
      deleteVpcConfig = Core.Nothing,
      idleDisconnectTimeoutInSeconds = Core.Nothing,
      imageName = Core.Nothing,
      name = Core.Nothing,
      streamView = Core.Nothing,
      description = Core.Nothing,
      displayName = Core.Nothing,
      enableDefaultInternetAccess = Core.Nothing,
      attributesToDelete = Core.Nothing,
      imageArn = Core.Nothing
    }

-- | The maximum amount of time that a streaming session can remain active,
-- in seconds. If users are still connected to a streaming instance five
-- minutes before this limit is reached, they are prompted to save any open
-- documents before being disconnected. After this time elapses, the
-- instance is terminated and replaced by a new instance.
--
-- Specify a value between 600 and 360000.
updateFleet_maxUserDurationInSeconds :: Lens.Lens' UpdateFleet (Core.Maybe Core.Int)
updateFleet_maxUserDurationInSeconds = Lens.lens (\UpdateFleet' {maxUserDurationInSeconds} -> maxUserDurationInSeconds) (\s@UpdateFleet' {} a -> s {maxUserDurationInSeconds = a} :: UpdateFleet)

-- | The amount of time that a streaming session remains active after users
-- disconnect. If users try to reconnect to the streaming session after a
-- disconnection or network interruption within this time interval, they
-- are connected to their previous session. Otherwise, they are connected
-- to a new session with a new streaming instance.
--
-- Specify a value between 60 and 360000.
updateFleet_disconnectTimeoutInSeconds :: Lens.Lens' UpdateFleet (Core.Maybe Core.Int)
updateFleet_disconnectTimeoutInSeconds = Lens.lens (\UpdateFleet' {disconnectTimeoutInSeconds} -> disconnectTimeoutInSeconds) (\s@UpdateFleet' {} a -> s {disconnectTimeoutInSeconds = a} :: UpdateFleet)

-- | The VPC configuration for the fleet.
updateFleet_vpcConfig :: Lens.Lens' UpdateFleet (Core.Maybe VpcConfig)
updateFleet_vpcConfig = Lens.lens (\UpdateFleet' {vpcConfig} -> vpcConfig) (\s@UpdateFleet' {} a -> s {vpcConfig = a} :: UpdateFleet)

-- | The Amazon Resource Name (ARN) of the IAM role to apply to the fleet. To
-- assume a role, a fleet instance calls the AWS Security Token Service
-- (STS) @AssumeRole@ API operation and passes the ARN of the role to use.
-- The operation creates a new session with temporary credentials.
-- AppStream 2.0 retrieves the temporary credentials and creates the
-- __appstream_machine_role__ credential profile on the instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances>
-- in the /Amazon AppStream 2.0 Administration Guide/.
updateFleet_iamRoleArn :: Lens.Lens' UpdateFleet (Core.Maybe Core.Text)
updateFleet_iamRoleArn = Lens.lens (\UpdateFleet' {iamRoleArn} -> iamRoleArn) (\s@UpdateFleet' {} a -> s {iamRoleArn = a} :: UpdateFleet)

-- | The name of the directory and organizational unit (OU) to use to join
-- the fleet to a Microsoft Active Directory domain.
updateFleet_domainJoinInfo :: Lens.Lens' UpdateFleet (Core.Maybe DomainJoinInfo)
updateFleet_domainJoinInfo = Lens.lens (\UpdateFleet' {domainJoinInfo} -> domainJoinInfo) (\s@UpdateFleet' {} a -> s {domainJoinInfo = a} :: UpdateFleet)

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
updateFleet_instanceType :: Lens.Lens' UpdateFleet (Core.Maybe Core.Text)
updateFleet_instanceType = Lens.lens (\UpdateFleet' {instanceType} -> instanceType) (\s@UpdateFleet' {} a -> s {instanceType = a} :: UpdateFleet)

-- | The desired capacity for the fleet.
updateFleet_computeCapacity :: Lens.Lens' UpdateFleet (Core.Maybe ComputeCapacity)
updateFleet_computeCapacity = Lens.lens (\UpdateFleet' {computeCapacity} -> computeCapacity) (\s@UpdateFleet' {} a -> s {computeCapacity = a} :: UpdateFleet)

-- | Deletes the VPC association for the specified fleet.
updateFleet_deleteVpcConfig :: Lens.Lens' UpdateFleet (Core.Maybe Core.Bool)
updateFleet_deleteVpcConfig = Lens.lens (\UpdateFleet' {deleteVpcConfig} -> deleteVpcConfig) (\s@UpdateFleet' {} a -> s {deleteVpcConfig = a} :: UpdateFleet)

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
updateFleet_idleDisconnectTimeoutInSeconds :: Lens.Lens' UpdateFleet (Core.Maybe Core.Int)
updateFleet_idleDisconnectTimeoutInSeconds = Lens.lens (\UpdateFleet' {idleDisconnectTimeoutInSeconds} -> idleDisconnectTimeoutInSeconds) (\s@UpdateFleet' {} a -> s {idleDisconnectTimeoutInSeconds = a} :: UpdateFleet)

-- | The name of the image used to create the fleet.
updateFleet_imageName :: Lens.Lens' UpdateFleet (Core.Maybe Core.Text)
updateFleet_imageName = Lens.lens (\UpdateFleet' {imageName} -> imageName) (\s@UpdateFleet' {} a -> s {imageName = a} :: UpdateFleet)

-- | A unique name for the fleet.
updateFleet_name :: Lens.Lens' UpdateFleet (Core.Maybe Core.Text)
updateFleet_name = Lens.lens (\UpdateFleet' {name} -> name) (\s@UpdateFleet' {} a -> s {name = a} :: UpdateFleet)

-- | The AppStream 2.0 view that is displayed to your users when they stream
-- from the fleet. When @APP@ is specified, only the windows of
-- applications opened by users display. When @DESKTOP@ is specified, the
-- standard desktop that is provided by the operating system displays.
--
-- The default value is @APP@.
updateFleet_streamView :: Lens.Lens' UpdateFleet (Core.Maybe StreamView)
updateFleet_streamView = Lens.lens (\UpdateFleet' {streamView} -> streamView) (\s@UpdateFleet' {} a -> s {streamView = a} :: UpdateFleet)

-- | The description to display.
updateFleet_description :: Lens.Lens' UpdateFleet (Core.Maybe Core.Text)
updateFleet_description = Lens.lens (\UpdateFleet' {description} -> description) (\s@UpdateFleet' {} a -> s {description = a} :: UpdateFleet)

-- | The fleet name to display.
updateFleet_displayName :: Lens.Lens' UpdateFleet (Core.Maybe Core.Text)
updateFleet_displayName = Lens.lens (\UpdateFleet' {displayName} -> displayName) (\s@UpdateFleet' {} a -> s {displayName = a} :: UpdateFleet)

-- | Enables or disables default internet access for the fleet.
updateFleet_enableDefaultInternetAccess :: Lens.Lens' UpdateFleet (Core.Maybe Core.Bool)
updateFleet_enableDefaultInternetAccess = Lens.lens (\UpdateFleet' {enableDefaultInternetAccess} -> enableDefaultInternetAccess) (\s@UpdateFleet' {} a -> s {enableDefaultInternetAccess = a} :: UpdateFleet)

-- | The fleet attributes to delete.
updateFleet_attributesToDelete :: Lens.Lens' UpdateFleet (Core.Maybe [FleetAttribute])
updateFleet_attributesToDelete = Lens.lens (\UpdateFleet' {attributesToDelete} -> attributesToDelete) (\s@UpdateFleet' {} a -> s {attributesToDelete = a} :: UpdateFleet) Core.. Lens.mapping Lens._Coerce

-- | The ARN of the public, private, or shared image to use.
updateFleet_imageArn :: Lens.Lens' UpdateFleet (Core.Maybe Core.Text)
updateFleet_imageArn = Lens.lens (\UpdateFleet' {imageArn} -> imageArn) (\s@UpdateFleet' {} a -> s {imageArn = a} :: UpdateFleet)

instance Core.AWSRequest UpdateFleet where
  type AWSResponse UpdateFleet = UpdateFleetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFleetResponse'
            Core.<$> (x Core..?> "Fleet")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateFleet

instance Core.NFData UpdateFleet

instance Core.ToHeaders UpdateFleet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.UpdateFleet" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateFleet where
  toJSON UpdateFleet' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxUserDurationInSeconds" Core..=)
              Core.<$> maxUserDurationInSeconds,
            ("DisconnectTimeoutInSeconds" Core..=)
              Core.<$> disconnectTimeoutInSeconds,
            ("VpcConfig" Core..=) Core.<$> vpcConfig,
            ("IamRoleArn" Core..=) Core.<$> iamRoleArn,
            ("DomainJoinInfo" Core..=) Core.<$> domainJoinInfo,
            ("InstanceType" Core..=) Core.<$> instanceType,
            ("ComputeCapacity" Core..=) Core.<$> computeCapacity,
            ("DeleteVpcConfig" Core..=) Core.<$> deleteVpcConfig,
            ("IdleDisconnectTimeoutInSeconds" Core..=)
              Core.<$> idleDisconnectTimeoutInSeconds,
            ("ImageName" Core..=) Core.<$> imageName,
            ("Name" Core..=) Core.<$> name,
            ("StreamView" Core..=) Core.<$> streamView,
            ("Description" Core..=) Core.<$> description,
            ("DisplayName" Core..=) Core.<$> displayName,
            ("EnableDefaultInternetAccess" Core..=)
              Core.<$> enableDefaultInternetAccess,
            ("AttributesToDelete" Core..=)
              Core.<$> attributesToDelete,
            ("ImageArn" Core..=) Core.<$> imageArn
          ]
      )

instance Core.ToPath UpdateFleet where
  toPath = Core.const "/"

instance Core.ToQuery UpdateFleet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateFleetResponse' smart constructor.
data UpdateFleetResponse = UpdateFleetResponse'
  { -- | Information about the fleet.
    fleet :: Core.Maybe Fleet,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleet', 'updateFleetResponse_fleet' - Information about the fleet.
--
-- 'httpStatus', 'updateFleetResponse_httpStatus' - The response's http status code.
newUpdateFleetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateFleetResponse
newUpdateFleetResponse pHttpStatus_ =
  UpdateFleetResponse'
    { fleet = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the fleet.
updateFleetResponse_fleet :: Lens.Lens' UpdateFleetResponse (Core.Maybe Fleet)
updateFleetResponse_fleet = Lens.lens (\UpdateFleetResponse' {fleet} -> fleet) (\s@UpdateFleetResponse' {} a -> s {fleet = a} :: UpdateFleetResponse)

-- | The response's http status code.
updateFleetResponse_httpStatus :: Lens.Lens' UpdateFleetResponse Core.Int
updateFleetResponse_httpStatus = Lens.lens (\UpdateFleetResponse' {httpStatus} -> httpStatus) (\s@UpdateFleetResponse' {} a -> s {httpStatus = a} :: UpdateFleetResponse)

instance Core.NFData UpdateFleetResponse
