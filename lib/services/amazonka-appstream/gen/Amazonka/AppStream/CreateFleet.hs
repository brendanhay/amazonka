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
-- Module      : Amazonka.AppStream.CreateFleet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a fleet. A fleet consists of streaming instances that your users
-- access for their applications and desktops.
module Amazonka.AppStream.CreateFleet
  ( -- * Creating a Request
    CreateFleet (..),
    newCreateFleet,

    -- * Request Lenses
    createFleet_computeCapacity,
    createFleet_description,
    createFleet_disconnectTimeoutInSeconds,
    createFleet_displayName,
    createFleet_domainJoinInfo,
    createFleet_enableDefaultInternetAccess,
    createFleet_fleetType,
    createFleet_iamRoleArn,
    createFleet_idleDisconnectTimeoutInSeconds,
    createFleet_imageArn,
    createFleet_imageName,
    createFleet_maxConcurrentSessions,
    createFleet_maxUserDurationInSeconds,
    createFleet_platform,
    createFleet_sessionScriptS3Location,
    createFleet_streamView,
    createFleet_tags,
    createFleet_usbDeviceFilterStrings,
    createFleet_vpcConfig,
    createFleet_name,
    createFleet_instanceType,

    -- * Destructuring the Response
    CreateFleetResponse (..),
    newCreateFleetResponse,

    -- * Response Lenses
    createFleetResponse_fleet,
    createFleetResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFleet' smart constructor.
data CreateFleet = CreateFleet'
  { -- | The desired capacity for the fleet. This is not allowed for Elastic
    -- fleets. For Elastic fleets, specify MaxConcurrentSessions instead.
    computeCapacity :: Prelude.Maybe ComputeCapacity,
    -- | The description to display.
    description :: Prelude.Maybe Prelude.Text,
    -- | The amount of time that a streaming session remains active after users
    -- disconnect. If users try to reconnect to the streaming session after a
    -- disconnection or network interruption within this time interval, they
    -- are connected to their previous session. Otherwise, they are connected
    -- to a new session with a new streaming instance.
    --
    -- Specify a value between 60 and 360000.
    disconnectTimeoutInSeconds :: Prelude.Maybe Prelude.Int,
    -- | The fleet name to display.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The name of the directory and organizational unit (OU) to use to join
    -- the fleet to a Microsoft Active Directory domain. This is not allowed
    -- for Elastic fleets.
    domainJoinInfo :: Prelude.Maybe DomainJoinInfo,
    -- | Enables or disables default internet access for the fleet.
    enableDefaultInternetAccess :: Prelude.Maybe Prelude.Bool,
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
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The amount of time that users can be idle (inactive) before they are
    -- disconnected from their streaming session and the
    -- @DisconnectTimeoutInSeconds@ time interval begins. Users are notified
    -- before they are disconnected due to inactivity. If they try to reconnect
    -- to the streaming session before the time interval specified in
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
    -- | The ARN of the public, private, or shared image to use.
    imageArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the image used to create the fleet.
    imageName :: Prelude.Maybe Prelude.Text,
    -- | The maximum concurrent sessions of the Elastic fleet. This is required
    -- for Elastic fleets, and not allowed for other fleet types.
    maxConcurrentSessions :: Prelude.Maybe Prelude.Int,
    -- | The maximum amount of time that a streaming session can remain active,
    -- in seconds. If users are still connected to a streaming instance five
    -- minutes before this limit is reached, they are prompted to save any open
    -- documents before being disconnected. After this time elapses, the
    -- instance is terminated and replaced by a new instance.
    --
    -- Specify a value between 600 and 360000.
    maxUserDurationInSeconds :: Prelude.Maybe Prelude.Int,
    -- | The fleet platform. WINDOWS_SERVER_2019 and AMAZON_LINUX2 are supported
    -- for Elastic fleets.
    platform :: Prelude.Maybe PlatformType,
    -- | The S3 location of the session scripts configuration zip file. This only
    -- applies to Elastic fleets.
    sessionScriptS3Location :: Prelude.Maybe S3Location,
    -- | The AppStream 2.0 view that is displayed to your users when they stream
    -- from the fleet. When @APP@ is specified, only the windows of
    -- applications opened by users display. When @DESKTOP@ is specified, the
    -- standard desktop that is provided by the operating system displays.
    --
    -- The default value is @APP@.
    streamView :: Prelude.Maybe StreamView,
    -- | The tags to associate with the fleet. A tag is a key-value pair, and the
    -- value is optional. For example, Environment=Test. If you do not specify
    -- a value, Environment=.
    --
    -- If you do not specify a value, the value is set to an empty string.
    --
    -- Generally allowed characters are: letters, numbers, and spaces
    -- representable in UTF-8, and the following special characters:
    --
    -- _ . : \/ = + \\ - \@
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources>
    -- in the /Amazon AppStream 2.0 Administration Guide/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The USB device filter strings that specify which USB devices a user can
    -- redirect to the fleet streaming session, when using the Windows native
    -- client. This is allowed but not required for Elastic fleets.
    usbDeviceFilterStrings :: Prelude.Maybe [Prelude.Text],
    -- | The VPC configuration for the fleet. This is required for Elastic
    -- fleets, but not required for other fleet types. Elastic fleets require
    -- that you specify at least two subnets in different availability zones.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | A unique name for the fleet.
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
    -- -   stream.standard.xlarge
    --
    -- -   stream.standard.2xlarge
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
    -- The following instance types are available for Elastic fleets:
    --
    -- -   stream.standard.small
    --
    -- -   stream.standard.medium
    --
    -- -   stream.standard.large
    --
    -- -   stream.standard.xlarge
    --
    -- -   stream.standard.2xlarge
    instanceType :: Prelude.Text
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
-- 'computeCapacity', 'createFleet_computeCapacity' - The desired capacity for the fleet. This is not allowed for Elastic
-- fleets. For Elastic fleets, specify MaxConcurrentSessions instead.
--
-- 'description', 'createFleet_description' - The description to display.
--
-- 'disconnectTimeoutInSeconds', 'createFleet_disconnectTimeoutInSeconds' - The amount of time that a streaming session remains active after users
-- disconnect. If users try to reconnect to the streaming session after a
-- disconnection or network interruption within this time interval, they
-- are connected to their previous session. Otherwise, they are connected
-- to a new session with a new streaming instance.
--
-- Specify a value between 60 and 360000.
--
-- 'displayName', 'createFleet_displayName' - The fleet name to display.
--
-- 'domainJoinInfo', 'createFleet_domainJoinInfo' - The name of the directory and organizational unit (OU) to use to join
-- the fleet to a Microsoft Active Directory domain. This is not allowed
-- for Elastic fleets.
--
-- 'enableDefaultInternetAccess', 'createFleet_enableDefaultInternetAccess' - Enables or disables default internet access for the fleet.
--
-- 'fleetType', 'createFleet_fleetType' - The fleet type.
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
-- 'iamRoleArn', 'createFleet_iamRoleArn' - The Amazon Resource Name (ARN) of the IAM role to apply to the fleet. To
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
-- 'idleDisconnectTimeoutInSeconds', 'createFleet_idleDisconnectTimeoutInSeconds' - The amount of time that users can be idle (inactive) before they are
-- disconnected from their streaming session and the
-- @DisconnectTimeoutInSeconds@ time interval begins. Users are notified
-- before they are disconnected due to inactivity. If they try to reconnect
-- to the streaming session before the time interval specified in
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
-- 'imageArn', 'createFleet_imageArn' - The ARN of the public, private, or shared image to use.
--
-- 'imageName', 'createFleet_imageName' - The name of the image used to create the fleet.
--
-- 'maxConcurrentSessions', 'createFleet_maxConcurrentSessions' - The maximum concurrent sessions of the Elastic fleet. This is required
-- for Elastic fleets, and not allowed for other fleet types.
--
-- 'maxUserDurationInSeconds', 'createFleet_maxUserDurationInSeconds' - The maximum amount of time that a streaming session can remain active,
-- in seconds. If users are still connected to a streaming instance five
-- minutes before this limit is reached, they are prompted to save any open
-- documents before being disconnected. After this time elapses, the
-- instance is terminated and replaced by a new instance.
--
-- Specify a value between 600 and 360000.
--
-- 'platform', 'createFleet_platform' - The fleet platform. WINDOWS_SERVER_2019 and AMAZON_LINUX2 are supported
-- for Elastic fleets.
--
-- 'sessionScriptS3Location', 'createFleet_sessionScriptS3Location' - The S3 location of the session scripts configuration zip file. This only
-- applies to Elastic fleets.
--
-- 'streamView', 'createFleet_streamView' - The AppStream 2.0 view that is displayed to your users when they stream
-- from the fleet. When @APP@ is specified, only the windows of
-- applications opened by users display. When @DESKTOP@ is specified, the
-- standard desktop that is provided by the operating system displays.
--
-- The default value is @APP@.
--
-- 'tags', 'createFleet_tags' - The tags to associate with the fleet. A tag is a key-value pair, and the
-- value is optional. For example, Environment=Test. If you do not specify
-- a value, Environment=.
--
-- If you do not specify a value, the value is set to an empty string.
--
-- Generally allowed characters are: letters, numbers, and spaces
-- representable in UTF-8, and the following special characters:
--
-- _ . : \/ = + \\ - \@
--
-- For more information, see
-- <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources>
-- in the /Amazon AppStream 2.0 Administration Guide/.
--
-- 'usbDeviceFilterStrings', 'createFleet_usbDeviceFilterStrings' - The USB device filter strings that specify which USB devices a user can
-- redirect to the fleet streaming session, when using the Windows native
-- client. This is allowed but not required for Elastic fleets.
--
-- 'vpcConfig', 'createFleet_vpcConfig' - The VPC configuration for the fleet. This is required for Elastic
-- fleets, but not required for other fleet types. Elastic fleets require
-- that you specify at least two subnets in different availability zones.
--
-- 'name', 'createFleet_name' - A unique name for the fleet.
--
-- 'instanceType', 'createFleet_instanceType' - The instance type to use when launching fleet instances. The following
-- instance types are available:
--
-- -   stream.standard.small
--
-- -   stream.standard.medium
--
-- -   stream.standard.large
--
-- -   stream.standard.xlarge
--
-- -   stream.standard.2xlarge
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
-- The following instance types are available for Elastic fleets:
--
-- -   stream.standard.small
--
-- -   stream.standard.medium
--
-- -   stream.standard.large
--
-- -   stream.standard.xlarge
--
-- -   stream.standard.2xlarge
newCreateFleet ::
  -- | 'name'
  Prelude.Text ->
  -- | 'instanceType'
  Prelude.Text ->
  CreateFleet
newCreateFleet pName_ pInstanceType_ =
  CreateFleet'
    { computeCapacity = Prelude.Nothing,
      description = Prelude.Nothing,
      disconnectTimeoutInSeconds = Prelude.Nothing,
      displayName = Prelude.Nothing,
      domainJoinInfo = Prelude.Nothing,
      enableDefaultInternetAccess = Prelude.Nothing,
      fleetType = Prelude.Nothing,
      iamRoleArn = Prelude.Nothing,
      idleDisconnectTimeoutInSeconds = Prelude.Nothing,
      imageArn = Prelude.Nothing,
      imageName = Prelude.Nothing,
      maxConcurrentSessions = Prelude.Nothing,
      maxUserDurationInSeconds = Prelude.Nothing,
      platform = Prelude.Nothing,
      sessionScriptS3Location = Prelude.Nothing,
      streamView = Prelude.Nothing,
      tags = Prelude.Nothing,
      usbDeviceFilterStrings = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      name = pName_,
      instanceType = pInstanceType_
    }

-- | The desired capacity for the fleet. This is not allowed for Elastic
-- fleets. For Elastic fleets, specify MaxConcurrentSessions instead.
createFleet_computeCapacity :: Lens.Lens' CreateFleet (Prelude.Maybe ComputeCapacity)
createFleet_computeCapacity = Lens.lens (\CreateFleet' {computeCapacity} -> computeCapacity) (\s@CreateFleet' {} a -> s {computeCapacity = a} :: CreateFleet)

-- | The description to display.
createFleet_description :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_description = Lens.lens (\CreateFleet' {description} -> description) (\s@CreateFleet' {} a -> s {description = a} :: CreateFleet)

-- | The amount of time that a streaming session remains active after users
-- disconnect. If users try to reconnect to the streaming session after a
-- disconnection or network interruption within this time interval, they
-- are connected to their previous session. Otherwise, they are connected
-- to a new session with a new streaming instance.
--
-- Specify a value between 60 and 360000.
createFleet_disconnectTimeoutInSeconds :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Int)
createFleet_disconnectTimeoutInSeconds = Lens.lens (\CreateFleet' {disconnectTimeoutInSeconds} -> disconnectTimeoutInSeconds) (\s@CreateFleet' {} a -> s {disconnectTimeoutInSeconds = a} :: CreateFleet)

-- | The fleet name to display.
createFleet_displayName :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_displayName = Lens.lens (\CreateFleet' {displayName} -> displayName) (\s@CreateFleet' {} a -> s {displayName = a} :: CreateFleet)

-- | The name of the directory and organizational unit (OU) to use to join
-- the fleet to a Microsoft Active Directory domain. This is not allowed
-- for Elastic fleets.
createFleet_domainJoinInfo :: Lens.Lens' CreateFleet (Prelude.Maybe DomainJoinInfo)
createFleet_domainJoinInfo = Lens.lens (\CreateFleet' {domainJoinInfo} -> domainJoinInfo) (\s@CreateFleet' {} a -> s {domainJoinInfo = a} :: CreateFleet)

-- | Enables or disables default internet access for the fleet.
createFleet_enableDefaultInternetAccess :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Bool)
createFleet_enableDefaultInternetAccess = Lens.lens (\CreateFleet' {enableDefaultInternetAccess} -> enableDefaultInternetAccess) (\s@CreateFleet' {} a -> s {enableDefaultInternetAccess = a} :: CreateFleet)

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
createFleet_fleetType :: Lens.Lens' CreateFleet (Prelude.Maybe FleetType)
createFleet_fleetType = Lens.lens (\CreateFleet' {fleetType} -> fleetType) (\s@CreateFleet' {} a -> s {fleetType = a} :: CreateFleet)

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
createFleet_iamRoleArn :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_iamRoleArn = Lens.lens (\CreateFleet' {iamRoleArn} -> iamRoleArn) (\s@CreateFleet' {} a -> s {iamRoleArn = a} :: CreateFleet)

-- | The amount of time that users can be idle (inactive) before they are
-- disconnected from their streaming session and the
-- @DisconnectTimeoutInSeconds@ time interval begins. Users are notified
-- before they are disconnected due to inactivity. If they try to reconnect
-- to the streaming session before the time interval specified in
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
createFleet_idleDisconnectTimeoutInSeconds :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Int)
createFleet_idleDisconnectTimeoutInSeconds = Lens.lens (\CreateFleet' {idleDisconnectTimeoutInSeconds} -> idleDisconnectTimeoutInSeconds) (\s@CreateFleet' {} a -> s {idleDisconnectTimeoutInSeconds = a} :: CreateFleet)

-- | The ARN of the public, private, or shared image to use.
createFleet_imageArn :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_imageArn = Lens.lens (\CreateFleet' {imageArn} -> imageArn) (\s@CreateFleet' {} a -> s {imageArn = a} :: CreateFleet)

-- | The name of the image used to create the fleet.
createFleet_imageName :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_imageName = Lens.lens (\CreateFleet' {imageName} -> imageName) (\s@CreateFleet' {} a -> s {imageName = a} :: CreateFleet)

-- | The maximum concurrent sessions of the Elastic fleet. This is required
-- for Elastic fleets, and not allowed for other fleet types.
createFleet_maxConcurrentSessions :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Int)
createFleet_maxConcurrentSessions = Lens.lens (\CreateFleet' {maxConcurrentSessions} -> maxConcurrentSessions) (\s@CreateFleet' {} a -> s {maxConcurrentSessions = a} :: CreateFleet)

-- | The maximum amount of time that a streaming session can remain active,
-- in seconds. If users are still connected to a streaming instance five
-- minutes before this limit is reached, they are prompted to save any open
-- documents before being disconnected. After this time elapses, the
-- instance is terminated and replaced by a new instance.
--
-- Specify a value between 600 and 360000.
createFleet_maxUserDurationInSeconds :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Int)
createFleet_maxUserDurationInSeconds = Lens.lens (\CreateFleet' {maxUserDurationInSeconds} -> maxUserDurationInSeconds) (\s@CreateFleet' {} a -> s {maxUserDurationInSeconds = a} :: CreateFleet)

-- | The fleet platform. WINDOWS_SERVER_2019 and AMAZON_LINUX2 are supported
-- for Elastic fleets.
createFleet_platform :: Lens.Lens' CreateFleet (Prelude.Maybe PlatformType)
createFleet_platform = Lens.lens (\CreateFleet' {platform} -> platform) (\s@CreateFleet' {} a -> s {platform = a} :: CreateFleet)

-- | The S3 location of the session scripts configuration zip file. This only
-- applies to Elastic fleets.
createFleet_sessionScriptS3Location :: Lens.Lens' CreateFleet (Prelude.Maybe S3Location)
createFleet_sessionScriptS3Location = Lens.lens (\CreateFleet' {sessionScriptS3Location} -> sessionScriptS3Location) (\s@CreateFleet' {} a -> s {sessionScriptS3Location = a} :: CreateFleet)

-- | The AppStream 2.0 view that is displayed to your users when they stream
-- from the fleet. When @APP@ is specified, only the windows of
-- applications opened by users display. When @DESKTOP@ is specified, the
-- standard desktop that is provided by the operating system displays.
--
-- The default value is @APP@.
createFleet_streamView :: Lens.Lens' CreateFleet (Prelude.Maybe StreamView)
createFleet_streamView = Lens.lens (\CreateFleet' {streamView} -> streamView) (\s@CreateFleet' {} a -> s {streamView = a} :: CreateFleet)

-- | The tags to associate with the fleet. A tag is a key-value pair, and the
-- value is optional. For example, Environment=Test. If you do not specify
-- a value, Environment=.
--
-- If you do not specify a value, the value is set to an empty string.
--
-- Generally allowed characters are: letters, numbers, and spaces
-- representable in UTF-8, and the following special characters:
--
-- _ . : \/ = + \\ - \@
--
-- For more information, see
-- <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources>
-- in the /Amazon AppStream 2.0 Administration Guide/.
createFleet_tags :: Lens.Lens' CreateFleet (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createFleet_tags = Lens.lens (\CreateFleet' {tags} -> tags) (\s@CreateFleet' {} a -> s {tags = a} :: CreateFleet) Prelude.. Lens.mapping Lens.coerced

-- | The USB device filter strings that specify which USB devices a user can
-- redirect to the fleet streaming session, when using the Windows native
-- client. This is allowed but not required for Elastic fleets.
createFleet_usbDeviceFilterStrings :: Lens.Lens' CreateFleet (Prelude.Maybe [Prelude.Text])
createFleet_usbDeviceFilterStrings = Lens.lens (\CreateFleet' {usbDeviceFilterStrings} -> usbDeviceFilterStrings) (\s@CreateFleet' {} a -> s {usbDeviceFilterStrings = a} :: CreateFleet) Prelude.. Lens.mapping Lens.coerced

-- | The VPC configuration for the fleet. This is required for Elastic
-- fleets, but not required for other fleet types. Elastic fleets require
-- that you specify at least two subnets in different availability zones.
createFleet_vpcConfig :: Lens.Lens' CreateFleet (Prelude.Maybe VpcConfig)
createFleet_vpcConfig = Lens.lens (\CreateFleet' {vpcConfig} -> vpcConfig) (\s@CreateFleet' {} a -> s {vpcConfig = a} :: CreateFleet)

-- | A unique name for the fleet.
createFleet_name :: Lens.Lens' CreateFleet Prelude.Text
createFleet_name = Lens.lens (\CreateFleet' {name} -> name) (\s@CreateFleet' {} a -> s {name = a} :: CreateFleet)

-- | The instance type to use when launching fleet instances. The following
-- instance types are available:
--
-- -   stream.standard.small
--
-- -   stream.standard.medium
--
-- -   stream.standard.large
--
-- -   stream.standard.xlarge
--
-- -   stream.standard.2xlarge
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
-- The following instance types are available for Elastic fleets:
--
-- -   stream.standard.small
--
-- -   stream.standard.medium
--
-- -   stream.standard.large
--
-- -   stream.standard.xlarge
--
-- -   stream.standard.2xlarge
createFleet_instanceType :: Lens.Lens' CreateFleet Prelude.Text
createFleet_instanceType = Lens.lens (\CreateFleet' {instanceType} -> instanceType) (\s@CreateFleet' {} a -> s {instanceType = a} :: CreateFleet)

instance Core.AWSRequest CreateFleet where
  type AWSResponse CreateFleet = CreateFleetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFleetResponse'
            Prelude.<$> (x Data..?> "Fleet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFleet where
  hashWithSalt _salt CreateFleet' {..} =
    _salt
      `Prelude.hashWithSalt` computeCapacity
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` disconnectTimeoutInSeconds
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` domainJoinInfo
      `Prelude.hashWithSalt` enableDefaultInternetAccess
      `Prelude.hashWithSalt` fleetType
      `Prelude.hashWithSalt` iamRoleArn
      `Prelude.hashWithSalt` idleDisconnectTimeoutInSeconds
      `Prelude.hashWithSalt` imageArn
      `Prelude.hashWithSalt` imageName
      `Prelude.hashWithSalt` maxConcurrentSessions
      `Prelude.hashWithSalt` maxUserDurationInSeconds
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` sessionScriptS3Location
      `Prelude.hashWithSalt` streamView
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` usbDeviceFilterStrings
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` instanceType

instance Prelude.NFData CreateFleet where
  rnf CreateFleet' {..} =
    Prelude.rnf computeCapacity
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf disconnectTimeoutInSeconds
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf domainJoinInfo
      `Prelude.seq` Prelude.rnf enableDefaultInternetAccess
      `Prelude.seq` Prelude.rnf fleetType
      `Prelude.seq` Prelude.rnf iamRoleArn
      `Prelude.seq` Prelude.rnf idleDisconnectTimeoutInSeconds
      `Prelude.seq` Prelude.rnf imageArn
      `Prelude.seq` Prelude.rnf imageName
      `Prelude.seq` Prelude.rnf maxConcurrentSessions
      `Prelude.seq` Prelude.rnf maxUserDurationInSeconds
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf sessionScriptS3Location
      `Prelude.seq` Prelude.rnf streamView
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf usbDeviceFilterStrings
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf instanceType

instance Data.ToHeaders CreateFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.CreateFleet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFleet where
  toJSON CreateFleet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ComputeCapacity" Data..=)
              Prelude.<$> computeCapacity,
            ("Description" Data..=) Prelude.<$> description,
            ("DisconnectTimeoutInSeconds" Data..=)
              Prelude.<$> disconnectTimeoutInSeconds,
            ("DisplayName" Data..=) Prelude.<$> displayName,
            ("DomainJoinInfo" Data..=)
              Prelude.<$> domainJoinInfo,
            ("EnableDefaultInternetAccess" Data..=)
              Prelude.<$> enableDefaultInternetAccess,
            ("FleetType" Data..=) Prelude.<$> fleetType,
            ("IamRoleArn" Data..=) Prelude.<$> iamRoleArn,
            ("IdleDisconnectTimeoutInSeconds" Data..=)
              Prelude.<$> idleDisconnectTimeoutInSeconds,
            ("ImageArn" Data..=) Prelude.<$> imageArn,
            ("ImageName" Data..=) Prelude.<$> imageName,
            ("MaxConcurrentSessions" Data..=)
              Prelude.<$> maxConcurrentSessions,
            ("MaxUserDurationInSeconds" Data..=)
              Prelude.<$> maxUserDurationInSeconds,
            ("Platform" Data..=) Prelude.<$> platform,
            ("SessionScriptS3Location" Data..=)
              Prelude.<$> sessionScriptS3Location,
            ("StreamView" Data..=) Prelude.<$> streamView,
            ("Tags" Data..=) Prelude.<$> tags,
            ("UsbDeviceFilterStrings" Data..=)
              Prelude.<$> usbDeviceFilterStrings,
            ("VpcConfig" Data..=) Prelude.<$> vpcConfig,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("InstanceType" Data..= instanceType)
          ]
      )

instance Data.ToPath CreateFleet where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFleetResponse' smart constructor.
data CreateFleetResponse = CreateFleetResponse'
  { -- | Information about the fleet.
    fleet :: Prelude.Maybe Fleet,
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
-- 'fleet', 'createFleetResponse_fleet' - Information about the fleet.
--
-- 'httpStatus', 'createFleetResponse_httpStatus' - The response's http status code.
newCreateFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFleetResponse
newCreateFleetResponse pHttpStatus_ =
  CreateFleetResponse'
    { fleet = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the fleet.
createFleetResponse_fleet :: Lens.Lens' CreateFleetResponse (Prelude.Maybe Fleet)
createFleetResponse_fleet = Lens.lens (\CreateFleetResponse' {fleet} -> fleet) (\s@CreateFleetResponse' {} a -> s {fleet = a} :: CreateFleetResponse)

-- | The response's http status code.
createFleetResponse_httpStatus :: Lens.Lens' CreateFleetResponse Prelude.Int
createFleetResponse_httpStatus = Lens.lens (\CreateFleetResponse' {httpStatus} -> httpStatus) (\s@CreateFleetResponse' {} a -> s {httpStatus = a} :: CreateFleetResponse)

instance Prelude.NFData CreateFleetResponse where
  rnf CreateFleetResponse' {..} =
    Prelude.rnf fleet
      `Prelude.seq` Prelude.rnf httpStatus
