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
-- Module      : Network.AWS.AppStream.Types.ImageBuilder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ImageBuilder where

import Network.AWS.AppStream.Types.AccessEndpoint
import Network.AWS.AppStream.Types.DomainJoinInfo
import Network.AWS.AppStream.Types.ImageBuilderState
import Network.AWS.AppStream.Types.ImageBuilderStateChangeReason
import Network.AWS.AppStream.Types.NetworkAccessConfiguration
import Network.AWS.AppStream.Types.PlatformType
import Network.AWS.AppStream.Types.ResourceError
import Network.AWS.AppStream.Types.VpcConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a virtual machine that is used to create an image.
--
-- /See:/ 'newImageBuilder' smart constructor.
data ImageBuilder = ImageBuilder'
  { -- | The operating system platform of the image builder.
    platform :: Core.Maybe PlatformType,
    -- | The VPC configuration of the image builder.
    vpcConfig :: Core.Maybe VpcConfig,
    -- | The ARN of the IAM role that is applied to the image builder. To assume
    -- a role, the image builder calls the AWS Security Token Service (STS)
    -- @AssumeRole@ API operation and passes the ARN of the role to use. The
    -- operation creates a new session with temporary credentials. AppStream
    -- 2.0 retrieves the temporary credentials and creates the
    -- __appstream_machine_role__ credential profile on the instance.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances>
    -- in the /Amazon AppStream 2.0 Administration Guide/.
    iamRoleArn :: Core.Maybe Core.Text,
    -- | The list of virtual private cloud (VPC) interface endpoint objects.
    -- Administrators can connect to the image builder only through the
    -- specified endpoints.
    accessEndpoints :: Core.Maybe (Core.NonEmpty AccessEndpoint),
    -- | The name of the directory and organizational unit (OU) to use to join
    -- the image builder to a Microsoft Active Directory domain.
    domainJoinInfo :: Core.Maybe DomainJoinInfo,
    -- | The instance type for the image builder. The following instance types
    -- are available:
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
    -- | The ARN for the image builder.
    arn :: Core.Maybe Core.Text,
    -- | The reason why the last state change occurred.
    stateChangeReason :: Core.Maybe ImageBuilderStateChangeReason,
    -- | The time stamp when the image builder was created.
    createdTime :: Core.Maybe Core.POSIX,
    networkAccessConfiguration :: Core.Maybe NetworkAccessConfiguration,
    -- | The state of the image builder.
    state :: Core.Maybe ImageBuilderState,
    -- | The version of the AppStream 2.0 agent that is currently being used by
    -- the image builder.
    appstreamAgentVersion :: Core.Maybe Core.Text,
    -- | The description to display.
    description :: Core.Maybe Core.Text,
    -- | The image builder name to display.
    displayName :: Core.Maybe Core.Text,
    -- | Enables or disables default internet access for the image builder.
    enableDefaultInternetAccess :: Core.Maybe Core.Bool,
    -- | The image builder errors.
    imageBuilderErrors :: Core.Maybe [ResourceError],
    -- | The ARN of the image from which this builder was created.
    imageArn :: Core.Maybe Core.Text,
    -- | The name of the image builder.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImageBuilder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platform', 'imageBuilder_platform' - The operating system platform of the image builder.
--
-- 'vpcConfig', 'imageBuilder_vpcConfig' - The VPC configuration of the image builder.
--
-- 'iamRoleArn', 'imageBuilder_iamRoleArn' - The ARN of the IAM role that is applied to the image builder. To assume
-- a role, the image builder calls the AWS Security Token Service (STS)
-- @AssumeRole@ API operation and passes the ARN of the role to use. The
-- operation creates a new session with temporary credentials. AppStream
-- 2.0 retrieves the temporary credentials and creates the
-- __appstream_machine_role__ credential profile on the instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances>
-- in the /Amazon AppStream 2.0 Administration Guide/.
--
-- 'accessEndpoints', 'imageBuilder_accessEndpoints' - The list of virtual private cloud (VPC) interface endpoint objects.
-- Administrators can connect to the image builder only through the
-- specified endpoints.
--
-- 'domainJoinInfo', 'imageBuilder_domainJoinInfo' - The name of the directory and organizational unit (OU) to use to join
-- the image builder to a Microsoft Active Directory domain.
--
-- 'instanceType', 'imageBuilder_instanceType' - The instance type for the image builder. The following instance types
-- are available:
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
-- 'arn', 'imageBuilder_arn' - The ARN for the image builder.
--
-- 'stateChangeReason', 'imageBuilder_stateChangeReason' - The reason why the last state change occurred.
--
-- 'createdTime', 'imageBuilder_createdTime' - The time stamp when the image builder was created.
--
-- 'networkAccessConfiguration', 'imageBuilder_networkAccessConfiguration' - Undocumented member.
--
-- 'state', 'imageBuilder_state' - The state of the image builder.
--
-- 'appstreamAgentVersion', 'imageBuilder_appstreamAgentVersion' - The version of the AppStream 2.0 agent that is currently being used by
-- the image builder.
--
-- 'description', 'imageBuilder_description' - The description to display.
--
-- 'displayName', 'imageBuilder_displayName' - The image builder name to display.
--
-- 'enableDefaultInternetAccess', 'imageBuilder_enableDefaultInternetAccess' - Enables or disables default internet access for the image builder.
--
-- 'imageBuilderErrors', 'imageBuilder_imageBuilderErrors' - The image builder errors.
--
-- 'imageArn', 'imageBuilder_imageArn' - The ARN of the image from which this builder was created.
--
-- 'name', 'imageBuilder_name' - The name of the image builder.
newImageBuilder ::
  -- | 'name'
  Core.Text ->
  ImageBuilder
newImageBuilder pName_ =
  ImageBuilder'
    { platform = Core.Nothing,
      vpcConfig = Core.Nothing,
      iamRoleArn = Core.Nothing,
      accessEndpoints = Core.Nothing,
      domainJoinInfo = Core.Nothing,
      instanceType = Core.Nothing,
      arn = Core.Nothing,
      stateChangeReason = Core.Nothing,
      createdTime = Core.Nothing,
      networkAccessConfiguration = Core.Nothing,
      state = Core.Nothing,
      appstreamAgentVersion = Core.Nothing,
      description = Core.Nothing,
      displayName = Core.Nothing,
      enableDefaultInternetAccess = Core.Nothing,
      imageBuilderErrors = Core.Nothing,
      imageArn = Core.Nothing,
      name = pName_
    }

-- | The operating system platform of the image builder.
imageBuilder_platform :: Lens.Lens' ImageBuilder (Core.Maybe PlatformType)
imageBuilder_platform = Lens.lens (\ImageBuilder' {platform} -> platform) (\s@ImageBuilder' {} a -> s {platform = a} :: ImageBuilder)

-- | The VPC configuration of the image builder.
imageBuilder_vpcConfig :: Lens.Lens' ImageBuilder (Core.Maybe VpcConfig)
imageBuilder_vpcConfig = Lens.lens (\ImageBuilder' {vpcConfig} -> vpcConfig) (\s@ImageBuilder' {} a -> s {vpcConfig = a} :: ImageBuilder)

-- | The ARN of the IAM role that is applied to the image builder. To assume
-- a role, the image builder calls the AWS Security Token Service (STS)
-- @AssumeRole@ API operation and passes the ARN of the role to use. The
-- operation creates a new session with temporary credentials. AppStream
-- 2.0 retrieves the temporary credentials and creates the
-- __appstream_machine_role__ credential profile on the instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances>
-- in the /Amazon AppStream 2.0 Administration Guide/.
imageBuilder_iamRoleArn :: Lens.Lens' ImageBuilder (Core.Maybe Core.Text)
imageBuilder_iamRoleArn = Lens.lens (\ImageBuilder' {iamRoleArn} -> iamRoleArn) (\s@ImageBuilder' {} a -> s {iamRoleArn = a} :: ImageBuilder)

-- | The list of virtual private cloud (VPC) interface endpoint objects.
-- Administrators can connect to the image builder only through the
-- specified endpoints.
imageBuilder_accessEndpoints :: Lens.Lens' ImageBuilder (Core.Maybe (Core.NonEmpty AccessEndpoint))
imageBuilder_accessEndpoints = Lens.lens (\ImageBuilder' {accessEndpoints} -> accessEndpoints) (\s@ImageBuilder' {} a -> s {accessEndpoints = a} :: ImageBuilder) Core.. Lens.mapping Lens._Coerce

-- | The name of the directory and organizational unit (OU) to use to join
-- the image builder to a Microsoft Active Directory domain.
imageBuilder_domainJoinInfo :: Lens.Lens' ImageBuilder (Core.Maybe DomainJoinInfo)
imageBuilder_domainJoinInfo = Lens.lens (\ImageBuilder' {domainJoinInfo} -> domainJoinInfo) (\s@ImageBuilder' {} a -> s {domainJoinInfo = a} :: ImageBuilder)

-- | The instance type for the image builder. The following instance types
-- are available:
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
imageBuilder_instanceType :: Lens.Lens' ImageBuilder (Core.Maybe Core.Text)
imageBuilder_instanceType = Lens.lens (\ImageBuilder' {instanceType} -> instanceType) (\s@ImageBuilder' {} a -> s {instanceType = a} :: ImageBuilder)

-- | The ARN for the image builder.
imageBuilder_arn :: Lens.Lens' ImageBuilder (Core.Maybe Core.Text)
imageBuilder_arn = Lens.lens (\ImageBuilder' {arn} -> arn) (\s@ImageBuilder' {} a -> s {arn = a} :: ImageBuilder)

-- | The reason why the last state change occurred.
imageBuilder_stateChangeReason :: Lens.Lens' ImageBuilder (Core.Maybe ImageBuilderStateChangeReason)
imageBuilder_stateChangeReason = Lens.lens (\ImageBuilder' {stateChangeReason} -> stateChangeReason) (\s@ImageBuilder' {} a -> s {stateChangeReason = a} :: ImageBuilder)

-- | The time stamp when the image builder was created.
imageBuilder_createdTime :: Lens.Lens' ImageBuilder (Core.Maybe Core.UTCTime)
imageBuilder_createdTime = Lens.lens (\ImageBuilder' {createdTime} -> createdTime) (\s@ImageBuilder' {} a -> s {createdTime = a} :: ImageBuilder) Core.. Lens.mapping Core._Time

-- | Undocumented member.
imageBuilder_networkAccessConfiguration :: Lens.Lens' ImageBuilder (Core.Maybe NetworkAccessConfiguration)
imageBuilder_networkAccessConfiguration = Lens.lens (\ImageBuilder' {networkAccessConfiguration} -> networkAccessConfiguration) (\s@ImageBuilder' {} a -> s {networkAccessConfiguration = a} :: ImageBuilder)

-- | The state of the image builder.
imageBuilder_state :: Lens.Lens' ImageBuilder (Core.Maybe ImageBuilderState)
imageBuilder_state = Lens.lens (\ImageBuilder' {state} -> state) (\s@ImageBuilder' {} a -> s {state = a} :: ImageBuilder)

-- | The version of the AppStream 2.0 agent that is currently being used by
-- the image builder.
imageBuilder_appstreamAgentVersion :: Lens.Lens' ImageBuilder (Core.Maybe Core.Text)
imageBuilder_appstreamAgentVersion = Lens.lens (\ImageBuilder' {appstreamAgentVersion} -> appstreamAgentVersion) (\s@ImageBuilder' {} a -> s {appstreamAgentVersion = a} :: ImageBuilder)

-- | The description to display.
imageBuilder_description :: Lens.Lens' ImageBuilder (Core.Maybe Core.Text)
imageBuilder_description = Lens.lens (\ImageBuilder' {description} -> description) (\s@ImageBuilder' {} a -> s {description = a} :: ImageBuilder)

-- | The image builder name to display.
imageBuilder_displayName :: Lens.Lens' ImageBuilder (Core.Maybe Core.Text)
imageBuilder_displayName = Lens.lens (\ImageBuilder' {displayName} -> displayName) (\s@ImageBuilder' {} a -> s {displayName = a} :: ImageBuilder)

-- | Enables or disables default internet access for the image builder.
imageBuilder_enableDefaultInternetAccess :: Lens.Lens' ImageBuilder (Core.Maybe Core.Bool)
imageBuilder_enableDefaultInternetAccess = Lens.lens (\ImageBuilder' {enableDefaultInternetAccess} -> enableDefaultInternetAccess) (\s@ImageBuilder' {} a -> s {enableDefaultInternetAccess = a} :: ImageBuilder)

-- | The image builder errors.
imageBuilder_imageBuilderErrors :: Lens.Lens' ImageBuilder (Core.Maybe [ResourceError])
imageBuilder_imageBuilderErrors = Lens.lens (\ImageBuilder' {imageBuilderErrors} -> imageBuilderErrors) (\s@ImageBuilder' {} a -> s {imageBuilderErrors = a} :: ImageBuilder) Core.. Lens.mapping Lens._Coerce

-- | The ARN of the image from which this builder was created.
imageBuilder_imageArn :: Lens.Lens' ImageBuilder (Core.Maybe Core.Text)
imageBuilder_imageArn = Lens.lens (\ImageBuilder' {imageArn} -> imageArn) (\s@ImageBuilder' {} a -> s {imageArn = a} :: ImageBuilder)

-- | The name of the image builder.
imageBuilder_name :: Lens.Lens' ImageBuilder Core.Text
imageBuilder_name = Lens.lens (\ImageBuilder' {name} -> name) (\s@ImageBuilder' {} a -> s {name = a} :: ImageBuilder)

instance Core.FromJSON ImageBuilder where
  parseJSON =
    Core.withObject
      "ImageBuilder"
      ( \x ->
          ImageBuilder'
            Core.<$> (x Core..:? "Platform")
            Core.<*> (x Core..:? "VpcConfig")
            Core.<*> (x Core..:? "IamRoleArn")
            Core.<*> (x Core..:? "AccessEndpoints")
            Core.<*> (x Core..:? "DomainJoinInfo")
            Core.<*> (x Core..:? "InstanceType")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "StateChangeReason")
            Core.<*> (x Core..:? "CreatedTime")
            Core.<*> (x Core..:? "NetworkAccessConfiguration")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "AppstreamAgentVersion")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "DisplayName")
            Core.<*> (x Core..:? "EnableDefaultInternetAccess")
            Core.<*> ( x Core..:? "ImageBuilderErrors"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "ImageArn")
            Core.<*> (x Core..: "Name")
      )

instance Core.Hashable ImageBuilder

instance Core.NFData ImageBuilder
