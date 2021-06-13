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
import qualified Network.AWS.Prelude as Prelude

-- | Describes a virtual machine that is used to create an image.
--
-- /See:/ 'newImageBuilder' smart constructor.
data ImageBuilder = ImageBuilder'
  { -- | The operating system platform of the image builder.
    platform :: Prelude.Maybe PlatformType,
    -- | The VPC configuration of the image builder.
    vpcConfig :: Prelude.Maybe VpcConfig,
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
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The list of virtual private cloud (VPC) interface endpoint objects.
    -- Administrators can connect to the image builder only through the
    -- specified endpoints.
    accessEndpoints :: Prelude.Maybe (Prelude.NonEmpty AccessEndpoint),
    -- | The name of the directory and organizational unit (OU) to use to join
    -- the image builder to a Microsoft Active Directory domain.
    domainJoinInfo :: Prelude.Maybe DomainJoinInfo,
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
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the image builder.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The reason why the last state change occurred.
    stateChangeReason :: Prelude.Maybe ImageBuilderStateChangeReason,
    -- | The time stamp when the image builder was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    networkAccessConfiguration :: Prelude.Maybe NetworkAccessConfiguration,
    -- | The state of the image builder.
    state :: Prelude.Maybe ImageBuilderState,
    -- | The version of the AppStream 2.0 agent that is currently being used by
    -- the image builder.
    appstreamAgentVersion :: Prelude.Maybe Prelude.Text,
    -- | The description to display.
    description :: Prelude.Maybe Prelude.Text,
    -- | The image builder name to display.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | Enables or disables default internet access for the image builder.
    enableDefaultInternetAccess :: Prelude.Maybe Prelude.Bool,
    -- | The image builder errors.
    imageBuilderErrors :: Prelude.Maybe [ResourceError],
    -- | The ARN of the image from which this builder was created.
    imageArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the image builder.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ImageBuilder
newImageBuilder pName_ =
  ImageBuilder'
    { platform = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      iamRoleArn = Prelude.Nothing,
      accessEndpoints = Prelude.Nothing,
      domainJoinInfo = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      arn = Prelude.Nothing,
      stateChangeReason = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      networkAccessConfiguration = Prelude.Nothing,
      state = Prelude.Nothing,
      appstreamAgentVersion = Prelude.Nothing,
      description = Prelude.Nothing,
      displayName = Prelude.Nothing,
      enableDefaultInternetAccess = Prelude.Nothing,
      imageBuilderErrors = Prelude.Nothing,
      imageArn = Prelude.Nothing,
      name = pName_
    }

-- | The operating system platform of the image builder.
imageBuilder_platform :: Lens.Lens' ImageBuilder (Prelude.Maybe PlatformType)
imageBuilder_platform = Lens.lens (\ImageBuilder' {platform} -> platform) (\s@ImageBuilder' {} a -> s {platform = a} :: ImageBuilder)

-- | The VPC configuration of the image builder.
imageBuilder_vpcConfig :: Lens.Lens' ImageBuilder (Prelude.Maybe VpcConfig)
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
imageBuilder_iamRoleArn :: Lens.Lens' ImageBuilder (Prelude.Maybe Prelude.Text)
imageBuilder_iamRoleArn = Lens.lens (\ImageBuilder' {iamRoleArn} -> iamRoleArn) (\s@ImageBuilder' {} a -> s {iamRoleArn = a} :: ImageBuilder)

-- | The list of virtual private cloud (VPC) interface endpoint objects.
-- Administrators can connect to the image builder only through the
-- specified endpoints.
imageBuilder_accessEndpoints :: Lens.Lens' ImageBuilder (Prelude.Maybe (Prelude.NonEmpty AccessEndpoint))
imageBuilder_accessEndpoints = Lens.lens (\ImageBuilder' {accessEndpoints} -> accessEndpoints) (\s@ImageBuilder' {} a -> s {accessEndpoints = a} :: ImageBuilder) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the directory and organizational unit (OU) to use to join
-- the image builder to a Microsoft Active Directory domain.
imageBuilder_domainJoinInfo :: Lens.Lens' ImageBuilder (Prelude.Maybe DomainJoinInfo)
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
imageBuilder_instanceType :: Lens.Lens' ImageBuilder (Prelude.Maybe Prelude.Text)
imageBuilder_instanceType = Lens.lens (\ImageBuilder' {instanceType} -> instanceType) (\s@ImageBuilder' {} a -> s {instanceType = a} :: ImageBuilder)

-- | The ARN for the image builder.
imageBuilder_arn :: Lens.Lens' ImageBuilder (Prelude.Maybe Prelude.Text)
imageBuilder_arn = Lens.lens (\ImageBuilder' {arn} -> arn) (\s@ImageBuilder' {} a -> s {arn = a} :: ImageBuilder)

-- | The reason why the last state change occurred.
imageBuilder_stateChangeReason :: Lens.Lens' ImageBuilder (Prelude.Maybe ImageBuilderStateChangeReason)
imageBuilder_stateChangeReason = Lens.lens (\ImageBuilder' {stateChangeReason} -> stateChangeReason) (\s@ImageBuilder' {} a -> s {stateChangeReason = a} :: ImageBuilder)

-- | The time stamp when the image builder was created.
imageBuilder_createdTime :: Lens.Lens' ImageBuilder (Prelude.Maybe Prelude.UTCTime)
imageBuilder_createdTime = Lens.lens (\ImageBuilder' {createdTime} -> createdTime) (\s@ImageBuilder' {} a -> s {createdTime = a} :: ImageBuilder) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
imageBuilder_networkAccessConfiguration :: Lens.Lens' ImageBuilder (Prelude.Maybe NetworkAccessConfiguration)
imageBuilder_networkAccessConfiguration = Lens.lens (\ImageBuilder' {networkAccessConfiguration} -> networkAccessConfiguration) (\s@ImageBuilder' {} a -> s {networkAccessConfiguration = a} :: ImageBuilder)

-- | The state of the image builder.
imageBuilder_state :: Lens.Lens' ImageBuilder (Prelude.Maybe ImageBuilderState)
imageBuilder_state = Lens.lens (\ImageBuilder' {state} -> state) (\s@ImageBuilder' {} a -> s {state = a} :: ImageBuilder)

-- | The version of the AppStream 2.0 agent that is currently being used by
-- the image builder.
imageBuilder_appstreamAgentVersion :: Lens.Lens' ImageBuilder (Prelude.Maybe Prelude.Text)
imageBuilder_appstreamAgentVersion = Lens.lens (\ImageBuilder' {appstreamAgentVersion} -> appstreamAgentVersion) (\s@ImageBuilder' {} a -> s {appstreamAgentVersion = a} :: ImageBuilder)

-- | The description to display.
imageBuilder_description :: Lens.Lens' ImageBuilder (Prelude.Maybe Prelude.Text)
imageBuilder_description = Lens.lens (\ImageBuilder' {description} -> description) (\s@ImageBuilder' {} a -> s {description = a} :: ImageBuilder)

-- | The image builder name to display.
imageBuilder_displayName :: Lens.Lens' ImageBuilder (Prelude.Maybe Prelude.Text)
imageBuilder_displayName = Lens.lens (\ImageBuilder' {displayName} -> displayName) (\s@ImageBuilder' {} a -> s {displayName = a} :: ImageBuilder)

-- | Enables or disables default internet access for the image builder.
imageBuilder_enableDefaultInternetAccess :: Lens.Lens' ImageBuilder (Prelude.Maybe Prelude.Bool)
imageBuilder_enableDefaultInternetAccess = Lens.lens (\ImageBuilder' {enableDefaultInternetAccess} -> enableDefaultInternetAccess) (\s@ImageBuilder' {} a -> s {enableDefaultInternetAccess = a} :: ImageBuilder)

-- | The image builder errors.
imageBuilder_imageBuilderErrors :: Lens.Lens' ImageBuilder (Prelude.Maybe [ResourceError])
imageBuilder_imageBuilderErrors = Lens.lens (\ImageBuilder' {imageBuilderErrors} -> imageBuilderErrors) (\s@ImageBuilder' {} a -> s {imageBuilderErrors = a} :: ImageBuilder) Prelude.. Lens.mapping Lens._Coerce

-- | The ARN of the image from which this builder was created.
imageBuilder_imageArn :: Lens.Lens' ImageBuilder (Prelude.Maybe Prelude.Text)
imageBuilder_imageArn = Lens.lens (\ImageBuilder' {imageArn} -> imageArn) (\s@ImageBuilder' {} a -> s {imageArn = a} :: ImageBuilder)

-- | The name of the image builder.
imageBuilder_name :: Lens.Lens' ImageBuilder Prelude.Text
imageBuilder_name = Lens.lens (\ImageBuilder' {name} -> name) (\s@ImageBuilder' {} a -> s {name = a} :: ImageBuilder)

instance Core.FromJSON ImageBuilder where
  parseJSON =
    Core.withObject
      "ImageBuilder"
      ( \x ->
          ImageBuilder'
            Prelude.<$> (x Core..:? "Platform")
            Prelude.<*> (x Core..:? "VpcConfig")
            Prelude.<*> (x Core..:? "IamRoleArn")
            Prelude.<*> (x Core..:? "AccessEndpoints")
            Prelude.<*> (x Core..:? "DomainJoinInfo")
            Prelude.<*> (x Core..:? "InstanceType")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "StateChangeReason")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "NetworkAccessConfiguration")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "AppstreamAgentVersion")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "DisplayName")
            Prelude.<*> (x Core..:? "EnableDefaultInternetAccess")
            Prelude.<*> ( x Core..:? "ImageBuilderErrors"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ImageArn")
            Prelude.<*> (x Core..: "Name")
      )

instance Prelude.Hashable ImageBuilder

instance Prelude.NFData ImageBuilder
