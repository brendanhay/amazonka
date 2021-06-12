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
-- Module      : Network.AWS.AppStream.CreateImageBuilder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an image builder. An image builder is a virtual machine that is
-- used to create an image.
--
-- The initial state of the builder is @PENDING@. When it is ready, the
-- state is @RUNNING@.
module Network.AWS.AppStream.CreateImageBuilder
  ( -- * Creating a Request
    CreateImageBuilder (..),
    newCreateImageBuilder,

    -- * Request Lenses
    createImageBuilder_vpcConfig,
    createImageBuilder_iamRoleArn,
    createImageBuilder_accessEndpoints,
    createImageBuilder_domainJoinInfo,
    createImageBuilder_imageName,
    createImageBuilder_tags,
    createImageBuilder_appstreamAgentVersion,
    createImageBuilder_description,
    createImageBuilder_displayName,
    createImageBuilder_enableDefaultInternetAccess,
    createImageBuilder_imageArn,
    createImageBuilder_name,
    createImageBuilder_instanceType,

    -- * Destructuring the Response
    CreateImageBuilderResponse (..),
    newCreateImageBuilderResponse,

    -- * Response Lenses
    createImageBuilderResponse_imageBuilder,
    createImageBuilderResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateImageBuilder' smart constructor.
data CreateImageBuilder = CreateImageBuilder'
  { -- | The VPC configuration for the image builder. You can specify only one
    -- subnet.
    vpcConfig :: Core.Maybe VpcConfig,
    -- | The Amazon Resource Name (ARN) of the IAM role to apply to the image
    -- builder. To assume a role, the image builder calls the AWS Security
    -- Token Service (STS) @AssumeRole@ API operation and passes the ARN of the
    -- role to use. The operation creates a new session with temporary
    -- credentials. AppStream 2.0 retrieves the temporary credentials and
    -- creates the __appstream_machine_role__ credential profile on the
    -- instance.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances>
    -- in the /Amazon AppStream 2.0 Administration Guide/.
    iamRoleArn :: Core.Maybe Core.Text,
    -- | The list of interface VPC endpoint (interface endpoint) objects.
    -- Administrators can connect to the image builder only through the
    -- specified endpoints.
    accessEndpoints :: Core.Maybe (Core.NonEmpty AccessEndpoint),
    -- | The name of the directory and organizational unit (OU) to use to join
    -- the image builder to a Microsoft Active Directory domain.
    domainJoinInfo :: Core.Maybe DomainJoinInfo,
    -- | The name of the image used to create the image builder.
    imageName :: Core.Maybe Core.Text,
    -- | The tags to associate with the image builder. A tag is a key-value pair,
    -- and the value is optional. For example, Environment=Test. If you do not
    -- specify a value, Environment=.
    --
    -- Generally allowed characters are: letters, numbers, and spaces
    -- representable in UTF-8, and the following special characters:
    --
    -- _ . : \/ = + \\ - \@
    --
    -- If you do not specify a value, the value is set to an empty string.
    --
    -- For more information about tags, see
    -- <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources>
    -- in the /Amazon AppStream 2.0 Administration Guide/.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The version of the AppStream 2.0 agent to use for this image builder. To
    -- use the latest version of the AppStream 2.0 agent, specify [LATEST].
    appstreamAgentVersion :: Core.Maybe Core.Text,
    -- | The description to display.
    description :: Core.Maybe Core.Text,
    -- | The image builder name to display.
    displayName :: Core.Maybe Core.Text,
    -- | Enables or disables default internet access for the image builder.
    enableDefaultInternetAccess :: Core.Maybe Core.Bool,
    -- | The ARN of the public, private, or shared image to use.
    imageArn :: Core.Maybe Core.Text,
    -- | A unique name for the image builder.
    name :: Core.Text,
    -- | The instance type to use when launching the image builder. The following
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
    instanceType :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateImageBuilder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'createImageBuilder_vpcConfig' - The VPC configuration for the image builder. You can specify only one
-- subnet.
--
-- 'iamRoleArn', 'createImageBuilder_iamRoleArn' - The Amazon Resource Name (ARN) of the IAM role to apply to the image
-- builder. To assume a role, the image builder calls the AWS Security
-- Token Service (STS) @AssumeRole@ API operation and passes the ARN of the
-- role to use. The operation creates a new session with temporary
-- credentials. AppStream 2.0 retrieves the temporary credentials and
-- creates the __appstream_machine_role__ credential profile on the
-- instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances>
-- in the /Amazon AppStream 2.0 Administration Guide/.
--
-- 'accessEndpoints', 'createImageBuilder_accessEndpoints' - The list of interface VPC endpoint (interface endpoint) objects.
-- Administrators can connect to the image builder only through the
-- specified endpoints.
--
-- 'domainJoinInfo', 'createImageBuilder_domainJoinInfo' - The name of the directory and organizational unit (OU) to use to join
-- the image builder to a Microsoft Active Directory domain.
--
-- 'imageName', 'createImageBuilder_imageName' - The name of the image used to create the image builder.
--
-- 'tags', 'createImageBuilder_tags' - The tags to associate with the image builder. A tag is a key-value pair,
-- and the value is optional. For example, Environment=Test. If you do not
-- specify a value, Environment=.
--
-- Generally allowed characters are: letters, numbers, and spaces
-- representable in UTF-8, and the following special characters:
--
-- _ . : \/ = + \\ - \@
--
-- If you do not specify a value, the value is set to an empty string.
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources>
-- in the /Amazon AppStream 2.0 Administration Guide/.
--
-- 'appstreamAgentVersion', 'createImageBuilder_appstreamAgentVersion' - The version of the AppStream 2.0 agent to use for this image builder. To
-- use the latest version of the AppStream 2.0 agent, specify [LATEST].
--
-- 'description', 'createImageBuilder_description' - The description to display.
--
-- 'displayName', 'createImageBuilder_displayName' - The image builder name to display.
--
-- 'enableDefaultInternetAccess', 'createImageBuilder_enableDefaultInternetAccess' - Enables or disables default internet access for the image builder.
--
-- 'imageArn', 'createImageBuilder_imageArn' - The ARN of the public, private, or shared image to use.
--
-- 'name', 'createImageBuilder_name' - A unique name for the image builder.
--
-- 'instanceType', 'createImageBuilder_instanceType' - The instance type to use when launching the image builder. The following
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
newCreateImageBuilder ::
  -- | 'name'
  Core.Text ->
  -- | 'instanceType'
  Core.Text ->
  CreateImageBuilder
newCreateImageBuilder pName_ pInstanceType_ =
  CreateImageBuilder'
    { vpcConfig = Core.Nothing,
      iamRoleArn = Core.Nothing,
      accessEndpoints = Core.Nothing,
      domainJoinInfo = Core.Nothing,
      imageName = Core.Nothing,
      tags = Core.Nothing,
      appstreamAgentVersion = Core.Nothing,
      description = Core.Nothing,
      displayName = Core.Nothing,
      enableDefaultInternetAccess = Core.Nothing,
      imageArn = Core.Nothing,
      name = pName_,
      instanceType = pInstanceType_
    }

-- | The VPC configuration for the image builder. You can specify only one
-- subnet.
createImageBuilder_vpcConfig :: Lens.Lens' CreateImageBuilder (Core.Maybe VpcConfig)
createImageBuilder_vpcConfig = Lens.lens (\CreateImageBuilder' {vpcConfig} -> vpcConfig) (\s@CreateImageBuilder' {} a -> s {vpcConfig = a} :: CreateImageBuilder)

-- | The Amazon Resource Name (ARN) of the IAM role to apply to the image
-- builder. To assume a role, the image builder calls the AWS Security
-- Token Service (STS) @AssumeRole@ API operation and passes the ARN of the
-- role to use. The operation creates a new session with temporary
-- credentials. AppStream 2.0 retrieves the temporary credentials and
-- creates the __appstream_machine_role__ credential profile on the
-- instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances>
-- in the /Amazon AppStream 2.0 Administration Guide/.
createImageBuilder_iamRoleArn :: Lens.Lens' CreateImageBuilder (Core.Maybe Core.Text)
createImageBuilder_iamRoleArn = Lens.lens (\CreateImageBuilder' {iamRoleArn} -> iamRoleArn) (\s@CreateImageBuilder' {} a -> s {iamRoleArn = a} :: CreateImageBuilder)

-- | The list of interface VPC endpoint (interface endpoint) objects.
-- Administrators can connect to the image builder only through the
-- specified endpoints.
createImageBuilder_accessEndpoints :: Lens.Lens' CreateImageBuilder (Core.Maybe (Core.NonEmpty AccessEndpoint))
createImageBuilder_accessEndpoints = Lens.lens (\CreateImageBuilder' {accessEndpoints} -> accessEndpoints) (\s@CreateImageBuilder' {} a -> s {accessEndpoints = a} :: CreateImageBuilder) Core.. Lens.mapping Lens._Coerce

-- | The name of the directory and organizational unit (OU) to use to join
-- the image builder to a Microsoft Active Directory domain.
createImageBuilder_domainJoinInfo :: Lens.Lens' CreateImageBuilder (Core.Maybe DomainJoinInfo)
createImageBuilder_domainJoinInfo = Lens.lens (\CreateImageBuilder' {domainJoinInfo} -> domainJoinInfo) (\s@CreateImageBuilder' {} a -> s {domainJoinInfo = a} :: CreateImageBuilder)

-- | The name of the image used to create the image builder.
createImageBuilder_imageName :: Lens.Lens' CreateImageBuilder (Core.Maybe Core.Text)
createImageBuilder_imageName = Lens.lens (\CreateImageBuilder' {imageName} -> imageName) (\s@CreateImageBuilder' {} a -> s {imageName = a} :: CreateImageBuilder)

-- | The tags to associate with the image builder. A tag is a key-value pair,
-- and the value is optional. For example, Environment=Test. If you do not
-- specify a value, Environment=.
--
-- Generally allowed characters are: letters, numbers, and spaces
-- representable in UTF-8, and the following special characters:
--
-- _ . : \/ = + \\ - \@
--
-- If you do not specify a value, the value is set to an empty string.
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources>
-- in the /Amazon AppStream 2.0 Administration Guide/.
createImageBuilder_tags :: Lens.Lens' CreateImageBuilder (Core.Maybe (Core.HashMap Core.Text Core.Text))
createImageBuilder_tags = Lens.lens (\CreateImageBuilder' {tags} -> tags) (\s@CreateImageBuilder' {} a -> s {tags = a} :: CreateImageBuilder) Core.. Lens.mapping Lens._Coerce

-- | The version of the AppStream 2.0 agent to use for this image builder. To
-- use the latest version of the AppStream 2.0 agent, specify [LATEST].
createImageBuilder_appstreamAgentVersion :: Lens.Lens' CreateImageBuilder (Core.Maybe Core.Text)
createImageBuilder_appstreamAgentVersion = Lens.lens (\CreateImageBuilder' {appstreamAgentVersion} -> appstreamAgentVersion) (\s@CreateImageBuilder' {} a -> s {appstreamAgentVersion = a} :: CreateImageBuilder)

-- | The description to display.
createImageBuilder_description :: Lens.Lens' CreateImageBuilder (Core.Maybe Core.Text)
createImageBuilder_description = Lens.lens (\CreateImageBuilder' {description} -> description) (\s@CreateImageBuilder' {} a -> s {description = a} :: CreateImageBuilder)

-- | The image builder name to display.
createImageBuilder_displayName :: Lens.Lens' CreateImageBuilder (Core.Maybe Core.Text)
createImageBuilder_displayName = Lens.lens (\CreateImageBuilder' {displayName} -> displayName) (\s@CreateImageBuilder' {} a -> s {displayName = a} :: CreateImageBuilder)

-- | Enables or disables default internet access for the image builder.
createImageBuilder_enableDefaultInternetAccess :: Lens.Lens' CreateImageBuilder (Core.Maybe Core.Bool)
createImageBuilder_enableDefaultInternetAccess = Lens.lens (\CreateImageBuilder' {enableDefaultInternetAccess} -> enableDefaultInternetAccess) (\s@CreateImageBuilder' {} a -> s {enableDefaultInternetAccess = a} :: CreateImageBuilder)

-- | The ARN of the public, private, or shared image to use.
createImageBuilder_imageArn :: Lens.Lens' CreateImageBuilder (Core.Maybe Core.Text)
createImageBuilder_imageArn = Lens.lens (\CreateImageBuilder' {imageArn} -> imageArn) (\s@CreateImageBuilder' {} a -> s {imageArn = a} :: CreateImageBuilder)

-- | A unique name for the image builder.
createImageBuilder_name :: Lens.Lens' CreateImageBuilder Core.Text
createImageBuilder_name = Lens.lens (\CreateImageBuilder' {name} -> name) (\s@CreateImageBuilder' {} a -> s {name = a} :: CreateImageBuilder)

-- | The instance type to use when launching the image builder. The following
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
createImageBuilder_instanceType :: Lens.Lens' CreateImageBuilder Core.Text
createImageBuilder_instanceType = Lens.lens (\CreateImageBuilder' {instanceType} -> instanceType) (\s@CreateImageBuilder' {} a -> s {instanceType = a} :: CreateImageBuilder)

instance Core.AWSRequest CreateImageBuilder where
  type
    AWSResponse CreateImageBuilder =
      CreateImageBuilderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateImageBuilderResponse'
            Core.<$> (x Core..?> "ImageBuilder")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateImageBuilder

instance Core.NFData CreateImageBuilder

instance Core.ToHeaders CreateImageBuilder where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.CreateImageBuilder" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateImageBuilder where
  toJSON CreateImageBuilder' {..} =
    Core.object
      ( Core.catMaybes
          [ ("VpcConfig" Core..=) Core.<$> vpcConfig,
            ("IamRoleArn" Core..=) Core.<$> iamRoleArn,
            ("AccessEndpoints" Core..=) Core.<$> accessEndpoints,
            ("DomainJoinInfo" Core..=) Core.<$> domainJoinInfo,
            ("ImageName" Core..=) Core.<$> imageName,
            ("Tags" Core..=) Core.<$> tags,
            ("AppstreamAgentVersion" Core..=)
              Core.<$> appstreamAgentVersion,
            ("Description" Core..=) Core.<$> description,
            ("DisplayName" Core..=) Core.<$> displayName,
            ("EnableDefaultInternetAccess" Core..=)
              Core.<$> enableDefaultInternetAccess,
            ("ImageArn" Core..=) Core.<$> imageArn,
            Core.Just ("Name" Core..= name),
            Core.Just ("InstanceType" Core..= instanceType)
          ]
      )

instance Core.ToPath CreateImageBuilder where
  toPath = Core.const "/"

instance Core.ToQuery CreateImageBuilder where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateImageBuilderResponse' smart constructor.
data CreateImageBuilderResponse = CreateImageBuilderResponse'
  { -- | Information about the image builder.
    imageBuilder :: Core.Maybe ImageBuilder,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateImageBuilderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageBuilder', 'createImageBuilderResponse_imageBuilder' - Information about the image builder.
--
-- 'httpStatus', 'createImageBuilderResponse_httpStatus' - The response's http status code.
newCreateImageBuilderResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateImageBuilderResponse
newCreateImageBuilderResponse pHttpStatus_ =
  CreateImageBuilderResponse'
    { imageBuilder =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the image builder.
createImageBuilderResponse_imageBuilder :: Lens.Lens' CreateImageBuilderResponse (Core.Maybe ImageBuilder)
createImageBuilderResponse_imageBuilder = Lens.lens (\CreateImageBuilderResponse' {imageBuilder} -> imageBuilder) (\s@CreateImageBuilderResponse' {} a -> s {imageBuilder = a} :: CreateImageBuilderResponse)

-- | The response's http status code.
createImageBuilderResponse_httpStatus :: Lens.Lens' CreateImageBuilderResponse Core.Int
createImageBuilderResponse_httpStatus = Lens.lens (\CreateImageBuilderResponse' {httpStatus} -> httpStatus) (\s@CreateImageBuilderResponse' {} a -> s {httpStatus = a} :: CreateImageBuilderResponse)

instance Core.NFData CreateImageBuilderResponse
