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
-- Module      : Amazonka.AppStream.CreateImageBuilder
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.AppStream.CreateImageBuilder
  ( -- * Creating a Request
    CreateImageBuilder (..),
    newCreateImageBuilder,

    -- * Request Lenses
    createImageBuilder_tags,
    createImageBuilder_vpcConfig,
    createImageBuilder_displayName,
    createImageBuilder_accessEndpoints,
    createImageBuilder_imageArn,
    createImageBuilder_description,
    createImageBuilder_iamRoleArn,
    createImageBuilder_domainJoinInfo,
    createImageBuilder_appstreamAgentVersion,
    createImageBuilder_enableDefaultInternetAccess,
    createImageBuilder_imageName,
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

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateImageBuilder' smart constructor.
data CreateImageBuilder = CreateImageBuilder'
  { -- | The tags to associate with the image builder. A tag is a key-value pair,
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
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The VPC configuration for the image builder. You can specify only one
    -- subnet.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | The image builder name to display.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The list of interface VPC endpoint (interface endpoint) objects.
    -- Administrators can connect to the image builder only through the
    -- specified endpoints.
    accessEndpoints :: Prelude.Maybe (Prelude.NonEmpty AccessEndpoint),
    -- | The ARN of the public, private, or shared image to use.
    imageArn :: Prelude.Maybe Prelude.Text,
    -- | The description to display.
    description :: Prelude.Maybe Prelude.Text,
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
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the directory and organizational unit (OU) to use to join
    -- the image builder to a Microsoft Active Directory domain.
    domainJoinInfo :: Prelude.Maybe DomainJoinInfo,
    -- | The version of the AppStream 2.0 agent to use for this image builder. To
    -- use the latest version of the AppStream 2.0 agent, specify [LATEST].
    appstreamAgentVersion :: Prelude.Maybe Prelude.Text,
    -- | Enables or disables default internet access for the image builder.
    enableDefaultInternetAccess :: Prelude.Maybe Prelude.Bool,
    -- | The name of the image used to create the image builder.
    imageName :: Prelude.Maybe Prelude.Text,
    -- | A unique name for the image builder.
    name :: Prelude.Text,
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
    instanceType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateImageBuilder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'vpcConfig', 'createImageBuilder_vpcConfig' - The VPC configuration for the image builder. You can specify only one
-- subnet.
--
-- 'displayName', 'createImageBuilder_displayName' - The image builder name to display.
--
-- 'accessEndpoints', 'createImageBuilder_accessEndpoints' - The list of interface VPC endpoint (interface endpoint) objects.
-- Administrators can connect to the image builder only through the
-- specified endpoints.
--
-- 'imageArn', 'createImageBuilder_imageArn' - The ARN of the public, private, or shared image to use.
--
-- 'description', 'createImageBuilder_description' - The description to display.
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
-- 'domainJoinInfo', 'createImageBuilder_domainJoinInfo' - The name of the directory and organizational unit (OU) to use to join
-- the image builder to a Microsoft Active Directory domain.
--
-- 'appstreamAgentVersion', 'createImageBuilder_appstreamAgentVersion' - The version of the AppStream 2.0 agent to use for this image builder. To
-- use the latest version of the AppStream 2.0 agent, specify [LATEST].
--
-- 'enableDefaultInternetAccess', 'createImageBuilder_enableDefaultInternetAccess' - Enables or disables default internet access for the image builder.
--
-- 'imageName', 'createImageBuilder_imageName' - The name of the image used to create the image builder.
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
  Prelude.Text ->
  -- | 'instanceType'
  Prelude.Text ->
  CreateImageBuilder
newCreateImageBuilder pName_ pInstanceType_ =
  CreateImageBuilder'
    { tags = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      displayName = Prelude.Nothing,
      accessEndpoints = Prelude.Nothing,
      imageArn = Prelude.Nothing,
      description = Prelude.Nothing,
      iamRoleArn = Prelude.Nothing,
      domainJoinInfo = Prelude.Nothing,
      appstreamAgentVersion = Prelude.Nothing,
      enableDefaultInternetAccess = Prelude.Nothing,
      imageName = Prelude.Nothing,
      name = pName_,
      instanceType = pInstanceType_
    }

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
createImageBuilder_tags :: Lens.Lens' CreateImageBuilder (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createImageBuilder_tags = Lens.lens (\CreateImageBuilder' {tags} -> tags) (\s@CreateImageBuilder' {} a -> s {tags = a} :: CreateImageBuilder) Prelude.. Lens.mapping Lens.coerced

-- | The VPC configuration for the image builder. You can specify only one
-- subnet.
createImageBuilder_vpcConfig :: Lens.Lens' CreateImageBuilder (Prelude.Maybe VpcConfig)
createImageBuilder_vpcConfig = Lens.lens (\CreateImageBuilder' {vpcConfig} -> vpcConfig) (\s@CreateImageBuilder' {} a -> s {vpcConfig = a} :: CreateImageBuilder)

-- | The image builder name to display.
createImageBuilder_displayName :: Lens.Lens' CreateImageBuilder (Prelude.Maybe Prelude.Text)
createImageBuilder_displayName = Lens.lens (\CreateImageBuilder' {displayName} -> displayName) (\s@CreateImageBuilder' {} a -> s {displayName = a} :: CreateImageBuilder)

-- | The list of interface VPC endpoint (interface endpoint) objects.
-- Administrators can connect to the image builder only through the
-- specified endpoints.
createImageBuilder_accessEndpoints :: Lens.Lens' CreateImageBuilder (Prelude.Maybe (Prelude.NonEmpty AccessEndpoint))
createImageBuilder_accessEndpoints = Lens.lens (\CreateImageBuilder' {accessEndpoints} -> accessEndpoints) (\s@CreateImageBuilder' {} a -> s {accessEndpoints = a} :: CreateImageBuilder) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the public, private, or shared image to use.
createImageBuilder_imageArn :: Lens.Lens' CreateImageBuilder (Prelude.Maybe Prelude.Text)
createImageBuilder_imageArn = Lens.lens (\CreateImageBuilder' {imageArn} -> imageArn) (\s@CreateImageBuilder' {} a -> s {imageArn = a} :: CreateImageBuilder)

-- | The description to display.
createImageBuilder_description :: Lens.Lens' CreateImageBuilder (Prelude.Maybe Prelude.Text)
createImageBuilder_description = Lens.lens (\CreateImageBuilder' {description} -> description) (\s@CreateImageBuilder' {} a -> s {description = a} :: CreateImageBuilder)

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
createImageBuilder_iamRoleArn :: Lens.Lens' CreateImageBuilder (Prelude.Maybe Prelude.Text)
createImageBuilder_iamRoleArn = Lens.lens (\CreateImageBuilder' {iamRoleArn} -> iamRoleArn) (\s@CreateImageBuilder' {} a -> s {iamRoleArn = a} :: CreateImageBuilder)

-- | The name of the directory and organizational unit (OU) to use to join
-- the image builder to a Microsoft Active Directory domain.
createImageBuilder_domainJoinInfo :: Lens.Lens' CreateImageBuilder (Prelude.Maybe DomainJoinInfo)
createImageBuilder_domainJoinInfo = Lens.lens (\CreateImageBuilder' {domainJoinInfo} -> domainJoinInfo) (\s@CreateImageBuilder' {} a -> s {domainJoinInfo = a} :: CreateImageBuilder)

-- | The version of the AppStream 2.0 agent to use for this image builder. To
-- use the latest version of the AppStream 2.0 agent, specify [LATEST].
createImageBuilder_appstreamAgentVersion :: Lens.Lens' CreateImageBuilder (Prelude.Maybe Prelude.Text)
createImageBuilder_appstreamAgentVersion = Lens.lens (\CreateImageBuilder' {appstreamAgentVersion} -> appstreamAgentVersion) (\s@CreateImageBuilder' {} a -> s {appstreamAgentVersion = a} :: CreateImageBuilder)

-- | Enables or disables default internet access for the image builder.
createImageBuilder_enableDefaultInternetAccess :: Lens.Lens' CreateImageBuilder (Prelude.Maybe Prelude.Bool)
createImageBuilder_enableDefaultInternetAccess = Lens.lens (\CreateImageBuilder' {enableDefaultInternetAccess} -> enableDefaultInternetAccess) (\s@CreateImageBuilder' {} a -> s {enableDefaultInternetAccess = a} :: CreateImageBuilder)

-- | The name of the image used to create the image builder.
createImageBuilder_imageName :: Lens.Lens' CreateImageBuilder (Prelude.Maybe Prelude.Text)
createImageBuilder_imageName = Lens.lens (\CreateImageBuilder' {imageName} -> imageName) (\s@CreateImageBuilder' {} a -> s {imageName = a} :: CreateImageBuilder)

-- | A unique name for the image builder.
createImageBuilder_name :: Lens.Lens' CreateImageBuilder Prelude.Text
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
createImageBuilder_instanceType :: Lens.Lens' CreateImageBuilder Prelude.Text
createImageBuilder_instanceType = Lens.lens (\CreateImageBuilder' {instanceType} -> instanceType) (\s@CreateImageBuilder' {} a -> s {instanceType = a} :: CreateImageBuilder)

instance Core.AWSRequest CreateImageBuilder where
  type
    AWSResponse CreateImageBuilder =
      CreateImageBuilderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateImageBuilderResponse'
            Prelude.<$> (x Core..?> "ImageBuilder")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateImageBuilder where
  hashWithSalt _salt CreateImageBuilder' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` accessEndpoints
      `Prelude.hashWithSalt` imageArn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` iamRoleArn
      `Prelude.hashWithSalt` domainJoinInfo
      `Prelude.hashWithSalt` appstreamAgentVersion
      `Prelude.hashWithSalt` enableDefaultInternetAccess
      `Prelude.hashWithSalt` imageName
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` instanceType

instance Prelude.NFData CreateImageBuilder where
  rnf CreateImageBuilder' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf accessEndpoints
      `Prelude.seq` Prelude.rnf imageArn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf iamRoleArn
      `Prelude.seq` Prelude.rnf domainJoinInfo
      `Prelude.seq` Prelude.rnf appstreamAgentVersion
      `Prelude.seq` Prelude.rnf enableDefaultInternetAccess
      `Prelude.seq` Prelude.rnf imageName
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf instanceType

instance Core.ToHeaders CreateImageBuilder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.CreateImageBuilder" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateImageBuilder where
  toJSON CreateImageBuilder' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("VpcConfig" Core..=) Prelude.<$> vpcConfig,
            ("DisplayName" Core..=) Prelude.<$> displayName,
            ("AccessEndpoints" Core..=)
              Prelude.<$> accessEndpoints,
            ("ImageArn" Core..=) Prelude.<$> imageArn,
            ("Description" Core..=) Prelude.<$> description,
            ("IamRoleArn" Core..=) Prelude.<$> iamRoleArn,
            ("DomainJoinInfo" Core..=)
              Prelude.<$> domainJoinInfo,
            ("AppstreamAgentVersion" Core..=)
              Prelude.<$> appstreamAgentVersion,
            ("EnableDefaultInternetAccess" Core..=)
              Prelude.<$> enableDefaultInternetAccess,
            ("ImageName" Core..=) Prelude.<$> imageName,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("InstanceType" Core..= instanceType)
          ]
      )

instance Core.ToPath CreateImageBuilder where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateImageBuilder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateImageBuilderResponse' smart constructor.
data CreateImageBuilderResponse = CreateImageBuilderResponse'
  { -- | Information about the image builder.
    imageBuilder :: Prelude.Maybe ImageBuilder,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateImageBuilderResponse
newCreateImageBuilderResponse pHttpStatus_ =
  CreateImageBuilderResponse'
    { imageBuilder =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the image builder.
createImageBuilderResponse_imageBuilder :: Lens.Lens' CreateImageBuilderResponse (Prelude.Maybe ImageBuilder)
createImageBuilderResponse_imageBuilder = Lens.lens (\CreateImageBuilderResponse' {imageBuilder} -> imageBuilder) (\s@CreateImageBuilderResponse' {} a -> s {imageBuilder = a} :: CreateImageBuilderResponse)

-- | The response's http status code.
createImageBuilderResponse_httpStatus :: Lens.Lens' CreateImageBuilderResponse Prelude.Int
createImageBuilderResponse_httpStatus = Lens.lens (\CreateImageBuilderResponse' {httpStatus} -> httpStatus) (\s@CreateImageBuilderResponse' {} a -> s {httpStatus = a} :: CreateImageBuilderResponse)

instance Prelude.NFData CreateImageBuilderResponse where
  rnf CreateImageBuilderResponse' {..} =
    Prelude.rnf imageBuilder
      `Prelude.seq` Prelude.rnf httpStatus
