{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.CreateImageBuilder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an image builder. An image builder is a virtual machine that is used to create an image.
--
-- The initial state of the builder is @PENDING@ . When it is ready, the state is @RUNNING@ .
module Network.AWS.AppStream.CreateImageBuilder
  ( -- * Creating a request
    CreateImageBuilder (..),
    mkCreateImageBuilder,

    -- ** Request lenses
    cibDomainJoinInfo,
    cibIAMRoleARN,
    cibAccessEndpoints,
    cibVPCConfig,
    cibImageARN,
    cibDisplayName,
    cibEnableDefaultInternetAccess,
    cibImageName,
    cibDescription,
    cibAppstreamAgentVersion,
    cibTags,
    cibName,
    cibInstanceType,

    -- * Destructuring the response
    CreateImageBuilderResponse (..),
    mkCreateImageBuilderResponse,

    -- ** Response lenses
    cibrsImageBuilder,
    cibrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateImageBuilder' smart constructor.
data CreateImageBuilder = CreateImageBuilder'
  { domainJoinInfo ::
      Lude.Maybe DomainJoinInfo,
    iamRoleARN :: Lude.Maybe Lude.Text,
    accessEndpoints ::
      Lude.Maybe (Lude.NonEmpty AccessEndpoint),
    vpcConfig :: Lude.Maybe VPCConfig,
    imageARN :: Lude.Maybe Lude.Text,
    displayName :: Lude.Maybe Lude.Text,
    enableDefaultInternetAccess :: Lude.Maybe Lude.Bool,
    imageName :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    appstreamAgentVersion :: Lude.Maybe Lude.Text,
    tags ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    name :: Lude.Text,
    instanceType :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateImageBuilder' with the minimum fields required to make a request.
--
-- * 'accessEndpoints' - The list of interface VPC endpoint (interface endpoint) objects. Administrators can connect to the image builder only through the specified endpoints.
-- * 'appstreamAgentVersion' - The version of the AppStream 2.0 agent to use for this image builder. To use the latest version of the AppStream 2.0 agent, specify [LATEST].
-- * 'description' - The description to display.
-- * 'displayName' - The image builder name to display.
-- * 'domainJoinInfo' - The name of the directory and organizational unit (OU) to use to join the image builder to a Microsoft Active Directory domain.
-- * 'enableDefaultInternetAccess' - Enables or disables default internet access for the image builder.
-- * 'iamRoleARN' - The Amazon Resource Name (ARN) of the IAM role to apply to the image builder. To assume a role, the image builder calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance.
--
-- For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
-- * 'imageARN' - The ARN of the public, private, or shared image to use.
-- * 'imageName' - The name of the image used to create the image builder.
-- * 'instanceType' - The instance type to use when launching the image builder. The following instance types are available:
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
-- * 'name' - A unique name for the image builder.
-- * 'tags' - The tags to associate with the image builder. A tag is a key-value pair, and the value is optional. For example, Environment=Test. If you do not specify a value, Environment=.
--
-- Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following special characters:
-- _ . : / = + \ - @
-- If you do not specify a value, the value is set to an empty string.
-- For more information about tags, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources> in the /Amazon AppStream 2.0 Administration Guide/ .
-- * 'vpcConfig' - The VPC configuration for the image builder. You can specify only one subnet.
mkCreateImageBuilder ::
  -- | 'name'
  Lude.Text ->
  -- | 'instanceType'
  Lude.Text ->
  CreateImageBuilder
mkCreateImageBuilder pName_ pInstanceType_ =
  CreateImageBuilder'
    { domainJoinInfo = Lude.Nothing,
      iamRoleARN = Lude.Nothing,
      accessEndpoints = Lude.Nothing,
      vpcConfig = Lude.Nothing,
      imageARN = Lude.Nothing,
      displayName = Lude.Nothing,
      enableDefaultInternetAccess = Lude.Nothing,
      imageName = Lude.Nothing,
      description = Lude.Nothing,
      appstreamAgentVersion = Lude.Nothing,
      tags = Lude.Nothing,
      name = pName_,
      instanceType = pInstanceType_
    }

-- | The name of the directory and organizational unit (OU) to use to join the image builder to a Microsoft Active Directory domain.
--
-- /Note:/ Consider using 'domainJoinInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibDomainJoinInfo :: Lens.Lens' CreateImageBuilder (Lude.Maybe DomainJoinInfo)
cibDomainJoinInfo = Lens.lens (domainJoinInfo :: CreateImageBuilder -> Lude.Maybe DomainJoinInfo) (\s a -> s {domainJoinInfo = a} :: CreateImageBuilder)
{-# DEPRECATED cibDomainJoinInfo "Use generic-lens or generic-optics with 'domainJoinInfo' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role to apply to the image builder. To assume a role, the image builder calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance.
--
-- For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
--
-- /Note:/ Consider using 'iamRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibIAMRoleARN :: Lens.Lens' CreateImageBuilder (Lude.Maybe Lude.Text)
cibIAMRoleARN = Lens.lens (iamRoleARN :: CreateImageBuilder -> Lude.Maybe Lude.Text) (\s a -> s {iamRoleARN = a} :: CreateImageBuilder)
{-# DEPRECATED cibIAMRoleARN "Use generic-lens or generic-optics with 'iamRoleARN' instead." #-}

-- | The list of interface VPC endpoint (interface endpoint) objects. Administrators can connect to the image builder only through the specified endpoints.
--
-- /Note:/ Consider using 'accessEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibAccessEndpoints :: Lens.Lens' CreateImageBuilder (Lude.Maybe (Lude.NonEmpty AccessEndpoint))
cibAccessEndpoints = Lens.lens (accessEndpoints :: CreateImageBuilder -> Lude.Maybe (Lude.NonEmpty AccessEndpoint)) (\s a -> s {accessEndpoints = a} :: CreateImageBuilder)
{-# DEPRECATED cibAccessEndpoints "Use generic-lens or generic-optics with 'accessEndpoints' instead." #-}

-- | The VPC configuration for the image builder. You can specify only one subnet.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibVPCConfig :: Lens.Lens' CreateImageBuilder (Lude.Maybe VPCConfig)
cibVPCConfig = Lens.lens (vpcConfig :: CreateImageBuilder -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: CreateImageBuilder)
{-# DEPRECATED cibVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | The ARN of the public, private, or shared image to use.
--
-- /Note:/ Consider using 'imageARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibImageARN :: Lens.Lens' CreateImageBuilder (Lude.Maybe Lude.Text)
cibImageARN = Lens.lens (imageARN :: CreateImageBuilder -> Lude.Maybe Lude.Text) (\s a -> s {imageARN = a} :: CreateImageBuilder)
{-# DEPRECATED cibImageARN "Use generic-lens or generic-optics with 'imageARN' instead." #-}

-- | The image builder name to display.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibDisplayName :: Lens.Lens' CreateImageBuilder (Lude.Maybe Lude.Text)
cibDisplayName = Lens.lens (displayName :: CreateImageBuilder -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: CreateImageBuilder)
{-# DEPRECATED cibDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | Enables or disables default internet access for the image builder.
--
-- /Note:/ Consider using 'enableDefaultInternetAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibEnableDefaultInternetAccess :: Lens.Lens' CreateImageBuilder (Lude.Maybe Lude.Bool)
cibEnableDefaultInternetAccess = Lens.lens (enableDefaultInternetAccess :: CreateImageBuilder -> Lude.Maybe Lude.Bool) (\s a -> s {enableDefaultInternetAccess = a} :: CreateImageBuilder)
{-# DEPRECATED cibEnableDefaultInternetAccess "Use generic-lens or generic-optics with 'enableDefaultInternetAccess' instead." #-}

-- | The name of the image used to create the image builder.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibImageName :: Lens.Lens' CreateImageBuilder (Lude.Maybe Lude.Text)
cibImageName = Lens.lens (imageName :: CreateImageBuilder -> Lude.Maybe Lude.Text) (\s a -> s {imageName = a} :: CreateImageBuilder)
{-# DEPRECATED cibImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

-- | The description to display.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibDescription :: Lens.Lens' CreateImageBuilder (Lude.Maybe Lude.Text)
cibDescription = Lens.lens (description :: CreateImageBuilder -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateImageBuilder)
{-# DEPRECATED cibDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The version of the AppStream 2.0 agent to use for this image builder. To use the latest version of the AppStream 2.0 agent, specify [LATEST].
--
-- /Note:/ Consider using 'appstreamAgentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibAppstreamAgentVersion :: Lens.Lens' CreateImageBuilder (Lude.Maybe Lude.Text)
cibAppstreamAgentVersion = Lens.lens (appstreamAgentVersion :: CreateImageBuilder -> Lude.Maybe Lude.Text) (\s a -> s {appstreamAgentVersion = a} :: CreateImageBuilder)
{-# DEPRECATED cibAppstreamAgentVersion "Use generic-lens or generic-optics with 'appstreamAgentVersion' instead." #-}

-- | The tags to associate with the image builder. A tag is a key-value pair, and the value is optional. For example, Environment=Test. If you do not specify a value, Environment=.
--
-- Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following special characters:
-- _ . : / = + \ - @
-- If you do not specify a value, the value is set to an empty string.
-- For more information about tags, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources> in the /Amazon AppStream 2.0 Administration Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibTags :: Lens.Lens' CreateImageBuilder (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cibTags = Lens.lens (tags :: CreateImageBuilder -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateImageBuilder)
{-# DEPRECATED cibTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A unique name for the image builder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibName :: Lens.Lens' CreateImageBuilder Lude.Text
cibName = Lens.lens (name :: CreateImageBuilder -> Lude.Text) (\s a -> s {name = a} :: CreateImageBuilder)
{-# DEPRECATED cibName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The instance type to use when launching the image builder. The following instance types are available:
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
cibInstanceType :: Lens.Lens' CreateImageBuilder Lude.Text
cibInstanceType = Lens.lens (instanceType :: CreateImageBuilder -> Lude.Text) (\s a -> s {instanceType = a} :: CreateImageBuilder)
{-# DEPRECATED cibInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

instance Lude.AWSRequest CreateImageBuilder where
  type Rs CreateImageBuilder = CreateImageBuilderResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateImageBuilderResponse'
            Lude.<$> (x Lude..?> "ImageBuilder") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateImageBuilder where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("PhotonAdminProxyService.CreateImageBuilder" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateImageBuilder where
  toJSON CreateImageBuilder' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DomainJoinInfo" Lude..=) Lude.<$> domainJoinInfo,
            ("IamRoleArn" Lude..=) Lude.<$> iamRoleARN,
            ("AccessEndpoints" Lude..=) Lude.<$> accessEndpoints,
            ("VpcConfig" Lude..=) Lude.<$> vpcConfig,
            ("ImageArn" Lude..=) Lude.<$> imageARN,
            ("DisplayName" Lude..=) Lude.<$> displayName,
            ("EnableDefaultInternetAccess" Lude..=)
              Lude.<$> enableDefaultInternetAccess,
            ("ImageName" Lude..=) Lude.<$> imageName,
            ("Description" Lude..=) Lude.<$> description,
            ("AppstreamAgentVersion" Lude..=) Lude.<$> appstreamAgentVersion,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("InstanceType" Lude..= instanceType)
          ]
      )

instance Lude.ToPath CreateImageBuilder where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateImageBuilder where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateImageBuilderResponse' smart constructor.
data CreateImageBuilderResponse = CreateImageBuilderResponse'
  { imageBuilder ::
      Lude.Maybe ImageBuilder,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateImageBuilderResponse' with the minimum fields required to make a request.
--
-- * 'imageBuilder' - Information about the image builder.
-- * 'responseStatus' - The response status code.
mkCreateImageBuilderResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateImageBuilderResponse
mkCreateImageBuilderResponse pResponseStatus_ =
  CreateImageBuilderResponse'
    { imageBuilder = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the image builder.
--
-- /Note:/ Consider using 'imageBuilder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibrsImageBuilder :: Lens.Lens' CreateImageBuilderResponse (Lude.Maybe ImageBuilder)
cibrsImageBuilder = Lens.lens (imageBuilder :: CreateImageBuilderResponse -> Lude.Maybe ImageBuilder) (\s a -> s {imageBuilder = a} :: CreateImageBuilderResponse)
{-# DEPRECATED cibrsImageBuilder "Use generic-lens or generic-optics with 'imageBuilder' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibrsResponseStatus :: Lens.Lens' CreateImageBuilderResponse Lude.Int
cibrsResponseStatus = Lens.lens (responseStatus :: CreateImageBuilderResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateImageBuilderResponse)
{-# DEPRECATED cibrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
