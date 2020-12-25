{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    cibName,
    cibInstanceType,
    cibAccessEndpoints,
    cibAppstreamAgentVersion,
    cibDescription,
    cibDisplayName,
    cibDomainJoinInfo,
    cibEnableDefaultInternetAccess,
    cibIamRoleArn,
    cibImageArn,
    cibImageName,
    cibTags,
    cibVpcConfig,

    -- * Destructuring the response
    CreateImageBuilderResponse (..),
    mkCreateImageBuilderResponse,

    -- ** Response lenses
    cibrrsImageBuilder,
    cibrrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateImageBuilder' smart constructor.
data CreateImageBuilder = CreateImageBuilder'
  { -- | A unique name for the image builder.
    name :: Types.Name,
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
    instanceType :: Types.String,
    -- | The list of interface VPC endpoint (interface endpoint) objects. Administrators can connect to the image builder only through the specified endpoints.
    accessEndpoints :: Core.Maybe (Core.NonEmpty Types.AccessEndpoint),
    -- | The version of the AppStream 2.0 agent to use for this image builder. To use the latest version of the AppStream 2.0 agent, specify [LATEST].
    appstreamAgentVersion :: Core.Maybe Types.AppstreamAgentVersion,
    -- | The description to display.
    description :: Core.Maybe Types.Description,
    -- | The image builder name to display.
    displayName :: Core.Maybe Types.DisplayName,
    -- | The name of the directory and organizational unit (OU) to use to join the image builder to a Microsoft Active Directory domain.
    domainJoinInfo :: Core.Maybe Types.DomainJoinInfo,
    -- | Enables or disables default internet access for the image builder.
    enableDefaultInternetAccess :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the IAM role to apply to the image builder. To assume a role, the image builder calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance.
    --
    -- For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
    iamRoleArn :: Core.Maybe Types.Arn,
    -- | The ARN of the public, private, or shared image to use.
    imageArn :: Core.Maybe Types.Arn,
    -- | The name of the image used to create the image builder.
    imageName :: Core.Maybe Types.String,
    -- | The tags to associate with the image builder. A tag is a key-value pair, and the value is optional. For example, Environment=Test. If you do not specify a value, Environment=.
    --
    -- Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following special characters:
    -- _ . : / = + \ - @
    -- If you do not specify a value, the value is set to an empty string.
    -- For more information about tags, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources> in the /Amazon AppStream 2.0 Administration Guide/ .
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue),
    -- | The VPC configuration for the image builder. You can specify only one subnet.
    vpcConfig :: Core.Maybe Types.VpcConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateImageBuilder' value with any optional fields omitted.
mkCreateImageBuilder ::
  -- | 'name'
  Types.Name ->
  -- | 'instanceType'
  Types.String ->
  CreateImageBuilder
mkCreateImageBuilder name instanceType =
  CreateImageBuilder'
    { name,
      instanceType,
      accessEndpoints = Core.Nothing,
      appstreamAgentVersion = Core.Nothing,
      description = Core.Nothing,
      displayName = Core.Nothing,
      domainJoinInfo = Core.Nothing,
      enableDefaultInternetAccess = Core.Nothing,
      iamRoleArn = Core.Nothing,
      imageArn = Core.Nothing,
      imageName = Core.Nothing,
      tags = Core.Nothing,
      vpcConfig = Core.Nothing
    }

-- | A unique name for the image builder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibName :: Lens.Lens' CreateImageBuilder Types.Name
cibName = Lens.field @"name"
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
cibInstanceType :: Lens.Lens' CreateImageBuilder Types.String
cibInstanceType = Lens.field @"instanceType"
{-# DEPRECATED cibInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The list of interface VPC endpoint (interface endpoint) objects. Administrators can connect to the image builder only through the specified endpoints.
--
-- /Note:/ Consider using 'accessEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibAccessEndpoints :: Lens.Lens' CreateImageBuilder (Core.Maybe (Core.NonEmpty Types.AccessEndpoint))
cibAccessEndpoints = Lens.field @"accessEndpoints"
{-# DEPRECATED cibAccessEndpoints "Use generic-lens or generic-optics with 'accessEndpoints' instead." #-}

-- | The version of the AppStream 2.0 agent to use for this image builder. To use the latest version of the AppStream 2.0 agent, specify [LATEST].
--
-- /Note:/ Consider using 'appstreamAgentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibAppstreamAgentVersion :: Lens.Lens' CreateImageBuilder (Core.Maybe Types.AppstreamAgentVersion)
cibAppstreamAgentVersion = Lens.field @"appstreamAgentVersion"
{-# DEPRECATED cibAppstreamAgentVersion "Use generic-lens or generic-optics with 'appstreamAgentVersion' instead." #-}

-- | The description to display.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibDescription :: Lens.Lens' CreateImageBuilder (Core.Maybe Types.Description)
cibDescription = Lens.field @"description"
{-# DEPRECATED cibDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The image builder name to display.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibDisplayName :: Lens.Lens' CreateImageBuilder (Core.Maybe Types.DisplayName)
cibDisplayName = Lens.field @"displayName"
{-# DEPRECATED cibDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The name of the directory and organizational unit (OU) to use to join the image builder to a Microsoft Active Directory domain.
--
-- /Note:/ Consider using 'domainJoinInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibDomainJoinInfo :: Lens.Lens' CreateImageBuilder (Core.Maybe Types.DomainJoinInfo)
cibDomainJoinInfo = Lens.field @"domainJoinInfo"
{-# DEPRECATED cibDomainJoinInfo "Use generic-lens or generic-optics with 'domainJoinInfo' instead." #-}

-- | Enables or disables default internet access for the image builder.
--
-- /Note:/ Consider using 'enableDefaultInternetAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibEnableDefaultInternetAccess :: Lens.Lens' CreateImageBuilder (Core.Maybe Core.Bool)
cibEnableDefaultInternetAccess = Lens.field @"enableDefaultInternetAccess"
{-# DEPRECATED cibEnableDefaultInternetAccess "Use generic-lens or generic-optics with 'enableDefaultInternetAccess' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role to apply to the image builder. To assume a role, the image builder calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance.
--
-- For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
--
-- /Note:/ Consider using 'iamRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibIamRoleArn :: Lens.Lens' CreateImageBuilder (Core.Maybe Types.Arn)
cibIamRoleArn = Lens.field @"iamRoleArn"
{-# DEPRECATED cibIamRoleArn "Use generic-lens or generic-optics with 'iamRoleArn' instead." #-}

-- | The ARN of the public, private, or shared image to use.
--
-- /Note:/ Consider using 'imageArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibImageArn :: Lens.Lens' CreateImageBuilder (Core.Maybe Types.Arn)
cibImageArn = Lens.field @"imageArn"
{-# DEPRECATED cibImageArn "Use generic-lens or generic-optics with 'imageArn' instead." #-}

-- | The name of the image used to create the image builder.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibImageName :: Lens.Lens' CreateImageBuilder (Core.Maybe Types.String)
cibImageName = Lens.field @"imageName"
{-# DEPRECATED cibImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

-- | The tags to associate with the image builder. A tag is a key-value pair, and the value is optional. For example, Environment=Test. If you do not specify a value, Environment=.
--
-- Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following special characters:
-- _ . : / = + \ - @
-- If you do not specify a value, the value is set to an empty string.
-- For more information about tags, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources> in the /Amazon AppStream 2.0 Administration Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibTags :: Lens.Lens' CreateImageBuilder (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
cibTags = Lens.field @"tags"
{-# DEPRECATED cibTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The VPC configuration for the image builder. You can specify only one subnet.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibVpcConfig :: Lens.Lens' CreateImageBuilder (Core.Maybe Types.VpcConfig)
cibVpcConfig = Lens.field @"vpcConfig"
{-# DEPRECATED cibVpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

instance Core.FromJSON CreateImageBuilder where
  toJSON CreateImageBuilder {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("InstanceType" Core..= instanceType),
            ("AccessEndpoints" Core..=) Core.<$> accessEndpoints,
            ("AppstreamAgentVersion" Core..=) Core.<$> appstreamAgentVersion,
            ("Description" Core..=) Core.<$> description,
            ("DisplayName" Core..=) Core.<$> displayName,
            ("DomainJoinInfo" Core..=) Core.<$> domainJoinInfo,
            ("EnableDefaultInternetAccess" Core..=)
              Core.<$> enableDefaultInternetAccess,
            ("IamRoleArn" Core..=) Core.<$> iamRoleArn,
            ("ImageArn" Core..=) Core.<$> imageArn,
            ("ImageName" Core..=) Core.<$> imageName,
            ("Tags" Core..=) Core.<$> tags,
            ("VpcConfig" Core..=) Core.<$> vpcConfig
          ]
      )

instance Core.AWSRequest CreateImageBuilder where
  type Rs CreateImageBuilder = CreateImageBuilderResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "PhotonAdminProxyService.CreateImageBuilder")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateImageBuilderResponse'
            Core.<$> (x Core..:? "ImageBuilder") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateImageBuilderResponse' smart constructor.
data CreateImageBuilderResponse = CreateImageBuilderResponse'
  { -- | Information about the image builder.
    imageBuilder :: Core.Maybe Types.ImageBuilder,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateImageBuilderResponse' value with any optional fields omitted.
mkCreateImageBuilderResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateImageBuilderResponse
mkCreateImageBuilderResponse responseStatus =
  CreateImageBuilderResponse'
    { imageBuilder = Core.Nothing,
      responseStatus
    }

-- | Information about the image builder.
--
-- /Note:/ Consider using 'imageBuilder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibrrsImageBuilder :: Lens.Lens' CreateImageBuilderResponse (Core.Maybe Types.ImageBuilder)
cibrrsImageBuilder = Lens.field @"imageBuilder"
{-# DEPRECATED cibrrsImageBuilder "Use generic-lens or generic-optics with 'imageBuilder' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibrrsResponseStatus :: Lens.Lens' CreateImageBuilderResponse Core.Int
cibrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cibrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
