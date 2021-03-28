{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateImageBuilder (..)
    , mkCreateImageBuilder
    -- ** Request lenses
    , cibName
    , cibInstanceType
    , cibAccessEndpoints
    , cibAppstreamAgentVersion
    , cibDescription
    , cibDisplayName
    , cibDomainJoinInfo
    , cibEnableDefaultInternetAccess
    , cibIamRoleArn
    , cibImageArn
    , cibImageName
    , cibTags
    , cibVpcConfig

    -- * Destructuring the response
    , CreateImageBuilderResponse (..)
    , mkCreateImageBuilderResponse
    -- ** Response lenses
    , cibrrsImageBuilder
    , cibrrsResponseStatus
    ) where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateImageBuilder' smart constructor.
data CreateImageBuilder = CreateImageBuilder'
  { name :: Types.Name
    -- ^ A unique name for the image builder.
  , instanceType :: Core.Text
    -- ^ The instance type to use when launching the image builder. The following instance types are available:
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
  , accessEndpoints :: Core.Maybe (Core.NonEmpty Types.AccessEndpoint)
    -- ^ The list of interface VPC endpoint (interface endpoint) objects. Administrators can connect to the image builder only through the specified endpoints.
  , appstreamAgentVersion :: Core.Maybe Types.AppstreamAgentVersion
    -- ^ The version of the AppStream 2.0 agent to use for this image builder. To use the latest version of the AppStream 2.0 agent, specify [LATEST]. 
  , description :: Core.Maybe Types.Description
    -- ^ The description to display.
  , displayName :: Core.Maybe Types.DisplayName
    -- ^ The image builder name to display.
  , domainJoinInfo :: Core.Maybe Types.DomainJoinInfo
    -- ^ The name of the directory and organizational unit (OU) to use to join the image builder to a Microsoft Active Directory domain. 
  , enableDefaultInternetAccess :: Core.Maybe Core.Bool
    -- ^ Enables or disables default internet access for the image builder.
  , iamRoleArn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the IAM role to apply to the image builder. To assume a role, the image builder calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance.
--
-- For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
  , imageArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the public, private, or shared image to use.
  , imageName :: Core.Maybe Core.Text
    -- ^ The name of the image used to create the image builder.
  , tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
    -- ^ The tags to associate with the image builder. A tag is a key-value pair, and the value is optional. For example, Environment=Test. If you do not specify a value, Environment=. 
--
-- Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following special characters: 
-- _ . : / = + \ - @
-- If you do not specify a value, the value is set to an empty string.
-- For more information about tags, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources> in the /Amazon AppStream 2.0 Administration Guide/ .
  , vpcConfig :: Core.Maybe Types.VpcConfig
    -- ^ The VPC configuration for the image builder. You can specify only one subnet.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateImageBuilder' value with any optional fields omitted.
mkCreateImageBuilder
    :: Types.Name -- ^ 'name'
    -> Core.Text -- ^ 'instanceType'
    -> CreateImageBuilder
mkCreateImageBuilder name instanceType
  = CreateImageBuilder'{name, instanceType,
                        accessEndpoints = Core.Nothing,
                        appstreamAgentVersion = Core.Nothing, description = Core.Nothing,
                        displayName = Core.Nothing, domainJoinInfo = Core.Nothing,
                        enableDefaultInternetAccess = Core.Nothing,
                        iamRoleArn = Core.Nothing, imageArn = Core.Nothing,
                        imageName = Core.Nothing, tags = Core.Nothing,
                        vpcConfig = Core.Nothing}

-- | A unique name for the image builder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibName :: Lens.Lens' CreateImageBuilder Types.Name
cibName = Lens.field @"name"
{-# INLINEABLE cibName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

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
cibInstanceType :: Lens.Lens' CreateImageBuilder Core.Text
cibInstanceType = Lens.field @"instanceType"
{-# INLINEABLE cibInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The list of interface VPC endpoint (interface endpoint) objects. Administrators can connect to the image builder only through the specified endpoints.
--
-- /Note:/ Consider using 'accessEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibAccessEndpoints :: Lens.Lens' CreateImageBuilder (Core.Maybe (Core.NonEmpty Types.AccessEndpoint))
cibAccessEndpoints = Lens.field @"accessEndpoints"
{-# INLINEABLE cibAccessEndpoints #-}
{-# DEPRECATED accessEndpoints "Use generic-lens or generic-optics with 'accessEndpoints' instead"  #-}

-- | The version of the AppStream 2.0 agent to use for this image builder. To use the latest version of the AppStream 2.0 agent, specify [LATEST]. 
--
-- /Note:/ Consider using 'appstreamAgentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibAppstreamAgentVersion :: Lens.Lens' CreateImageBuilder (Core.Maybe Types.AppstreamAgentVersion)
cibAppstreamAgentVersion = Lens.field @"appstreamAgentVersion"
{-# INLINEABLE cibAppstreamAgentVersion #-}
{-# DEPRECATED appstreamAgentVersion "Use generic-lens or generic-optics with 'appstreamAgentVersion' instead"  #-}

-- | The description to display.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibDescription :: Lens.Lens' CreateImageBuilder (Core.Maybe Types.Description)
cibDescription = Lens.field @"description"
{-# INLINEABLE cibDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The image builder name to display.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibDisplayName :: Lens.Lens' CreateImageBuilder (Core.Maybe Types.DisplayName)
cibDisplayName = Lens.field @"displayName"
{-# INLINEABLE cibDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | The name of the directory and organizational unit (OU) to use to join the image builder to a Microsoft Active Directory domain. 
--
-- /Note:/ Consider using 'domainJoinInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibDomainJoinInfo :: Lens.Lens' CreateImageBuilder (Core.Maybe Types.DomainJoinInfo)
cibDomainJoinInfo = Lens.field @"domainJoinInfo"
{-# INLINEABLE cibDomainJoinInfo #-}
{-# DEPRECATED domainJoinInfo "Use generic-lens or generic-optics with 'domainJoinInfo' instead"  #-}

-- | Enables or disables default internet access for the image builder.
--
-- /Note:/ Consider using 'enableDefaultInternetAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibEnableDefaultInternetAccess :: Lens.Lens' CreateImageBuilder (Core.Maybe Core.Bool)
cibEnableDefaultInternetAccess = Lens.field @"enableDefaultInternetAccess"
{-# INLINEABLE cibEnableDefaultInternetAccess #-}
{-# DEPRECATED enableDefaultInternetAccess "Use generic-lens or generic-optics with 'enableDefaultInternetAccess' instead"  #-}

-- | The Amazon Resource Name (ARN) of the IAM role to apply to the image builder. To assume a role, the image builder calls the AWS Security Token Service (STS) @AssumeRole@ API operation and passes the ARN of the role to use. The operation creates a new session with temporary credentials. AppStream 2.0 retrieves the temporary credentials and creates the __appstream_machine_role__ credential profile on the instance.
--
-- For more information, see <https://docs.aws.amazon.com/appstream2/latest/developerguide/using-iam-roles-to-grant-permissions-to-applications-scripts-streaming-instances.html Using an IAM Role to Grant Permissions to Applications and Scripts Running on AppStream 2.0 Streaming Instances> in the /Amazon AppStream 2.0 Administration Guide/ .
--
-- /Note:/ Consider using 'iamRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibIamRoleArn :: Lens.Lens' CreateImageBuilder (Core.Maybe Types.Arn)
cibIamRoleArn = Lens.field @"iamRoleArn"
{-# INLINEABLE cibIamRoleArn #-}
{-# DEPRECATED iamRoleArn "Use generic-lens or generic-optics with 'iamRoleArn' instead"  #-}

-- | The ARN of the public, private, or shared image to use.
--
-- /Note:/ Consider using 'imageArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibImageArn :: Lens.Lens' CreateImageBuilder (Core.Maybe Types.Arn)
cibImageArn = Lens.field @"imageArn"
{-# INLINEABLE cibImageArn #-}
{-# DEPRECATED imageArn "Use generic-lens or generic-optics with 'imageArn' instead"  #-}

-- | The name of the image used to create the image builder.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibImageName :: Lens.Lens' CreateImageBuilder (Core.Maybe Core.Text)
cibImageName = Lens.field @"imageName"
{-# INLINEABLE cibImageName #-}
{-# DEPRECATED imageName "Use generic-lens or generic-optics with 'imageName' instead"  #-}

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
{-# INLINEABLE cibTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The VPC configuration for the image builder. You can specify only one subnet.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibVpcConfig :: Lens.Lens' CreateImageBuilder (Core.Maybe Types.VpcConfig)
cibVpcConfig = Lens.field @"vpcConfig"
{-# INLINEABLE cibVpcConfig #-}
{-# DEPRECATED vpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead"  #-}

instance Core.ToQuery CreateImageBuilder where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateImageBuilder where
        toHeaders CreateImageBuilder{..}
          = Core.pure
              ("X-Amz-Target", "PhotonAdminProxyService.CreateImageBuilder")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateImageBuilder where
        toJSON CreateImageBuilder{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("InstanceType" Core..= instanceType),
                  ("AccessEndpoints" Core..=) Core.<$> accessEndpoints,
                  ("AppstreamAgentVersion" Core..=) Core.<$> appstreamAgentVersion,
                  ("Description" Core..=) Core.<$> description,
                  ("DisplayName" Core..=) Core.<$> displayName,
                  ("DomainJoinInfo" Core..=) Core.<$> domainJoinInfo,
                  ("EnableDefaultInternetAccess" Core..=) Core.<$>
                    enableDefaultInternetAccess,
                  ("IamRoleArn" Core..=) Core.<$> iamRoleArn,
                  ("ImageArn" Core..=) Core.<$> imageArn,
                  ("ImageName" Core..=) Core.<$> imageName,
                  ("Tags" Core..=) Core.<$> tags,
                  ("VpcConfig" Core..=) Core.<$> vpcConfig])

instance Core.AWSRequest CreateImageBuilder where
        type Rs CreateImageBuilder = CreateImageBuilderResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateImageBuilderResponse' Core.<$>
                   (x Core..:? "ImageBuilder") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateImageBuilderResponse' smart constructor.
data CreateImageBuilderResponse = CreateImageBuilderResponse'
  { imageBuilder :: Core.Maybe Types.ImageBuilder
    -- ^ Information about the image builder.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateImageBuilderResponse' value with any optional fields omitted.
mkCreateImageBuilderResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateImageBuilderResponse
mkCreateImageBuilderResponse responseStatus
  = CreateImageBuilderResponse'{imageBuilder = Core.Nothing,
                                responseStatus}

-- | Information about the image builder.
--
-- /Note:/ Consider using 'imageBuilder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibrrsImageBuilder :: Lens.Lens' CreateImageBuilderResponse (Core.Maybe Types.ImageBuilder)
cibrrsImageBuilder = Lens.field @"imageBuilder"
{-# INLINEABLE cibrrsImageBuilder #-}
{-# DEPRECATED imageBuilder "Use generic-lens or generic-optics with 'imageBuilder' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibrrsResponseStatus :: Lens.Lens' CreateImageBuilderResponse Core.Int
cibrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cibrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
