{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.RegisterContainerInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an EC2 instance into the specified cluster. This instance becomes available to place containers on.
module Network.AWS.ECS.RegisterContainerInstance
  ( -- * Creating a request
    RegisterContainerInstance (..),
    mkRegisterContainerInstance,

    -- ** Request lenses
    rciAttributes,
    rciCluster,
    rciContainerInstanceArn,
    rciInstanceIdentityDocument,
    rciInstanceIdentityDocumentSignature,
    rciPlatformDevices,
    rciTags,
    rciTotalResources,
    rciVersionInfo,

    -- * Destructuring the response
    RegisterContainerInstanceResponse (..),
    mkRegisterContainerInstanceResponse,

    -- ** Response lenses
    rcirrsContainerInstance,
    rcirrsResponseStatus,
  )
where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterContainerInstance' smart constructor.
data RegisterContainerInstance = RegisterContainerInstance'
  { -- | The container instance attributes that this container instance supports.
    attributes :: Core.Maybe [Types.Attribute],
    -- | The short name or full Amazon Resource Name (ARN) of the cluster with which to register your container instance. If you do not specify a cluster, the default cluster is assumed.
    cluster :: Core.Maybe Types.String,
    -- | The ARN of the container instance (if it was previously registered).
    containerInstanceArn :: Core.Maybe Types.String,
    -- | The instance identity document for the EC2 instance to register. This document can be found by running the following command from the instance: @curl http://169.254.169.254/latest/dynamic/instance-identity/document/@
    instanceIdentityDocument :: Core.Maybe Types.String,
    -- | The instance identity document signature for the EC2 instance to register. This signature can be found by running the following command from the instance: @curl http://169.254.169.254/latest/dynamic/instance-identity/signature/@
    instanceIdentityDocumentSignature :: Core.Maybe Types.String,
    -- | The devices that are available on the container instance. The only supported device type is a GPU.
    platformDevices :: Core.Maybe [Types.PlatformDevice],
    -- | The metadata that you apply to the container instance to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
    --
    -- The following basic restrictions apply to tags:
    --
    --     * Maximum number of tags per resource - 50
    --
    --
    --     * For each resource, each tag key must be unique, and each tag key can have only one value.
    --
    --
    --     * Maximum key length - 128 Unicode characters in UTF-8
    --
    --
    --     * Maximum value length - 256 Unicode characters in UTF-8
    --
    --
    --     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.
    --
    --
    --     * Tag keys and values are case-sensitive.
    --
    --
    --     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
    tags :: Core.Maybe [Types.Tag],
    -- | The resources available on the instance.
    totalResources :: Core.Maybe [Types.Resource],
    -- | The version information for the Amazon ECS container agent and Docker daemon running on the container instance.
    versionInfo :: Core.Maybe Types.VersionInfo
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterContainerInstance' value with any optional fields omitted.
mkRegisterContainerInstance ::
  RegisterContainerInstance
mkRegisterContainerInstance =
  RegisterContainerInstance'
    { attributes = Core.Nothing,
      cluster = Core.Nothing,
      containerInstanceArn = Core.Nothing,
      instanceIdentityDocument = Core.Nothing,
      instanceIdentityDocumentSignature = Core.Nothing,
      platformDevices = Core.Nothing,
      tags = Core.Nothing,
      totalResources = Core.Nothing,
      versionInfo = Core.Nothing
    }

-- | The container instance attributes that this container instance supports.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciAttributes :: Lens.Lens' RegisterContainerInstance (Core.Maybe [Types.Attribute])
rciAttributes = Lens.field @"attributes"
{-# DEPRECATED rciAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster with which to register your container instance. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciCluster :: Lens.Lens' RegisterContainerInstance (Core.Maybe Types.String)
rciCluster = Lens.field @"cluster"
{-# DEPRECATED rciCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The ARN of the container instance (if it was previously registered).
--
-- /Note:/ Consider using 'containerInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciContainerInstanceArn :: Lens.Lens' RegisterContainerInstance (Core.Maybe Types.String)
rciContainerInstanceArn = Lens.field @"containerInstanceArn"
{-# DEPRECATED rciContainerInstanceArn "Use generic-lens or generic-optics with 'containerInstanceArn' instead." #-}

-- | The instance identity document for the EC2 instance to register. This document can be found by running the following command from the instance: @curl http://169.254.169.254/latest/dynamic/instance-identity/document/@
--
-- /Note:/ Consider using 'instanceIdentityDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciInstanceIdentityDocument :: Lens.Lens' RegisterContainerInstance (Core.Maybe Types.String)
rciInstanceIdentityDocument = Lens.field @"instanceIdentityDocument"
{-# DEPRECATED rciInstanceIdentityDocument "Use generic-lens or generic-optics with 'instanceIdentityDocument' instead." #-}

-- | The instance identity document signature for the EC2 instance to register. This signature can be found by running the following command from the instance: @curl http://169.254.169.254/latest/dynamic/instance-identity/signature/@
--
-- /Note:/ Consider using 'instanceIdentityDocumentSignature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciInstanceIdentityDocumentSignature :: Lens.Lens' RegisterContainerInstance (Core.Maybe Types.String)
rciInstanceIdentityDocumentSignature = Lens.field @"instanceIdentityDocumentSignature"
{-# DEPRECATED rciInstanceIdentityDocumentSignature "Use generic-lens or generic-optics with 'instanceIdentityDocumentSignature' instead." #-}

-- | The devices that are available on the container instance. The only supported device type is a GPU.
--
-- /Note:/ Consider using 'platformDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciPlatformDevices :: Lens.Lens' RegisterContainerInstance (Core.Maybe [Types.PlatformDevice])
rciPlatformDevices = Lens.field @"platformDevices"
{-# DEPRECATED rciPlatformDevices "Use generic-lens or generic-optics with 'platformDevices' instead." #-}

-- | The metadata that you apply to the container instance to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
--
-- The following basic restrictions apply to tags:
--
--     * Maximum number of tags per resource - 50
--
--
--     * For each resource, each tag key must be unique, and each tag key can have only one value.
--
--
--     * Maximum key length - 128 Unicode characters in UTF-8
--
--
--     * Maximum value length - 256 Unicode characters in UTF-8
--
--
--     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.
--
--
--     * Tag keys and values are case-sensitive.
--
--
--     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciTags :: Lens.Lens' RegisterContainerInstance (Core.Maybe [Types.Tag])
rciTags = Lens.field @"tags"
{-# DEPRECATED rciTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The resources available on the instance.
--
-- /Note:/ Consider using 'totalResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciTotalResources :: Lens.Lens' RegisterContainerInstance (Core.Maybe [Types.Resource])
rciTotalResources = Lens.field @"totalResources"
{-# DEPRECATED rciTotalResources "Use generic-lens or generic-optics with 'totalResources' instead." #-}

-- | The version information for the Amazon ECS container agent and Docker daemon running on the container instance.
--
-- /Note:/ Consider using 'versionInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciVersionInfo :: Lens.Lens' RegisterContainerInstance (Core.Maybe Types.VersionInfo)
rciVersionInfo = Lens.field @"versionInfo"
{-# DEPRECATED rciVersionInfo "Use generic-lens or generic-optics with 'versionInfo' instead." #-}

instance Core.FromJSON RegisterContainerInstance where
  toJSON RegisterContainerInstance {..} =
    Core.object
      ( Core.catMaybes
          [ ("attributes" Core..=) Core.<$> attributes,
            ("cluster" Core..=) Core.<$> cluster,
            ("containerInstanceArn" Core..=) Core.<$> containerInstanceArn,
            ("instanceIdentityDocument" Core..=)
              Core.<$> instanceIdentityDocument,
            ("instanceIdentityDocumentSignature" Core..=)
              Core.<$> instanceIdentityDocumentSignature,
            ("platformDevices" Core..=) Core.<$> platformDevices,
            ("tags" Core..=) Core.<$> tags,
            ("totalResources" Core..=) Core.<$> totalResources,
            ("versionInfo" Core..=) Core.<$> versionInfo
          ]
      )

instance Core.AWSRequest RegisterContainerInstance where
  type
    Rs RegisterContainerInstance =
      RegisterContainerInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerServiceV20141113.RegisterContainerInstance"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterContainerInstanceResponse'
            Core.<$> (x Core..:? "containerInstance")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRegisterContainerInstanceResponse' smart constructor.
data RegisterContainerInstanceResponse = RegisterContainerInstanceResponse'
  { -- | The container instance that was registered.
    containerInstance :: Core.Maybe Types.ContainerInstance,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RegisterContainerInstanceResponse' value with any optional fields omitted.
mkRegisterContainerInstanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RegisterContainerInstanceResponse
mkRegisterContainerInstanceResponse responseStatus =
  RegisterContainerInstanceResponse'
    { containerInstance =
        Core.Nothing,
      responseStatus
    }

-- | The container instance that was registered.
--
-- /Note:/ Consider using 'containerInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcirrsContainerInstance :: Lens.Lens' RegisterContainerInstanceResponse (Core.Maybe Types.ContainerInstance)
rcirrsContainerInstance = Lens.field @"containerInstance"
{-# DEPRECATED rcirrsContainerInstance "Use generic-lens or generic-optics with 'containerInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcirrsResponseStatus :: Lens.Lens' RegisterContainerInstanceResponse Core.Int
rcirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rcirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
