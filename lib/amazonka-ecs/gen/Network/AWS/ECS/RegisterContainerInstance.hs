{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    rciPlatformDevices,
    rciInstanceIdentityDocumentSignature,
    rciCluster,
    rciInstanceIdentityDocument,
    rciContainerInstanceARN,
    rciVersionInfo,
    rciAttributes,
    rciTotalResources,
    rciTags,

    -- * Destructuring the response
    RegisterContainerInstanceResponse (..),
    mkRegisterContainerInstanceResponse,

    -- ** Response lenses
    rcirsContainerInstance,
    rcirsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRegisterContainerInstance' smart constructor.
data RegisterContainerInstance = RegisterContainerInstance'
  { platformDevices ::
      Lude.Maybe [PlatformDevice],
    instanceIdentityDocumentSignature ::
      Lude.Maybe Lude.Text,
    cluster :: Lude.Maybe Lude.Text,
    instanceIdentityDocument ::
      Lude.Maybe Lude.Text,
    containerInstanceARN ::
      Lude.Maybe Lude.Text,
    versionInfo :: Lude.Maybe VersionInfo,
    attributes :: Lude.Maybe [Attribute],
    totalResources :: Lude.Maybe [Resource],
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterContainerInstance' with the minimum fields required to make a request.
--
-- * 'attributes' - The container instance attributes that this container instance supports.
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster with which to register your container instance. If you do not specify a cluster, the default cluster is assumed.
-- * 'containerInstanceARN' - The ARN of the container instance (if it was previously registered).
-- * 'instanceIdentityDocument' - The instance identity document for the EC2 instance to register. This document can be found by running the following command from the instance: @curl http://169.254.169.254/latest/dynamic/instance-identity/document/@
-- * 'instanceIdentityDocumentSignature' - The instance identity document signature for the EC2 instance to register. This signature can be found by running the following command from the instance: @curl http://169.254.169.254/latest/dynamic/instance-identity/signature/@
-- * 'platformDevices' - The devices that are available on the container instance. The only supported device type is a GPU.
-- * 'tags' - The metadata that you apply to the container instance to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define.
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
-- * 'totalResources' - The resources available on the instance.
-- * 'versionInfo' - The version information for the Amazon ECS container agent and Docker daemon running on the container instance.
mkRegisterContainerInstance ::
  RegisterContainerInstance
mkRegisterContainerInstance =
  RegisterContainerInstance'
    { platformDevices = Lude.Nothing,
      instanceIdentityDocumentSignature = Lude.Nothing,
      cluster = Lude.Nothing,
      instanceIdentityDocument = Lude.Nothing,
      containerInstanceARN = Lude.Nothing,
      versionInfo = Lude.Nothing,
      attributes = Lude.Nothing,
      totalResources = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The devices that are available on the container instance. The only supported device type is a GPU.
--
-- /Note:/ Consider using 'platformDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciPlatformDevices :: Lens.Lens' RegisterContainerInstance (Lude.Maybe [PlatformDevice])
rciPlatformDevices = Lens.lens (platformDevices :: RegisterContainerInstance -> Lude.Maybe [PlatformDevice]) (\s a -> s {platformDevices = a} :: RegisterContainerInstance)
{-# DEPRECATED rciPlatformDevices "Use generic-lens or generic-optics with 'platformDevices' instead." #-}

-- | The instance identity document signature for the EC2 instance to register. This signature can be found by running the following command from the instance: @curl http://169.254.169.254/latest/dynamic/instance-identity/signature/@
--
-- /Note:/ Consider using 'instanceIdentityDocumentSignature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciInstanceIdentityDocumentSignature :: Lens.Lens' RegisterContainerInstance (Lude.Maybe Lude.Text)
rciInstanceIdentityDocumentSignature = Lens.lens (instanceIdentityDocumentSignature :: RegisterContainerInstance -> Lude.Maybe Lude.Text) (\s a -> s {instanceIdentityDocumentSignature = a} :: RegisterContainerInstance)
{-# DEPRECATED rciInstanceIdentityDocumentSignature "Use generic-lens or generic-optics with 'instanceIdentityDocumentSignature' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster with which to register your container instance. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciCluster :: Lens.Lens' RegisterContainerInstance (Lude.Maybe Lude.Text)
rciCluster = Lens.lens (cluster :: RegisterContainerInstance -> Lude.Maybe Lude.Text) (\s a -> s {cluster = a} :: RegisterContainerInstance)
{-# DEPRECATED rciCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The instance identity document for the EC2 instance to register. This document can be found by running the following command from the instance: @curl http://169.254.169.254/latest/dynamic/instance-identity/document/@
--
-- /Note:/ Consider using 'instanceIdentityDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciInstanceIdentityDocument :: Lens.Lens' RegisterContainerInstance (Lude.Maybe Lude.Text)
rciInstanceIdentityDocument = Lens.lens (instanceIdentityDocument :: RegisterContainerInstance -> Lude.Maybe Lude.Text) (\s a -> s {instanceIdentityDocument = a} :: RegisterContainerInstance)
{-# DEPRECATED rciInstanceIdentityDocument "Use generic-lens or generic-optics with 'instanceIdentityDocument' instead." #-}

-- | The ARN of the container instance (if it was previously registered).
--
-- /Note:/ Consider using 'containerInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciContainerInstanceARN :: Lens.Lens' RegisterContainerInstance (Lude.Maybe Lude.Text)
rciContainerInstanceARN = Lens.lens (containerInstanceARN :: RegisterContainerInstance -> Lude.Maybe Lude.Text) (\s a -> s {containerInstanceARN = a} :: RegisterContainerInstance)
{-# DEPRECATED rciContainerInstanceARN "Use generic-lens or generic-optics with 'containerInstanceARN' instead." #-}

-- | The version information for the Amazon ECS container agent and Docker daemon running on the container instance.
--
-- /Note:/ Consider using 'versionInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciVersionInfo :: Lens.Lens' RegisterContainerInstance (Lude.Maybe VersionInfo)
rciVersionInfo = Lens.lens (versionInfo :: RegisterContainerInstance -> Lude.Maybe VersionInfo) (\s a -> s {versionInfo = a} :: RegisterContainerInstance)
{-# DEPRECATED rciVersionInfo "Use generic-lens or generic-optics with 'versionInfo' instead." #-}

-- | The container instance attributes that this container instance supports.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciAttributes :: Lens.Lens' RegisterContainerInstance (Lude.Maybe [Attribute])
rciAttributes = Lens.lens (attributes :: RegisterContainerInstance -> Lude.Maybe [Attribute]) (\s a -> s {attributes = a} :: RegisterContainerInstance)
{-# DEPRECATED rciAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The resources available on the instance.
--
-- /Note:/ Consider using 'totalResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rciTotalResources :: Lens.Lens' RegisterContainerInstance (Lude.Maybe [Resource])
rciTotalResources = Lens.lens (totalResources :: RegisterContainerInstance -> Lude.Maybe [Resource]) (\s a -> s {totalResources = a} :: RegisterContainerInstance)
{-# DEPRECATED rciTotalResources "Use generic-lens or generic-optics with 'totalResources' instead." #-}

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
rciTags :: Lens.Lens' RegisterContainerInstance (Lude.Maybe [Tag])
rciTags = Lens.lens (tags :: RegisterContainerInstance -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: RegisterContainerInstance)
{-# DEPRECATED rciTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest RegisterContainerInstance where
  type
    Rs RegisterContainerInstance =
      RegisterContainerInstanceResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          RegisterContainerInstanceResponse'
            Lude.<$> (x Lude..?> "containerInstance")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterContainerInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.RegisterContainerInstance" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterContainerInstance where
  toJSON RegisterContainerInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("platformDevices" Lude..=) Lude.<$> platformDevices,
            ("instanceIdentityDocumentSignature" Lude..=)
              Lude.<$> instanceIdentityDocumentSignature,
            ("cluster" Lude..=) Lude.<$> cluster,
            ("instanceIdentityDocument" Lude..=)
              Lude.<$> instanceIdentityDocument,
            ("containerInstanceArn" Lude..=) Lude.<$> containerInstanceARN,
            ("versionInfo" Lude..=) Lude.<$> versionInfo,
            ("attributes" Lude..=) Lude.<$> attributes,
            ("totalResources" Lude..=) Lude.<$> totalResources,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath RegisterContainerInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterContainerInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterContainerInstanceResponse' smart constructor.
data RegisterContainerInstanceResponse = RegisterContainerInstanceResponse'
  { containerInstance ::
      Lude.Maybe
        ContainerInstance,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterContainerInstanceResponse' with the minimum fields required to make a request.
--
-- * 'containerInstance' - The container instance that was registered.
-- * 'responseStatus' - The response status code.
mkRegisterContainerInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterContainerInstanceResponse
mkRegisterContainerInstanceResponse pResponseStatus_ =
  RegisterContainerInstanceResponse'
    { containerInstance =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The container instance that was registered.
--
-- /Note:/ Consider using 'containerInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcirsContainerInstance :: Lens.Lens' RegisterContainerInstanceResponse (Lude.Maybe ContainerInstance)
rcirsContainerInstance = Lens.lens (containerInstance :: RegisterContainerInstanceResponse -> Lude.Maybe ContainerInstance) (\s a -> s {containerInstance = a} :: RegisterContainerInstanceResponse)
{-# DEPRECATED rcirsContainerInstance "Use generic-lens or generic-optics with 'containerInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcirsResponseStatus :: Lens.Lens' RegisterContainerInstanceResponse Lude.Int
rcirsResponseStatus = Lens.lens (responseStatus :: RegisterContainerInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterContainerInstanceResponse)
{-# DEPRECATED rcirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
