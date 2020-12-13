{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.CreateContainer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a storage container to hold objects. A container is similar to a bucket in the Amazon S3 service.
module Network.AWS.MediaStore.CreateContainer
  ( -- * Creating a request
    CreateContainer (..),
    mkCreateContainer,

    -- ** Request lenses
    ccContainerName,
    ccTags,

    -- * Destructuring the response
    CreateContainerResponse (..),
    mkCreateContainerResponse,

    -- ** Response lenses
    ccrsContainer,
    ccrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateContainer' smart constructor.
data CreateContainer = CreateContainer'
  { -- | The name for the container. The name must be from 1 to 255 characters. Container names must be unique to your AWS account within a specific region. As an example, you could create a container named @movies@ in every region, as long as you don’t have an existing container with that name.
    containerName :: Lude.Text,
    -- | An array of key:value pairs that you define. These values can be anything that you want. Typically, the tag key represents a category (such as "environment") and the tag value represents a specific value within that category (such as "test," "development," or "production"). You can add up to 50 tags to each container. For more information about tagging, including naming and usage conventions, see <https://docs.aws.amazon.com/mediastore/latest/ug/tagging.html Tagging Resources in MediaStore> .
    tags :: Lude.Maybe (Lude.NonEmpty Tag)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateContainer' with the minimum fields required to make a request.
--
-- * 'containerName' - The name for the container. The name must be from 1 to 255 characters. Container names must be unique to your AWS account within a specific region. As an example, you could create a container named @movies@ in every region, as long as you don’t have an existing container with that name.
-- * 'tags' - An array of key:value pairs that you define. These values can be anything that you want. Typically, the tag key represents a category (such as "environment") and the tag value represents a specific value within that category (such as "test," "development," or "production"). You can add up to 50 tags to each container. For more information about tagging, including naming and usage conventions, see <https://docs.aws.amazon.com/mediastore/latest/ug/tagging.html Tagging Resources in MediaStore> .
mkCreateContainer ::
  -- | 'containerName'
  Lude.Text ->
  CreateContainer
mkCreateContainer pContainerName_ =
  CreateContainer'
    { containerName = pContainerName_,
      tags = Lude.Nothing
    }

-- | The name for the container. The name must be from 1 to 255 characters. Container names must be unique to your AWS account within a specific region. As an example, you could create a container named @movies@ in every region, as long as you don’t have an existing container with that name.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccContainerName :: Lens.Lens' CreateContainer Lude.Text
ccContainerName = Lens.lens (containerName :: CreateContainer -> Lude.Text) (\s a -> s {containerName = a} :: CreateContainer)
{-# DEPRECATED ccContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

-- | An array of key:value pairs that you define. These values can be anything that you want. Typically, the tag key represents a category (such as "environment") and the tag value represents a specific value within that category (such as "test," "development," or "production"). You can add up to 50 tags to each container. For more information about tagging, including naming and usage conventions, see <https://docs.aws.amazon.com/mediastore/latest/ug/tagging.html Tagging Resources in MediaStore> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTags :: Lens.Lens' CreateContainer (Lude.Maybe (Lude.NonEmpty Tag))
ccTags = Lens.lens (tags :: CreateContainer -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: CreateContainer)
{-# DEPRECATED ccTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateContainer where
  type Rs CreateContainer = CreateContainerResponse
  request = Req.postJSON mediaStoreService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateContainerResponse'
            Lude.<$> (x Lude..:> "Container") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateContainer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("MediaStore_20170901.CreateContainer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateContainer where
  toJSON CreateContainer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ContainerName" Lude..= containerName),
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateContainer where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateContainer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateContainerResponse' smart constructor.
data CreateContainerResponse = CreateContainerResponse'
  { -- | ContainerARN: The Amazon Resource Name (ARN) of the newly created container. The ARN has the following format: arn:aws:<region>:<account that owns this container>:container/<name of container>. For example: arn:aws:mediastore:us-west-2:111122223333:container/movies
    --
    -- ContainerName: The container name as specified in the request.
    -- CreationTime: Unix time stamp.
    -- Status: The status of container creation or deletion. The status is one of the following: @CREATING@ , @ACTIVE@ , or @DELETING@ . While the service is creating the container, the status is @CREATING@ . When an endpoint is available, the status changes to @ACTIVE@ .
    -- The return value does not include the container's endpoint. To make downstream requests, you must obtain this value by using 'DescribeContainer' or 'ListContainers' .
    container :: Container,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateContainerResponse' with the minimum fields required to make a request.
--
-- * 'container' - ContainerARN: The Amazon Resource Name (ARN) of the newly created container. The ARN has the following format: arn:aws:<region>:<account that owns this container>:container/<name of container>. For example: arn:aws:mediastore:us-west-2:111122223333:container/movies
--
-- ContainerName: The container name as specified in the request.
-- CreationTime: Unix time stamp.
-- Status: The status of container creation or deletion. The status is one of the following: @CREATING@ , @ACTIVE@ , or @DELETING@ . While the service is creating the container, the status is @CREATING@ . When an endpoint is available, the status changes to @ACTIVE@ .
-- The return value does not include the container's endpoint. To make downstream requests, you must obtain this value by using 'DescribeContainer' or 'ListContainers' .
-- * 'responseStatus' - The response status code.
mkCreateContainerResponse ::
  -- | 'container'
  Container ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateContainerResponse
mkCreateContainerResponse pContainer_ pResponseStatus_ =
  CreateContainerResponse'
    { container = pContainer_,
      responseStatus = pResponseStatus_
    }

-- | ContainerARN: The Amazon Resource Name (ARN) of the newly created container. The ARN has the following format: arn:aws:<region>:<account that owns this container>:container/<name of container>. For example: arn:aws:mediastore:us-west-2:111122223333:container/movies
--
-- ContainerName: The container name as specified in the request.
-- CreationTime: Unix time stamp.
-- Status: The status of container creation or deletion. The status is one of the following: @CREATING@ , @ACTIVE@ , or @DELETING@ . While the service is creating the container, the status is @CREATING@ . When an endpoint is available, the status changes to @ACTIVE@ .
-- The return value does not include the container's endpoint. To make downstream requests, you must obtain this value by using 'DescribeContainer' or 'ListContainers' .
--
-- /Note:/ Consider using 'container' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsContainer :: Lens.Lens' CreateContainerResponse Container
ccrsContainer = Lens.lens (container :: CreateContainerResponse -> Container) (\s a -> s {container = a} :: CreateContainerResponse)
{-# DEPRECATED ccrsContainer "Use generic-lens or generic-optics with 'container' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsResponseStatus :: Lens.Lens' CreateContainerResponse Lude.Int
ccrsResponseStatus = Lens.lens (responseStatus :: CreateContainerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateContainerResponse)
{-# DEPRECATED ccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
