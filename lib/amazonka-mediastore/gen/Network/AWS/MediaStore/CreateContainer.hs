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
    ccrrsContainer,
    ccrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateContainer' smart constructor.
data CreateContainer = CreateContainer'
  { -- | The name for the container. The name must be from 1 to 255 characters. Container names must be unique to your AWS account within a specific region. As an example, you could create a container named @movies@ in every region, as long as you don’t have an existing container with that name.
    containerName :: Types.ContainerName,
    -- | An array of key:value pairs that you define. These values can be anything that you want. Typically, the tag key represents a category (such as "environment") and the tag value represents a specific value within that category (such as "test," "development," or "production"). You can add up to 50 tags to each container. For more information about tagging, including naming and usage conventions, see <https://docs.aws.amazon.com/mediastore/latest/ug/tagging.html Tagging Resources in MediaStore> .
    tags :: Core.Maybe (Core.NonEmpty Types.Tag)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateContainer' value with any optional fields omitted.
mkCreateContainer ::
  -- | 'containerName'
  Types.ContainerName ->
  CreateContainer
mkCreateContainer containerName =
  CreateContainer' {containerName, tags = Core.Nothing}

-- | The name for the container. The name must be from 1 to 255 characters. Container names must be unique to your AWS account within a specific region. As an example, you could create a container named @movies@ in every region, as long as you don’t have an existing container with that name.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccContainerName :: Lens.Lens' CreateContainer Types.ContainerName
ccContainerName = Lens.field @"containerName"
{-# DEPRECATED ccContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

-- | An array of key:value pairs that you define. These values can be anything that you want. Typically, the tag key represents a category (such as "environment") and the tag value represents a specific value within that category (such as "test," "development," or "production"). You can add up to 50 tags to each container. For more information about tagging, including naming and usage conventions, see <https://docs.aws.amazon.com/mediastore/latest/ug/tagging.html Tagging Resources in MediaStore> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTags :: Lens.Lens' CreateContainer (Core.Maybe (Core.NonEmpty Types.Tag))
ccTags = Lens.field @"tags"
{-# DEPRECATED ccTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateContainer where
  toJSON CreateContainer {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ContainerName" Core..= containerName),
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateContainer where
  type Rs CreateContainer = CreateContainerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "MediaStore_20170901.CreateContainer")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateContainerResponse'
            Core.<$> (x Core..: "Container") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateContainerResponse' smart constructor.
data CreateContainerResponse = CreateContainerResponse'
  { -- | ContainerARN: The Amazon Resource Name (ARN) of the newly created container. The ARN has the following format: arn:aws:<region>:<account that owns this container>:container/<name of container>. For example: arn:aws:mediastore:us-west-2:111122223333:container/movies
    --
    -- ContainerName: The container name as specified in the request.
    -- CreationTime: Unix time stamp.
    -- Status: The status of container creation or deletion. The status is one of the following: @CREATING@ , @ACTIVE@ , or @DELETING@ . While the service is creating the container, the status is @CREATING@ . When an endpoint is available, the status changes to @ACTIVE@ .
    -- The return value does not include the container's endpoint. To make downstream requests, you must obtain this value by using 'DescribeContainer' or 'ListContainers' .
    container :: Types.Container,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateContainerResponse' value with any optional fields omitted.
mkCreateContainerResponse ::
  -- | 'container'
  Types.Container ->
  -- | 'responseStatus'
  Core.Int ->
  CreateContainerResponse
mkCreateContainerResponse container responseStatus =
  CreateContainerResponse' {container, responseStatus}

-- | ContainerARN: The Amazon Resource Name (ARN) of the newly created container. The ARN has the following format: arn:aws:<region>:<account that owns this container>:container/<name of container>. For example: arn:aws:mediastore:us-west-2:111122223333:container/movies
--
-- ContainerName: The container name as specified in the request.
-- CreationTime: Unix time stamp.
-- Status: The status of container creation or deletion. The status is one of the following: @CREATING@ , @ACTIVE@ , or @DELETING@ . While the service is creating the container, the status is @CREATING@ . When an endpoint is available, the status changes to @ACTIVE@ .
-- The return value does not include the container's endpoint. To make downstream requests, you must obtain this value by using 'DescribeContainer' or 'ListContainers' .
--
-- /Note:/ Consider using 'container' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsContainer :: Lens.Lens' CreateContainerResponse Types.Container
ccrrsContainer = Lens.field @"container"
{-# DEPRECATED ccrrsContainer "Use generic-lens or generic-optics with 'container' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CreateContainerResponse Core.Int
ccrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
