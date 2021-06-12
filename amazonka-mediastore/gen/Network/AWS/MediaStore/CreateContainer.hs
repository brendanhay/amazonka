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
-- Module      : Network.AWS.MediaStore.CreateContainer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a storage container to hold objects. A container is similar to a
-- bucket in the Amazon S3 service.
module Network.AWS.MediaStore.CreateContainer
  ( -- * Creating a Request
    CreateContainer (..),
    newCreateContainer,

    -- * Request Lenses
    createContainer_tags,
    createContainer_containerName,

    -- * Destructuring the Response
    CreateContainerResponse (..),
    newCreateContainerResponse,

    -- * Response Lenses
    createContainerResponse_httpStatus,
    createContainerResponse_container,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateContainer' smart constructor.
data CreateContainer = CreateContainer'
  { -- | An array of key:value pairs that you define. These values can be
    -- anything that you want. Typically, the tag key represents a category
    -- (such as \"environment\") and the tag value represents a specific value
    -- within that category (such as \"test,\" \"development,\" or
    -- \"production\"). You can add up to 50 tags to each container. For more
    -- information about tagging, including naming and usage conventions, see
    -- <https://docs.aws.amazon.com/mediastore/latest/ug/tagging.html Tagging Resources in MediaStore>.
    tags :: Core.Maybe (Core.NonEmpty Tag),
    -- | The name for the container. The name must be from 1 to 255 characters.
    -- Container names must be unique to your AWS account within a specific
    -- region. As an example, you could create a container named @movies@ in
    -- every region, as long as you don’t have an existing container with that
    -- name.
    containerName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateContainer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createContainer_tags' - An array of key:value pairs that you define. These values can be
-- anything that you want. Typically, the tag key represents a category
-- (such as \"environment\") and the tag value represents a specific value
-- within that category (such as \"test,\" \"development,\" or
-- \"production\"). You can add up to 50 tags to each container. For more
-- information about tagging, including naming and usage conventions, see
-- <https://docs.aws.amazon.com/mediastore/latest/ug/tagging.html Tagging Resources in MediaStore>.
--
-- 'containerName', 'createContainer_containerName' - The name for the container. The name must be from 1 to 255 characters.
-- Container names must be unique to your AWS account within a specific
-- region. As an example, you could create a container named @movies@ in
-- every region, as long as you don’t have an existing container with that
-- name.
newCreateContainer ::
  -- | 'containerName'
  Core.Text ->
  CreateContainer
newCreateContainer pContainerName_ =
  CreateContainer'
    { tags = Core.Nothing,
      containerName = pContainerName_
    }

-- | An array of key:value pairs that you define. These values can be
-- anything that you want. Typically, the tag key represents a category
-- (such as \"environment\") and the tag value represents a specific value
-- within that category (such as \"test,\" \"development,\" or
-- \"production\"). You can add up to 50 tags to each container. For more
-- information about tagging, including naming and usage conventions, see
-- <https://docs.aws.amazon.com/mediastore/latest/ug/tagging.html Tagging Resources in MediaStore>.
createContainer_tags :: Lens.Lens' CreateContainer (Core.Maybe (Core.NonEmpty Tag))
createContainer_tags = Lens.lens (\CreateContainer' {tags} -> tags) (\s@CreateContainer' {} a -> s {tags = a} :: CreateContainer) Core.. Lens.mapping Lens._Coerce

-- | The name for the container. The name must be from 1 to 255 characters.
-- Container names must be unique to your AWS account within a specific
-- region. As an example, you could create a container named @movies@ in
-- every region, as long as you don’t have an existing container with that
-- name.
createContainer_containerName :: Lens.Lens' CreateContainer Core.Text
createContainer_containerName = Lens.lens (\CreateContainer' {containerName} -> containerName) (\s@CreateContainer' {} a -> s {containerName = a} :: CreateContainer)

instance Core.AWSRequest CreateContainer where
  type
    AWSResponse CreateContainer =
      CreateContainerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateContainerResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "Container")
      )

instance Core.Hashable CreateContainer

instance Core.NFData CreateContainer

instance Core.ToHeaders CreateContainer where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MediaStore_20170901.CreateContainer" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateContainer where
  toJSON CreateContainer' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            Core.Just ("ContainerName" Core..= containerName)
          ]
      )

instance Core.ToPath CreateContainer where
  toPath = Core.const "/"

instance Core.ToQuery CreateContainer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateContainerResponse' smart constructor.
data CreateContainerResponse = CreateContainerResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | ContainerARN: The Amazon Resource Name (ARN) of the newly created
    -- container. The ARN has the following format: arn:aws:\<region>:\<account
    -- that owns this container>:container\/\<name of container>. For example:
    -- arn:aws:mediastore:us-west-2:111122223333:container\/movies
    --
    -- ContainerName: The container name as specified in the request.
    --
    -- CreationTime: Unix time stamp.
    --
    -- Status: The status of container creation or deletion. The status is one
    -- of the following: @CREATING@, @ACTIVE@, or @DELETING@. While the service
    -- is creating the container, the status is @CREATING@. When an endpoint is
    -- available, the status changes to @ACTIVE@.
    --
    -- The return value does not include the container\'s endpoint. To make
    -- downstream requests, you must obtain this value by using
    -- DescribeContainer or ListContainers.
    container :: Container
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateContainerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createContainerResponse_httpStatus' - The response's http status code.
--
-- 'container', 'createContainerResponse_container' - ContainerARN: The Amazon Resource Name (ARN) of the newly created
-- container. The ARN has the following format: arn:aws:\<region>:\<account
-- that owns this container>:container\/\<name of container>. For example:
-- arn:aws:mediastore:us-west-2:111122223333:container\/movies
--
-- ContainerName: The container name as specified in the request.
--
-- CreationTime: Unix time stamp.
--
-- Status: The status of container creation or deletion. The status is one
-- of the following: @CREATING@, @ACTIVE@, or @DELETING@. While the service
-- is creating the container, the status is @CREATING@. When an endpoint is
-- available, the status changes to @ACTIVE@.
--
-- The return value does not include the container\'s endpoint. To make
-- downstream requests, you must obtain this value by using
-- DescribeContainer or ListContainers.
newCreateContainerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'container'
  Container ->
  CreateContainerResponse
newCreateContainerResponse pHttpStatus_ pContainer_ =
  CreateContainerResponse'
    { httpStatus = pHttpStatus_,
      container = pContainer_
    }

-- | The response's http status code.
createContainerResponse_httpStatus :: Lens.Lens' CreateContainerResponse Core.Int
createContainerResponse_httpStatus = Lens.lens (\CreateContainerResponse' {httpStatus} -> httpStatus) (\s@CreateContainerResponse' {} a -> s {httpStatus = a} :: CreateContainerResponse)

-- | ContainerARN: The Amazon Resource Name (ARN) of the newly created
-- container. The ARN has the following format: arn:aws:\<region>:\<account
-- that owns this container>:container\/\<name of container>. For example:
-- arn:aws:mediastore:us-west-2:111122223333:container\/movies
--
-- ContainerName: The container name as specified in the request.
--
-- CreationTime: Unix time stamp.
--
-- Status: The status of container creation or deletion. The status is one
-- of the following: @CREATING@, @ACTIVE@, or @DELETING@. While the service
-- is creating the container, the status is @CREATING@. When an endpoint is
-- available, the status changes to @ACTIVE@.
--
-- The return value does not include the container\'s endpoint. To make
-- downstream requests, you must obtain this value by using
-- DescribeContainer or ListContainers.
createContainerResponse_container :: Lens.Lens' CreateContainerResponse Container
createContainerResponse_container = Lens.lens (\CreateContainerResponse' {container} -> container) (\s@CreateContainerResponse' {} a -> s {container = a} :: CreateContainerResponse)

instance Core.NFData CreateContainerResponse
