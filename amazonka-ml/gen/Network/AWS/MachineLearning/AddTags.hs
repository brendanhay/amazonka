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
-- Module      : Network.AWS.MachineLearning.AddTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to an object, up to a limit of 10. Each tag
-- consists of a key and an optional value. If you add a tag using a key
-- that is already associated with the ML object, @AddTags@ updates the
-- tag\'s value.
module Network.AWS.MachineLearning.AddTags
  ( -- * Creating a Request
    AddTags (..),
    newAddTags,

    -- * Request Lenses
    addTags_tags,
    addTags_resourceId,
    addTags_resourceType,

    -- * Destructuring the Response
    AddTagsResponse (..),
    newAddTagsResponse,

    -- * Response Lenses
    addTagsResponse_resourceId,
    addTagsResponse_resourceType,
    addTagsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAddTags' smart constructor.
data AddTags = AddTags'
  { -- | The key-value pairs to use to create tags. If you specify a key without
    -- specifying a value, Amazon ML creates a tag with the specified key and a
    -- value of null.
    tags :: [Tag],
    -- | The ID of the ML object to tag. For example, @exampleModelId@.
    resourceId :: Core.Text,
    -- | The type of the ML object to tag.
    resourceType :: TaggableResourceType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'addTags_tags' - The key-value pairs to use to create tags. If you specify a key without
-- specifying a value, Amazon ML creates a tag with the specified key and a
-- value of null.
--
-- 'resourceId', 'addTags_resourceId' - The ID of the ML object to tag. For example, @exampleModelId@.
--
-- 'resourceType', 'addTags_resourceType' - The type of the ML object to tag.
newAddTags ::
  -- | 'resourceId'
  Core.Text ->
  -- | 'resourceType'
  TaggableResourceType ->
  AddTags
newAddTags pResourceId_ pResourceType_ =
  AddTags'
    { tags = Core.mempty,
      resourceId = pResourceId_,
      resourceType = pResourceType_
    }

-- | The key-value pairs to use to create tags. If you specify a key without
-- specifying a value, Amazon ML creates a tag with the specified key and a
-- value of null.
addTags_tags :: Lens.Lens' AddTags [Tag]
addTags_tags = Lens.lens (\AddTags' {tags} -> tags) (\s@AddTags' {} a -> s {tags = a} :: AddTags) Core.. Lens._Coerce

-- | The ID of the ML object to tag. For example, @exampleModelId@.
addTags_resourceId :: Lens.Lens' AddTags Core.Text
addTags_resourceId = Lens.lens (\AddTags' {resourceId} -> resourceId) (\s@AddTags' {} a -> s {resourceId = a} :: AddTags)

-- | The type of the ML object to tag.
addTags_resourceType :: Lens.Lens' AddTags TaggableResourceType
addTags_resourceType = Lens.lens (\AddTags' {resourceType} -> resourceType) (\s@AddTags' {} a -> s {resourceType = a} :: AddTags)

instance Core.AWSRequest AddTags where
  type AWSResponse AddTags = AddTagsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AddTagsResponse'
            Core.<$> (x Core..?> "ResourceId")
            Core.<*> (x Core..?> "ResourceType")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AddTags

instance Core.NFData AddTags

instance Core.ToHeaders AddTags where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonML_20141212.AddTags" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AddTags where
  toJSON AddTags' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Tags" Core..= tags),
            Core.Just ("ResourceId" Core..= resourceId),
            Core.Just ("ResourceType" Core..= resourceType)
          ]
      )

instance Core.ToPath AddTags where
  toPath = Core.const "/"

instance Core.ToQuery AddTags where
  toQuery = Core.const Core.mempty

-- | Amazon ML returns the following elements.
--
-- /See:/ 'newAddTagsResponse' smart constructor.
data AddTagsResponse = AddTagsResponse'
  { -- | The ID of the ML object that was tagged.
    resourceId :: Core.Maybe Core.Text,
    -- | The type of the ML object that was tagged.
    resourceType :: Core.Maybe TaggableResourceType,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'addTagsResponse_resourceId' - The ID of the ML object that was tagged.
--
-- 'resourceType', 'addTagsResponse_resourceType' - The type of the ML object that was tagged.
--
-- 'httpStatus', 'addTagsResponse_httpStatus' - The response's http status code.
newAddTagsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AddTagsResponse
newAddTagsResponse pHttpStatus_ =
  AddTagsResponse'
    { resourceId = Core.Nothing,
      resourceType = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the ML object that was tagged.
addTagsResponse_resourceId :: Lens.Lens' AddTagsResponse (Core.Maybe Core.Text)
addTagsResponse_resourceId = Lens.lens (\AddTagsResponse' {resourceId} -> resourceId) (\s@AddTagsResponse' {} a -> s {resourceId = a} :: AddTagsResponse)

-- | The type of the ML object that was tagged.
addTagsResponse_resourceType :: Lens.Lens' AddTagsResponse (Core.Maybe TaggableResourceType)
addTagsResponse_resourceType = Lens.lens (\AddTagsResponse' {resourceType} -> resourceType) (\s@AddTagsResponse' {} a -> s {resourceType = a} :: AddTagsResponse)

-- | The response's http status code.
addTagsResponse_httpStatus :: Lens.Lens' AddTagsResponse Core.Int
addTagsResponse_httpStatus = Lens.lens (\AddTagsResponse' {httpStatus} -> httpStatus) (\s@AddTagsResponse' {} a -> s {httpStatus = a} :: AddTagsResponse)

instance Core.NFData AddTagsResponse
