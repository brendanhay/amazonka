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
-- Module      : Network.AWS.WorkSpaces.CreateTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the specified tags for the specified WorkSpaces resource.
module Network.AWS.WorkSpaces.CreateTags
  ( -- * Creating a Request
    CreateTags (..),
    newCreateTags,

    -- * Request Lenses
    createTags_resourceId,
    createTags_tags,

    -- * Destructuring the Response
    CreateTagsResponse (..),
    newCreateTagsResponse,

    -- * Response Lenses
    createTagsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newCreateTags' smart constructor.
data CreateTags = CreateTags'
  { -- | The identifier of the WorkSpaces resource. The supported resource types
    -- are WorkSpaces, registered directories, images, custom bundles, IP
    -- access control groups, and connection aliases.
    resourceId :: Core.Text,
    -- | The tags. Each WorkSpaces resource can have a maximum of 50 tags.
    tags :: [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'createTags_resourceId' - The identifier of the WorkSpaces resource. The supported resource types
-- are WorkSpaces, registered directories, images, custom bundles, IP
-- access control groups, and connection aliases.
--
-- 'tags', 'createTags_tags' - The tags. Each WorkSpaces resource can have a maximum of 50 tags.
newCreateTags ::
  -- | 'resourceId'
  Core.Text ->
  CreateTags
newCreateTags pResourceId_ =
  CreateTags'
    { resourceId = pResourceId_,
      tags = Core.mempty
    }

-- | The identifier of the WorkSpaces resource. The supported resource types
-- are WorkSpaces, registered directories, images, custom bundles, IP
-- access control groups, and connection aliases.
createTags_resourceId :: Lens.Lens' CreateTags Core.Text
createTags_resourceId = Lens.lens (\CreateTags' {resourceId} -> resourceId) (\s@CreateTags' {} a -> s {resourceId = a} :: CreateTags)

-- | The tags. Each WorkSpaces resource can have a maximum of 50 tags.
createTags_tags :: Lens.Lens' CreateTags [Tag]
createTags_tags = Lens.lens (\CreateTags' {tags} -> tags) (\s@CreateTags' {} a -> s {tags = a} :: CreateTags) Core.. Lens._Coerce

instance Core.AWSRequest CreateTags where
  type AWSResponse CreateTags = CreateTagsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateTagsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateTags

instance Core.NFData CreateTags

instance Core.ToHeaders CreateTags where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("WorkspacesService.CreateTags" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateTags where
  toJSON CreateTags' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceId" Core..= resourceId),
            Core.Just ("Tags" Core..= tags)
          ]
      )

instance Core.ToPath CreateTags where
  toPath = Core.const "/"

instance Core.ToQuery CreateTags where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateTagsResponse' smart constructor.
data CreateTagsResponse = CreateTagsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createTagsResponse_httpStatus' - The response's http status code.
newCreateTagsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateTagsResponse
newCreateTagsResponse pHttpStatus_ =
  CreateTagsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createTagsResponse_httpStatus :: Lens.Lens' CreateTagsResponse Core.Int
createTagsResponse_httpStatus = Lens.lens (\CreateTagsResponse' {httpStatus} -> httpStatus) (\s@CreateTagsResponse' {} a -> s {httpStatus = a} :: CreateTagsResponse)

instance Core.NFData CreateTagsResponse
