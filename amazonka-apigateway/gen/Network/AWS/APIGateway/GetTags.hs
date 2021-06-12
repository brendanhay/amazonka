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
-- Module      : Network.AWS.APIGateway.GetTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the Tags collection for a given resource.
module Network.AWS.APIGateway.GetTags
  ( -- * Creating a Request
    GetTags (..),
    newGetTags,

    -- * Request Lenses
    getTags_position,
    getTags_limit,
    getTags_resourceArn,

    -- * Destructuring the Response
    GetTagsResponse (..),
    newGetTagsResponse,

    -- * Response Lenses
    getTagsResponse_tags,
    getTagsResponse_httpStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Gets the Tags collection for a given resource.
--
-- /See:/ 'newGetTags' smart constructor.
data GetTags = GetTags'
  { -- | (Not currently supported) The current pagination position in the paged
    -- result set.
    position :: Core.Maybe Core.Text,
    -- | (Not currently supported) The maximum number of returned results per
    -- page. The default value is 25 and the maximum value is 500.
    limit :: Core.Maybe Core.Int,
    -- | [Required] The ARN of a resource that can be tagged.
    resourceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'position', 'getTags_position' - (Not currently supported) The current pagination position in the paged
-- result set.
--
-- 'limit', 'getTags_limit' - (Not currently supported) The maximum number of returned results per
-- page. The default value is 25 and the maximum value is 500.
--
-- 'resourceArn', 'getTags_resourceArn' - [Required] The ARN of a resource that can be tagged.
newGetTags ::
  -- | 'resourceArn'
  Core.Text ->
  GetTags
newGetTags pResourceArn_ =
  GetTags'
    { position = Core.Nothing,
      limit = Core.Nothing,
      resourceArn = pResourceArn_
    }

-- | (Not currently supported) The current pagination position in the paged
-- result set.
getTags_position :: Lens.Lens' GetTags (Core.Maybe Core.Text)
getTags_position = Lens.lens (\GetTags' {position} -> position) (\s@GetTags' {} a -> s {position = a} :: GetTags)

-- | (Not currently supported) The maximum number of returned results per
-- page. The default value is 25 and the maximum value is 500.
getTags_limit :: Lens.Lens' GetTags (Core.Maybe Core.Int)
getTags_limit = Lens.lens (\GetTags' {limit} -> limit) (\s@GetTags' {} a -> s {limit = a} :: GetTags)

-- | [Required] The ARN of a resource that can be tagged.
getTags_resourceArn :: Lens.Lens' GetTags Core.Text
getTags_resourceArn = Lens.lens (\GetTags' {resourceArn} -> resourceArn) (\s@GetTags' {} a -> s {resourceArn = a} :: GetTags)

instance Core.AWSRequest GetTags where
  type AWSResponse GetTags = GetTagsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTagsResponse'
            Core.<$> (x Core..?> "tags" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetTags

instance Core.NFData GetTags

instance Core.ToHeaders GetTags where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetTags where
  toPath GetTags' {..} =
    Core.mconcat ["/tags/", Core.toBS resourceArn]

instance Core.ToQuery GetTags where
  toQuery GetTags' {..} =
    Core.mconcat
      ["position" Core.=: position, "limit" Core.=: limit]

-- | The collection of tags. Each tag element is associated with a given
-- resource.
--
-- /See:/ 'newGetTagsResponse' smart constructor.
data GetTagsResponse = GetTagsResponse'
  { -- | The collection of tags. Each tag element is associated with a given
    -- resource.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getTagsResponse_tags' - The collection of tags. Each tag element is associated with a given
-- resource.
--
-- 'httpStatus', 'getTagsResponse_httpStatus' - The response's http status code.
newGetTagsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetTagsResponse
newGetTagsResponse pHttpStatus_ =
  GetTagsResponse'
    { tags = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The collection of tags. Each tag element is associated with a given
-- resource.
getTagsResponse_tags :: Lens.Lens' GetTagsResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
getTagsResponse_tags = Lens.lens (\GetTagsResponse' {tags} -> tags) (\s@GetTagsResponse' {} a -> s {tags = a} :: GetTagsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getTagsResponse_httpStatus :: Lens.Lens' GetTagsResponse Core.Int
getTagsResponse_httpStatus = Lens.lens (\GetTagsResponse' {httpStatus} -> httpStatus) (\s@GetTagsResponse' {} a -> s {httpStatus = a} :: GetTagsResponse)

instance Core.NFData GetTagsResponse
