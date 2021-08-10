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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Gets the Tags collection for a given resource.
--
-- /See:/ 'newGetTags' smart constructor.
data GetTags = GetTags'
  { -- | (Not currently supported) The current pagination position in the paged
    -- result set.
    position :: Prelude.Maybe Prelude.Text,
    -- | (Not currently supported) The maximum number of returned results per
    -- page. The default value is 25 and the maximum value is 500.
    limit :: Prelude.Maybe Prelude.Int,
    -- | [Required] The ARN of a resource that can be tagged.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetTags
newGetTags pResourceArn_ =
  GetTags'
    { position = Prelude.Nothing,
      limit = Prelude.Nothing,
      resourceArn = pResourceArn_
    }

-- | (Not currently supported) The current pagination position in the paged
-- result set.
getTags_position :: Lens.Lens' GetTags (Prelude.Maybe Prelude.Text)
getTags_position = Lens.lens (\GetTags' {position} -> position) (\s@GetTags' {} a -> s {position = a} :: GetTags)

-- | (Not currently supported) The maximum number of returned results per
-- page. The default value is 25 and the maximum value is 500.
getTags_limit :: Lens.Lens' GetTags (Prelude.Maybe Prelude.Int)
getTags_limit = Lens.lens (\GetTags' {limit} -> limit) (\s@GetTags' {} a -> s {limit = a} :: GetTags)

-- | [Required] The ARN of a resource that can be tagged.
getTags_resourceArn :: Lens.Lens' GetTags Prelude.Text
getTags_resourceArn = Lens.lens (\GetTags' {resourceArn} -> resourceArn) (\s@GetTags' {} a -> s {resourceArn = a} :: GetTags)

instance Core.AWSRequest GetTags where
  type AWSResponse GetTags = GetTagsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTagsResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTags

instance Prelude.NFData GetTags

instance Core.ToHeaders GetTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath GetTags where
  toPath GetTags' {..} =
    Prelude.mconcat ["/tags/", Core.toBS resourceArn]

instance Core.ToQuery GetTags where
  toQuery GetTags' {..} =
    Prelude.mconcat
      ["position" Core.=: position, "limit" Core.=: limit]

-- | The collection of tags. Each tag element is associated with a given
-- resource.
--
-- /See:/ 'newGetTagsResponse' smart constructor.
data GetTagsResponse = GetTagsResponse'
  { -- | The collection of tags. Each tag element is associated with a given
    -- resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetTagsResponse
newGetTagsResponse pHttpStatus_ =
  GetTagsResponse'
    { tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The collection of tags. Each tag element is associated with a given
-- resource.
getTagsResponse_tags :: Lens.Lens' GetTagsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getTagsResponse_tags = Lens.lens (\GetTagsResponse' {tags} -> tags) (\s@GetTagsResponse' {} a -> s {tags = a} :: GetTagsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getTagsResponse_httpStatus :: Lens.Lens' GetTagsResponse Prelude.Int
getTagsResponse_httpStatus = Lens.lens (\GetTagsResponse' {httpStatus} -> httpStatus) (\s@GetTagsResponse' {} a -> s {httpStatus = a} :: GetTagsResponse)

instance Prelude.NFData GetTagsResponse
