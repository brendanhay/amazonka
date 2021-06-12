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
-- Module      : Network.AWS.MQ.ListTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists tags for a resource.
module Network.AWS.MQ.ListTags
  ( -- * Creating a Request
    ListTags (..),
    newListTags,

    -- * Request Lenses
    listTags_resourceArn,

    -- * Destructuring the Response
    ListTagsResponse (..),
    newListTagsResponse,

    -- * Response Lenses
    listTagsResponse_tags,
    listTagsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTags' smart constructor.
data ListTags = ListTags'
  { -- | The Amazon Resource Name (ARN) of the resource tag.
    resourceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'listTags_resourceArn' - The Amazon Resource Name (ARN) of the resource tag.
newListTags ::
  -- | 'resourceArn'
  Core.Text ->
  ListTags
newListTags pResourceArn_ =
  ListTags' {resourceArn = pResourceArn_}

-- | The Amazon Resource Name (ARN) of the resource tag.
listTags_resourceArn :: Lens.Lens' ListTags Core.Text
listTags_resourceArn = Lens.lens (\ListTags' {resourceArn} -> resourceArn) (\s@ListTags' {} a -> s {resourceArn = a} :: ListTags)

instance Core.AWSRequest ListTags where
  type AWSResponse ListTags = ListTagsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsResponse'
            Core.<$> (x Core..?> "tags" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTags

instance Core.NFData ListTags

instance Core.ToHeaders ListTags where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListTags where
  toPath ListTags' {..} =
    Core.mconcat ["/v1/tags/", Core.toBS resourceArn]

instance Core.ToQuery ListTags where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { -- | The key-value pair for the resource tag.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'listTagsResponse_tags' - The key-value pair for the resource tag.
--
-- 'httpStatus', 'listTagsResponse_httpStatus' - The response's http status code.
newListTagsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTagsResponse
newListTagsResponse pHttpStatus_ =
  ListTagsResponse'
    { tags = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The key-value pair for the resource tag.
listTagsResponse_tags :: Lens.Lens' ListTagsResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
listTagsResponse_tags = Lens.lens (\ListTagsResponse' {tags} -> tags) (\s@ListTagsResponse' {} a -> s {tags = a} :: ListTagsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTagsResponse_httpStatus :: Lens.Lens' ListTagsResponse Core.Int
listTagsResponse_httpStatus = Lens.lens (\ListTagsResponse' {httpStatus} -> httpStatus) (\s@ListTagsResponse' {} a -> s {httpStatus = a} :: ListTagsResponse)

instance Core.NFData ListTagsResponse
