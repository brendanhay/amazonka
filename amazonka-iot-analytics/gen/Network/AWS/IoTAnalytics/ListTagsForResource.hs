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
-- Module      : Network.AWS.IoTAnalytics.ListTagsForResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags (metadata) that you have assigned to the resource.
module Network.AWS.IoTAnalytics.ListTagsForResource
  ( -- * Creating a Request
    ListTagsForResource (..),
    newListTagsForResource,

    -- * Request Lenses
    listTagsForResource_resourceArn,

    -- * Destructuring the Response
    ListTagsForResourceResponse (..),
    newListTagsForResourceResponse,

    -- * Response Lenses
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { -- | The ARN of the resource whose tags you want to list.
    resourceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTagsForResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'listTagsForResource_resourceArn' - The ARN of the resource whose tags you want to list.
newListTagsForResource ::
  -- | 'resourceArn'
  Core.Text ->
  ListTagsForResource
newListTagsForResource pResourceArn_ =
  ListTagsForResource' {resourceArn = pResourceArn_}

-- | The ARN of the resource whose tags you want to list.
listTagsForResource_resourceArn :: Lens.Lens' ListTagsForResource Core.Text
listTagsForResource_resourceArn = Lens.lens (\ListTagsForResource' {resourceArn} -> resourceArn) (\s@ListTagsForResource' {} a -> s {resourceArn = a} :: ListTagsForResource)

instance Core.AWSRequest ListTagsForResource where
  type
    AWSResponse ListTagsForResource =
      ListTagsForResourceResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForResourceResponse'
            Core.<$> (x Core..?> "tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTagsForResource

instance Core.NFData ListTagsForResource

instance Core.ToHeaders ListTagsForResource where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListTagsForResource where
  toPath = Core.const "/tags"

instance Core.ToQuery ListTagsForResource where
  toQuery ListTagsForResource' {..} =
    Core.mconcat ["resourceArn" Core.=: resourceArn]

-- | /See:/ 'newListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { -- | The tags (metadata) that you have assigned to the resource.
    tags :: Core.Maybe (Core.NonEmpty Tag),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTagsForResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'listTagsForResourceResponse_tags' - The tags (metadata) that you have assigned to the resource.
--
-- 'httpStatus', 'listTagsForResourceResponse_httpStatus' - The response's http status code.
newListTagsForResourceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTagsForResourceResponse
newListTagsForResourceResponse pHttpStatus_ =
  ListTagsForResourceResponse'
    { tags = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tags (metadata) that you have assigned to the resource.
listTagsForResourceResponse_tags :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe (Core.NonEmpty Tag))
listTagsForResourceResponse_tags = Lens.lens (\ListTagsForResourceResponse' {tags} -> tags) (\s@ListTagsForResourceResponse' {} a -> s {tags = a} :: ListTagsForResourceResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTagsForResourceResponse_httpStatus :: Lens.Lens' ListTagsForResourceResponse Core.Int
listTagsForResourceResponse_httpStatus = Lens.lens (\ListTagsForResourceResponse' {httpStatus} -> httpStatus) (\s@ListTagsForResourceResponse' {} a -> s {httpStatus = a} :: ListTagsForResourceResponse)

instance Core.NFData ListTagsForResourceResponse
