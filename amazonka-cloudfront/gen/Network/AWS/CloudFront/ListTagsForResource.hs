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
-- Module      : Network.AWS.CloudFront.ListTagsForResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List tags for a CloudFront resource.
module Network.AWS.CloudFront.ListTagsForResource
  ( -- * Creating a Request
    ListTagsForResource (..),
    newListTagsForResource,

    -- * Request Lenses
    listTagsForResource_resource,

    -- * Destructuring the Response
    ListTagsForResourceResponse (..),
    newListTagsForResourceResponse,

    -- * Response Lenses
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_tags,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to list tags for a CloudFront resource.
--
-- /See:/ 'newListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { -- | An ARN of a CloudFront resource.
    resource :: Core.Text
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
-- 'resource', 'listTagsForResource_resource' - An ARN of a CloudFront resource.
newListTagsForResource ::
  -- | 'resource'
  Core.Text ->
  ListTagsForResource
newListTagsForResource pResource_ =
  ListTagsForResource' {resource = pResource_}

-- | An ARN of a CloudFront resource.
listTagsForResource_resource :: Lens.Lens' ListTagsForResource Core.Text
listTagsForResource_resource = Lens.lens (\ListTagsForResource' {resource} -> resource) (\s@ListTagsForResource' {} a -> s {resource = a} :: ListTagsForResource)

instance Core.AWSRequest ListTagsForResource where
  type
    AWSResponse ListTagsForResource =
      ListTagsForResourceResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListTagsForResourceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.parseXML x)
      )

instance Core.Hashable ListTagsForResource

instance Core.NFData ListTagsForResource

instance Core.ToHeaders ListTagsForResource where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListTagsForResource where
  toPath = Core.const "/2020-05-31/tagging"

instance Core.ToQuery ListTagsForResource where
  toQuery ListTagsForResource' {..} =
    Core.mconcat ["Resource" Core.=: resource]

-- | The returned result of the corresponding request.
--
-- /See:/ 'newListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A complex type that contains zero or more @Tag@ elements.
    tags :: Tags
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
-- 'httpStatus', 'listTagsForResourceResponse_httpStatus' - The response's http status code.
--
-- 'tags', 'listTagsForResourceResponse_tags' - A complex type that contains zero or more @Tag@ elements.
newListTagsForResourceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'tags'
  Tags ->
  ListTagsForResourceResponse
newListTagsForResourceResponse pHttpStatus_ pTags_ =
  ListTagsForResourceResponse'
    { httpStatus =
        pHttpStatus_,
      tags = pTags_
    }

-- | The response's http status code.
listTagsForResourceResponse_httpStatus :: Lens.Lens' ListTagsForResourceResponse Core.Int
listTagsForResourceResponse_httpStatus = Lens.lens (\ListTagsForResourceResponse' {httpStatus} -> httpStatus) (\s@ListTagsForResourceResponse' {} a -> s {httpStatus = a} :: ListTagsForResourceResponse)

-- | A complex type that contains zero or more @Tag@ elements.
listTagsForResourceResponse_tags :: Lens.Lens' ListTagsForResourceResponse Tags
listTagsForResourceResponse_tags = Lens.lens (\ListTagsForResourceResponse' {tags} -> tags) (\s@ListTagsForResourceResponse' {} a -> s {tags = a} :: ListTagsForResourceResponse)

instance Core.NFData ListTagsForResourceResponse
