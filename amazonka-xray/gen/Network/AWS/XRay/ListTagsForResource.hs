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
-- Module      : Network.AWS.XRay.ListTagsForResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tags that are applied to the specified AWS X-Ray group
-- or sampling rule.
module Network.AWS.XRay.ListTagsForResource
  ( -- * Creating a Request
    ListTagsForResource (..),
    newListTagsForResource,

    -- * Request Lenses
    listTagsForResource_nextToken,
    listTagsForResource_resourceARN,

    -- * Destructuring the Response
    ListTagsForResourceResponse (..),
    newListTagsForResourceResponse,

    -- * Response Lenses
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { -- | A pagination token. If multiple pages of results are returned, use the
    -- @NextToken@ value returned with the current page of results as the value
    -- of this parameter to get the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The Amazon Resource Number (ARN) of an X-Ray group or sampling rule.
    resourceARN :: Core.Text
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
-- 'nextToken', 'listTagsForResource_nextToken' - A pagination token. If multiple pages of results are returned, use the
-- @NextToken@ value returned with the current page of results as the value
-- of this parameter to get the next page of results.
--
-- 'resourceARN', 'listTagsForResource_resourceARN' - The Amazon Resource Number (ARN) of an X-Ray group or sampling rule.
newListTagsForResource ::
  -- | 'resourceARN'
  Core.Text ->
  ListTagsForResource
newListTagsForResource pResourceARN_ =
  ListTagsForResource'
    { nextToken = Core.Nothing,
      resourceARN = pResourceARN_
    }

-- | A pagination token. If multiple pages of results are returned, use the
-- @NextToken@ value returned with the current page of results as the value
-- of this parameter to get the next page of results.
listTagsForResource_nextToken :: Lens.Lens' ListTagsForResource (Core.Maybe Core.Text)
listTagsForResource_nextToken = Lens.lens (\ListTagsForResource' {nextToken} -> nextToken) (\s@ListTagsForResource' {} a -> s {nextToken = a} :: ListTagsForResource)

-- | The Amazon Resource Number (ARN) of an X-Ray group or sampling rule.
listTagsForResource_resourceARN :: Lens.Lens' ListTagsForResource Core.Text
listTagsForResource_resourceARN = Lens.lens (\ListTagsForResource' {resourceARN} -> resourceARN) (\s@ListTagsForResource' {} a -> s {resourceARN = a} :: ListTagsForResource)

instance Core.AWSRequest ListTagsForResource where
  type
    AWSResponse ListTagsForResource =
      ListTagsForResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForResourceResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Tags" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTagsForResource

instance Core.NFData ListTagsForResource

instance Core.ToHeaders ListTagsForResource where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON ListTagsForResource where
  toJSON ListTagsForResource' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            Core.Just ("ResourceARN" Core..= resourceARN)
          ]
      )

instance Core.ToPath ListTagsForResource where
  toPath = Core.const "/ListTagsForResource"

instance Core.ToQuery ListTagsForResource where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { -- | A pagination token. If multiple pages of results are returned, use the
    -- @NextToken@ value returned with the current page of results to get the
    -- next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of tags, as key and value pairs, that is associated with the
    -- specified X-Ray group or sampling rule.
    tags :: Core.Maybe [Tag],
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
-- 'nextToken', 'listTagsForResourceResponse_nextToken' - A pagination token. If multiple pages of results are returned, use the
-- @NextToken@ value returned with the current page of results to get the
-- next page of results.
--
-- 'tags', 'listTagsForResourceResponse_tags' - A list of tags, as key and value pairs, that is associated with the
-- specified X-Ray group or sampling rule.
--
-- 'httpStatus', 'listTagsForResourceResponse_httpStatus' - The response's http status code.
newListTagsForResourceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTagsForResourceResponse
newListTagsForResourceResponse pHttpStatus_ =
  ListTagsForResourceResponse'
    { nextToken =
        Core.Nothing,
      tags = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token. If multiple pages of results are returned, use the
-- @NextToken@ value returned with the current page of results to get the
-- next page of results.
listTagsForResourceResponse_nextToken :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe Core.Text)
listTagsForResourceResponse_nextToken = Lens.lens (\ListTagsForResourceResponse' {nextToken} -> nextToken) (\s@ListTagsForResourceResponse' {} a -> s {nextToken = a} :: ListTagsForResourceResponse)

-- | A list of tags, as key and value pairs, that is associated with the
-- specified X-Ray group or sampling rule.
listTagsForResourceResponse_tags :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe [Tag])
listTagsForResourceResponse_tags = Lens.lens (\ListTagsForResourceResponse' {tags} -> tags) (\s@ListTagsForResourceResponse' {} a -> s {tags = a} :: ListTagsForResourceResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTagsForResourceResponse_httpStatus :: Lens.Lens' ListTagsForResourceResponse Core.Int
listTagsForResourceResponse_httpStatus = Lens.lens (\ListTagsForResourceResponse' {httpStatus} -> httpStatus) (\s@ListTagsForResourceResponse' {} a -> s {httpStatus = a} :: ListTagsForResourceResponse)

instance Core.NFData ListTagsForResourceResponse
