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
-- Module      : Network.AWS.EFS.ListTagsForResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all tags for a top-level EFS resource. You must provide the ID of
-- the resource that you want to retrieve the tags for.
--
-- This operation requires permissions for the
-- @elasticfilesystem:DescribeAccessPoints@ action.
module Network.AWS.EFS.ListTagsForResource
  ( -- * Creating a Request
    ListTagsForResource (..),
    newListTagsForResource,

    -- * Request Lenses
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_resourceId,

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
import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { -- | You can use @NextToken@ in a subsequent request to fetch the next page
    -- of access point descriptions if the response payload was paginated.
    nextToken :: Core.Maybe Core.Text,
    -- | (Optional) Specifies the maximum number of tag objects to return in the
    -- response. The default value is 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | Specifies the EFS resource you want to retrieve tags for. You can
    -- retrieve tags for EFS file systems and access points using this API
    -- endpoint.
    resourceId :: Core.Text
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
-- 'nextToken', 'listTagsForResource_nextToken' - You can use @NextToken@ in a subsequent request to fetch the next page
-- of access point descriptions if the response payload was paginated.
--
-- 'maxResults', 'listTagsForResource_maxResults' - (Optional) Specifies the maximum number of tag objects to return in the
-- response. The default value is 100.
--
-- 'resourceId', 'listTagsForResource_resourceId' - Specifies the EFS resource you want to retrieve tags for. You can
-- retrieve tags for EFS file systems and access points using this API
-- endpoint.
newListTagsForResource ::
  -- | 'resourceId'
  Core.Text ->
  ListTagsForResource
newListTagsForResource pResourceId_ =
  ListTagsForResource'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      resourceId = pResourceId_
    }

-- | You can use @NextToken@ in a subsequent request to fetch the next page
-- of access point descriptions if the response payload was paginated.
listTagsForResource_nextToken :: Lens.Lens' ListTagsForResource (Core.Maybe Core.Text)
listTagsForResource_nextToken = Lens.lens (\ListTagsForResource' {nextToken} -> nextToken) (\s@ListTagsForResource' {} a -> s {nextToken = a} :: ListTagsForResource)

-- | (Optional) Specifies the maximum number of tag objects to return in the
-- response. The default value is 100.
listTagsForResource_maxResults :: Lens.Lens' ListTagsForResource (Core.Maybe Core.Natural)
listTagsForResource_maxResults = Lens.lens (\ListTagsForResource' {maxResults} -> maxResults) (\s@ListTagsForResource' {} a -> s {maxResults = a} :: ListTagsForResource)

-- | Specifies the EFS resource you want to retrieve tags for. You can
-- retrieve tags for EFS file systems and access points using this API
-- endpoint.
listTagsForResource_resourceId :: Lens.Lens' ListTagsForResource Core.Text
listTagsForResource_resourceId = Lens.lens (\ListTagsForResource' {resourceId} -> resourceId) (\s@ListTagsForResource' {} a -> s {resourceId = a} :: ListTagsForResource)

instance Core.AWSRequest ListTagsForResource where
  type
    AWSResponse ListTagsForResource =
      ListTagsForResourceResponse
  request = Request.get defaultService
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

instance Core.ToPath ListTagsForResource where
  toPath ListTagsForResource' {..} =
    Core.mconcat
      ["/2015-02-01/resource-tags/", Core.toBS resourceId]

instance Core.ToQuery ListTagsForResource where
  toQuery ListTagsForResource' {..} =
    Core.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { -- | @NextToken@ is present if the response payload is paginated. You can use
    -- @NextToken@ in a subsequent request to fetch the next page of access
    -- point descriptions.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of the tags for the specified EFS resource.
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
-- 'nextToken', 'listTagsForResourceResponse_nextToken' - @NextToken@ is present if the response payload is paginated. You can use
-- @NextToken@ in a subsequent request to fetch the next page of access
-- point descriptions.
--
-- 'tags', 'listTagsForResourceResponse_tags' - An array of the tags for the specified EFS resource.
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

-- | @NextToken@ is present if the response payload is paginated. You can use
-- @NextToken@ in a subsequent request to fetch the next page of access
-- point descriptions.
listTagsForResourceResponse_nextToken :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe Core.Text)
listTagsForResourceResponse_nextToken = Lens.lens (\ListTagsForResourceResponse' {nextToken} -> nextToken) (\s@ListTagsForResourceResponse' {} a -> s {nextToken = a} :: ListTagsForResourceResponse)

-- | An array of the tags for the specified EFS resource.
listTagsForResourceResponse_tags :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe [Tag])
listTagsForResourceResponse_tags = Lens.lens (\ListTagsForResourceResponse' {tags} -> tags) (\s@ListTagsForResourceResponse' {} a -> s {tags = a} :: ListTagsForResourceResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTagsForResourceResponse_httpStatus :: Lens.Lens' ListTagsForResourceResponse Core.Int
listTagsForResourceResponse_httpStatus = Lens.lens (\ListTagsForResourceResponse' {httpStatus} -> httpStatus) (\s@ListTagsForResourceResponse' {} a -> s {httpStatus = a} :: ListTagsForResourceResponse)

instance Core.NFData ListTagsForResourceResponse
