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
-- Module      : Network.AWS.OpsWorks.ListTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tags that are applied to the specified stack or layer.
module Network.AWS.OpsWorks.ListTags
  ( -- * Creating a Request
    ListTags (..),
    newListTags,

    -- * Request Lenses
    listTags_nextToken,
    listTags_maxResults,
    listTags_resourceArn,

    -- * Destructuring the Response
    ListTagsResponse (..),
    newListTagsResponse,

    -- * Response Lenses
    listTagsResponse_nextToken,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTags' smart constructor.
data ListTags = ListTags'
  { -- | Do not use. A validation exception occurs if you add a @NextToken@
    -- parameter to a @ListTagsRequest@ call.
    nextToken :: Core.Maybe Core.Text,
    -- | Do not use. A validation exception occurs if you add a @MaxResults@
    -- parameter to a @ListTagsRequest@ call.
    maxResults :: Core.Maybe Core.Int,
    -- | The stack or layer\'s Amazon Resource Number (ARN).
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
-- 'nextToken', 'listTags_nextToken' - Do not use. A validation exception occurs if you add a @NextToken@
-- parameter to a @ListTagsRequest@ call.
--
-- 'maxResults', 'listTags_maxResults' - Do not use. A validation exception occurs if you add a @MaxResults@
-- parameter to a @ListTagsRequest@ call.
--
-- 'resourceArn', 'listTags_resourceArn' - The stack or layer\'s Amazon Resource Number (ARN).
newListTags ::
  -- | 'resourceArn'
  Core.Text ->
  ListTags
newListTags pResourceArn_ =
  ListTags'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      resourceArn = pResourceArn_
    }

-- | Do not use. A validation exception occurs if you add a @NextToken@
-- parameter to a @ListTagsRequest@ call.
listTags_nextToken :: Lens.Lens' ListTags (Core.Maybe Core.Text)
listTags_nextToken = Lens.lens (\ListTags' {nextToken} -> nextToken) (\s@ListTags' {} a -> s {nextToken = a} :: ListTags)

-- | Do not use. A validation exception occurs if you add a @MaxResults@
-- parameter to a @ListTagsRequest@ call.
listTags_maxResults :: Lens.Lens' ListTags (Core.Maybe Core.Int)
listTags_maxResults = Lens.lens (\ListTags' {maxResults} -> maxResults) (\s@ListTags' {} a -> s {maxResults = a} :: ListTags)

-- | The stack or layer\'s Amazon Resource Number (ARN).
listTags_resourceArn :: Lens.Lens' ListTags Core.Text
listTags_resourceArn = Lens.lens (\ListTags' {resourceArn} -> resourceArn) (\s@ListTags' {} a -> s {resourceArn = a} :: ListTags)

instance Core.AWSRequest ListTags where
  type AWSResponse ListTags = ListTagsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Tags" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTags

instance Core.NFData ListTags

instance Core.ToHeaders ListTags where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("OpsWorks_20130218.ListTags" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListTags where
  toJSON ListTags' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("ResourceArn" Core..= resourceArn)
          ]
      )

instance Core.ToPath ListTags where
  toPath = Core.const "/"

instance Core.ToQuery ListTags where
  toQuery = Core.const Core.mempty

-- | Contains the response to a @ListTags@ request.
--
-- /See:/ 'newListTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { -- | If a paginated request does not return all of the remaining results,
    -- this parameter is set to a token that you can assign to the request
    -- object\'s @NextToken@ parameter to get the next set of results. If the
    -- previous paginated request returned all of the remaining results, this
    -- parameter is set to @null@.
    nextToken :: Core.Maybe Core.Text,
    -- | A set of key-value pairs that contain tag keys and tag values that are
    -- attached to a stack or layer.
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
-- 'nextToken', 'listTagsResponse_nextToken' - If a paginated request does not return all of the remaining results,
-- this parameter is set to a token that you can assign to the request
-- object\'s @NextToken@ parameter to get the next set of results. If the
-- previous paginated request returned all of the remaining results, this
-- parameter is set to @null@.
--
-- 'tags', 'listTagsResponse_tags' - A set of key-value pairs that contain tag keys and tag values that are
-- attached to a stack or layer.
--
-- 'httpStatus', 'listTagsResponse_httpStatus' - The response's http status code.
newListTagsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTagsResponse
newListTagsResponse pHttpStatus_ =
  ListTagsResponse'
    { nextToken = Core.Nothing,
      tags = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If a paginated request does not return all of the remaining results,
-- this parameter is set to a token that you can assign to the request
-- object\'s @NextToken@ parameter to get the next set of results. If the
-- previous paginated request returned all of the remaining results, this
-- parameter is set to @null@.
listTagsResponse_nextToken :: Lens.Lens' ListTagsResponse (Core.Maybe Core.Text)
listTagsResponse_nextToken = Lens.lens (\ListTagsResponse' {nextToken} -> nextToken) (\s@ListTagsResponse' {} a -> s {nextToken = a} :: ListTagsResponse)

-- | A set of key-value pairs that contain tag keys and tag values that are
-- attached to a stack or layer.
listTagsResponse_tags :: Lens.Lens' ListTagsResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
listTagsResponse_tags = Lens.lens (\ListTagsResponse' {tags} -> tags) (\s@ListTagsResponse' {} a -> s {tags = a} :: ListTagsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTagsResponse_httpStatus :: Lens.Lens' ListTagsResponse Core.Int
listTagsResponse_httpStatus = Lens.lens (\ListTagsResponse' {httpStatus} -> httpStatus) (\s@ListTagsResponse' {} a -> s {httpStatus = a} :: ListTagsResponse)

instance Core.NFData ListTagsResponse
