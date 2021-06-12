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
-- Module      : Network.AWS.SageMaker.ListTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the tags for the specified Amazon SageMaker resource.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListTags
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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListTags' smart constructor.
data ListTags = ListTags'
  { -- | If the response to the previous @ListTags@ request is truncated, Amazon
    -- SageMaker returns this token. To retrieve the next set of tags, use it
    -- in the subsequent request.
    nextToken :: Core.Maybe Core.Text,
    -- | Maximum number of tags to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) of the resource whose tags you want to
    -- retrieve.
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
-- 'nextToken', 'listTags_nextToken' - If the response to the previous @ListTags@ request is truncated, Amazon
-- SageMaker returns this token. To retrieve the next set of tags, use it
-- in the subsequent request.
--
-- 'maxResults', 'listTags_maxResults' - Maximum number of tags to return.
--
-- 'resourceArn', 'listTags_resourceArn' - The Amazon Resource Name (ARN) of the resource whose tags you want to
-- retrieve.
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

-- | If the response to the previous @ListTags@ request is truncated, Amazon
-- SageMaker returns this token. To retrieve the next set of tags, use it
-- in the subsequent request.
listTags_nextToken :: Lens.Lens' ListTags (Core.Maybe Core.Text)
listTags_nextToken = Lens.lens (\ListTags' {nextToken} -> nextToken) (\s@ListTags' {} a -> s {nextToken = a} :: ListTags)

-- | Maximum number of tags to return.
listTags_maxResults :: Lens.Lens' ListTags (Core.Maybe Core.Natural)
listTags_maxResults = Lens.lens (\ListTags' {maxResults} -> maxResults) (\s@ListTags' {} a -> s {maxResults = a} :: ListTags)

-- | The Amazon Resource Name (ARN) of the resource whose tags you want to
-- retrieve.
listTags_resourceArn :: Lens.Lens' ListTags Core.Text
listTags_resourceArn = Lens.lens (\ListTags' {resourceArn} -> resourceArn) (\s@ListTags' {} a -> s {resourceArn = a} :: ListTags)

instance Core.AWSPager ListTags where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTagsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^? listTagsResponse_tags Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listTags_nextToken
          Lens..~ rs
          Lens.^? listTagsResponse_nextToken Core.. Lens._Just

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
              Core.=# ("SageMaker.ListTags" :: Core.ByteString),
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

-- | /See:/ 'newListTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { -- | If response is truncated, Amazon SageMaker includes a token in the
    -- response. You can use this token in your subsequent request to fetch
    -- next set of tokens.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of @Tag@ objects, each with a tag key and a value.
    tags :: Core.Maybe [Tag],
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
-- 'nextToken', 'listTagsResponse_nextToken' - If response is truncated, Amazon SageMaker includes a token in the
-- response. You can use this token in your subsequent request to fetch
-- next set of tokens.
--
-- 'tags', 'listTagsResponse_tags' - An array of @Tag@ objects, each with a tag key and a value.
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

-- | If response is truncated, Amazon SageMaker includes a token in the
-- response. You can use this token in your subsequent request to fetch
-- next set of tokens.
listTagsResponse_nextToken :: Lens.Lens' ListTagsResponse (Core.Maybe Core.Text)
listTagsResponse_nextToken = Lens.lens (\ListTagsResponse' {nextToken} -> nextToken) (\s@ListTagsResponse' {} a -> s {nextToken = a} :: ListTagsResponse)

-- | An array of @Tag@ objects, each with a tag key and a value.
listTagsResponse_tags :: Lens.Lens' ListTagsResponse (Core.Maybe [Tag])
listTagsResponse_tags = Lens.lens (\ListTagsResponse' {tags} -> tags) (\s@ListTagsResponse' {} a -> s {tags = a} :: ListTagsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTagsResponse_httpStatus :: Lens.Lens' ListTagsResponse Core.Int
listTagsResponse_httpStatus = Lens.lens (\ListTagsResponse' {httpStatus} -> httpStatus) (\s@ListTagsResponse' {} a -> s {httpStatus = a} :: ListTagsResponse)

instance Core.NFData ListTagsResponse
