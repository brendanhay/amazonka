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
-- Module      : Network.AWS.DynamoDB.ListTagsOfResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all tags on an Amazon DynamoDB resource. You can call
-- ListTagsOfResource up to 10 times per second, per account.
--
-- For an overview on tagging DynamoDB resources, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Tagging.html Tagging for DynamoDB>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- This operation returns paginated results.
module Network.AWS.DynamoDB.ListTagsOfResource
  ( -- * Creating a Request
    ListTagsOfResource (..),
    newListTagsOfResource,

    -- * Request Lenses
    listTagsOfResource_nextToken,
    listTagsOfResource_resourceArn,

    -- * Destructuring the Response
    ListTagsOfResourceResponse (..),
    newListTagsOfResourceResponse,

    -- * Response Lenses
    listTagsOfResourceResponse_nextToken,
    listTagsOfResourceResponse_tags,
    listTagsOfResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTagsOfResource' smart constructor.
data ListTagsOfResource = ListTagsOfResource'
  { -- | An optional string that, if supplied, must be copied from the output of
    -- a previous call to ListTagOfResource. When provided in this manner, this
    -- API fetches the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The Amazon DynamoDB resource with tags to be listed. This value is an
    -- Amazon Resource Name (ARN).
    resourceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTagsOfResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTagsOfResource_nextToken' - An optional string that, if supplied, must be copied from the output of
-- a previous call to ListTagOfResource. When provided in this manner, this
-- API fetches the next page of results.
--
-- 'resourceArn', 'listTagsOfResource_resourceArn' - The Amazon DynamoDB resource with tags to be listed. This value is an
-- Amazon Resource Name (ARN).
newListTagsOfResource ::
  -- | 'resourceArn'
  Core.Text ->
  ListTagsOfResource
newListTagsOfResource pResourceArn_ =
  ListTagsOfResource'
    { nextToken = Core.Nothing,
      resourceArn = pResourceArn_
    }

-- | An optional string that, if supplied, must be copied from the output of
-- a previous call to ListTagOfResource. When provided in this manner, this
-- API fetches the next page of results.
listTagsOfResource_nextToken :: Lens.Lens' ListTagsOfResource (Core.Maybe Core.Text)
listTagsOfResource_nextToken = Lens.lens (\ListTagsOfResource' {nextToken} -> nextToken) (\s@ListTagsOfResource' {} a -> s {nextToken = a} :: ListTagsOfResource)

-- | The Amazon DynamoDB resource with tags to be listed. This value is an
-- Amazon Resource Name (ARN).
listTagsOfResource_resourceArn :: Lens.Lens' ListTagsOfResource Core.Text
listTagsOfResource_resourceArn = Lens.lens (\ListTagsOfResource' {resourceArn} -> resourceArn) (\s@ListTagsOfResource' {} a -> s {resourceArn = a} :: ListTagsOfResource)

instance Core.AWSPager ListTagsOfResource where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTagsOfResourceResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listTagsOfResourceResponse_tags Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listTagsOfResource_nextToken
          Lens..~ rs
          Lens.^? listTagsOfResourceResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListTagsOfResource where
  type
    AWSResponse ListTagsOfResource =
      ListTagsOfResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsOfResourceResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Tags" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTagsOfResource

instance Core.NFData ListTagsOfResource

instance Core.ToHeaders ListTagsOfResource where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.ListTagsOfResource" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListTagsOfResource where
  toJSON ListTagsOfResource' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            Core.Just ("ResourceArn" Core..= resourceArn)
          ]
      )

instance Core.ToPath ListTagsOfResource where
  toPath = Core.const "/"

instance Core.ToQuery ListTagsOfResource where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListTagsOfResourceResponse' smart constructor.
data ListTagsOfResourceResponse = ListTagsOfResourceResponse'
  { -- | If this value is returned, there are additional results to be displayed.
    -- To retrieve them, call ListTagsOfResource again, with NextToken set to
    -- this value.
    nextToken :: Core.Maybe Core.Text,
    -- | The tags currently associated with the Amazon DynamoDB resource.
    tags :: Core.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTagsOfResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTagsOfResourceResponse_nextToken' - If this value is returned, there are additional results to be displayed.
-- To retrieve them, call ListTagsOfResource again, with NextToken set to
-- this value.
--
-- 'tags', 'listTagsOfResourceResponse_tags' - The tags currently associated with the Amazon DynamoDB resource.
--
-- 'httpStatus', 'listTagsOfResourceResponse_httpStatus' - The response's http status code.
newListTagsOfResourceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTagsOfResourceResponse
newListTagsOfResourceResponse pHttpStatus_ =
  ListTagsOfResourceResponse'
    { nextToken =
        Core.Nothing,
      tags = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If this value is returned, there are additional results to be displayed.
-- To retrieve them, call ListTagsOfResource again, with NextToken set to
-- this value.
listTagsOfResourceResponse_nextToken :: Lens.Lens' ListTagsOfResourceResponse (Core.Maybe Core.Text)
listTagsOfResourceResponse_nextToken = Lens.lens (\ListTagsOfResourceResponse' {nextToken} -> nextToken) (\s@ListTagsOfResourceResponse' {} a -> s {nextToken = a} :: ListTagsOfResourceResponse)

-- | The tags currently associated with the Amazon DynamoDB resource.
listTagsOfResourceResponse_tags :: Lens.Lens' ListTagsOfResourceResponse (Core.Maybe [Tag])
listTagsOfResourceResponse_tags = Lens.lens (\ListTagsOfResourceResponse' {tags} -> tags) (\s@ListTagsOfResourceResponse' {} a -> s {tags = a} :: ListTagsOfResourceResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTagsOfResourceResponse_httpStatus :: Lens.Lens' ListTagsOfResourceResponse Core.Int
listTagsOfResourceResponse_httpStatus = Lens.lens (\ListTagsOfResourceResponse' {httpStatus} -> httpStatus) (\s@ListTagsOfResourceResponse' {} a -> s {httpStatus = a} :: ListTagsOfResourceResponse)

instance Core.NFData ListTagsOfResourceResponse
