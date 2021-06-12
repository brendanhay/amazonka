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
-- Module      : Network.AWS.CloudTrail.ListTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags for the trail in the current region.
--
-- This operation returns paginated results.
module Network.AWS.CloudTrail.ListTags
  ( -- * Creating a Request
    ListTags (..),
    newListTags,

    -- * Request Lenses
    listTags_nextToken,
    listTags_resourceIdList,

    -- * Destructuring the Response
    ListTagsResponse (..),
    newListTagsResponse,

    -- * Response Lenses
    listTagsResponse_nextToken,
    listTagsResponse_resourceTagList,
    listTagsResponse_httpStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Specifies a list of trail tags to return.
--
-- /See:/ 'newListTags' smart constructor.
data ListTags = ListTags'
  { -- | Reserved for future use.
    nextToken :: Core.Maybe Core.Text,
    -- | Specifies a list of trail ARNs whose tags will be listed. The list has a
    -- limit of 20 ARNs. The format of a trail ARN is:
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
    resourceIdList :: [Core.Text]
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
-- 'nextToken', 'listTags_nextToken' - Reserved for future use.
--
-- 'resourceIdList', 'listTags_resourceIdList' - Specifies a list of trail ARNs whose tags will be listed. The list has a
-- limit of 20 ARNs. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
newListTags ::
  ListTags
newListTags =
  ListTags'
    { nextToken = Core.Nothing,
      resourceIdList = Core.mempty
    }

-- | Reserved for future use.
listTags_nextToken :: Lens.Lens' ListTags (Core.Maybe Core.Text)
listTags_nextToken = Lens.lens (\ListTags' {nextToken} -> nextToken) (\s@ListTags' {} a -> s {nextToken = a} :: ListTags)

-- | Specifies a list of trail ARNs whose tags will be listed. The list has a
-- limit of 20 ARNs. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
listTags_resourceIdList :: Lens.Lens' ListTags [Core.Text]
listTags_resourceIdList = Lens.lens (\ListTags' {resourceIdList} -> resourceIdList) (\s@ListTags' {} a -> s {resourceIdList = a} :: ListTags) Core.. Lens._Coerce

instance Core.AWSPager ListTags where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTagsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listTagsResponse_resourceTagList Core.. Lens._Just
        ) =
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
            Core.<*> (x Core..?> "ResourceTagList" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTags

instance Core.NFData ListTags

instance Core.ToHeaders ListTags where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.ListTags" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListTags where
  toJSON ListTags' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            Core.Just ("ResourceIdList" Core..= resourceIdList)
          ]
      )

instance Core.ToPath ListTags where
  toPath = Core.const "/"

instance Core.ToQuery ListTags where
  toQuery = Core.const Core.mempty

-- | Returns the objects or data listed below if successful. Otherwise,
-- returns an error.
--
-- /See:/ 'newListTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { -- | Reserved for future use.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of resource tags.
    resourceTagList :: Core.Maybe [ResourceTag],
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
-- 'nextToken', 'listTagsResponse_nextToken' - Reserved for future use.
--
-- 'resourceTagList', 'listTagsResponse_resourceTagList' - A list of resource tags.
--
-- 'httpStatus', 'listTagsResponse_httpStatus' - The response's http status code.
newListTagsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTagsResponse
newListTagsResponse pHttpStatus_ =
  ListTagsResponse'
    { nextToken = Core.Nothing,
      resourceTagList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Reserved for future use.
listTagsResponse_nextToken :: Lens.Lens' ListTagsResponse (Core.Maybe Core.Text)
listTagsResponse_nextToken = Lens.lens (\ListTagsResponse' {nextToken} -> nextToken) (\s@ListTagsResponse' {} a -> s {nextToken = a} :: ListTagsResponse)

-- | A list of resource tags.
listTagsResponse_resourceTagList :: Lens.Lens' ListTagsResponse (Core.Maybe [ResourceTag])
listTagsResponse_resourceTagList = Lens.lens (\ListTagsResponse' {resourceTagList} -> resourceTagList) (\s@ListTagsResponse' {} a -> s {resourceTagList = a} :: ListTagsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTagsResponse_httpStatus :: Lens.Lens' ListTagsResponse Core.Int
listTagsResponse_httpStatus = Lens.lens (\ListTagsResponse' {httpStatus} -> httpStatus) (\s@ListTagsResponse' {} a -> s {httpStatus = a} :: ListTagsResponse)

instance Core.NFData ListTagsResponse
