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
-- Module      : Network.AWS.Organizations.ListTagsForResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists tags that are attached to the specified resource.
--
-- You can attach tags to the following resources in AWS Organizations.
--
-- -   AWS account
--
-- -   Organization root
--
-- -   Organizational unit (OU)
--
-- -   Policy (any type)
--
-- This operation can be called only from the organization\'s management
-- account or by a member account that is a delegated administrator for an
-- AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListTagsForResource
  ( -- * Creating a Request
    ListTagsForResource (..),
    newListTagsForResource,

    -- * Request Lenses
    listTagsForResource_nextToken,
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
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { -- | The parameter for receiving additional results if you receive a
    -- @NextToken@ response in a previous request. A @NextToken@ response
    -- indicates that more output is available. Set this parameter to the value
    -- of the previous call\'s @NextToken@ response to indicate where the
    -- output should continue from.
    nextToken :: Core.Maybe Core.Text,
    -- | The ID of the resource with the tags to list.
    --
    -- You can specify any of the following taggable resources.
    --
    -- -   AWS account – specify the account ID number.
    --
    -- -   Organizational unit – specify the OU ID that begins with @ou-@ and
    --     looks similar to: @ou-1a2b-34uvwxyz @
    --
    -- -   Root – specify the root ID that begins with @r-@ and looks similar
    --     to: @r-1a2b @
    --
    -- -   Policy – specify the policy ID that begins with @p-@ andlooks
    --     similar to: @p-12abcdefg3 @
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
-- 'nextToken', 'listTagsForResource_nextToken' - The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
--
-- 'resourceId', 'listTagsForResource_resourceId' - The ID of the resource with the tags to list.
--
-- You can specify any of the following taggable resources.
--
-- -   AWS account – specify the account ID number.
--
-- -   Organizational unit – specify the OU ID that begins with @ou-@ and
--     looks similar to: @ou-1a2b-34uvwxyz @
--
-- -   Root – specify the root ID that begins with @r-@ and looks similar
--     to: @r-1a2b @
--
-- -   Policy – specify the policy ID that begins with @p-@ andlooks
--     similar to: @p-12abcdefg3 @
newListTagsForResource ::
  -- | 'resourceId'
  Core.Text ->
  ListTagsForResource
newListTagsForResource pResourceId_ =
  ListTagsForResource'
    { nextToken = Core.Nothing,
      resourceId = pResourceId_
    }

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
listTagsForResource_nextToken :: Lens.Lens' ListTagsForResource (Core.Maybe Core.Text)
listTagsForResource_nextToken = Lens.lens (\ListTagsForResource' {nextToken} -> nextToken) (\s@ListTagsForResource' {} a -> s {nextToken = a} :: ListTagsForResource)

-- | The ID of the resource with the tags to list.
--
-- You can specify any of the following taggable resources.
--
-- -   AWS account – specify the account ID number.
--
-- -   Organizational unit – specify the OU ID that begins with @ou-@ and
--     looks similar to: @ou-1a2b-34uvwxyz @
--
-- -   Root – specify the root ID that begins with @r-@ and looks similar
--     to: @r-1a2b @
--
-- -   Policy – specify the policy ID that begins with @p-@ andlooks
--     similar to: @p-12abcdefg3 @
listTagsForResource_resourceId :: Lens.Lens' ListTagsForResource Core.Text
listTagsForResource_resourceId = Lens.lens (\ListTagsForResource' {resourceId} -> resourceId) (\s@ListTagsForResource' {} a -> s {resourceId = a} :: ListTagsForResource)

instance Core.AWSPager ListTagsForResource where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTagsForResourceResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listTagsForResourceResponse_tags Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listTagsForResource_nextToken
          Lens..~ rs
          Lens.^? listTagsForResourceResponse_nextToken
            Core.. Lens._Just

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
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.ListTagsForResource" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListTagsForResource where
  toJSON ListTagsForResource' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            Core.Just ("ResourceId" Core..= resourceId)
          ]
      )

instance Core.ToPath ListTagsForResource where
  toPath = Core.const "/"

instance Core.ToQuery ListTagsForResource where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Core.Maybe Core.Text,
    -- | The tags that are assigned to the resource.
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
-- 'nextToken', 'listTagsForResourceResponse_nextToken' - If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
--
-- 'tags', 'listTagsForResourceResponse_tags' - The tags that are assigned to the resource.
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

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listTagsForResourceResponse_nextToken :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe Core.Text)
listTagsForResourceResponse_nextToken = Lens.lens (\ListTagsForResourceResponse' {nextToken} -> nextToken) (\s@ListTagsForResourceResponse' {} a -> s {nextToken = a} :: ListTagsForResourceResponse)

-- | The tags that are assigned to the resource.
listTagsForResourceResponse_tags :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe [Tag])
listTagsForResourceResponse_tags = Lens.lens (\ListTagsForResourceResponse' {tags} -> tags) (\s@ListTagsForResourceResponse' {} a -> s {tags = a} :: ListTagsForResourceResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTagsForResourceResponse_httpStatus :: Lens.Lens' ListTagsForResourceResponse Core.Int
listTagsForResourceResponse_httpStatus = Lens.lens (\ListTagsForResourceResponse' {httpStatus} -> httpStatus) (\s@ListTagsForResourceResponse' {} a -> s {httpStatus = a} :: ListTagsForResourceResponse)

instance Core.NFData ListTagsForResourceResponse
