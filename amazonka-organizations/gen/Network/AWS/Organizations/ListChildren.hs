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
-- Module      : Network.AWS.Organizations.ListChildren
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the organizational units (OUs) or accounts that are
-- contained in the specified parent OU or root. This operation, along with
-- ListParents enables you to traverse the tree structure that makes up
-- this root.
--
-- Always check the @NextToken@ response parameter for a @null@ value when
-- calling a @List*@ operation. These operations can occasionally return an
-- empty set of results even when there are more results available. The
-- @NextToken@ response parameter value is @null@ /only/ when there are no
-- more results to display.
--
-- This operation can be called only from the organization\'s management
-- account or by a member account that is a delegated administrator for an
-- AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListChildren
  ( -- * Creating a Request
    ListChildren (..),
    newListChildren,

    -- * Request Lenses
    listChildren_nextToken,
    listChildren_maxResults,
    listChildren_parentId,
    listChildren_childType,

    -- * Destructuring the Response
    ListChildrenResponse (..),
    newListChildrenResponse,

    -- * Response Lenses
    listChildrenResponse_nextToken,
    listChildrenResponse_children,
    listChildrenResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListChildren' smart constructor.
data ListChildren = ListChildren'
  { -- | The parameter for receiving additional results if you receive a
    -- @NextToken@ response in a previous request. A @NextToken@ response
    -- indicates that more output is available. Set this parameter to the value
    -- of the previous call\'s @NextToken@ response to indicate where the
    -- output should continue from.
    nextToken :: Core.Maybe Core.Text,
    -- | The total number of results that you want included on each page of the
    -- response. If you do not include this parameter, it defaults to a value
    -- that is specific to the operation. If additional items exist beyond the
    -- maximum you specify, the @NextToken@ response element is present and has
    -- a value (is not null). Include that value as the @NextToken@ request
    -- parameter in the next call to the operation to get the next part of the
    -- results. Note that Organizations might return fewer results than the
    -- maximum even when there are more results available. You should check
    -- @NextToken@ after every operation to ensure that you receive all of the
    -- results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The unique identifier (ID) for the parent root or OU whose children you
    -- want to list.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID
    -- string requires one of the following:
    --
    -- -   __Root__ - A string that begins with \"r-\" followed by from 4 to 32
    --     lowercase letters or digits.
    --
    -- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
    --     followed by from 4 to 32 lowercase letters or digits (the ID of the
    --     root that the OU is in). This string is followed by a second \"-\"
    --     dash and from 8 to 32 additional lowercase letters or digits.
    parentId :: Core.Text,
    -- | Filters the output to include only the specified child type.
    childType :: ChildType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListChildren' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listChildren_nextToken' - The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
--
-- 'maxResults', 'listChildren_maxResults' - The total number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- that is specific to the operation. If additional items exist beyond the
-- maximum you specify, the @NextToken@ response element is present and has
-- a value (is not null). Include that value as the @NextToken@ request
-- parameter in the next call to the operation to get the next part of the
-- results. Note that Organizations might return fewer results than the
-- maximum even when there are more results available. You should check
-- @NextToken@ after every operation to ensure that you receive all of the
-- results.
--
-- 'parentId', 'listChildren_parentId' - The unique identifier (ID) for the parent root or OU whose children you
-- want to list.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID
-- string requires one of the following:
--
-- -   __Root__ - A string that begins with \"r-\" followed by from 4 to 32
--     lowercase letters or digits.
--
-- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
--     followed by from 4 to 32 lowercase letters or digits (the ID of the
--     root that the OU is in). This string is followed by a second \"-\"
--     dash and from 8 to 32 additional lowercase letters or digits.
--
-- 'childType', 'listChildren_childType' - Filters the output to include only the specified child type.
newListChildren ::
  -- | 'parentId'
  Core.Text ->
  -- | 'childType'
  ChildType ->
  ListChildren
newListChildren pParentId_ pChildType_ =
  ListChildren'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      parentId = pParentId_,
      childType = pChildType_
    }

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
listChildren_nextToken :: Lens.Lens' ListChildren (Core.Maybe Core.Text)
listChildren_nextToken = Lens.lens (\ListChildren' {nextToken} -> nextToken) (\s@ListChildren' {} a -> s {nextToken = a} :: ListChildren)

-- | The total number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- that is specific to the operation. If additional items exist beyond the
-- maximum you specify, the @NextToken@ response element is present and has
-- a value (is not null). Include that value as the @NextToken@ request
-- parameter in the next call to the operation to get the next part of the
-- results. Note that Organizations might return fewer results than the
-- maximum even when there are more results available. You should check
-- @NextToken@ after every operation to ensure that you receive all of the
-- results.
listChildren_maxResults :: Lens.Lens' ListChildren (Core.Maybe Core.Natural)
listChildren_maxResults = Lens.lens (\ListChildren' {maxResults} -> maxResults) (\s@ListChildren' {} a -> s {maxResults = a} :: ListChildren)

-- | The unique identifier (ID) for the parent root or OU whose children you
-- want to list.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID
-- string requires one of the following:
--
-- -   __Root__ - A string that begins with \"r-\" followed by from 4 to 32
--     lowercase letters or digits.
--
-- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
--     followed by from 4 to 32 lowercase letters or digits (the ID of the
--     root that the OU is in). This string is followed by a second \"-\"
--     dash and from 8 to 32 additional lowercase letters or digits.
listChildren_parentId :: Lens.Lens' ListChildren Core.Text
listChildren_parentId = Lens.lens (\ListChildren' {parentId} -> parentId) (\s@ListChildren' {} a -> s {parentId = a} :: ListChildren)

-- | Filters the output to include only the specified child type.
listChildren_childType :: Lens.Lens' ListChildren ChildType
listChildren_childType = Lens.lens (\ListChildren' {childType} -> childType) (\s@ListChildren' {} a -> s {childType = a} :: ListChildren)

instance Core.AWSPager ListChildren where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listChildrenResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listChildrenResponse_children Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listChildren_nextToken
          Lens..~ rs
          Lens.^? listChildrenResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListChildren where
  type AWSResponse ListChildren = ListChildrenResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListChildrenResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Children" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListChildren

instance Core.NFData ListChildren

instance Core.ToHeaders ListChildren where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.ListChildren" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListChildren where
  toJSON ListChildren' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("ParentId" Core..= parentId),
            Core.Just ("ChildType" Core..= childType)
          ]
      )

instance Core.ToPath ListChildren where
  toPath = Core.const "/"

instance Core.ToQuery ListChildren where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListChildrenResponse' smart constructor.
data ListChildrenResponse = ListChildrenResponse'
  { -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of children of the specified parent container.
    children :: Core.Maybe [Child],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListChildrenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listChildrenResponse_nextToken' - If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
--
-- 'children', 'listChildrenResponse_children' - The list of children of the specified parent container.
--
-- 'httpStatus', 'listChildrenResponse_httpStatus' - The response's http status code.
newListChildrenResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListChildrenResponse
newListChildrenResponse pHttpStatus_ =
  ListChildrenResponse'
    { nextToken = Core.Nothing,
      children = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listChildrenResponse_nextToken :: Lens.Lens' ListChildrenResponse (Core.Maybe Core.Text)
listChildrenResponse_nextToken = Lens.lens (\ListChildrenResponse' {nextToken} -> nextToken) (\s@ListChildrenResponse' {} a -> s {nextToken = a} :: ListChildrenResponse)

-- | The list of children of the specified parent container.
listChildrenResponse_children :: Lens.Lens' ListChildrenResponse (Core.Maybe [Child])
listChildrenResponse_children = Lens.lens (\ListChildrenResponse' {children} -> children) (\s@ListChildrenResponse' {} a -> s {children = a} :: ListChildrenResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listChildrenResponse_httpStatus :: Lens.Lens' ListChildrenResponse Core.Int
listChildrenResponse_httpStatus = Lens.lens (\ListChildrenResponse' {httpStatus} -> httpStatus) (\s@ListChildrenResponse' {} a -> s {httpStatus = a} :: ListChildrenResponse)

instance Core.NFData ListChildrenResponse
