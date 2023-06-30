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
-- Module      : Amazonka.Organizations.ListChildren
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- Amazon Web Services service.
--
-- This operation returns paginated results.
module Amazonka.Organizations.ListChildren
  ( -- * Creating a Request
    ListChildren (..),
    newListChildren,

    -- * Request Lenses
    listChildren_maxResults,
    listChildren_nextToken,
    listChildren_parentId,
    listChildren_childType,

    -- * Destructuring the Response
    ListChildrenResponse (..),
    newListChildrenResponse,

    -- * Response Lenses
    listChildrenResponse_children,
    listChildrenResponse_nextToken,
    listChildrenResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListChildren' smart constructor.
data ListChildren = ListChildren'
  { -- | The total number of results that you want included on each page of the
    -- response. If you do not include this parameter, it defaults to a value
    -- that is specific to the operation. If additional items exist beyond the
    -- maximum you specify, the @NextToken@ response element is present and has
    -- a value (is not null). Include that value as the @NextToken@ request
    -- parameter in the next call to the operation to get the next part of the
    -- results. Note that Organizations might return fewer results than the
    -- maximum even when there are more results available. You should check
    -- @NextToken@ after every operation to ensure that you receive all of the
    -- results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The parameter for receiving additional results if you receive a
    -- @NextToken@ response in a previous request. A @NextToken@ response
    -- indicates that more output is available. Set this parameter to the value
    -- of the previous call\'s @NextToken@ response to indicate where the
    -- output should continue from.
    nextToken :: Prelude.Maybe Prelude.Text,
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
    parentId :: Prelude.Text,
    -- | Filters the output to include only the specified child type.
    childType :: ChildType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChildren' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'nextToken', 'listChildren_nextToken' - The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
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
  Prelude.Text ->
  -- | 'childType'
  ChildType ->
  ListChildren
newListChildren pParentId_ pChildType_ =
  ListChildren'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      parentId = pParentId_,
      childType = pChildType_
    }

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
listChildren_maxResults :: Lens.Lens' ListChildren (Prelude.Maybe Prelude.Natural)
listChildren_maxResults = Lens.lens (\ListChildren' {maxResults} -> maxResults) (\s@ListChildren' {} a -> s {maxResults = a} :: ListChildren)

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
listChildren_nextToken :: Lens.Lens' ListChildren (Prelude.Maybe Prelude.Text)
listChildren_nextToken = Lens.lens (\ListChildren' {nextToken} -> nextToken) (\s@ListChildren' {} a -> s {nextToken = a} :: ListChildren)

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
listChildren_parentId :: Lens.Lens' ListChildren Prelude.Text
listChildren_parentId = Lens.lens (\ListChildren' {parentId} -> parentId) (\s@ListChildren' {} a -> s {parentId = a} :: ListChildren)

-- | Filters the output to include only the specified child type.
listChildren_childType :: Lens.Lens' ListChildren ChildType
listChildren_childType = Lens.lens (\ListChildren' {childType} -> childType) (\s@ListChildren' {} a -> s {childType = a} :: ListChildren)

instance Core.AWSPager ListChildren where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listChildrenResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listChildrenResponse_children
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listChildren_nextToken
          Lens..~ rs
          Lens.^? listChildrenResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListChildren where
  type AWSResponse ListChildren = ListChildrenResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListChildrenResponse'
            Prelude.<$> (x Data..?> "Children" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListChildren where
  hashWithSalt _salt ListChildren' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` parentId
      `Prelude.hashWithSalt` childType

instance Prelude.NFData ListChildren where
  rnf ListChildren' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf parentId
      `Prelude.seq` Prelude.rnf childType

instance Data.ToHeaders ListChildren where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.ListChildren" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListChildren where
  toJSON ListChildren' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("ParentId" Data..= parentId),
            Prelude.Just ("ChildType" Data..= childType)
          ]
      )

instance Data.ToPath ListChildren where
  toPath = Prelude.const "/"

instance Data.ToQuery ListChildren where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListChildrenResponse' smart constructor.
data ListChildrenResponse = ListChildrenResponse'
  { -- | The list of children of the specified parent container.
    children :: Prelude.Maybe [Child],
    -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChildrenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'children', 'listChildrenResponse_children' - The list of children of the specified parent container.
--
-- 'nextToken', 'listChildrenResponse_nextToken' - If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
--
-- 'httpStatus', 'listChildrenResponse_httpStatus' - The response's http status code.
newListChildrenResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListChildrenResponse
newListChildrenResponse pHttpStatus_ =
  ListChildrenResponse'
    { children = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of children of the specified parent container.
listChildrenResponse_children :: Lens.Lens' ListChildrenResponse (Prelude.Maybe [Child])
listChildrenResponse_children = Lens.lens (\ListChildrenResponse' {children} -> children) (\s@ListChildrenResponse' {} a -> s {children = a} :: ListChildrenResponse) Prelude.. Lens.mapping Lens.coerced

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listChildrenResponse_nextToken :: Lens.Lens' ListChildrenResponse (Prelude.Maybe Prelude.Text)
listChildrenResponse_nextToken = Lens.lens (\ListChildrenResponse' {nextToken} -> nextToken) (\s@ListChildrenResponse' {} a -> s {nextToken = a} :: ListChildrenResponse)

-- | The response's http status code.
listChildrenResponse_httpStatus :: Lens.Lens' ListChildrenResponse Prelude.Int
listChildrenResponse_httpStatus = Lens.lens (\ListChildrenResponse' {httpStatus} -> httpStatus) (\s@ListChildrenResponse' {} a -> s {httpStatus = a} :: ListChildrenResponse)

instance Prelude.NFData ListChildrenResponse where
  rnf ListChildrenResponse' {..} =
    Prelude.rnf children
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
