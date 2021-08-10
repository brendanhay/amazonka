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
-- Module      : Network.AWS.Organizations.ListParents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the root or organizational units (OUs) that serve as the immediate
-- parent of the specified child OU or account. This operation, along with
-- ListChildren enables you to traverse the tree structure that makes up
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
-- In the current release, a child can have only a single parent.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListParents
  ( -- * Creating a Request
    ListParents (..),
    newListParents,

    -- * Request Lenses
    listParents_nextToken,
    listParents_maxResults,
    listParents_childId,

    -- * Destructuring the Response
    ListParentsResponse (..),
    newListParentsResponse,

    -- * Response Lenses
    listParentsResponse_parents,
    listParentsResponse_nextToken,
    listParentsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListParents' smart constructor.
data ListParents = ListParents'
  { -- | The parameter for receiving additional results if you receive a
    -- @NextToken@ response in a previous request. A @NextToken@ response
    -- indicates that more output is available. Set this parameter to the value
    -- of the previous call\'s @NextToken@ response to indicate where the
    -- output should continue from.
    nextToken :: Prelude.Maybe Prelude.Text,
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
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The unique identifier (ID) of the OU or account whose parent containers
    -- you want to list. Don\'t specify a root.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a child ID
    -- string requires one of the following:
    --
    -- -   __Account__ - A string that consists of exactly 12 digits.
    --
    -- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
    --     followed by from 4 to 32 lowercase letters or digits (the ID of the
    --     root that contains the OU). This string is followed by a second
    --     \"-\" dash and from 8 to 32 additional lowercase letters or digits.
    childId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListParents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listParents_nextToken' - The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
--
-- 'maxResults', 'listParents_maxResults' - The total number of results that you want included on each page of the
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
-- 'childId', 'listParents_childId' - The unique identifier (ID) of the OU or account whose parent containers
-- you want to list. Don\'t specify a root.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a child ID
-- string requires one of the following:
--
-- -   __Account__ - A string that consists of exactly 12 digits.
--
-- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
--     followed by from 4 to 32 lowercase letters or digits (the ID of the
--     root that contains the OU). This string is followed by a second
--     \"-\" dash and from 8 to 32 additional lowercase letters or digits.
newListParents ::
  -- | 'childId'
  Prelude.Text ->
  ListParents
newListParents pChildId_ =
  ListParents'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      childId = pChildId_
    }

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
listParents_nextToken :: Lens.Lens' ListParents (Prelude.Maybe Prelude.Text)
listParents_nextToken = Lens.lens (\ListParents' {nextToken} -> nextToken) (\s@ListParents' {} a -> s {nextToken = a} :: ListParents)

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
listParents_maxResults :: Lens.Lens' ListParents (Prelude.Maybe Prelude.Natural)
listParents_maxResults = Lens.lens (\ListParents' {maxResults} -> maxResults) (\s@ListParents' {} a -> s {maxResults = a} :: ListParents)

-- | The unique identifier (ID) of the OU or account whose parent containers
-- you want to list. Don\'t specify a root.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a child ID
-- string requires one of the following:
--
-- -   __Account__ - A string that consists of exactly 12 digits.
--
-- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
--     followed by from 4 to 32 lowercase letters or digits (the ID of the
--     root that contains the OU). This string is followed by a second
--     \"-\" dash and from 8 to 32 additional lowercase letters or digits.
listParents_childId :: Lens.Lens' ListParents Prelude.Text
listParents_childId = Lens.lens (\ListParents' {childId} -> childId) (\s@ListParents' {} a -> s {childId = a} :: ListParents)

instance Core.AWSPager ListParents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listParentsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listParentsResponse_parents Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listParents_nextToken
          Lens..~ rs
          Lens.^? listParentsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListParents where
  type AWSResponse ListParents = ListParentsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListParentsResponse'
            Prelude.<$> (x Core..?> "Parents" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListParents

instance Prelude.NFData ListParents

instance Core.ToHeaders ListParents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.ListParents" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListParents where
  toJSON ListParents' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("ChildId" Core..= childId)
          ]
      )

instance Core.ToPath ListParents where
  toPath = Prelude.const "/"

instance Core.ToQuery ListParents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListParentsResponse' smart constructor.
data ListParentsResponse = ListParentsResponse'
  { -- | A list of parents for the specified child account or OU.
    parents :: Prelude.Maybe [Parent],
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
-- Create a value of 'ListParentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parents', 'listParentsResponse_parents' - A list of parents for the specified child account or OU.
--
-- 'nextToken', 'listParentsResponse_nextToken' - If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
--
-- 'httpStatus', 'listParentsResponse_httpStatus' - The response's http status code.
newListParentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListParentsResponse
newListParentsResponse pHttpStatus_ =
  ListParentsResponse'
    { parents = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of parents for the specified child account or OU.
listParentsResponse_parents :: Lens.Lens' ListParentsResponse (Prelude.Maybe [Parent])
listParentsResponse_parents = Lens.lens (\ListParentsResponse' {parents} -> parents) (\s@ListParentsResponse' {} a -> s {parents = a} :: ListParentsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listParentsResponse_nextToken :: Lens.Lens' ListParentsResponse (Prelude.Maybe Prelude.Text)
listParentsResponse_nextToken = Lens.lens (\ListParentsResponse' {nextToken} -> nextToken) (\s@ListParentsResponse' {} a -> s {nextToken = a} :: ListParentsResponse)

-- | The response's http status code.
listParentsResponse_httpStatus :: Lens.Lens' ListParentsResponse Prelude.Int
listParentsResponse_httpStatus = Lens.lens (\ListParentsResponse' {httpStatus} -> httpStatus) (\s@ListParentsResponse' {} a -> s {httpStatus = a} :: ListParentsResponse)

instance Prelude.NFData ListParentsResponse
