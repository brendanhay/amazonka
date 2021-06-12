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
-- Module      : Network.AWS.Organizations.ListRoots
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the roots that are defined in the current organization.
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
-- Policy types can be enabled and disabled in roots. This is distinct from
-- whether they\'re available in the organization. When you enable all
-- features, you make policy types available for use in that organization.
-- Individual policy types can then be enabled and disabled in a root. To
-- see the availability of a policy type in an organization, use
-- DescribeOrganization.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListRoots
  ( -- * Creating a Request
    ListRoots (..),
    newListRoots,

    -- * Request Lenses
    listRoots_nextToken,
    listRoots_maxResults,

    -- * Destructuring the Response
    ListRootsResponse (..),
    newListRootsResponse,

    -- * Response Lenses
    listRootsResponse_nextToken,
    listRootsResponse_roots,
    listRootsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListRoots' smart constructor.
data ListRoots = ListRoots'
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
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListRoots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRoots_nextToken' - The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
--
-- 'maxResults', 'listRoots_maxResults' - The total number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- that is specific to the operation. If additional items exist beyond the
-- maximum you specify, the @NextToken@ response element is present and has
-- a value (is not null). Include that value as the @NextToken@ request
-- parameter in the next call to the operation to get the next part of the
-- results. Note that Organizations might return fewer results than the
-- maximum even when there are more results available. You should check
-- @NextToken@ after every operation to ensure that you receive all of the
-- results.
newListRoots ::
  ListRoots
newListRoots =
  ListRoots'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
listRoots_nextToken :: Lens.Lens' ListRoots (Core.Maybe Core.Text)
listRoots_nextToken = Lens.lens (\ListRoots' {nextToken} -> nextToken) (\s@ListRoots' {} a -> s {nextToken = a} :: ListRoots)

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
listRoots_maxResults :: Lens.Lens' ListRoots (Core.Maybe Core.Natural)
listRoots_maxResults = Lens.lens (\ListRoots' {maxResults} -> maxResults) (\s@ListRoots' {} a -> s {maxResults = a} :: ListRoots)

instance Core.AWSPager ListRoots where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRootsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listRootsResponse_roots Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listRoots_nextToken
          Lens..~ rs
          Lens.^? listRootsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListRoots where
  type AWSResponse ListRoots = ListRootsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRootsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Roots" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListRoots

instance Core.NFData ListRoots

instance Core.ToHeaders ListRoots where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.ListRoots" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListRoots where
  toJSON ListRoots' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListRoots where
  toPath = Core.const "/"

instance Core.ToQuery ListRoots where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListRootsResponse' smart constructor.
data ListRootsResponse = ListRootsResponse'
  { -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of roots that are defined in an organization.
    roots :: Core.Maybe [Root],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListRootsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRootsResponse_nextToken' - If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
--
-- 'roots', 'listRootsResponse_roots' - A list of roots that are defined in an organization.
--
-- 'httpStatus', 'listRootsResponse_httpStatus' - The response's http status code.
newListRootsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListRootsResponse
newListRootsResponse pHttpStatus_ =
  ListRootsResponse'
    { nextToken = Core.Nothing,
      roots = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listRootsResponse_nextToken :: Lens.Lens' ListRootsResponse (Core.Maybe Core.Text)
listRootsResponse_nextToken = Lens.lens (\ListRootsResponse' {nextToken} -> nextToken) (\s@ListRootsResponse' {} a -> s {nextToken = a} :: ListRootsResponse)

-- | A list of roots that are defined in an organization.
listRootsResponse_roots :: Lens.Lens' ListRootsResponse (Core.Maybe [Root])
listRootsResponse_roots = Lens.lens (\ListRootsResponse' {roots} -> roots) (\s@ListRootsResponse' {} a -> s {roots = a} :: ListRootsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listRootsResponse_httpStatus :: Lens.Lens' ListRootsResponse Core.Int
listRootsResponse_httpStatus = Lens.lens (\ListRootsResponse' {httpStatus} -> httpStatus) (\s@ListRootsResponse' {} a -> s {httpStatus = a} :: ListRootsResponse)

instance Core.NFData ListRootsResponse
