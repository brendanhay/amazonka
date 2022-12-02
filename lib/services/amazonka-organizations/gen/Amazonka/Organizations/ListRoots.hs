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
-- Module      : Amazonka.Organizations.ListRoots
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- Amazon Web Services service.
--
-- Policy types can be enabled and disabled in roots. This is distinct from
-- whether they\'re available in the organization. When you enable all
-- features, you make policy types available for use in that organization.
-- Individual policy types can then be enabled and disabled in a root. To
-- see the availability of a policy type in an organization, use
-- DescribeOrganization.
--
-- This operation returns paginated results.
module Amazonka.Organizations.ListRoots
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRoots' smart constructor.
data ListRoots = ListRoots'
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
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
listRoots_nextToken :: Lens.Lens' ListRoots (Prelude.Maybe Prelude.Text)
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
listRoots_maxResults :: Lens.Lens' ListRoots (Prelude.Maybe Prelude.Natural)
listRoots_maxResults = Lens.lens (\ListRoots' {maxResults} -> maxResults) (\s@ListRoots' {} a -> s {maxResults = a} :: ListRoots)

instance Core.AWSPager ListRoots where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRootsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRootsResponse_roots Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRoots_nextToken
          Lens..~ rs
          Lens.^? listRootsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListRoots where
  type AWSResponse ListRoots = ListRootsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRootsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Roots" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRoots where
  hashWithSalt _salt ListRoots' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListRoots where
  rnf ListRoots' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListRoots where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.ListRoots" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListRoots where
  toJSON ListRoots' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListRoots where
  toPath = Prelude.const "/"

instance Data.ToQuery ListRoots where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRootsResponse' smart constructor.
data ListRootsResponse = ListRootsResponse'
  { -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of roots that are defined in an organization.
    roots :: Prelude.Maybe [Root],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListRootsResponse
newListRootsResponse pHttpStatus_ =
  ListRootsResponse'
    { nextToken = Prelude.Nothing,
      roots = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listRootsResponse_nextToken :: Lens.Lens' ListRootsResponse (Prelude.Maybe Prelude.Text)
listRootsResponse_nextToken = Lens.lens (\ListRootsResponse' {nextToken} -> nextToken) (\s@ListRootsResponse' {} a -> s {nextToken = a} :: ListRootsResponse)

-- | A list of roots that are defined in an organization.
listRootsResponse_roots :: Lens.Lens' ListRootsResponse (Prelude.Maybe [Root])
listRootsResponse_roots = Lens.lens (\ListRootsResponse' {roots} -> roots) (\s@ListRootsResponse' {} a -> s {roots = a} :: ListRootsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRootsResponse_httpStatus :: Lens.Lens' ListRootsResponse Prelude.Int
listRootsResponse_httpStatus = Lens.lens (\ListRootsResponse' {httpStatus} -> httpStatus) (\s@ListRootsResponse' {} a -> s {httpStatus = a} :: ListRootsResponse)

instance Prelude.NFData ListRootsResponse where
  rnf ListRootsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf roots
      `Prelude.seq` Prelude.rnf httpStatus
