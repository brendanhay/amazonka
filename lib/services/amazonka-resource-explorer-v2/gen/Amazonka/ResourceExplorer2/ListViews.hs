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
-- Module      : Amazonka.ResourceExplorer2.ListViews
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource names (ARNs)>
-- of the views available in the Amazon Web Services Region in which you
-- call this operation.
--
-- Always check the @NextToken@ response parameter for a @null@ value when
-- calling a paginated operation. These operations can occasionally return
-- an empty set of results even when there are more results available. The
-- @NextToken@ response parameter value is @null@ /only/ when there are no
-- more results to display.
--
-- This operation returns paginated results.
module Amazonka.ResourceExplorer2.ListViews
  ( -- * Creating a Request
    ListViews (..),
    newListViews,

    -- * Request Lenses
    listViews_maxResults,
    listViews_nextToken,

    -- * Destructuring the Response
    ListViewsResponse (..),
    newListViewsResponse,

    -- * Response Lenses
    listViewsResponse_nextToken,
    listViewsResponse_views,
    listViewsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResourceExplorer2.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newListViews' smart constructor.
data ListViews = ListViews'
  { -- | The maximum number of results that you want included on each page of the
    -- response. If you do not include this parameter, it defaults to a value
    -- appropriate to the operation. If additional items exist beyond those
    -- included in the current response, the @NextToken@ response element is
    -- present and has a value (is not null). Include that value as the
    -- @NextToken@ request parameter in the next call to the operation to get
    -- the next part of the results.
    --
    -- An API operation can return fewer results than the maximum even when
    -- there are more results available. You should check @NextToken@ after
    -- every operation to ensure that you receive all of the results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The parameter for receiving additional results if you receive a
    -- @NextToken@ response in a previous request. A @NextToken@ response
    -- indicates that more output is available. Set this parameter to the value
    -- of the previous call\'s @NextToken@ response to indicate where the
    -- output should continue from.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListViews' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listViews_maxResults' - The maximum number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- appropriate to the operation. If additional items exist beyond those
-- included in the current response, the @NextToken@ response element is
-- present and has a value (is not null). Include that value as the
-- @NextToken@ request parameter in the next call to the operation to get
-- the next part of the results.
--
-- An API operation can return fewer results than the maximum even when
-- there are more results available. You should check @NextToken@ after
-- every operation to ensure that you receive all of the results.
--
-- 'nextToken', 'listViews_nextToken' - The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
newListViews ::
  ListViews
newListViews =
  ListViews'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- appropriate to the operation. If additional items exist beyond those
-- included in the current response, the @NextToken@ response element is
-- present and has a value (is not null). Include that value as the
-- @NextToken@ request parameter in the next call to the operation to get
-- the next part of the results.
--
-- An API operation can return fewer results than the maximum even when
-- there are more results available. You should check @NextToken@ after
-- every operation to ensure that you receive all of the results.
listViews_maxResults :: Lens.Lens' ListViews (Prelude.Maybe Prelude.Natural)
listViews_maxResults = Lens.lens (\ListViews' {maxResults} -> maxResults) (\s@ListViews' {} a -> s {maxResults = a} :: ListViews)

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
listViews_nextToken :: Lens.Lens' ListViews (Prelude.Maybe Prelude.Text)
listViews_nextToken = Lens.lens (\ListViews' {nextToken} -> nextToken) (\s@ListViews' {} a -> s {nextToken = a} :: ListViews)

instance Core.AWSPager ListViews where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listViewsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listViewsResponse_views
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listViews_nextToken
              Lens..~ rs
              Lens.^? listViewsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListViews where
  type AWSResponse ListViews = ListViewsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListViewsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Views" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListViews where
  hashWithSalt _salt ListViews' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListViews where
  rnf ListViews' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListViews where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListViews where
  toJSON ListViews' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListViews where
  toPath = Prelude.const "/ListViews"

instance Data.ToQuery ListViews where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListViewsResponse' smart constructor.
data ListViewsResponse = ListViewsResponse'
  { -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of views available in the Amazon Web Services Region in which
    -- you called this operation.
    views :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListViewsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listViewsResponse_nextToken' - If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
--
-- 'views', 'listViewsResponse_views' - The list of views available in the Amazon Web Services Region in which
-- you called this operation.
--
-- 'httpStatus', 'listViewsResponse_httpStatus' - The response's http status code.
newListViewsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListViewsResponse
newListViewsResponse pHttpStatus_ =
  ListViewsResponse'
    { nextToken = Prelude.Nothing,
      views = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listViewsResponse_nextToken :: Lens.Lens' ListViewsResponse (Prelude.Maybe Prelude.Text)
listViewsResponse_nextToken = Lens.lens (\ListViewsResponse' {nextToken} -> nextToken) (\s@ListViewsResponse' {} a -> s {nextToken = a} :: ListViewsResponse)

-- | The list of views available in the Amazon Web Services Region in which
-- you called this operation.
listViewsResponse_views :: Lens.Lens' ListViewsResponse (Prelude.Maybe [Prelude.Text])
listViewsResponse_views = Lens.lens (\ListViewsResponse' {views} -> views) (\s@ListViewsResponse' {} a -> s {views = a} :: ListViewsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listViewsResponse_httpStatus :: Lens.Lens' ListViewsResponse Prelude.Int
listViewsResponse_httpStatus = Lens.lens (\ListViewsResponse' {httpStatus} -> httpStatus) (\s@ListViewsResponse' {} a -> s {httpStatus = a} :: ListViewsResponse)

instance Prelude.NFData ListViewsResponse where
  rnf ListViewsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf views `Prelude.seq`
        Prelude.rnf httpStatus
