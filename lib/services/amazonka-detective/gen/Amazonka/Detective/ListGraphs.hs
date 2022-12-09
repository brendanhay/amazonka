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
-- Module      : Amazonka.Detective.ListGraphs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of behavior graphs that the calling account is an
-- administrator account of. This operation can only be called by an
-- administrator account.
--
-- Because an account can currently only be the administrator of one
-- behavior graph within a Region, the results always contain a single
-- behavior graph.
module Amazonka.Detective.ListGraphs
  ( -- * Creating a Request
    ListGraphs (..),
    newListGraphs,

    -- * Request Lenses
    listGraphs_maxResults,
    listGraphs_nextToken,

    -- * Destructuring the Response
    ListGraphsResponse (..),
    newListGraphsResponse,

    -- * Response Lenses
    listGraphsResponse_graphList,
    listGraphsResponse_nextToken,
    listGraphsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Detective.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListGraphs' smart constructor.
data ListGraphs = ListGraphs'
  { -- | The maximum number of graphs to return at a time. The total must be less
    -- than the overall limit on the number of results to return, which is
    -- currently 200.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | For requests to get the next page of results, the pagination token that
    -- was returned with the previous set of results. The initial request does
    -- not include a pagination token.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGraphs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listGraphs_maxResults' - The maximum number of graphs to return at a time. The total must be less
-- than the overall limit on the number of results to return, which is
-- currently 200.
--
-- 'nextToken', 'listGraphs_nextToken' - For requests to get the next page of results, the pagination token that
-- was returned with the previous set of results. The initial request does
-- not include a pagination token.
newListGraphs ::
  ListGraphs
newListGraphs =
  ListGraphs'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of graphs to return at a time. The total must be less
-- than the overall limit on the number of results to return, which is
-- currently 200.
listGraphs_maxResults :: Lens.Lens' ListGraphs (Prelude.Maybe Prelude.Natural)
listGraphs_maxResults = Lens.lens (\ListGraphs' {maxResults} -> maxResults) (\s@ListGraphs' {} a -> s {maxResults = a} :: ListGraphs)

-- | For requests to get the next page of results, the pagination token that
-- was returned with the previous set of results. The initial request does
-- not include a pagination token.
listGraphs_nextToken :: Lens.Lens' ListGraphs (Prelude.Maybe Prelude.Text)
listGraphs_nextToken = Lens.lens (\ListGraphs' {nextToken} -> nextToken) (\s@ListGraphs' {} a -> s {nextToken = a} :: ListGraphs)

instance Core.AWSRequest ListGraphs where
  type AWSResponse ListGraphs = ListGraphsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGraphsResponse'
            Prelude.<$> (x Data..?> "GraphList" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGraphs where
  hashWithSalt _salt ListGraphs' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListGraphs where
  rnf ListGraphs' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListGraphs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListGraphs where
  toJSON ListGraphs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListGraphs where
  toPath = Prelude.const "/graphs/list"

instance Data.ToQuery ListGraphs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListGraphsResponse' smart constructor.
data ListGraphsResponse = ListGraphsResponse'
  { -- | A list of behavior graphs that the account is an administrator account
    -- for.
    graphList :: Prelude.Maybe [Graph],
    -- | If there are more behavior graphs remaining in the results, then this is
    -- the pagination token to use to request the next page of behavior graphs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGraphsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'graphList', 'listGraphsResponse_graphList' - A list of behavior graphs that the account is an administrator account
-- for.
--
-- 'nextToken', 'listGraphsResponse_nextToken' - If there are more behavior graphs remaining in the results, then this is
-- the pagination token to use to request the next page of behavior graphs.
--
-- 'httpStatus', 'listGraphsResponse_httpStatus' - The response's http status code.
newListGraphsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGraphsResponse
newListGraphsResponse pHttpStatus_ =
  ListGraphsResponse'
    { graphList = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of behavior graphs that the account is an administrator account
-- for.
listGraphsResponse_graphList :: Lens.Lens' ListGraphsResponse (Prelude.Maybe [Graph])
listGraphsResponse_graphList = Lens.lens (\ListGraphsResponse' {graphList} -> graphList) (\s@ListGraphsResponse' {} a -> s {graphList = a} :: ListGraphsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are more behavior graphs remaining in the results, then this is
-- the pagination token to use to request the next page of behavior graphs.
listGraphsResponse_nextToken :: Lens.Lens' ListGraphsResponse (Prelude.Maybe Prelude.Text)
listGraphsResponse_nextToken = Lens.lens (\ListGraphsResponse' {nextToken} -> nextToken) (\s@ListGraphsResponse' {} a -> s {nextToken = a} :: ListGraphsResponse)

-- | The response's http status code.
listGraphsResponse_httpStatus :: Lens.Lens' ListGraphsResponse Prelude.Int
listGraphsResponse_httpStatus = Lens.lens (\ListGraphsResponse' {httpStatus} -> httpStatus) (\s@ListGraphsResponse' {} a -> s {httpStatus = a} :: ListGraphsResponse)

instance Prelude.NFData ListGraphsResponse where
  rnf ListGraphsResponse' {..} =
    Prelude.rnf graphList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
