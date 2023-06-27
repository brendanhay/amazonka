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
-- Module      : Amazonka.Kendra.ListFeaturedResultsSets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all your sets of featured results for a given index. Features
-- results are placed above all other results for certain queries. If
-- there\'s an exact match of a query, then one or more specific documents
-- are featured in the search results.
module Amazonka.Kendra.ListFeaturedResultsSets
  ( -- * Creating a Request
    ListFeaturedResultsSets (..),
    newListFeaturedResultsSets,

    -- * Request Lenses
    listFeaturedResultsSets_maxResults,
    listFeaturedResultsSets_nextToken,
    listFeaturedResultsSets_indexId,

    -- * Destructuring the Response
    ListFeaturedResultsSetsResponse (..),
    newListFeaturedResultsSetsResponse,

    -- * Response Lenses
    listFeaturedResultsSetsResponse_featuredResultsSetSummaryItems,
    listFeaturedResultsSetsResponse_nextToken,
    listFeaturedResultsSetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFeaturedResultsSets' smart constructor.
data ListFeaturedResultsSets = ListFeaturedResultsSets'
  { -- | The maximum number of featured results sets to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the response is truncated, Amazon Kendra returns a pagination token
    -- in the response. You can use this pagination token to retrieve the next
    -- set of featured results sets.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the index used for featuring results.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFeaturedResultsSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listFeaturedResultsSets_maxResults' - The maximum number of featured results sets to return.
--
-- 'nextToken', 'listFeaturedResultsSets_nextToken' - If the response is truncated, Amazon Kendra returns a pagination token
-- in the response. You can use this pagination token to retrieve the next
-- set of featured results sets.
--
-- 'indexId', 'listFeaturedResultsSets_indexId' - The identifier of the index used for featuring results.
newListFeaturedResultsSets ::
  -- | 'indexId'
  Prelude.Text ->
  ListFeaturedResultsSets
newListFeaturedResultsSets pIndexId_ =
  ListFeaturedResultsSets'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      indexId = pIndexId_
    }

-- | The maximum number of featured results sets to return.
listFeaturedResultsSets_maxResults :: Lens.Lens' ListFeaturedResultsSets (Prelude.Maybe Prelude.Natural)
listFeaturedResultsSets_maxResults = Lens.lens (\ListFeaturedResultsSets' {maxResults} -> maxResults) (\s@ListFeaturedResultsSets' {} a -> s {maxResults = a} :: ListFeaturedResultsSets)

-- | If the response is truncated, Amazon Kendra returns a pagination token
-- in the response. You can use this pagination token to retrieve the next
-- set of featured results sets.
listFeaturedResultsSets_nextToken :: Lens.Lens' ListFeaturedResultsSets (Prelude.Maybe Prelude.Text)
listFeaturedResultsSets_nextToken = Lens.lens (\ListFeaturedResultsSets' {nextToken} -> nextToken) (\s@ListFeaturedResultsSets' {} a -> s {nextToken = a} :: ListFeaturedResultsSets)

-- | The identifier of the index used for featuring results.
listFeaturedResultsSets_indexId :: Lens.Lens' ListFeaturedResultsSets Prelude.Text
listFeaturedResultsSets_indexId = Lens.lens (\ListFeaturedResultsSets' {indexId} -> indexId) (\s@ListFeaturedResultsSets' {} a -> s {indexId = a} :: ListFeaturedResultsSets)

instance Core.AWSRequest ListFeaturedResultsSets where
  type
    AWSResponse ListFeaturedResultsSets =
      ListFeaturedResultsSetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFeaturedResultsSetsResponse'
            Prelude.<$> ( x
                            Data..?> "FeaturedResultsSetSummaryItems"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFeaturedResultsSets where
  hashWithSalt _salt ListFeaturedResultsSets' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` indexId

instance Prelude.NFData ListFeaturedResultsSets where
  rnf ListFeaturedResultsSets' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf indexId

instance Data.ToHeaders ListFeaturedResultsSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.ListFeaturedResultsSets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListFeaturedResultsSets where
  toJSON ListFeaturedResultsSets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("IndexId" Data..= indexId)
          ]
      )

instance Data.ToPath ListFeaturedResultsSets where
  toPath = Prelude.const "/"

instance Data.ToQuery ListFeaturedResultsSets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFeaturedResultsSetsResponse' smart constructor.
data ListFeaturedResultsSetsResponse = ListFeaturedResultsSetsResponse'
  { -- | An array of summary information for one or more featured results sets.
    featuredResultsSetSummaryItems :: Prelude.Maybe [FeaturedResultsSetSummary],
    -- | If the response is truncated, Amazon Kendra returns a pagination token
    -- in the response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFeaturedResultsSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featuredResultsSetSummaryItems', 'listFeaturedResultsSetsResponse_featuredResultsSetSummaryItems' - An array of summary information for one or more featured results sets.
--
-- 'nextToken', 'listFeaturedResultsSetsResponse_nextToken' - If the response is truncated, Amazon Kendra returns a pagination token
-- in the response.
--
-- 'httpStatus', 'listFeaturedResultsSetsResponse_httpStatus' - The response's http status code.
newListFeaturedResultsSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFeaturedResultsSetsResponse
newListFeaturedResultsSetsResponse pHttpStatus_ =
  ListFeaturedResultsSetsResponse'
    { featuredResultsSetSummaryItems =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of summary information for one or more featured results sets.
listFeaturedResultsSetsResponse_featuredResultsSetSummaryItems :: Lens.Lens' ListFeaturedResultsSetsResponse (Prelude.Maybe [FeaturedResultsSetSummary])
listFeaturedResultsSetsResponse_featuredResultsSetSummaryItems = Lens.lens (\ListFeaturedResultsSetsResponse' {featuredResultsSetSummaryItems} -> featuredResultsSetSummaryItems) (\s@ListFeaturedResultsSetsResponse' {} a -> s {featuredResultsSetSummaryItems = a} :: ListFeaturedResultsSetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response is truncated, Amazon Kendra returns a pagination token
-- in the response.
listFeaturedResultsSetsResponse_nextToken :: Lens.Lens' ListFeaturedResultsSetsResponse (Prelude.Maybe Prelude.Text)
listFeaturedResultsSetsResponse_nextToken = Lens.lens (\ListFeaturedResultsSetsResponse' {nextToken} -> nextToken) (\s@ListFeaturedResultsSetsResponse' {} a -> s {nextToken = a} :: ListFeaturedResultsSetsResponse)

-- | The response's http status code.
listFeaturedResultsSetsResponse_httpStatus :: Lens.Lens' ListFeaturedResultsSetsResponse Prelude.Int
listFeaturedResultsSetsResponse_httpStatus = Lens.lens (\ListFeaturedResultsSetsResponse' {httpStatus} -> httpStatus) (\s@ListFeaturedResultsSetsResponse' {} a -> s {httpStatus = a} :: ListFeaturedResultsSetsResponse)

instance
  Prelude.NFData
    ListFeaturedResultsSetsResponse
  where
  rnf ListFeaturedResultsSetsResponse' {..} =
    Prelude.rnf featuredResultsSetSummaryItems
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
