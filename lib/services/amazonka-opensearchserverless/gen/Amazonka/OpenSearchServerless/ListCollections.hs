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
-- Module      : Amazonka.OpenSearchServerless.ListCollections
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all OpenSearch Serverless collections. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-manage.html Creating and managing Amazon OpenSearch Serverless collections>.
--
-- Make sure to include an empty request body {} if you don\'t include any
-- collection filters in the request.
module Amazonka.OpenSearchServerless.ListCollections
  ( -- * Creating a Request
    ListCollections (..),
    newListCollections,

    -- * Request Lenses
    listCollections_collectionFilters,
    listCollections_maxResults,
    listCollections_nextToken,

    -- * Destructuring the Response
    ListCollectionsResponse (..),
    newListCollectionsResponse,

    -- * Response Lenses
    listCollectionsResponse_collectionSummaries,
    listCollectionsResponse_nextToken,
    listCollectionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCollections' smart constructor.
data ListCollections = ListCollections'
  { -- | List of filter names and values that you can use for requests.
    collectionFilters :: Prelude.Maybe CollectionFilters,
    -- | The maximum number of results to return. Default is 20. You can use
    -- @nextToken@ to get the next page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If your initial @ListCollections@ operation returns a @nextToken@, you
    -- can include the returned @nextToken@ in subsequent @ListCollections@
    -- operations, which returns results in the next page.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCollections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectionFilters', 'listCollections_collectionFilters' - List of filter names and values that you can use for requests.
--
-- 'maxResults', 'listCollections_maxResults' - The maximum number of results to return. Default is 20. You can use
-- @nextToken@ to get the next page of results.
--
-- 'nextToken', 'listCollections_nextToken' - If your initial @ListCollections@ operation returns a @nextToken@, you
-- can include the returned @nextToken@ in subsequent @ListCollections@
-- operations, which returns results in the next page.
newListCollections ::
  ListCollections
newListCollections =
  ListCollections'
    { collectionFilters =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | List of filter names and values that you can use for requests.
listCollections_collectionFilters :: Lens.Lens' ListCollections (Prelude.Maybe CollectionFilters)
listCollections_collectionFilters = Lens.lens (\ListCollections' {collectionFilters} -> collectionFilters) (\s@ListCollections' {} a -> s {collectionFilters = a} :: ListCollections)

-- | The maximum number of results to return. Default is 20. You can use
-- @nextToken@ to get the next page of results.
listCollections_maxResults :: Lens.Lens' ListCollections (Prelude.Maybe Prelude.Natural)
listCollections_maxResults = Lens.lens (\ListCollections' {maxResults} -> maxResults) (\s@ListCollections' {} a -> s {maxResults = a} :: ListCollections)

-- | If your initial @ListCollections@ operation returns a @nextToken@, you
-- can include the returned @nextToken@ in subsequent @ListCollections@
-- operations, which returns results in the next page.
listCollections_nextToken :: Lens.Lens' ListCollections (Prelude.Maybe Prelude.Text)
listCollections_nextToken = Lens.lens (\ListCollections' {nextToken} -> nextToken) (\s@ListCollections' {} a -> s {nextToken = a} :: ListCollections)

instance Core.AWSRequest ListCollections where
  type
    AWSResponse ListCollections =
      ListCollectionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCollectionsResponse'
            Prelude.<$> ( x
                            Data..?> "collectionSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCollections where
  hashWithSalt _salt ListCollections' {..} =
    _salt
      `Prelude.hashWithSalt` collectionFilters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListCollections where
  rnf ListCollections' {..} =
    Prelude.rnf collectionFilters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListCollections where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.ListCollections" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListCollections where
  toJSON ListCollections' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("collectionFilters" Data..=)
              Prelude.<$> collectionFilters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListCollections where
  toPath = Prelude.const "/"

instance Data.ToQuery ListCollections where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCollectionsResponse' smart constructor.
data ListCollectionsResponse = ListCollectionsResponse'
  { -- | Details about each collection.
    collectionSummaries :: Prelude.Maybe [CollectionSummary],
    -- | When @nextToken@ is returned, there are more results available. The
    -- value of @nextToken@ is a unique pagination token for each page. Make
    -- the call again using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCollectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectionSummaries', 'listCollectionsResponse_collectionSummaries' - Details about each collection.
--
-- 'nextToken', 'listCollectionsResponse_nextToken' - When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
--
-- 'httpStatus', 'listCollectionsResponse_httpStatus' - The response's http status code.
newListCollectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCollectionsResponse
newListCollectionsResponse pHttpStatus_ =
  ListCollectionsResponse'
    { collectionSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about each collection.
listCollectionsResponse_collectionSummaries :: Lens.Lens' ListCollectionsResponse (Prelude.Maybe [CollectionSummary])
listCollectionsResponse_collectionSummaries = Lens.lens (\ListCollectionsResponse' {collectionSummaries} -> collectionSummaries) (\s@ListCollectionsResponse' {} a -> s {collectionSummaries = a} :: ListCollectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
listCollectionsResponse_nextToken :: Lens.Lens' ListCollectionsResponse (Prelude.Maybe Prelude.Text)
listCollectionsResponse_nextToken = Lens.lens (\ListCollectionsResponse' {nextToken} -> nextToken) (\s@ListCollectionsResponse' {} a -> s {nextToken = a} :: ListCollectionsResponse)

-- | The response's http status code.
listCollectionsResponse_httpStatus :: Lens.Lens' ListCollectionsResponse Prelude.Int
listCollectionsResponse_httpStatus = Lens.lens (\ListCollectionsResponse' {httpStatus} -> httpStatus) (\s@ListCollectionsResponse' {} a -> s {httpStatus = a} :: ListCollectionsResponse)

instance Prelude.NFData ListCollectionsResponse where
  rnf ListCollectionsResponse' {..} =
    Prelude.rnf collectionSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
