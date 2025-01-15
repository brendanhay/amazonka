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
-- Module      : Amazonka.SageMakerGeoSpatial.ListRasterDataCollections
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to get raster data collections.
--
-- This operation returns paginated results.
module Amazonka.SageMakerGeoSpatial.ListRasterDataCollections
  ( -- * Creating a Request
    ListRasterDataCollections (..),
    newListRasterDataCollections,

    -- * Request Lenses
    listRasterDataCollections_maxResults,
    listRasterDataCollections_nextToken,

    -- * Destructuring the Response
    ListRasterDataCollectionsResponse (..),
    newListRasterDataCollectionsResponse,

    -- * Response Lenses
    listRasterDataCollectionsResponse_nextToken,
    listRasterDataCollectionsResponse_httpStatus,
    listRasterDataCollectionsResponse_rasterDataCollectionSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerGeoSpatial.Types

-- | /See:/ 'newListRasterDataCollections' smart constructor.
data ListRasterDataCollections = ListRasterDataCollections'
  { -- | The total number of items to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous response was truncated, you receive this token. Use it
    -- in your next request to receive the next set of results.
    nextToken :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRasterDataCollections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listRasterDataCollections_maxResults' - The total number of items to return.
--
-- 'nextToken', 'listRasterDataCollections_nextToken' - If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
newListRasterDataCollections ::
  ListRasterDataCollections
newListRasterDataCollections =
  ListRasterDataCollections'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The total number of items to return.
listRasterDataCollections_maxResults :: Lens.Lens' ListRasterDataCollections (Prelude.Maybe Prelude.Natural)
listRasterDataCollections_maxResults = Lens.lens (\ListRasterDataCollections' {maxResults} -> maxResults) (\s@ListRasterDataCollections' {} a -> s {maxResults = a} :: ListRasterDataCollections)

-- | If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
listRasterDataCollections_nextToken :: Lens.Lens' ListRasterDataCollections (Prelude.Maybe Prelude.Text)
listRasterDataCollections_nextToken = Lens.lens (\ListRasterDataCollections' {nextToken} -> nextToken) (\s@ListRasterDataCollections' {} a -> s {nextToken = a} :: ListRasterDataCollections) Prelude.. Lens.mapping Data._Sensitive

instance Core.AWSPager ListRasterDataCollections where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRasterDataCollectionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listRasterDataCollectionsResponse_rasterDataCollectionSummaries
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listRasterDataCollections_nextToken
              Lens..~ rs
              Lens.^? listRasterDataCollectionsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListRasterDataCollections where
  type
    AWSResponse ListRasterDataCollections =
      ListRasterDataCollectionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRasterDataCollectionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "RasterDataCollectionSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListRasterDataCollections where
  hashWithSalt _salt ListRasterDataCollections' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListRasterDataCollections where
  rnf ListRasterDataCollections' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListRasterDataCollections where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListRasterDataCollections where
  toPath = Prelude.const "/raster-data-collections"

instance Data.ToQuery ListRasterDataCollections where
  toQuery ListRasterDataCollections' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListRasterDataCollectionsResponse' smart constructor.
data ListRasterDataCollectionsResponse = ListRasterDataCollectionsResponse'
  { -- | If the previous response was truncated, you receive this token. Use it
    -- in your next request to receive the next set of results.
    nextToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Contains summary information about the raster data collection.
    rasterDataCollectionSummaries :: [RasterDataCollectionMetadata]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRasterDataCollectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRasterDataCollectionsResponse_nextToken' - If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
--
-- 'httpStatus', 'listRasterDataCollectionsResponse_httpStatus' - The response's http status code.
--
-- 'rasterDataCollectionSummaries', 'listRasterDataCollectionsResponse_rasterDataCollectionSummaries' - Contains summary information about the raster data collection.
newListRasterDataCollectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRasterDataCollectionsResponse
newListRasterDataCollectionsResponse pHttpStatus_ =
  ListRasterDataCollectionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      rasterDataCollectionSummaries =
        Prelude.mempty
    }

-- | If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
listRasterDataCollectionsResponse_nextToken :: Lens.Lens' ListRasterDataCollectionsResponse (Prelude.Maybe Prelude.Text)
listRasterDataCollectionsResponse_nextToken = Lens.lens (\ListRasterDataCollectionsResponse' {nextToken} -> nextToken) (\s@ListRasterDataCollectionsResponse' {} a -> s {nextToken = a} :: ListRasterDataCollectionsResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The response's http status code.
listRasterDataCollectionsResponse_httpStatus :: Lens.Lens' ListRasterDataCollectionsResponse Prelude.Int
listRasterDataCollectionsResponse_httpStatus = Lens.lens (\ListRasterDataCollectionsResponse' {httpStatus} -> httpStatus) (\s@ListRasterDataCollectionsResponse' {} a -> s {httpStatus = a} :: ListRasterDataCollectionsResponse)

-- | Contains summary information about the raster data collection.
listRasterDataCollectionsResponse_rasterDataCollectionSummaries :: Lens.Lens' ListRasterDataCollectionsResponse [RasterDataCollectionMetadata]
listRasterDataCollectionsResponse_rasterDataCollectionSummaries = Lens.lens (\ListRasterDataCollectionsResponse' {rasterDataCollectionSummaries} -> rasterDataCollectionSummaries) (\s@ListRasterDataCollectionsResponse' {} a -> s {rasterDataCollectionSummaries = a} :: ListRasterDataCollectionsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListRasterDataCollectionsResponse
  where
  rnf ListRasterDataCollectionsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf rasterDataCollectionSummaries
