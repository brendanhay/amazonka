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
-- Module      : Amazonka.SageMakerGeoSpatial.SearchRasterDataCollection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you run image query on a specific raster data collection to get a
-- list of the satellite imagery matching the selected filters.
module Amazonka.SageMakerGeoSpatial.SearchRasterDataCollection
  ( -- * Creating a Request
    SearchRasterDataCollection (..),
    newSearchRasterDataCollection,

    -- * Request Lenses
    searchRasterDataCollection_nextToken,
    searchRasterDataCollection_arn,
    searchRasterDataCollection_rasterDataCollectionQuery,

    -- * Destructuring the Response
    SearchRasterDataCollectionResponse (..),
    newSearchRasterDataCollectionResponse,

    -- * Response Lenses
    searchRasterDataCollectionResponse_items,
    searchRasterDataCollectionResponse_nextToken,
    searchRasterDataCollectionResponse_httpStatus,
    searchRasterDataCollectionResponse_approximateResultCount,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerGeoSpatial.Types

-- | /See:/ 'newSearchRasterDataCollection' smart constructor.
data SearchRasterDataCollection = SearchRasterDataCollection'
  { -- | If the previous response was truncated, you receive this token. Use it
    -- in your next request to receive the next set of results.
    nextToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the raster data collection.
    arn :: Prelude.Text,
    rasterDataCollectionQuery :: RasterDataCollectionQueryWithBandFilterInput
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchRasterDataCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchRasterDataCollection_nextToken' - If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
--
-- 'arn', 'searchRasterDataCollection_arn' - The Amazon Resource Name (ARN) of the raster data collection.
--
-- 'rasterDataCollectionQuery', 'searchRasterDataCollection_rasterDataCollectionQuery' -
newSearchRasterDataCollection ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'rasterDataCollectionQuery'
  RasterDataCollectionQueryWithBandFilterInput ->
  SearchRasterDataCollection
newSearchRasterDataCollection
  pArn_
  pRasterDataCollectionQuery_ =
    SearchRasterDataCollection'
      { nextToken =
          Prelude.Nothing,
        arn = pArn_,
        rasterDataCollectionQuery =
          pRasterDataCollectionQuery_
      }

-- | If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
searchRasterDataCollection_nextToken :: Lens.Lens' SearchRasterDataCollection (Prelude.Maybe Prelude.Text)
searchRasterDataCollection_nextToken = Lens.lens (\SearchRasterDataCollection' {nextToken} -> nextToken) (\s@SearchRasterDataCollection' {} a -> s {nextToken = a} :: SearchRasterDataCollection) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Resource Name (ARN) of the raster data collection.
searchRasterDataCollection_arn :: Lens.Lens' SearchRasterDataCollection Prelude.Text
searchRasterDataCollection_arn = Lens.lens (\SearchRasterDataCollection' {arn} -> arn) (\s@SearchRasterDataCollection' {} a -> s {arn = a} :: SearchRasterDataCollection)

searchRasterDataCollection_rasterDataCollectionQuery :: Lens.Lens' SearchRasterDataCollection RasterDataCollectionQueryWithBandFilterInput
searchRasterDataCollection_rasterDataCollectionQuery = Lens.lens (\SearchRasterDataCollection' {rasterDataCollectionQuery} -> rasterDataCollectionQuery) (\s@SearchRasterDataCollection' {} a -> s {rasterDataCollectionQuery = a} :: SearchRasterDataCollection)

instance Core.AWSRequest SearchRasterDataCollection where
  type
    AWSResponse SearchRasterDataCollection =
      SearchRasterDataCollectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchRasterDataCollectionResponse'
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ApproximateResultCount")
      )

instance Prelude.Hashable SearchRasterDataCollection where
  hashWithSalt _salt SearchRasterDataCollection' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` rasterDataCollectionQuery

instance Prelude.NFData SearchRasterDataCollection where
  rnf SearchRasterDataCollection' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf rasterDataCollectionQuery

instance Data.ToHeaders SearchRasterDataCollection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchRasterDataCollection where
  toJSON SearchRasterDataCollection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("Arn" Data..= arn),
            Prelude.Just
              ( "RasterDataCollectionQuery"
                  Data..= rasterDataCollectionQuery
              )
          ]
      )

instance Data.ToPath SearchRasterDataCollection where
  toPath =
    Prelude.const "/search-raster-data-collection"

instance Data.ToQuery SearchRasterDataCollection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchRasterDataCollectionResponse' smart constructor.
data SearchRasterDataCollectionResponse = SearchRasterDataCollectionResponse'
  { items :: Prelude.Maybe [ItemSource],
    -- | If the previous response was truncated, you receive this token. Use it
    -- in your next request to receive the next set of results.
    nextToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    approximateResultCount :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchRasterDataCollectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'searchRasterDataCollectionResponse_items' -
--
-- 'nextToken', 'searchRasterDataCollectionResponse_nextToken' - If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
--
-- 'httpStatus', 'searchRasterDataCollectionResponse_httpStatus' - The response's http status code.
--
-- 'approximateResultCount', 'searchRasterDataCollectionResponse_approximateResultCount' -
newSearchRasterDataCollectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'approximateResultCount'
  Prelude.Int ->
  SearchRasterDataCollectionResponse
newSearchRasterDataCollectionResponse
  pHttpStatus_
  pApproximateResultCount_ =
    SearchRasterDataCollectionResponse'
      { items =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        approximateResultCount =
          pApproximateResultCount_
      }

searchRasterDataCollectionResponse_items :: Lens.Lens' SearchRasterDataCollectionResponse (Prelude.Maybe [ItemSource])
searchRasterDataCollectionResponse_items = Lens.lens (\SearchRasterDataCollectionResponse' {items} -> items) (\s@SearchRasterDataCollectionResponse' {} a -> s {items = a} :: SearchRasterDataCollectionResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the previous response was truncated, you receive this token. Use it
-- in your next request to receive the next set of results.
searchRasterDataCollectionResponse_nextToken :: Lens.Lens' SearchRasterDataCollectionResponse (Prelude.Maybe Prelude.Text)
searchRasterDataCollectionResponse_nextToken = Lens.lens (\SearchRasterDataCollectionResponse' {nextToken} -> nextToken) (\s@SearchRasterDataCollectionResponse' {} a -> s {nextToken = a} :: SearchRasterDataCollectionResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The response's http status code.
searchRasterDataCollectionResponse_httpStatus :: Lens.Lens' SearchRasterDataCollectionResponse Prelude.Int
searchRasterDataCollectionResponse_httpStatus = Lens.lens (\SearchRasterDataCollectionResponse' {httpStatus} -> httpStatus) (\s@SearchRasterDataCollectionResponse' {} a -> s {httpStatus = a} :: SearchRasterDataCollectionResponse)

searchRasterDataCollectionResponse_approximateResultCount :: Lens.Lens' SearchRasterDataCollectionResponse Prelude.Int
searchRasterDataCollectionResponse_approximateResultCount = Lens.lens (\SearchRasterDataCollectionResponse' {approximateResultCount} -> approximateResultCount) (\s@SearchRasterDataCollectionResponse' {} a -> s {approximateResultCount = a} :: SearchRasterDataCollectionResponse)

instance
  Prelude.NFData
    SearchRasterDataCollectionResponse
  where
  rnf SearchRasterDataCollectionResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf approximateResultCount
