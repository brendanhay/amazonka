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
-- Module      : Amazonka.IoTSiteWise.ListAssets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a paginated list of asset summaries.
--
-- You can use this operation to do the following:
--
-- -   List assets based on a specific asset model.
--
-- -   List top-level assets.
--
-- You can\'t use this operation to list all assets. To retrieve summaries
-- for all of your assets, use
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_ListAssetModels.html ListAssetModels>
-- to get all of your asset model IDs. Then, use ListAssets to get all
-- assets for each asset model.
--
-- This operation returns paginated results.
module Amazonka.IoTSiteWise.ListAssets
  ( -- * Creating a Request
    ListAssets (..),
    newListAssets,

    -- * Request Lenses
    listAssets_nextToken,
    listAssets_filter,
    listAssets_assetModelId,
    listAssets_maxResults,

    -- * Destructuring the Response
    ListAssetsResponse (..),
    newListAssetsResponse,

    -- * Response Lenses
    listAssetsResponse_nextToken,
    listAssetsResponse_httpStatus,
    listAssetsResponse_assetSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAssets' smart constructor.
data ListAssets = ListAssets'
  { -- | The token to be used for the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The filter for the requested list of assets. Choose one of the following
    -- options:
    --
    -- -   @ALL@ – The list includes all assets for a given asset model ID. The
    --     @assetModelId@ parameter is required if you filter by @ALL@.
    --
    -- -   @TOP_LEVEL@ – The list includes only top-level assets in the asset
    --     hierarchy tree.
    --
    -- Default: @ALL@
    filter' :: Prelude.Maybe ListAssetsFilter,
    -- | The ID of the asset model by which to filter the list of assets. This
    -- parameter is required if you choose @ALL@ for @filter@.
    assetModelId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return for each paginated request.
    --
    -- Default: 50
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssets_nextToken' - The token to be used for the next set of paginated results.
--
-- 'filter'', 'listAssets_filter' - The filter for the requested list of assets. Choose one of the following
-- options:
--
-- -   @ALL@ – The list includes all assets for a given asset model ID. The
--     @assetModelId@ parameter is required if you filter by @ALL@.
--
-- -   @TOP_LEVEL@ – The list includes only top-level assets in the asset
--     hierarchy tree.
--
-- Default: @ALL@
--
-- 'assetModelId', 'listAssets_assetModelId' - The ID of the asset model by which to filter the list of assets. This
-- parameter is required if you choose @ALL@ for @filter@.
--
-- 'maxResults', 'listAssets_maxResults' - The maximum number of results to return for each paginated request.
--
-- Default: 50
newListAssets ::
  ListAssets
newListAssets =
  ListAssets'
    { nextToken = Prelude.Nothing,
      filter' = Prelude.Nothing,
      assetModelId = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token to be used for the next set of paginated results.
listAssets_nextToken :: Lens.Lens' ListAssets (Prelude.Maybe Prelude.Text)
listAssets_nextToken = Lens.lens (\ListAssets' {nextToken} -> nextToken) (\s@ListAssets' {} a -> s {nextToken = a} :: ListAssets)

-- | The filter for the requested list of assets. Choose one of the following
-- options:
--
-- -   @ALL@ – The list includes all assets for a given asset model ID. The
--     @assetModelId@ parameter is required if you filter by @ALL@.
--
-- -   @TOP_LEVEL@ – The list includes only top-level assets in the asset
--     hierarchy tree.
--
-- Default: @ALL@
listAssets_filter :: Lens.Lens' ListAssets (Prelude.Maybe ListAssetsFilter)
listAssets_filter = Lens.lens (\ListAssets' {filter'} -> filter') (\s@ListAssets' {} a -> s {filter' = a} :: ListAssets)

-- | The ID of the asset model by which to filter the list of assets. This
-- parameter is required if you choose @ALL@ for @filter@.
listAssets_assetModelId :: Lens.Lens' ListAssets (Prelude.Maybe Prelude.Text)
listAssets_assetModelId = Lens.lens (\ListAssets' {assetModelId} -> assetModelId) (\s@ListAssets' {} a -> s {assetModelId = a} :: ListAssets)

-- | The maximum number of results to return for each paginated request.
--
-- Default: 50
listAssets_maxResults :: Lens.Lens' ListAssets (Prelude.Maybe Prelude.Natural)
listAssets_maxResults = Lens.lens (\ListAssets' {maxResults} -> maxResults) (\s@ListAssets' {} a -> s {maxResults = a} :: ListAssets)

instance Core.AWSPager ListAssets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssetsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listAssetsResponse_assetSummaries) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAssets_nextToken
          Lens..~ rs
          Lens.^? listAssetsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListAssets where
  type AWSResponse ListAssets = ListAssetsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssetsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "assetSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListAssets where
  hashWithSalt _salt ListAssets' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` assetModelId
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListAssets where
  rnf ListAssets' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf assetModelId
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListAssets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListAssets where
  toPath = Prelude.const "/assets"

instance Core.ToQuery ListAssets where
  toQuery ListAssets' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "filter" Core.=: filter',
        "assetModelId" Core.=: assetModelId,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListAssetsResponse' smart constructor.
data ListAssetsResponse = ListAssetsResponse'
  { -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list that summarizes each asset.
    assetSummaries :: [AssetSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssetsResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'httpStatus', 'listAssetsResponse_httpStatus' - The response's http status code.
--
-- 'assetSummaries', 'listAssetsResponse_assetSummaries' - A list that summarizes each asset.
newListAssetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAssetsResponse
newListAssetsResponse pHttpStatus_ =
  ListAssetsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      assetSummaries = Prelude.mempty
    }

-- | The token for the next set of results, or null if there are no
-- additional results.
listAssetsResponse_nextToken :: Lens.Lens' ListAssetsResponse (Prelude.Maybe Prelude.Text)
listAssetsResponse_nextToken = Lens.lens (\ListAssetsResponse' {nextToken} -> nextToken) (\s@ListAssetsResponse' {} a -> s {nextToken = a} :: ListAssetsResponse)

-- | The response's http status code.
listAssetsResponse_httpStatus :: Lens.Lens' ListAssetsResponse Prelude.Int
listAssetsResponse_httpStatus = Lens.lens (\ListAssetsResponse' {httpStatus} -> httpStatus) (\s@ListAssetsResponse' {} a -> s {httpStatus = a} :: ListAssetsResponse)

-- | A list that summarizes each asset.
listAssetsResponse_assetSummaries :: Lens.Lens' ListAssetsResponse [AssetSummary]
listAssetsResponse_assetSummaries = Lens.lens (\ListAssetsResponse' {assetSummaries} -> assetSummaries) (\s@ListAssetsResponse' {} a -> s {assetSummaries = a} :: ListAssetsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListAssetsResponse where
  rnf ListAssetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf assetSummaries
