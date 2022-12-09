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
-- Module      : Amazonka.IoTSiteWise.ListAssociatedAssets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a paginated list of associated assets.
--
-- You can use this operation to do the following:
--
-- -   List child assets associated to a parent asset by a hierarchy that
--     you specify.
--
-- -   List an asset\'s parent asset.
--
-- This operation returns paginated results.
module Amazonka.IoTSiteWise.ListAssociatedAssets
  ( -- * Creating a Request
    ListAssociatedAssets (..),
    newListAssociatedAssets,

    -- * Request Lenses
    listAssociatedAssets_hierarchyId,
    listAssociatedAssets_maxResults,
    listAssociatedAssets_nextToken,
    listAssociatedAssets_traversalDirection,
    listAssociatedAssets_assetId,

    -- * Destructuring the Response
    ListAssociatedAssetsResponse (..),
    newListAssociatedAssetsResponse,

    -- * Response Lenses
    listAssociatedAssetsResponse_nextToken,
    listAssociatedAssetsResponse_httpStatus,
    listAssociatedAssetsResponse_assetSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAssociatedAssets' smart constructor.
data ListAssociatedAssets = ListAssociatedAssets'
  { -- | The ID of the hierarchy by which child assets are associated to the
    -- asset. To find a hierarchy ID, use the
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_DescribeAsset.html DescribeAsset>
    -- or
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_DescribeAssetModel.html DescribeAssetModel>
    -- operations. This parameter is required if you choose @CHILD@ for
    -- @traversalDirection@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/asset-hierarchies.html Asset hierarchies>
    -- in the /IoT SiteWise User Guide/.
    hierarchyId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return for each paginated request.
    --
    -- Default: 50
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to be used for the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The direction to list associated assets. Choose one of the following
    -- options:
    --
    -- -   @CHILD@ – The list includes all child assets associated to the
    --     asset. The @hierarchyId@ parameter is required if you choose
    --     @CHILD@.
    --
    -- -   @PARENT@ – The list includes the asset\'s parent asset.
    --
    -- Default: @CHILD@
    traversalDirection :: Prelude.Maybe TraversalDirection,
    -- | The ID of the asset to query.
    assetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssociatedAssets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hierarchyId', 'listAssociatedAssets_hierarchyId' - The ID of the hierarchy by which child assets are associated to the
-- asset. To find a hierarchy ID, use the
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_DescribeAsset.html DescribeAsset>
-- or
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_DescribeAssetModel.html DescribeAssetModel>
-- operations. This parameter is required if you choose @CHILD@ for
-- @traversalDirection@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/asset-hierarchies.html Asset hierarchies>
-- in the /IoT SiteWise User Guide/.
--
-- 'maxResults', 'listAssociatedAssets_maxResults' - The maximum number of results to return for each paginated request.
--
-- Default: 50
--
-- 'nextToken', 'listAssociatedAssets_nextToken' - The token to be used for the next set of paginated results.
--
-- 'traversalDirection', 'listAssociatedAssets_traversalDirection' - The direction to list associated assets. Choose one of the following
-- options:
--
-- -   @CHILD@ – The list includes all child assets associated to the
--     asset. The @hierarchyId@ parameter is required if you choose
--     @CHILD@.
--
-- -   @PARENT@ – The list includes the asset\'s parent asset.
--
-- Default: @CHILD@
--
-- 'assetId', 'listAssociatedAssets_assetId' - The ID of the asset to query.
newListAssociatedAssets ::
  -- | 'assetId'
  Prelude.Text ->
  ListAssociatedAssets
newListAssociatedAssets pAssetId_ =
  ListAssociatedAssets'
    { hierarchyId =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      traversalDirection = Prelude.Nothing,
      assetId = pAssetId_
    }

-- | The ID of the hierarchy by which child assets are associated to the
-- asset. To find a hierarchy ID, use the
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_DescribeAsset.html DescribeAsset>
-- or
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_DescribeAssetModel.html DescribeAssetModel>
-- operations. This parameter is required if you choose @CHILD@ for
-- @traversalDirection@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/asset-hierarchies.html Asset hierarchies>
-- in the /IoT SiteWise User Guide/.
listAssociatedAssets_hierarchyId :: Lens.Lens' ListAssociatedAssets (Prelude.Maybe Prelude.Text)
listAssociatedAssets_hierarchyId = Lens.lens (\ListAssociatedAssets' {hierarchyId} -> hierarchyId) (\s@ListAssociatedAssets' {} a -> s {hierarchyId = a} :: ListAssociatedAssets)

-- | The maximum number of results to return for each paginated request.
--
-- Default: 50
listAssociatedAssets_maxResults :: Lens.Lens' ListAssociatedAssets (Prelude.Maybe Prelude.Natural)
listAssociatedAssets_maxResults = Lens.lens (\ListAssociatedAssets' {maxResults} -> maxResults) (\s@ListAssociatedAssets' {} a -> s {maxResults = a} :: ListAssociatedAssets)

-- | The token to be used for the next set of paginated results.
listAssociatedAssets_nextToken :: Lens.Lens' ListAssociatedAssets (Prelude.Maybe Prelude.Text)
listAssociatedAssets_nextToken = Lens.lens (\ListAssociatedAssets' {nextToken} -> nextToken) (\s@ListAssociatedAssets' {} a -> s {nextToken = a} :: ListAssociatedAssets)

-- | The direction to list associated assets. Choose one of the following
-- options:
--
-- -   @CHILD@ – The list includes all child assets associated to the
--     asset. The @hierarchyId@ parameter is required if you choose
--     @CHILD@.
--
-- -   @PARENT@ – The list includes the asset\'s parent asset.
--
-- Default: @CHILD@
listAssociatedAssets_traversalDirection :: Lens.Lens' ListAssociatedAssets (Prelude.Maybe TraversalDirection)
listAssociatedAssets_traversalDirection = Lens.lens (\ListAssociatedAssets' {traversalDirection} -> traversalDirection) (\s@ListAssociatedAssets' {} a -> s {traversalDirection = a} :: ListAssociatedAssets)

-- | The ID of the asset to query.
listAssociatedAssets_assetId :: Lens.Lens' ListAssociatedAssets Prelude.Text
listAssociatedAssets_assetId = Lens.lens (\ListAssociatedAssets' {assetId} -> assetId) (\s@ListAssociatedAssets' {} a -> s {assetId = a} :: ListAssociatedAssets)

instance Core.AWSPager ListAssociatedAssets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssociatedAssetsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listAssociatedAssetsResponse_assetSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAssociatedAssets_nextToken
          Lens..~ rs
          Lens.^? listAssociatedAssetsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListAssociatedAssets where
  type
    AWSResponse ListAssociatedAssets =
      ListAssociatedAssetsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssociatedAssetsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "assetSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListAssociatedAssets where
  hashWithSalt _salt ListAssociatedAssets' {..} =
    _salt `Prelude.hashWithSalt` hierarchyId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` traversalDirection
      `Prelude.hashWithSalt` assetId

instance Prelude.NFData ListAssociatedAssets where
  rnf ListAssociatedAssets' {..} =
    Prelude.rnf hierarchyId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf traversalDirection
      `Prelude.seq` Prelude.rnf assetId

instance Data.ToHeaders ListAssociatedAssets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListAssociatedAssets where
  toPath ListAssociatedAssets' {..} =
    Prelude.mconcat
      ["/assets/", Data.toBS assetId, "/hierarchies"]

instance Data.ToQuery ListAssociatedAssets where
  toQuery ListAssociatedAssets' {..} =
    Prelude.mconcat
      [ "hierarchyId" Data.=: hierarchyId,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "traversalDirection" Data.=: traversalDirection
      ]

-- | /See:/ 'newListAssociatedAssetsResponse' smart constructor.
data ListAssociatedAssetsResponse = ListAssociatedAssetsResponse'
  { -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list that summarizes the associated assets.
    assetSummaries :: [AssociatedAssetsSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssociatedAssetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssociatedAssetsResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'httpStatus', 'listAssociatedAssetsResponse_httpStatus' - The response's http status code.
--
-- 'assetSummaries', 'listAssociatedAssetsResponse_assetSummaries' - A list that summarizes the associated assets.
newListAssociatedAssetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAssociatedAssetsResponse
newListAssociatedAssetsResponse pHttpStatus_ =
  ListAssociatedAssetsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      assetSummaries = Prelude.mempty
    }

-- | The token for the next set of results, or null if there are no
-- additional results.
listAssociatedAssetsResponse_nextToken :: Lens.Lens' ListAssociatedAssetsResponse (Prelude.Maybe Prelude.Text)
listAssociatedAssetsResponse_nextToken = Lens.lens (\ListAssociatedAssetsResponse' {nextToken} -> nextToken) (\s@ListAssociatedAssetsResponse' {} a -> s {nextToken = a} :: ListAssociatedAssetsResponse)

-- | The response's http status code.
listAssociatedAssetsResponse_httpStatus :: Lens.Lens' ListAssociatedAssetsResponse Prelude.Int
listAssociatedAssetsResponse_httpStatus = Lens.lens (\ListAssociatedAssetsResponse' {httpStatus} -> httpStatus) (\s@ListAssociatedAssetsResponse' {} a -> s {httpStatus = a} :: ListAssociatedAssetsResponse)

-- | A list that summarizes the associated assets.
listAssociatedAssetsResponse_assetSummaries :: Lens.Lens' ListAssociatedAssetsResponse [AssociatedAssetsSummary]
listAssociatedAssetsResponse_assetSummaries = Lens.lens (\ListAssociatedAssetsResponse' {assetSummaries} -> assetSummaries) (\s@ListAssociatedAssetsResponse' {} a -> s {assetSummaries = a} :: ListAssociatedAssetsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListAssociatedAssetsResponse where
  rnf ListAssociatedAssetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf assetSummaries
