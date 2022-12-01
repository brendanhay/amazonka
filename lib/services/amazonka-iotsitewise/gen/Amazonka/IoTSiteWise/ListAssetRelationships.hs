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
-- Module      : Amazonka.IoTSiteWise.ListAssetRelationships
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a paginated list of asset relationships for an asset. You can
-- use this operation to identify an asset\'s root asset and all associated
-- assets between that asset and its root.
--
-- This operation returns paginated results.
module Amazonka.IoTSiteWise.ListAssetRelationships
  ( -- * Creating a Request
    ListAssetRelationships (..),
    newListAssetRelationships,

    -- * Request Lenses
    listAssetRelationships_nextToken,
    listAssetRelationships_maxResults,
    listAssetRelationships_assetId,
    listAssetRelationships_traversalType,

    -- * Destructuring the Response
    ListAssetRelationshipsResponse (..),
    newListAssetRelationshipsResponse,

    -- * Response Lenses
    listAssetRelationshipsResponse_nextToken,
    listAssetRelationshipsResponse_httpStatus,
    listAssetRelationshipsResponse_assetRelationshipSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAssetRelationships' smart constructor.
data ListAssetRelationships = ListAssetRelationships'
  { -- | The token to be used for the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return for each paginated request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the asset.
    assetId :: Prelude.Text,
    -- | The type of traversal to use to identify asset relationships. Choose the
    -- following option:
    --
    -- -   @PATH_TO_ROOT@ – Identify the asset\'s parent assets up to the root
    --     asset. The asset that you specify in @assetId@ is the first result
    --     in the list of @assetRelationshipSummaries@, and the root asset is
    --     the last result.
    traversalType :: TraversalType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssetRelationships' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssetRelationships_nextToken' - The token to be used for the next set of paginated results.
--
-- 'maxResults', 'listAssetRelationships_maxResults' - The maximum number of results to return for each paginated request.
--
-- 'assetId', 'listAssetRelationships_assetId' - The ID of the asset.
--
-- 'traversalType', 'listAssetRelationships_traversalType' - The type of traversal to use to identify asset relationships. Choose the
-- following option:
--
-- -   @PATH_TO_ROOT@ – Identify the asset\'s parent assets up to the root
--     asset. The asset that you specify in @assetId@ is the first result
--     in the list of @assetRelationshipSummaries@, and the root asset is
--     the last result.
newListAssetRelationships ::
  -- | 'assetId'
  Prelude.Text ->
  -- | 'traversalType'
  TraversalType ->
  ListAssetRelationships
newListAssetRelationships pAssetId_ pTraversalType_ =
  ListAssetRelationships'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      assetId = pAssetId_,
      traversalType = pTraversalType_
    }

-- | The token to be used for the next set of paginated results.
listAssetRelationships_nextToken :: Lens.Lens' ListAssetRelationships (Prelude.Maybe Prelude.Text)
listAssetRelationships_nextToken = Lens.lens (\ListAssetRelationships' {nextToken} -> nextToken) (\s@ListAssetRelationships' {} a -> s {nextToken = a} :: ListAssetRelationships)

-- | The maximum number of results to return for each paginated request.
listAssetRelationships_maxResults :: Lens.Lens' ListAssetRelationships (Prelude.Maybe Prelude.Natural)
listAssetRelationships_maxResults = Lens.lens (\ListAssetRelationships' {maxResults} -> maxResults) (\s@ListAssetRelationships' {} a -> s {maxResults = a} :: ListAssetRelationships)

-- | The ID of the asset.
listAssetRelationships_assetId :: Lens.Lens' ListAssetRelationships Prelude.Text
listAssetRelationships_assetId = Lens.lens (\ListAssetRelationships' {assetId} -> assetId) (\s@ListAssetRelationships' {} a -> s {assetId = a} :: ListAssetRelationships)

-- | The type of traversal to use to identify asset relationships. Choose the
-- following option:
--
-- -   @PATH_TO_ROOT@ – Identify the asset\'s parent assets up to the root
--     asset. The asset that you specify in @assetId@ is the first result
--     in the list of @assetRelationshipSummaries@, and the root asset is
--     the last result.
listAssetRelationships_traversalType :: Lens.Lens' ListAssetRelationships TraversalType
listAssetRelationships_traversalType = Lens.lens (\ListAssetRelationships' {traversalType} -> traversalType) (\s@ListAssetRelationships' {} a -> s {traversalType = a} :: ListAssetRelationships)

instance Core.AWSPager ListAssetRelationships where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssetRelationshipsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listAssetRelationshipsResponse_assetRelationshipSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAssetRelationships_nextToken
          Lens..~ rs
          Lens.^? listAssetRelationshipsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListAssetRelationships where
  type
    AWSResponse ListAssetRelationships =
      ListAssetRelationshipsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssetRelationshipsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "assetRelationshipSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListAssetRelationships where
  hashWithSalt _salt ListAssetRelationships' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` assetId
      `Prelude.hashWithSalt` traversalType

instance Prelude.NFData ListAssetRelationships where
  rnf ListAssetRelationships' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf traversalType

instance Core.ToHeaders ListAssetRelationships where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListAssetRelationships where
  toPath ListAssetRelationships' {..} =
    Prelude.mconcat
      [ "/assets/",
        Core.toBS assetId,
        "/assetRelationships"
      ]

instance Core.ToQuery ListAssetRelationships where
  toQuery ListAssetRelationships' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "traversalType" Core.=: traversalType
      ]

-- | /See:/ 'newListAssetRelationshipsResponse' smart constructor.
data ListAssetRelationshipsResponse = ListAssetRelationshipsResponse'
  { -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list that summarizes each asset relationship.
    assetRelationshipSummaries :: [AssetRelationshipSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssetRelationshipsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssetRelationshipsResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'httpStatus', 'listAssetRelationshipsResponse_httpStatus' - The response's http status code.
--
-- 'assetRelationshipSummaries', 'listAssetRelationshipsResponse_assetRelationshipSummaries' - A list that summarizes each asset relationship.
newListAssetRelationshipsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAssetRelationshipsResponse
newListAssetRelationshipsResponse pHttpStatus_ =
  ListAssetRelationshipsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      assetRelationshipSummaries = Prelude.mempty
    }

-- | The token for the next set of results, or null if there are no
-- additional results.
listAssetRelationshipsResponse_nextToken :: Lens.Lens' ListAssetRelationshipsResponse (Prelude.Maybe Prelude.Text)
listAssetRelationshipsResponse_nextToken = Lens.lens (\ListAssetRelationshipsResponse' {nextToken} -> nextToken) (\s@ListAssetRelationshipsResponse' {} a -> s {nextToken = a} :: ListAssetRelationshipsResponse)

-- | The response's http status code.
listAssetRelationshipsResponse_httpStatus :: Lens.Lens' ListAssetRelationshipsResponse Prelude.Int
listAssetRelationshipsResponse_httpStatus = Lens.lens (\ListAssetRelationshipsResponse' {httpStatus} -> httpStatus) (\s@ListAssetRelationshipsResponse' {} a -> s {httpStatus = a} :: ListAssetRelationshipsResponse)

-- | A list that summarizes each asset relationship.
listAssetRelationshipsResponse_assetRelationshipSummaries :: Lens.Lens' ListAssetRelationshipsResponse [AssetRelationshipSummary]
listAssetRelationshipsResponse_assetRelationshipSummaries = Lens.lens (\ListAssetRelationshipsResponse' {assetRelationshipSummaries} -> assetRelationshipSummaries) (\s@ListAssetRelationshipsResponse' {} a -> s {assetRelationshipSummaries = a} :: ListAssetRelationshipsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListAssetRelationshipsResponse
  where
  rnf ListAssetRelationshipsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf assetRelationshipSummaries
