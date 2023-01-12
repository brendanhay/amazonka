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
-- Module      : Amazonka.IoTSiteWise.ListProjectAssets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a paginated list of assets associated with an IoT SiteWise
-- Monitor project.
--
-- This operation returns paginated results.
module Amazonka.IoTSiteWise.ListProjectAssets
  ( -- * Creating a Request
    ListProjectAssets (..),
    newListProjectAssets,

    -- * Request Lenses
    listProjectAssets_maxResults,
    listProjectAssets_nextToken,
    listProjectAssets_projectId,

    -- * Destructuring the Response
    ListProjectAssetsResponse (..),
    newListProjectAssetsResponse,

    -- * Response Lenses
    listProjectAssetsResponse_nextToken,
    listProjectAssetsResponse_httpStatus,
    listProjectAssetsResponse_assetIds,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListProjectAssets' smart constructor.
data ListProjectAssets = ListProjectAssets'
  { -- | The maximum number of results to return for each paginated request.
    --
    -- Default: 50
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to be used for the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the project.
    projectId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProjectAssets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listProjectAssets_maxResults' - The maximum number of results to return for each paginated request.
--
-- Default: 50
--
-- 'nextToken', 'listProjectAssets_nextToken' - The token to be used for the next set of paginated results.
--
-- 'projectId', 'listProjectAssets_projectId' - The ID of the project.
newListProjectAssets ::
  -- | 'projectId'
  Prelude.Text ->
  ListProjectAssets
newListProjectAssets pProjectId_ =
  ListProjectAssets'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      projectId = pProjectId_
    }

-- | The maximum number of results to return for each paginated request.
--
-- Default: 50
listProjectAssets_maxResults :: Lens.Lens' ListProjectAssets (Prelude.Maybe Prelude.Natural)
listProjectAssets_maxResults = Lens.lens (\ListProjectAssets' {maxResults} -> maxResults) (\s@ListProjectAssets' {} a -> s {maxResults = a} :: ListProjectAssets)

-- | The token to be used for the next set of paginated results.
listProjectAssets_nextToken :: Lens.Lens' ListProjectAssets (Prelude.Maybe Prelude.Text)
listProjectAssets_nextToken = Lens.lens (\ListProjectAssets' {nextToken} -> nextToken) (\s@ListProjectAssets' {} a -> s {nextToken = a} :: ListProjectAssets)

-- | The ID of the project.
listProjectAssets_projectId :: Lens.Lens' ListProjectAssets Prelude.Text
listProjectAssets_projectId = Lens.lens (\ListProjectAssets' {projectId} -> projectId) (\s@ListProjectAssets' {} a -> s {projectId = a} :: ListProjectAssets)

instance Core.AWSPager ListProjectAssets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProjectAssetsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listProjectAssetsResponse_assetIds) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listProjectAssets_nextToken
          Lens..~ rs
          Lens.^? listProjectAssetsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListProjectAssets where
  type
    AWSResponse ListProjectAssets =
      ListProjectAssetsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProjectAssetsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "assetIds" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListProjectAssets where
  hashWithSalt _salt ListProjectAssets' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` projectId

instance Prelude.NFData ListProjectAssets where
  rnf ListProjectAssets' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf projectId

instance Data.ToHeaders ListProjectAssets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListProjectAssets where
  toPath ListProjectAssets' {..} =
    Prelude.mconcat
      ["/projects/", Data.toBS projectId, "/assets"]

instance Data.ToQuery ListProjectAssets where
  toQuery ListProjectAssets' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListProjectAssetsResponse' smart constructor.
data ListProjectAssetsResponse = ListProjectAssetsResponse'
  { -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list that contains the IDs of each asset associated with the project.
    assetIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProjectAssetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProjectAssetsResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'httpStatus', 'listProjectAssetsResponse_httpStatus' - The response's http status code.
--
-- 'assetIds', 'listProjectAssetsResponse_assetIds' - A list that contains the IDs of each asset associated with the project.
newListProjectAssetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProjectAssetsResponse
newListProjectAssetsResponse pHttpStatus_ =
  ListProjectAssetsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      assetIds = Prelude.mempty
    }

-- | The token for the next set of results, or null if there are no
-- additional results.
listProjectAssetsResponse_nextToken :: Lens.Lens' ListProjectAssetsResponse (Prelude.Maybe Prelude.Text)
listProjectAssetsResponse_nextToken = Lens.lens (\ListProjectAssetsResponse' {nextToken} -> nextToken) (\s@ListProjectAssetsResponse' {} a -> s {nextToken = a} :: ListProjectAssetsResponse)

-- | The response's http status code.
listProjectAssetsResponse_httpStatus :: Lens.Lens' ListProjectAssetsResponse Prelude.Int
listProjectAssetsResponse_httpStatus = Lens.lens (\ListProjectAssetsResponse' {httpStatus} -> httpStatus) (\s@ListProjectAssetsResponse' {} a -> s {httpStatus = a} :: ListProjectAssetsResponse)

-- | A list that contains the IDs of each asset associated with the project.
listProjectAssetsResponse_assetIds :: Lens.Lens' ListProjectAssetsResponse [Prelude.Text]
listProjectAssetsResponse_assetIds = Lens.lens (\ListProjectAssetsResponse' {assetIds} -> assetIds) (\s@ListProjectAssetsResponse' {} a -> s {assetIds = a} :: ListProjectAssetsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListProjectAssetsResponse where
  rnf ListProjectAssetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf assetIds
