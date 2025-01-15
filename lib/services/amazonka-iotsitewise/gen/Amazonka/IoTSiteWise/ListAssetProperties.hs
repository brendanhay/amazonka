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
-- Module      : Amazonka.IoTSiteWise.ListAssetProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a paginated list of properties associated with an asset. If
-- you update properties associated with the model before you finish
-- listing all the properties, you need to start all over again.
--
-- This operation returns paginated results.
module Amazonka.IoTSiteWise.ListAssetProperties
  ( -- * Creating a Request
    ListAssetProperties (..),
    newListAssetProperties,

    -- * Request Lenses
    listAssetProperties_filter,
    listAssetProperties_maxResults,
    listAssetProperties_nextToken,
    listAssetProperties_assetId,

    -- * Destructuring the Response
    ListAssetPropertiesResponse (..),
    newListAssetPropertiesResponse,

    -- * Response Lenses
    listAssetPropertiesResponse_nextToken,
    listAssetPropertiesResponse_httpStatus,
    listAssetPropertiesResponse_assetPropertySummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAssetProperties' smart constructor.
data ListAssetProperties = ListAssetProperties'
  { -- | Filters the requested list of asset properties. You can choose one of
    -- the following options:
    --
    -- -   @ALL@ – The list includes all asset properties for a given asset
    --     model ID.
    --
    -- -   @BASE@ – The list includes only base asset properties for a given
    --     asset model ID.
    --
    -- Default: @BASE@
    filter' :: Prelude.Maybe ListAssetPropertiesFilter,
    -- | The maximum number of results to return for each paginated request. If
    -- not specified, the default value is 50.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to be used for the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset.
    assetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssetProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listAssetProperties_filter' - Filters the requested list of asset properties. You can choose one of
-- the following options:
--
-- -   @ALL@ – The list includes all asset properties for a given asset
--     model ID.
--
-- -   @BASE@ – The list includes only base asset properties for a given
--     asset model ID.
--
-- Default: @BASE@
--
-- 'maxResults', 'listAssetProperties_maxResults' - The maximum number of results to return for each paginated request. If
-- not specified, the default value is 50.
--
-- 'nextToken', 'listAssetProperties_nextToken' - The token to be used for the next set of paginated results.
--
-- 'assetId', 'listAssetProperties_assetId' - The ID of the asset.
newListAssetProperties ::
  -- | 'assetId'
  Prelude.Text ->
  ListAssetProperties
newListAssetProperties pAssetId_ =
  ListAssetProperties'
    { filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      assetId = pAssetId_
    }

-- | Filters the requested list of asset properties. You can choose one of
-- the following options:
--
-- -   @ALL@ – The list includes all asset properties for a given asset
--     model ID.
--
-- -   @BASE@ – The list includes only base asset properties for a given
--     asset model ID.
--
-- Default: @BASE@
listAssetProperties_filter :: Lens.Lens' ListAssetProperties (Prelude.Maybe ListAssetPropertiesFilter)
listAssetProperties_filter = Lens.lens (\ListAssetProperties' {filter'} -> filter') (\s@ListAssetProperties' {} a -> s {filter' = a} :: ListAssetProperties)

-- | The maximum number of results to return for each paginated request. If
-- not specified, the default value is 50.
listAssetProperties_maxResults :: Lens.Lens' ListAssetProperties (Prelude.Maybe Prelude.Natural)
listAssetProperties_maxResults = Lens.lens (\ListAssetProperties' {maxResults} -> maxResults) (\s@ListAssetProperties' {} a -> s {maxResults = a} :: ListAssetProperties)

-- | The token to be used for the next set of paginated results.
listAssetProperties_nextToken :: Lens.Lens' ListAssetProperties (Prelude.Maybe Prelude.Text)
listAssetProperties_nextToken = Lens.lens (\ListAssetProperties' {nextToken} -> nextToken) (\s@ListAssetProperties' {} a -> s {nextToken = a} :: ListAssetProperties)

-- | The ID of the asset.
listAssetProperties_assetId :: Lens.Lens' ListAssetProperties Prelude.Text
listAssetProperties_assetId = Lens.lens (\ListAssetProperties' {assetId} -> assetId) (\s@ListAssetProperties' {} a -> s {assetId = a} :: ListAssetProperties)

instance Core.AWSPager ListAssetProperties where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssetPropertiesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listAssetPropertiesResponse_assetPropertySummaries
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listAssetProperties_nextToken
              Lens..~ rs
              Lens.^? listAssetPropertiesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListAssetProperties where
  type
    AWSResponse ListAssetProperties =
      ListAssetPropertiesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssetPropertiesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "assetPropertySummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListAssetProperties where
  hashWithSalt _salt ListAssetProperties' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` assetId

instance Prelude.NFData ListAssetProperties where
  rnf ListAssetProperties' {..} =
    Prelude.rnf filter' `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf assetId

instance Data.ToHeaders ListAssetProperties where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListAssetProperties where
  toPath ListAssetProperties' {..} =
    Prelude.mconcat
      ["/assets/", Data.toBS assetId, "/properties"]

instance Data.ToQuery ListAssetProperties where
  toQuery ListAssetProperties' {..} =
    Prelude.mconcat
      [ "filter" Data.=: filter',
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListAssetPropertiesResponse' smart constructor.
data ListAssetPropertiesResponse = ListAssetPropertiesResponse'
  { -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list that summarizes the properties associated with the specified
    -- asset.
    assetPropertySummaries :: [AssetPropertySummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssetPropertiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssetPropertiesResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'httpStatus', 'listAssetPropertiesResponse_httpStatus' - The response's http status code.
--
-- 'assetPropertySummaries', 'listAssetPropertiesResponse_assetPropertySummaries' - A list that summarizes the properties associated with the specified
-- asset.
newListAssetPropertiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAssetPropertiesResponse
newListAssetPropertiesResponse pHttpStatus_ =
  ListAssetPropertiesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      assetPropertySummaries = Prelude.mempty
    }

-- | The token for the next set of results, or null if there are no
-- additional results.
listAssetPropertiesResponse_nextToken :: Lens.Lens' ListAssetPropertiesResponse (Prelude.Maybe Prelude.Text)
listAssetPropertiesResponse_nextToken = Lens.lens (\ListAssetPropertiesResponse' {nextToken} -> nextToken) (\s@ListAssetPropertiesResponse' {} a -> s {nextToken = a} :: ListAssetPropertiesResponse)

-- | The response's http status code.
listAssetPropertiesResponse_httpStatus :: Lens.Lens' ListAssetPropertiesResponse Prelude.Int
listAssetPropertiesResponse_httpStatus = Lens.lens (\ListAssetPropertiesResponse' {httpStatus} -> httpStatus) (\s@ListAssetPropertiesResponse' {} a -> s {httpStatus = a} :: ListAssetPropertiesResponse)

-- | A list that summarizes the properties associated with the specified
-- asset.
listAssetPropertiesResponse_assetPropertySummaries :: Lens.Lens' ListAssetPropertiesResponse [AssetPropertySummary]
listAssetPropertiesResponse_assetPropertySummaries = Lens.lens (\ListAssetPropertiesResponse' {assetPropertySummaries} -> assetPropertySummaries) (\s@ListAssetPropertiesResponse' {} a -> s {assetPropertySummaries = a} :: ListAssetPropertiesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListAssetPropertiesResponse where
  rnf ListAssetPropertiesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf assetPropertySummaries
