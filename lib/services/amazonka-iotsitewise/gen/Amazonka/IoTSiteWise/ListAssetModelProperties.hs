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
-- Module      : Amazonka.IoTSiteWise.ListAssetModelProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a paginated list of properties associated with an asset model.
-- If you update properties associated with the model before you finish
-- listing all the properties, you need to start all over again.
--
-- This operation returns paginated results.
module Amazonka.IoTSiteWise.ListAssetModelProperties
  ( -- * Creating a Request
    ListAssetModelProperties (..),
    newListAssetModelProperties,

    -- * Request Lenses
    listAssetModelProperties_filter,
    listAssetModelProperties_maxResults,
    listAssetModelProperties_nextToken,
    listAssetModelProperties_assetModelId,

    -- * Destructuring the Response
    ListAssetModelPropertiesResponse (..),
    newListAssetModelPropertiesResponse,

    -- * Response Lenses
    listAssetModelPropertiesResponse_nextToken,
    listAssetModelPropertiesResponse_httpStatus,
    listAssetModelPropertiesResponse_assetModelPropertySummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAssetModelProperties' smart constructor.
data ListAssetModelProperties = ListAssetModelProperties'
  { -- | Filters the requested list of asset model properties. You can choose one
    -- of the following options:
    --
    -- -   @ALL@ – The list includes all asset model properties for a given
    --     asset model ID.
    --
    -- -   @BASE@ – The list includes only base asset model properties for a
    --     given asset model ID.
    --
    -- Default: @BASE@
    filter' :: Prelude.Maybe ListAssetModelPropertiesFilter,
    -- | The maximum number of results to return for each paginated request. If
    -- not specified, the default value is 50.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to be used for the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset model.
    assetModelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssetModelProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listAssetModelProperties_filter' - Filters the requested list of asset model properties. You can choose one
-- of the following options:
--
-- -   @ALL@ – The list includes all asset model properties for a given
--     asset model ID.
--
-- -   @BASE@ – The list includes only base asset model properties for a
--     given asset model ID.
--
-- Default: @BASE@
--
-- 'maxResults', 'listAssetModelProperties_maxResults' - The maximum number of results to return for each paginated request. If
-- not specified, the default value is 50.
--
-- 'nextToken', 'listAssetModelProperties_nextToken' - The token to be used for the next set of paginated results.
--
-- 'assetModelId', 'listAssetModelProperties_assetModelId' - The ID of the asset model.
newListAssetModelProperties ::
  -- | 'assetModelId'
  Prelude.Text ->
  ListAssetModelProperties
newListAssetModelProperties pAssetModelId_ =
  ListAssetModelProperties'
    { filter' =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      assetModelId = pAssetModelId_
    }

-- | Filters the requested list of asset model properties. You can choose one
-- of the following options:
--
-- -   @ALL@ – The list includes all asset model properties for a given
--     asset model ID.
--
-- -   @BASE@ – The list includes only base asset model properties for a
--     given asset model ID.
--
-- Default: @BASE@
listAssetModelProperties_filter :: Lens.Lens' ListAssetModelProperties (Prelude.Maybe ListAssetModelPropertiesFilter)
listAssetModelProperties_filter = Lens.lens (\ListAssetModelProperties' {filter'} -> filter') (\s@ListAssetModelProperties' {} a -> s {filter' = a} :: ListAssetModelProperties)

-- | The maximum number of results to return for each paginated request. If
-- not specified, the default value is 50.
listAssetModelProperties_maxResults :: Lens.Lens' ListAssetModelProperties (Prelude.Maybe Prelude.Natural)
listAssetModelProperties_maxResults = Lens.lens (\ListAssetModelProperties' {maxResults} -> maxResults) (\s@ListAssetModelProperties' {} a -> s {maxResults = a} :: ListAssetModelProperties)

-- | The token to be used for the next set of paginated results.
listAssetModelProperties_nextToken :: Lens.Lens' ListAssetModelProperties (Prelude.Maybe Prelude.Text)
listAssetModelProperties_nextToken = Lens.lens (\ListAssetModelProperties' {nextToken} -> nextToken) (\s@ListAssetModelProperties' {} a -> s {nextToken = a} :: ListAssetModelProperties)

-- | The ID of the asset model.
listAssetModelProperties_assetModelId :: Lens.Lens' ListAssetModelProperties Prelude.Text
listAssetModelProperties_assetModelId = Lens.lens (\ListAssetModelProperties' {assetModelId} -> assetModelId) (\s@ListAssetModelProperties' {} a -> s {assetModelId = a} :: ListAssetModelProperties)

instance Core.AWSPager ListAssetModelProperties where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssetModelPropertiesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listAssetModelPropertiesResponse_assetModelPropertySummaries
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listAssetModelProperties_nextToken
          Lens..~ rs
          Lens.^? listAssetModelPropertiesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListAssetModelProperties where
  type
    AWSResponse ListAssetModelProperties =
      ListAssetModelPropertiesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssetModelPropertiesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "assetModelPropertySummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListAssetModelProperties where
  hashWithSalt _salt ListAssetModelProperties' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` assetModelId

instance Prelude.NFData ListAssetModelProperties where
  rnf ListAssetModelProperties' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf assetModelId

instance Data.ToHeaders ListAssetModelProperties where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListAssetModelProperties where
  toPath ListAssetModelProperties' {..} =
    Prelude.mconcat
      [ "/asset-models/",
        Data.toBS assetModelId,
        "/properties"
      ]

instance Data.ToQuery ListAssetModelProperties where
  toQuery ListAssetModelProperties' {..} =
    Prelude.mconcat
      [ "filter" Data.=: filter',
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListAssetModelPropertiesResponse' smart constructor.
data ListAssetModelPropertiesResponse = ListAssetModelPropertiesResponse'
  { -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list that summarizes the properties associated with the specified
    -- asset model.
    assetModelPropertySummaries :: [AssetModelPropertySummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssetModelPropertiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssetModelPropertiesResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'httpStatus', 'listAssetModelPropertiesResponse_httpStatus' - The response's http status code.
--
-- 'assetModelPropertySummaries', 'listAssetModelPropertiesResponse_assetModelPropertySummaries' - A list that summarizes the properties associated with the specified
-- asset model.
newListAssetModelPropertiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAssetModelPropertiesResponse
newListAssetModelPropertiesResponse pHttpStatus_ =
  ListAssetModelPropertiesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      assetModelPropertySummaries =
        Prelude.mempty
    }

-- | The token for the next set of results, or null if there are no
-- additional results.
listAssetModelPropertiesResponse_nextToken :: Lens.Lens' ListAssetModelPropertiesResponse (Prelude.Maybe Prelude.Text)
listAssetModelPropertiesResponse_nextToken = Lens.lens (\ListAssetModelPropertiesResponse' {nextToken} -> nextToken) (\s@ListAssetModelPropertiesResponse' {} a -> s {nextToken = a} :: ListAssetModelPropertiesResponse)

-- | The response's http status code.
listAssetModelPropertiesResponse_httpStatus :: Lens.Lens' ListAssetModelPropertiesResponse Prelude.Int
listAssetModelPropertiesResponse_httpStatus = Lens.lens (\ListAssetModelPropertiesResponse' {httpStatus} -> httpStatus) (\s@ListAssetModelPropertiesResponse' {} a -> s {httpStatus = a} :: ListAssetModelPropertiesResponse)

-- | A list that summarizes the properties associated with the specified
-- asset model.
listAssetModelPropertiesResponse_assetModelPropertySummaries :: Lens.Lens' ListAssetModelPropertiesResponse [AssetModelPropertySummary]
listAssetModelPropertiesResponse_assetModelPropertySummaries = Lens.lens (\ListAssetModelPropertiesResponse' {assetModelPropertySummaries} -> assetModelPropertySummaries) (\s@ListAssetModelPropertiesResponse' {} a -> s {assetModelPropertySummaries = a} :: ListAssetModelPropertiesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListAssetModelPropertiesResponse
  where
  rnf ListAssetModelPropertiesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf assetModelPropertySummaries
