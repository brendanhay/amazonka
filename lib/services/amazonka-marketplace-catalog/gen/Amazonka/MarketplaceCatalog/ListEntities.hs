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
-- Module      : Amazonka.MarketplaceCatalog.ListEntities
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the list of entities of a given type.
module Amazonka.MarketplaceCatalog.ListEntities
  ( -- * Creating a Request
    ListEntities (..),
    newListEntities,

    -- * Request Lenses
    listEntities_nextToken,
    listEntities_filterList,
    listEntities_sort,
    listEntities_maxResults,
    listEntities_catalog,
    listEntities_entityType,

    -- * Destructuring the Response
    ListEntitiesResponse (..),
    newListEntitiesResponse,

    -- * Response Lenses
    listEntitiesResponse_nextToken,
    listEntitiesResponse_entitySummaryList,
    listEntitiesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MarketplaceCatalog.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEntities' smart constructor.
data ListEntities = ListEntities'
  { -- | The value of the next token, if it exists. Null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of filter objects. Each filter object contains two attributes,
    -- @filterName@ and @filterValues@.
    filterList :: Prelude.Maybe (Prelude.NonEmpty Filter),
    -- | An object that contains two attributes, @SortBy@ and @SortOrder@.
    sort :: Prelude.Maybe Sort,
    -- | Specifies the upper limit of the elements on a single page. If a value
    -- isn\'t provided, the default value is 20.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The catalog related to the request. Fixed value: @AWSMarketplace@
    catalog :: Prelude.Text,
    -- | The type of entities to retrieve.
    entityType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEntities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEntities_nextToken' - The value of the next token, if it exists. Null if there are no more
-- results.
--
-- 'filterList', 'listEntities_filterList' - An array of filter objects. Each filter object contains two attributes,
-- @filterName@ and @filterValues@.
--
-- 'sort', 'listEntities_sort' - An object that contains two attributes, @SortBy@ and @SortOrder@.
--
-- 'maxResults', 'listEntities_maxResults' - Specifies the upper limit of the elements on a single page. If a value
-- isn\'t provided, the default value is 20.
--
-- 'catalog', 'listEntities_catalog' - The catalog related to the request. Fixed value: @AWSMarketplace@
--
-- 'entityType', 'listEntities_entityType' - The type of entities to retrieve.
newListEntities ::
  -- | 'catalog'
  Prelude.Text ->
  -- | 'entityType'
  Prelude.Text ->
  ListEntities
newListEntities pCatalog_ pEntityType_ =
  ListEntities'
    { nextToken = Prelude.Nothing,
      filterList = Prelude.Nothing,
      sort = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      catalog = pCatalog_,
      entityType = pEntityType_
    }

-- | The value of the next token, if it exists. Null if there are no more
-- results.
listEntities_nextToken :: Lens.Lens' ListEntities (Prelude.Maybe Prelude.Text)
listEntities_nextToken = Lens.lens (\ListEntities' {nextToken} -> nextToken) (\s@ListEntities' {} a -> s {nextToken = a} :: ListEntities)

-- | An array of filter objects. Each filter object contains two attributes,
-- @filterName@ and @filterValues@.
listEntities_filterList :: Lens.Lens' ListEntities (Prelude.Maybe (Prelude.NonEmpty Filter))
listEntities_filterList = Lens.lens (\ListEntities' {filterList} -> filterList) (\s@ListEntities' {} a -> s {filterList = a} :: ListEntities) Prelude.. Lens.mapping Lens.coerced

-- | An object that contains two attributes, @SortBy@ and @SortOrder@.
listEntities_sort :: Lens.Lens' ListEntities (Prelude.Maybe Sort)
listEntities_sort = Lens.lens (\ListEntities' {sort} -> sort) (\s@ListEntities' {} a -> s {sort = a} :: ListEntities)

-- | Specifies the upper limit of the elements on a single page. If a value
-- isn\'t provided, the default value is 20.
listEntities_maxResults :: Lens.Lens' ListEntities (Prelude.Maybe Prelude.Natural)
listEntities_maxResults = Lens.lens (\ListEntities' {maxResults} -> maxResults) (\s@ListEntities' {} a -> s {maxResults = a} :: ListEntities)

-- | The catalog related to the request. Fixed value: @AWSMarketplace@
listEntities_catalog :: Lens.Lens' ListEntities Prelude.Text
listEntities_catalog = Lens.lens (\ListEntities' {catalog} -> catalog) (\s@ListEntities' {} a -> s {catalog = a} :: ListEntities)

-- | The type of entities to retrieve.
listEntities_entityType :: Lens.Lens' ListEntities Prelude.Text
listEntities_entityType = Lens.lens (\ListEntities' {entityType} -> entityType) (\s@ListEntities' {} a -> s {entityType = a} :: ListEntities)

instance Core.AWSRequest ListEntities where
  type AWSResponse ListEntities = ListEntitiesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEntitiesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "EntitySummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEntities where
  hashWithSalt _salt ListEntities' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filterList
      `Prelude.hashWithSalt` sort
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` catalog
      `Prelude.hashWithSalt` entityType

instance Prelude.NFData ListEntities where
  rnf ListEntities' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filterList
      `Prelude.seq` Prelude.rnf sort
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf catalog
      `Prelude.seq` Prelude.rnf entityType

instance Data.ToHeaders ListEntities where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEntities where
  toJSON ListEntities' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("FilterList" Data..=) Prelude.<$> filterList,
            ("Sort" Data..=) Prelude.<$> sort,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("Catalog" Data..= catalog),
            Prelude.Just ("EntityType" Data..= entityType)
          ]
      )

instance Data.ToPath ListEntities where
  toPath = Prelude.const "/ListEntities"

instance Data.ToQuery ListEntities where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEntitiesResponse' smart constructor.
data ListEntitiesResponse = ListEntitiesResponse'
  { -- | The value of the next token if it exists. Null if there is no more
    -- result.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Array of @EntitySummary@ object.
    entitySummaryList :: Prelude.Maybe [EntitySummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEntitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEntitiesResponse_nextToken' - The value of the next token if it exists. Null if there is no more
-- result.
--
-- 'entitySummaryList', 'listEntitiesResponse_entitySummaryList' - Array of @EntitySummary@ object.
--
-- 'httpStatus', 'listEntitiesResponse_httpStatus' - The response's http status code.
newListEntitiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEntitiesResponse
newListEntitiesResponse pHttpStatus_ =
  ListEntitiesResponse'
    { nextToken = Prelude.Nothing,
      entitySummaryList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The value of the next token if it exists. Null if there is no more
-- result.
listEntitiesResponse_nextToken :: Lens.Lens' ListEntitiesResponse (Prelude.Maybe Prelude.Text)
listEntitiesResponse_nextToken = Lens.lens (\ListEntitiesResponse' {nextToken} -> nextToken) (\s@ListEntitiesResponse' {} a -> s {nextToken = a} :: ListEntitiesResponse)

-- | Array of @EntitySummary@ object.
listEntitiesResponse_entitySummaryList :: Lens.Lens' ListEntitiesResponse (Prelude.Maybe [EntitySummary])
listEntitiesResponse_entitySummaryList = Lens.lens (\ListEntitiesResponse' {entitySummaryList} -> entitySummaryList) (\s@ListEntitiesResponse' {} a -> s {entitySummaryList = a} :: ListEntitiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listEntitiesResponse_httpStatus :: Lens.Lens' ListEntitiesResponse Prelude.Int
listEntitiesResponse_httpStatus = Lens.lens (\ListEntitiesResponse' {httpStatus} -> httpStatus) (\s@ListEntitiesResponse' {} a -> s {httpStatus = a} :: ListEntitiesResponse)

instance Prelude.NFData ListEntitiesResponse where
  rnf ListEntitiesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf entitySummaryList
      `Prelude.seq` Prelude.rnf httpStatus
