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
-- Module      : Amazonka.Outposts.ListCatalogItems
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the items in the catalog.
--
-- Use filters to return specific results. If you specify multiple filters,
-- the results include only the resources that match all of the specified
-- filters. For a filter where you can specify multiple values, the results
-- include items that match any of the values that you specify for the
-- filter.
module Amazonka.Outposts.ListCatalogItems
  ( -- * Creating a Request
    ListCatalogItems (..),
    newListCatalogItems,

    -- * Request Lenses
    listCatalogItems_eC2FamilyFilter,
    listCatalogItems_itemClassFilter,
    listCatalogItems_maxResults,
    listCatalogItems_nextToken,
    listCatalogItems_supportedStorageFilter,

    -- * Destructuring the Response
    ListCatalogItemsResponse (..),
    newListCatalogItemsResponse,

    -- * Response Lenses
    listCatalogItemsResponse_catalogItems,
    listCatalogItemsResponse_nextToken,
    listCatalogItemsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Outposts.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCatalogItems' smart constructor.
data ListCatalogItems = ListCatalogItems'
  { -- | Filters the results by EC2 family (for example, M5).
    eC2FamilyFilter :: Prelude.Maybe [Prelude.Text],
    -- | Filters the results by item class.
    itemClassFilter :: Prelude.Maybe [CatalogItemClass],
    maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters the results by storage option.
    supportedStorageFilter :: Prelude.Maybe [SupportedStorageEnum]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCatalogItems' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eC2FamilyFilter', 'listCatalogItems_eC2FamilyFilter' - Filters the results by EC2 family (for example, M5).
--
-- 'itemClassFilter', 'listCatalogItems_itemClassFilter' - Filters the results by item class.
--
-- 'maxResults', 'listCatalogItems_maxResults' - Undocumented member.
--
-- 'nextToken', 'listCatalogItems_nextToken' - Undocumented member.
--
-- 'supportedStorageFilter', 'listCatalogItems_supportedStorageFilter' - Filters the results by storage option.
newListCatalogItems ::
  ListCatalogItems
newListCatalogItems =
  ListCatalogItems'
    { eC2FamilyFilter =
        Prelude.Nothing,
      itemClassFilter = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      supportedStorageFilter = Prelude.Nothing
    }

-- | Filters the results by EC2 family (for example, M5).
listCatalogItems_eC2FamilyFilter :: Lens.Lens' ListCatalogItems (Prelude.Maybe [Prelude.Text])
listCatalogItems_eC2FamilyFilter = Lens.lens (\ListCatalogItems' {eC2FamilyFilter} -> eC2FamilyFilter) (\s@ListCatalogItems' {} a -> s {eC2FamilyFilter = a} :: ListCatalogItems) Prelude.. Lens.mapping Lens.coerced

-- | Filters the results by item class.
listCatalogItems_itemClassFilter :: Lens.Lens' ListCatalogItems (Prelude.Maybe [CatalogItemClass])
listCatalogItems_itemClassFilter = Lens.lens (\ListCatalogItems' {itemClassFilter} -> itemClassFilter) (\s@ListCatalogItems' {} a -> s {itemClassFilter = a} :: ListCatalogItems) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
listCatalogItems_maxResults :: Lens.Lens' ListCatalogItems (Prelude.Maybe Prelude.Natural)
listCatalogItems_maxResults = Lens.lens (\ListCatalogItems' {maxResults} -> maxResults) (\s@ListCatalogItems' {} a -> s {maxResults = a} :: ListCatalogItems)

-- | Undocumented member.
listCatalogItems_nextToken :: Lens.Lens' ListCatalogItems (Prelude.Maybe Prelude.Text)
listCatalogItems_nextToken = Lens.lens (\ListCatalogItems' {nextToken} -> nextToken) (\s@ListCatalogItems' {} a -> s {nextToken = a} :: ListCatalogItems)

-- | Filters the results by storage option.
listCatalogItems_supportedStorageFilter :: Lens.Lens' ListCatalogItems (Prelude.Maybe [SupportedStorageEnum])
listCatalogItems_supportedStorageFilter = Lens.lens (\ListCatalogItems' {supportedStorageFilter} -> supportedStorageFilter) (\s@ListCatalogItems' {} a -> s {supportedStorageFilter = a} :: ListCatalogItems) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest ListCatalogItems where
  type
    AWSResponse ListCatalogItems =
      ListCatalogItemsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCatalogItemsResponse'
            Prelude.<$> (x Data..?> "CatalogItems" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCatalogItems where
  hashWithSalt _salt ListCatalogItems' {..} =
    _salt
      `Prelude.hashWithSalt` eC2FamilyFilter
      `Prelude.hashWithSalt` itemClassFilter
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` supportedStorageFilter

instance Prelude.NFData ListCatalogItems where
  rnf ListCatalogItems' {..} =
    Prelude.rnf eC2FamilyFilter `Prelude.seq`
      Prelude.rnf itemClassFilter `Prelude.seq`
        Prelude.rnf maxResults `Prelude.seq`
          Prelude.rnf nextToken `Prelude.seq`
            Prelude.rnf supportedStorageFilter

instance Data.ToHeaders ListCatalogItems where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListCatalogItems where
  toPath = Prelude.const "/catalog/items"

instance Data.ToQuery ListCatalogItems where
  toQuery ListCatalogItems' {..} =
    Prelude.mconcat
      [ "EC2FamilyFilter"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> eC2FamilyFilter
            ),
        "ItemClassFilter"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> itemClassFilter
            ),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "SupportedStorageFilter"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> supportedStorageFilter
            )
      ]

-- | /See:/ 'newListCatalogItemsResponse' smart constructor.
data ListCatalogItemsResponse = ListCatalogItemsResponse'
  { -- | Information about the catalog items.
    catalogItems :: Prelude.Maybe [CatalogItem],
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCatalogItemsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogItems', 'listCatalogItemsResponse_catalogItems' - Information about the catalog items.
--
-- 'nextToken', 'listCatalogItemsResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'listCatalogItemsResponse_httpStatus' - The response's http status code.
newListCatalogItemsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCatalogItemsResponse
newListCatalogItemsResponse pHttpStatus_ =
  ListCatalogItemsResponse'
    { catalogItems =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the catalog items.
listCatalogItemsResponse_catalogItems :: Lens.Lens' ListCatalogItemsResponse (Prelude.Maybe [CatalogItem])
listCatalogItemsResponse_catalogItems = Lens.lens (\ListCatalogItemsResponse' {catalogItems} -> catalogItems) (\s@ListCatalogItemsResponse' {} a -> s {catalogItems = a} :: ListCatalogItemsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
listCatalogItemsResponse_nextToken :: Lens.Lens' ListCatalogItemsResponse (Prelude.Maybe Prelude.Text)
listCatalogItemsResponse_nextToken = Lens.lens (\ListCatalogItemsResponse' {nextToken} -> nextToken) (\s@ListCatalogItemsResponse' {} a -> s {nextToken = a} :: ListCatalogItemsResponse)

-- | The response's http status code.
listCatalogItemsResponse_httpStatus :: Lens.Lens' ListCatalogItemsResponse Prelude.Int
listCatalogItemsResponse_httpStatus = Lens.lens (\ListCatalogItemsResponse' {httpStatus} -> httpStatus) (\s@ListCatalogItemsResponse' {} a -> s {httpStatus = a} :: ListCatalogItemsResponse)

instance Prelude.NFData ListCatalogItemsResponse where
  rnf ListCatalogItemsResponse' {..} =
    Prelude.rnf catalogItems `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
