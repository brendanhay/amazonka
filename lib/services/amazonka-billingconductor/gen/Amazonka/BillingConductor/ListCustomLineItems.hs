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
-- Module      : Amazonka.BillingConductor.ListCustomLineItems
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A paginated call to get a list of all custom line items (FFLIs) for the
-- given billing period. If you don\'t provide a billing period, the
-- current billing period is used.
--
-- This operation returns paginated results.
module Amazonka.BillingConductor.ListCustomLineItems
  ( -- * Creating a Request
    ListCustomLineItems (..),
    newListCustomLineItems,

    -- * Request Lenses
    listCustomLineItems_nextToken,
    listCustomLineItems_billingPeriod,
    listCustomLineItems_filters,
    listCustomLineItems_maxResults,

    -- * Destructuring the Response
    ListCustomLineItemsResponse (..),
    newListCustomLineItemsResponse,

    -- * Response Lenses
    listCustomLineItemsResponse_nextToken,
    listCustomLineItemsResponse_customLineItems,
    listCustomLineItemsResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCustomLineItems' smart constructor.
data ListCustomLineItems = ListCustomLineItems'
  { -- | The pagination token that\'s used on subsequent calls to get custom line
    -- items (FFLIs).
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The preferred billing period to get custom line items (FFLIs).
    billingPeriod :: Prelude.Maybe Prelude.Text,
    -- | A @ListCustomLineItemsFilter@ that specifies the custom line item names
    -- and\/or billing group Amazon Resource Names (ARNs) to retrieve FFLI
    -- information.
    filters :: Prelude.Maybe ListCustomLineItemsFilter,
    -- | The maximum number of billing groups to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomLineItems' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCustomLineItems_nextToken' - The pagination token that\'s used on subsequent calls to get custom line
-- items (FFLIs).
--
-- 'billingPeriod', 'listCustomLineItems_billingPeriod' - The preferred billing period to get custom line items (FFLIs).
--
-- 'filters', 'listCustomLineItems_filters' - A @ListCustomLineItemsFilter@ that specifies the custom line item names
-- and\/or billing group Amazon Resource Names (ARNs) to retrieve FFLI
-- information.
--
-- 'maxResults', 'listCustomLineItems_maxResults' - The maximum number of billing groups to retrieve.
newListCustomLineItems ::
  ListCustomLineItems
newListCustomLineItems =
  ListCustomLineItems'
    { nextToken = Prelude.Nothing,
      billingPeriod = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The pagination token that\'s used on subsequent calls to get custom line
-- items (FFLIs).
listCustomLineItems_nextToken :: Lens.Lens' ListCustomLineItems (Prelude.Maybe Prelude.Text)
listCustomLineItems_nextToken = Lens.lens (\ListCustomLineItems' {nextToken} -> nextToken) (\s@ListCustomLineItems' {} a -> s {nextToken = a} :: ListCustomLineItems)

-- | The preferred billing period to get custom line items (FFLIs).
listCustomLineItems_billingPeriod :: Lens.Lens' ListCustomLineItems (Prelude.Maybe Prelude.Text)
listCustomLineItems_billingPeriod = Lens.lens (\ListCustomLineItems' {billingPeriod} -> billingPeriod) (\s@ListCustomLineItems' {} a -> s {billingPeriod = a} :: ListCustomLineItems)

-- | A @ListCustomLineItemsFilter@ that specifies the custom line item names
-- and\/or billing group Amazon Resource Names (ARNs) to retrieve FFLI
-- information.
listCustomLineItems_filters :: Lens.Lens' ListCustomLineItems (Prelude.Maybe ListCustomLineItemsFilter)
listCustomLineItems_filters = Lens.lens (\ListCustomLineItems' {filters} -> filters) (\s@ListCustomLineItems' {} a -> s {filters = a} :: ListCustomLineItems)

-- | The maximum number of billing groups to retrieve.
listCustomLineItems_maxResults :: Lens.Lens' ListCustomLineItems (Prelude.Maybe Prelude.Natural)
listCustomLineItems_maxResults = Lens.lens (\ListCustomLineItems' {maxResults} -> maxResults) (\s@ListCustomLineItems' {} a -> s {maxResults = a} :: ListCustomLineItems)

instance Core.AWSPager ListCustomLineItems where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCustomLineItemsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCustomLineItemsResponse_customLineItems
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCustomLineItems_nextToken
          Lens..~ rs
          Lens.^? listCustomLineItemsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListCustomLineItems where
  type
    AWSResponse ListCustomLineItems =
      ListCustomLineItemsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCustomLineItemsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "CustomLineItems"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCustomLineItems where
  hashWithSalt _salt ListCustomLineItems' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` billingPeriod
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListCustomLineItems where
  rnf ListCustomLineItems' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf billingPeriod
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListCustomLineItems where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListCustomLineItems where
  toJSON ListCustomLineItems' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("BillingPeriod" Data..=) Prelude.<$> billingPeriod,
            ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListCustomLineItems where
  toPath = Prelude.const "/list-custom-line-items"

instance Data.ToQuery ListCustomLineItems where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCustomLineItemsResponse' smart constructor.
data ListCustomLineItemsResponse = ListCustomLineItemsResponse'
  { -- | The pagination token that\'s used on subsequent calls to get custom line
    -- items (FFLIs).
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @FreeFormLineItemListElements@ received.
    customLineItems :: Prelude.Maybe [CustomLineItemListElement],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomLineItemsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCustomLineItemsResponse_nextToken' - The pagination token that\'s used on subsequent calls to get custom line
-- items (FFLIs).
--
-- 'customLineItems', 'listCustomLineItemsResponse_customLineItems' - A list of @FreeFormLineItemListElements@ received.
--
-- 'httpStatus', 'listCustomLineItemsResponse_httpStatus' - The response's http status code.
newListCustomLineItemsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCustomLineItemsResponse
newListCustomLineItemsResponse pHttpStatus_ =
  ListCustomLineItemsResponse'
    { nextToken =
        Prelude.Nothing,
      customLineItems = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token that\'s used on subsequent calls to get custom line
-- items (FFLIs).
listCustomLineItemsResponse_nextToken :: Lens.Lens' ListCustomLineItemsResponse (Prelude.Maybe Prelude.Text)
listCustomLineItemsResponse_nextToken = Lens.lens (\ListCustomLineItemsResponse' {nextToken} -> nextToken) (\s@ListCustomLineItemsResponse' {} a -> s {nextToken = a} :: ListCustomLineItemsResponse)

-- | A list of @FreeFormLineItemListElements@ received.
listCustomLineItemsResponse_customLineItems :: Lens.Lens' ListCustomLineItemsResponse (Prelude.Maybe [CustomLineItemListElement])
listCustomLineItemsResponse_customLineItems = Lens.lens (\ListCustomLineItemsResponse' {customLineItems} -> customLineItems) (\s@ListCustomLineItemsResponse' {} a -> s {customLineItems = a} :: ListCustomLineItemsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCustomLineItemsResponse_httpStatus :: Lens.Lens' ListCustomLineItemsResponse Prelude.Int
listCustomLineItemsResponse_httpStatus = Lens.lens (\ListCustomLineItemsResponse' {httpStatus} -> httpStatus) (\s@ListCustomLineItemsResponse' {} a -> s {httpStatus = a} :: ListCustomLineItemsResponse)

instance Prelude.NFData ListCustomLineItemsResponse where
  rnf ListCustomLineItemsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf customLineItems
      `Prelude.seq` Prelude.rnf httpStatus
