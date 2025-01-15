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
-- Module      : Amazonka.Route53Domains.ListPrices
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the following prices for either all the TLDs supported by
-- Route 53, or the specified TLD:
--
-- -   Registration
--
-- -   Transfer
--
-- -   Owner change
--
-- -   Domain renewal
--
-- -   Domain restoration
--
-- This operation returns paginated results.
module Amazonka.Route53Domains.ListPrices
  ( -- * Creating a Request
    ListPrices (..),
    newListPrices,

    -- * Request Lenses
    listPrices_marker,
    listPrices_maxItems,
    listPrices_tld,

    -- * Destructuring the Response
    ListPricesResponse (..),
    newListPricesResponse,

    -- * Response Lenses
    listPricesResponse_nextPageMarker,
    listPricesResponse_prices,
    listPricesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

-- | /See:/ 'newListPrices' smart constructor.
data ListPrices = ListPrices'
  { -- | For an initial request for a list of prices, omit this element. If the
    -- number of prices that are not yet complete is greater than the value
    -- that you specified for @MaxItems@, you can use @Marker@ to return
    -- additional prices. Get the value of @NextPageMarker@ from the previous
    -- response, and submit another request that includes the value of
    -- @NextPageMarker@ in the @Marker@ element.
    --
    -- Used only for all TLDs. If you specify a TLD, don\'t specify a @Marker@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Number of @Prices@ to be returned.
    --
    -- Used only for all TLDs. If you specify a TLD, don\'t specify a
    -- @MaxItems@.
    maxItems :: Prelude.Maybe Prelude.Int,
    -- | The TLD for which you want to receive the pricing information. For
    -- example. @.net@.
    --
    -- If a @Tld@ value is not provided, a list of prices for all TLDs
    -- supported by Route 53 is returned.
    tld :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPrices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listPrices_marker' - For an initial request for a list of prices, omit this element. If the
-- number of prices that are not yet complete is greater than the value
-- that you specified for @MaxItems@, you can use @Marker@ to return
-- additional prices. Get the value of @NextPageMarker@ from the previous
-- response, and submit another request that includes the value of
-- @NextPageMarker@ in the @Marker@ element.
--
-- Used only for all TLDs. If you specify a TLD, don\'t specify a @Marker@.
--
-- 'maxItems', 'listPrices_maxItems' - Number of @Prices@ to be returned.
--
-- Used only for all TLDs. If you specify a TLD, don\'t specify a
-- @MaxItems@.
--
-- 'tld', 'listPrices_tld' - The TLD for which you want to receive the pricing information. For
-- example. @.net@.
--
-- If a @Tld@ value is not provided, a list of prices for all TLDs
-- supported by Route 53 is returned.
newListPrices ::
  ListPrices
newListPrices =
  ListPrices'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      tld = Prelude.Nothing
    }

-- | For an initial request for a list of prices, omit this element. If the
-- number of prices that are not yet complete is greater than the value
-- that you specified for @MaxItems@, you can use @Marker@ to return
-- additional prices. Get the value of @NextPageMarker@ from the previous
-- response, and submit another request that includes the value of
-- @NextPageMarker@ in the @Marker@ element.
--
-- Used only for all TLDs. If you specify a TLD, don\'t specify a @Marker@.
listPrices_marker :: Lens.Lens' ListPrices (Prelude.Maybe Prelude.Text)
listPrices_marker = Lens.lens (\ListPrices' {marker} -> marker) (\s@ListPrices' {} a -> s {marker = a} :: ListPrices)

-- | Number of @Prices@ to be returned.
--
-- Used only for all TLDs. If you specify a TLD, don\'t specify a
-- @MaxItems@.
listPrices_maxItems :: Lens.Lens' ListPrices (Prelude.Maybe Prelude.Int)
listPrices_maxItems = Lens.lens (\ListPrices' {maxItems} -> maxItems) (\s@ListPrices' {} a -> s {maxItems = a} :: ListPrices)

-- | The TLD for which you want to receive the pricing information. For
-- example. @.net@.
--
-- If a @Tld@ value is not provided, a list of prices for all TLDs
-- supported by Route 53 is returned.
listPrices_tld :: Lens.Lens' ListPrices (Prelude.Maybe Prelude.Text)
listPrices_tld = Lens.lens (\ListPrices' {tld} -> tld) (\s@ListPrices' {} a -> s {tld = a} :: ListPrices)

instance Core.AWSPager ListPrices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPricesResponse_nextPageMarker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPricesResponse_prices
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listPrices_marker
              Lens..~ rs
              Lens.^? listPricesResponse_nextPageMarker
              Prelude.. Lens._Just

instance Core.AWSRequest ListPrices where
  type AWSResponse ListPrices = ListPricesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPricesResponse'
            Prelude.<$> (x Data..?> "NextPageMarker")
            Prelude.<*> (x Data..?> "Prices" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPrices where
  hashWithSalt _salt ListPrices' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` tld

instance Prelude.NFData ListPrices where
  rnf ListPrices' {..} =
    Prelude.rnf marker `Prelude.seq`
      Prelude.rnf maxItems `Prelude.seq`
        Prelude.rnf tld

instance Data.ToHeaders ListPrices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Domains_v20140515.ListPrices" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPrices where
  toJSON ListPrices' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Marker" Data..=) Prelude.<$> marker,
            ("MaxItems" Data..=) Prelude.<$> maxItems,
            ("Tld" Data..=) Prelude.<$> tld
          ]
      )

instance Data.ToPath ListPrices where
  toPath = Prelude.const "/"

instance Data.ToQuery ListPrices where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPricesResponse' smart constructor.
data ListPricesResponse = ListPricesResponse'
  { -- | If there are more prices than you specified for @MaxItems@ in the
    -- request, submit another request and include the value of
    -- @NextPageMarker@ in the value of @Marker@.
    --
    -- Used only for all TLDs. If you specify a TLD, don\'t specify a
    -- @NextPageMarker@.
    nextPageMarker :: Prelude.Maybe Prelude.Text,
    -- | A complex type that includes all the pricing information. If you specify
    -- a TLD, this array contains only the pricing for that TLD.
    prices :: Prelude.Maybe [DomainPrice],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPricesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageMarker', 'listPricesResponse_nextPageMarker' - If there are more prices than you specified for @MaxItems@ in the
-- request, submit another request and include the value of
-- @NextPageMarker@ in the value of @Marker@.
--
-- Used only for all TLDs. If you specify a TLD, don\'t specify a
-- @NextPageMarker@.
--
-- 'prices', 'listPricesResponse_prices' - A complex type that includes all the pricing information. If you specify
-- a TLD, this array contains only the pricing for that TLD.
--
-- 'httpStatus', 'listPricesResponse_httpStatus' - The response's http status code.
newListPricesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPricesResponse
newListPricesResponse pHttpStatus_ =
  ListPricesResponse'
    { nextPageMarker =
        Prelude.Nothing,
      prices = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are more prices than you specified for @MaxItems@ in the
-- request, submit another request and include the value of
-- @NextPageMarker@ in the value of @Marker@.
--
-- Used only for all TLDs. If you specify a TLD, don\'t specify a
-- @NextPageMarker@.
listPricesResponse_nextPageMarker :: Lens.Lens' ListPricesResponse (Prelude.Maybe Prelude.Text)
listPricesResponse_nextPageMarker = Lens.lens (\ListPricesResponse' {nextPageMarker} -> nextPageMarker) (\s@ListPricesResponse' {} a -> s {nextPageMarker = a} :: ListPricesResponse)

-- | A complex type that includes all the pricing information. If you specify
-- a TLD, this array contains only the pricing for that TLD.
listPricesResponse_prices :: Lens.Lens' ListPricesResponse (Prelude.Maybe [DomainPrice])
listPricesResponse_prices = Lens.lens (\ListPricesResponse' {prices} -> prices) (\s@ListPricesResponse' {} a -> s {prices = a} :: ListPricesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPricesResponse_httpStatus :: Lens.Lens' ListPricesResponse Prelude.Int
listPricesResponse_httpStatus = Lens.lens (\ListPricesResponse' {httpStatus} -> httpStatus) (\s@ListPricesResponse' {} a -> s {httpStatus = a} :: ListPricesResponse)

instance Prelude.NFData ListPricesResponse where
  rnf ListPricesResponse' {..} =
    Prelude.rnf nextPageMarker `Prelude.seq`
      Prelude.rnf prices `Prelude.seq`
        Prelude.rnf httpStatus
