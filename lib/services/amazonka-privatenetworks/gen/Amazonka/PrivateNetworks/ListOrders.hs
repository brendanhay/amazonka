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
-- Module      : Amazonka.PrivateNetworks.ListOrders
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists orders. Add filters to your request to return a more specific list
-- of results. Use filters to match the Amazon Resource Name (ARN) of the
-- network site or the status of the order.
--
-- If you specify multiple filters, filters are joined with an OR, and the
-- request returns results that match all of the specified filters.
--
-- This operation returns paginated results.
module Amazonka.PrivateNetworks.ListOrders
  ( -- * Creating a Request
    ListOrders (..),
    newListOrders,

    -- * Request Lenses
    listOrders_filters,
    listOrders_maxResults,
    listOrders_startToken,
    listOrders_networkArn,

    -- * Destructuring the Response
    ListOrdersResponse (..),
    newListOrdersResponse,

    -- * Response Lenses
    listOrdersResponse_nextToken,
    listOrdersResponse_orders,
    listOrdersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListOrders' smart constructor.
data ListOrders = ListOrders'
  { -- | The filters.
    --
    -- -   @NETWORK_SITE@ - The Amazon Resource Name (ARN) of the network site.
    --
    -- -   @STATUS@ - The status (@ACKNOWLEDGING@ | @ACKNOWLEDGED@ |
    --     @UNACKNOWLEDGED@).
    --
    -- Filter values are case sensitive. If you specify multiple values for a
    -- filter, the values are joined with an @OR@, and the request returns all
    -- results that match any of the specified values.
    filters :: Prelude.Maybe (Prelude.HashMap OrderFilterKeys [Prelude.Text]),
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    startToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the network.
    networkArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOrders' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listOrders_filters' - The filters.
--
-- -   @NETWORK_SITE@ - The Amazon Resource Name (ARN) of the network site.
--
-- -   @STATUS@ - The status (@ACKNOWLEDGING@ | @ACKNOWLEDGED@ |
--     @UNACKNOWLEDGED@).
--
-- Filter values are case sensitive. If you specify multiple values for a
-- filter, the values are joined with an @OR@, and the request returns all
-- results that match any of the specified values.
--
-- 'maxResults', 'listOrders_maxResults' - The maximum number of results to return.
--
-- 'startToken', 'listOrders_startToken' - The token for the next page of results.
--
-- 'networkArn', 'listOrders_networkArn' - The Amazon Resource Name (ARN) of the network.
newListOrders ::
  -- | 'networkArn'
  Prelude.Text ->
  ListOrders
newListOrders pNetworkArn_ =
  ListOrders'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      startToken = Prelude.Nothing,
      networkArn = pNetworkArn_
    }

-- | The filters.
--
-- -   @NETWORK_SITE@ - The Amazon Resource Name (ARN) of the network site.
--
-- -   @STATUS@ - The status (@ACKNOWLEDGING@ | @ACKNOWLEDGED@ |
--     @UNACKNOWLEDGED@).
--
-- Filter values are case sensitive. If you specify multiple values for a
-- filter, the values are joined with an @OR@, and the request returns all
-- results that match any of the specified values.
listOrders_filters :: Lens.Lens' ListOrders (Prelude.Maybe (Prelude.HashMap OrderFilterKeys [Prelude.Text]))
listOrders_filters = Lens.lens (\ListOrders' {filters} -> filters) (\s@ListOrders' {} a -> s {filters = a} :: ListOrders) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return.
listOrders_maxResults :: Lens.Lens' ListOrders (Prelude.Maybe Prelude.Natural)
listOrders_maxResults = Lens.lens (\ListOrders' {maxResults} -> maxResults) (\s@ListOrders' {} a -> s {maxResults = a} :: ListOrders)

-- | The token for the next page of results.
listOrders_startToken :: Lens.Lens' ListOrders (Prelude.Maybe Prelude.Text)
listOrders_startToken = Lens.lens (\ListOrders' {startToken} -> startToken) (\s@ListOrders' {} a -> s {startToken = a} :: ListOrders)

-- | The Amazon Resource Name (ARN) of the network.
listOrders_networkArn :: Lens.Lens' ListOrders Prelude.Text
listOrders_networkArn = Lens.lens (\ListOrders' {networkArn} -> networkArn) (\s@ListOrders' {} a -> s {networkArn = a} :: ListOrders)

instance Core.AWSPager ListOrders where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOrdersResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listOrdersResponse_orders Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listOrders_startToken
          Lens..~ rs
          Lens.^? listOrdersResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListOrders where
  type AWSResponse ListOrders = ListOrdersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOrdersResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "orders" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOrders where
  hashWithSalt _salt ListOrders' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` startToken
      `Prelude.hashWithSalt` networkArn

instance Prelude.NFData ListOrders where
  rnf ListOrders' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf startToken
      `Prelude.seq` Prelude.rnf networkArn

instance Core.ToHeaders ListOrders where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListOrders where
  toJSON ListOrders' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("filters" Core..=) Prelude.<$> filters,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("startToken" Core..=) Prelude.<$> startToken,
            Prelude.Just ("networkArn" Core..= networkArn)
          ]
      )

instance Core.ToPath ListOrders where
  toPath = Prelude.const "/v1/orders/list"

instance Core.ToQuery ListOrders where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListOrdersResponse' smart constructor.
data ListOrdersResponse = ListOrdersResponse'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the orders.
    orders :: Prelude.Maybe [Order],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOrdersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOrdersResponse_nextToken' - The token for the next page of results.
--
-- 'orders', 'listOrdersResponse_orders' - Information about the orders.
--
-- 'httpStatus', 'listOrdersResponse_httpStatus' - The response's http status code.
newListOrdersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListOrdersResponse
newListOrdersResponse pHttpStatus_ =
  ListOrdersResponse'
    { nextToken = Prelude.Nothing,
      orders = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next page of results.
listOrdersResponse_nextToken :: Lens.Lens' ListOrdersResponse (Prelude.Maybe Prelude.Text)
listOrdersResponse_nextToken = Lens.lens (\ListOrdersResponse' {nextToken} -> nextToken) (\s@ListOrdersResponse' {} a -> s {nextToken = a} :: ListOrdersResponse)

-- | Information about the orders.
listOrdersResponse_orders :: Lens.Lens' ListOrdersResponse (Prelude.Maybe [Order])
listOrdersResponse_orders = Lens.lens (\ListOrdersResponse' {orders} -> orders) (\s@ListOrdersResponse' {} a -> s {orders = a} :: ListOrdersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listOrdersResponse_httpStatus :: Lens.Lens' ListOrdersResponse Prelude.Int
listOrdersResponse_httpStatus = Lens.lens (\ListOrdersResponse' {httpStatus} -> httpStatus) (\s@ListOrdersResponse' {} a -> s {httpStatus = a} :: ListOrdersResponse)

instance Prelude.NFData ListOrdersResponse where
  rnf ListOrdersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf orders
      `Prelude.seq` Prelude.rnf httpStatus
