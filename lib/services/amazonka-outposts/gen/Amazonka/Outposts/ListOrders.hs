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
-- Module      : Amazonka.Outposts.ListOrders
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Outpost orders for your Amazon Web Services account.
module Amazonka.Outposts.ListOrders
  ( -- * Creating a Request
    ListOrders (..),
    newListOrders,

    -- * Request Lenses
    listOrders_maxResults,
    listOrders_nextToken,
    listOrders_outpostIdentifierFilter,

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
import qualified Amazonka.Data as Data
import Amazonka.Outposts.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListOrders' smart constructor.
data ListOrders = ListOrders'
  { maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID or the Amazon Resource Name (ARN) of the Outpost.
    outpostIdentifierFilter :: Prelude.Maybe Prelude.Text
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
-- 'maxResults', 'listOrders_maxResults' - Undocumented member.
--
-- 'nextToken', 'listOrders_nextToken' - Undocumented member.
--
-- 'outpostIdentifierFilter', 'listOrders_outpostIdentifierFilter' - The ID or the Amazon Resource Name (ARN) of the Outpost.
newListOrders ::
  ListOrders
newListOrders =
  ListOrders'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      outpostIdentifierFilter = Prelude.Nothing
    }

-- | Undocumented member.
listOrders_maxResults :: Lens.Lens' ListOrders (Prelude.Maybe Prelude.Natural)
listOrders_maxResults = Lens.lens (\ListOrders' {maxResults} -> maxResults) (\s@ListOrders' {} a -> s {maxResults = a} :: ListOrders)

-- | Undocumented member.
listOrders_nextToken :: Lens.Lens' ListOrders (Prelude.Maybe Prelude.Text)
listOrders_nextToken = Lens.lens (\ListOrders' {nextToken} -> nextToken) (\s@ListOrders' {} a -> s {nextToken = a} :: ListOrders)

-- | The ID or the Amazon Resource Name (ARN) of the Outpost.
listOrders_outpostIdentifierFilter :: Lens.Lens' ListOrders (Prelude.Maybe Prelude.Text)
listOrders_outpostIdentifierFilter = Lens.lens (\ListOrders' {outpostIdentifierFilter} -> outpostIdentifierFilter) (\s@ListOrders' {} a -> s {outpostIdentifierFilter = a} :: ListOrders)

instance Core.AWSRequest ListOrders where
  type AWSResponse ListOrders = ListOrdersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOrdersResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Orders" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOrders where
  hashWithSalt _salt ListOrders' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` outpostIdentifierFilter

instance Prelude.NFData ListOrders where
  rnf ListOrders' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf outpostIdentifierFilter

instance Data.ToHeaders ListOrders where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListOrders where
  toPath = Prelude.const "/list-orders"

instance Data.ToQuery ListOrders where
  toQuery ListOrders' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "OutpostIdentifierFilter"
          Data.=: outpostIdentifierFilter
      ]

-- | /See:/ 'newListOrdersResponse' smart constructor.
data ListOrdersResponse = ListOrdersResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the orders.
    orders :: Prelude.Maybe [OrderSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOrdersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOrdersResponse_nextToken' - Undocumented member.
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

-- | Undocumented member.
listOrdersResponse_nextToken :: Lens.Lens' ListOrdersResponse (Prelude.Maybe Prelude.Text)
listOrdersResponse_nextToken = Lens.lens (\ListOrdersResponse' {nextToken} -> nextToken) (\s@ListOrdersResponse' {} a -> s {nextToken = a} :: ListOrdersResponse)

-- | Information about the orders.
listOrdersResponse_orders :: Lens.Lens' ListOrdersResponse (Prelude.Maybe [OrderSummary])
listOrdersResponse_orders = Lens.lens (\ListOrdersResponse' {orders} -> orders) (\s@ListOrdersResponse' {} a -> s {orders = a} :: ListOrdersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listOrdersResponse_httpStatus :: Lens.Lens' ListOrdersResponse Prelude.Int
listOrdersResponse_httpStatus = Lens.lens (\ListOrdersResponse' {httpStatus} -> httpStatus) (\s@ListOrdersResponse' {} a -> s {httpStatus = a} :: ListOrdersResponse)

instance Prelude.NFData ListOrdersResponse where
  rnf ListOrdersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf orders
      `Prelude.seq` Prelude.rnf httpStatus
