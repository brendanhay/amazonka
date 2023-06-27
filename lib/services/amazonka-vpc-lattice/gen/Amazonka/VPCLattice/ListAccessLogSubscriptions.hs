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
-- Module      : Amazonka.VPCLattice.ListAccessLogSubscriptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all access log subscriptions for the specified service network or
-- service.
--
-- This operation returns paginated results.
module Amazonka.VPCLattice.ListAccessLogSubscriptions
  ( -- * Creating a Request
    ListAccessLogSubscriptions (..),
    newListAccessLogSubscriptions,

    -- * Request Lenses
    listAccessLogSubscriptions_maxResults,
    listAccessLogSubscriptions_nextToken,
    listAccessLogSubscriptions_resourceIdentifier,

    -- * Destructuring the Response
    ListAccessLogSubscriptionsResponse (..),
    newListAccessLogSubscriptionsResponse,

    -- * Response Lenses
    listAccessLogSubscriptionsResponse_nextToken,
    listAccessLogSubscriptionsResponse_httpStatus,
    listAccessLogSubscriptionsResponse_items,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newListAccessLogSubscriptions' smart constructor.
data ListAccessLogSubscriptions = ListAccessLogSubscriptions'
  { -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID or Amazon Resource Name (ARN) of the service network or service.
    resourceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccessLogSubscriptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAccessLogSubscriptions_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'listAccessLogSubscriptions_nextToken' - A pagination token for the next page of results.
--
-- 'resourceIdentifier', 'listAccessLogSubscriptions_resourceIdentifier' - The ID or Amazon Resource Name (ARN) of the service network or service.
newListAccessLogSubscriptions ::
  -- | 'resourceIdentifier'
  Prelude.Text ->
  ListAccessLogSubscriptions
newListAccessLogSubscriptions pResourceIdentifier_ =
  ListAccessLogSubscriptions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceIdentifier = pResourceIdentifier_
    }

-- | The maximum number of results to return.
listAccessLogSubscriptions_maxResults :: Lens.Lens' ListAccessLogSubscriptions (Prelude.Maybe Prelude.Natural)
listAccessLogSubscriptions_maxResults = Lens.lens (\ListAccessLogSubscriptions' {maxResults} -> maxResults) (\s@ListAccessLogSubscriptions' {} a -> s {maxResults = a} :: ListAccessLogSubscriptions)

-- | A pagination token for the next page of results.
listAccessLogSubscriptions_nextToken :: Lens.Lens' ListAccessLogSubscriptions (Prelude.Maybe Prelude.Text)
listAccessLogSubscriptions_nextToken = Lens.lens (\ListAccessLogSubscriptions' {nextToken} -> nextToken) (\s@ListAccessLogSubscriptions' {} a -> s {nextToken = a} :: ListAccessLogSubscriptions)

-- | The ID or Amazon Resource Name (ARN) of the service network or service.
listAccessLogSubscriptions_resourceIdentifier :: Lens.Lens' ListAccessLogSubscriptions Prelude.Text
listAccessLogSubscriptions_resourceIdentifier = Lens.lens (\ListAccessLogSubscriptions' {resourceIdentifier} -> resourceIdentifier) (\s@ListAccessLogSubscriptions' {} a -> s {resourceIdentifier = a} :: ListAccessLogSubscriptions)

instance Core.AWSPager ListAccessLogSubscriptions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAccessLogSubscriptionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listAccessLogSubscriptionsResponse_items) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listAccessLogSubscriptions_nextToken
          Lens..~ rs
          Lens.^? listAccessLogSubscriptionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListAccessLogSubscriptions where
  type
    AWSResponse ListAccessLogSubscriptions =
      ListAccessLogSubscriptionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAccessLogSubscriptionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "items" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListAccessLogSubscriptions where
  hashWithSalt _salt ListAccessLogSubscriptions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceIdentifier

instance Prelude.NFData ListAccessLogSubscriptions where
  rnf ListAccessLogSubscriptions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceIdentifier

instance Data.ToHeaders ListAccessLogSubscriptions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListAccessLogSubscriptions where
  toPath = Prelude.const "/accesslogsubscriptions"

instance Data.ToQuery ListAccessLogSubscriptions where
  toQuery ListAccessLogSubscriptions' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "resourceIdentifier" Data.=: resourceIdentifier
      ]

-- | /See:/ 'newListAccessLogSubscriptionsResponse' smart constructor.
data ListAccessLogSubscriptionsResponse = ListAccessLogSubscriptionsResponse'
  { -- | A pagination token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The access log subscriptions.
    items :: [AccessLogSubscriptionSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccessLogSubscriptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAccessLogSubscriptionsResponse_nextToken' - A pagination token for the next page of results.
--
-- 'httpStatus', 'listAccessLogSubscriptionsResponse_httpStatus' - The response's http status code.
--
-- 'items', 'listAccessLogSubscriptionsResponse_items' - The access log subscriptions.
newListAccessLogSubscriptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAccessLogSubscriptionsResponse
newListAccessLogSubscriptionsResponse pHttpStatus_ =
  ListAccessLogSubscriptionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      items = Prelude.mempty
    }

-- | A pagination token for the next page of results.
listAccessLogSubscriptionsResponse_nextToken :: Lens.Lens' ListAccessLogSubscriptionsResponse (Prelude.Maybe Prelude.Text)
listAccessLogSubscriptionsResponse_nextToken = Lens.lens (\ListAccessLogSubscriptionsResponse' {nextToken} -> nextToken) (\s@ListAccessLogSubscriptionsResponse' {} a -> s {nextToken = a} :: ListAccessLogSubscriptionsResponse)

-- | The response's http status code.
listAccessLogSubscriptionsResponse_httpStatus :: Lens.Lens' ListAccessLogSubscriptionsResponse Prelude.Int
listAccessLogSubscriptionsResponse_httpStatus = Lens.lens (\ListAccessLogSubscriptionsResponse' {httpStatus} -> httpStatus) (\s@ListAccessLogSubscriptionsResponse' {} a -> s {httpStatus = a} :: ListAccessLogSubscriptionsResponse)

-- | The access log subscriptions.
listAccessLogSubscriptionsResponse_items :: Lens.Lens' ListAccessLogSubscriptionsResponse [AccessLogSubscriptionSummary]
listAccessLogSubscriptionsResponse_items = Lens.lens (\ListAccessLogSubscriptionsResponse' {items} -> items) (\s@ListAccessLogSubscriptionsResponse' {} a -> s {items = a} :: ListAccessLogSubscriptionsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListAccessLogSubscriptionsResponse
  where
  rnf ListAccessLogSubscriptionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf items
