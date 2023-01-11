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
-- Module      : Amazonka.SNS.ListSubscriptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the requester\'s subscriptions. Each call returns a
-- limited list of subscriptions, up to 100. If there are more
-- subscriptions, a @NextToken@ is also returned. Use the @NextToken@
-- parameter in a new @ListSubscriptions@ call to get further results.
--
-- This action is throttled at 30 transactions per second (TPS).
--
-- This operation returns paginated results.
module Amazonka.SNS.ListSubscriptions
  ( -- * Creating a Request
    ListSubscriptions (..),
    newListSubscriptions,

    -- * Request Lenses
    listSubscriptions_nextToken,

    -- * Destructuring the Response
    ListSubscriptionsResponse (..),
    newListSubscriptionsResponse,

    -- * Response Lenses
    listSubscriptionsResponse_nextToken,
    listSubscriptionsResponse_subscriptions,
    listSubscriptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | Input for ListSubscriptions action.
--
-- /See:/ 'newListSubscriptions' smart constructor.
data ListSubscriptions = ListSubscriptions'
  { -- | Token returned by the previous @ListSubscriptions@ request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSubscriptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSubscriptions_nextToken' - Token returned by the previous @ListSubscriptions@ request.
newListSubscriptions ::
  ListSubscriptions
newListSubscriptions =
  ListSubscriptions' {nextToken = Prelude.Nothing}

-- | Token returned by the previous @ListSubscriptions@ request.
listSubscriptions_nextToken :: Lens.Lens' ListSubscriptions (Prelude.Maybe Prelude.Text)
listSubscriptions_nextToken = Lens.lens (\ListSubscriptions' {nextToken} -> nextToken) (\s@ListSubscriptions' {} a -> s {nextToken = a} :: ListSubscriptions)

instance Core.AWSPager ListSubscriptions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSubscriptionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSubscriptionsResponse_subscriptions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSubscriptions_nextToken
          Lens..~ rs
          Lens.^? listSubscriptionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListSubscriptions where
  type
    AWSResponse ListSubscriptions =
      ListSubscriptionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListSubscriptionsResult"
      ( \s h x ->
          ListSubscriptionsResponse'
            Prelude.<$> (x Data..@? "NextToken")
            Prelude.<*> ( x Data..@? "Subscriptions" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSubscriptions where
  hashWithSalt _salt ListSubscriptions' {..} =
    _salt `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListSubscriptions where
  rnf ListSubscriptions' {..} = Prelude.rnf nextToken

instance Data.ToHeaders ListSubscriptions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListSubscriptions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListSubscriptions where
  toQuery ListSubscriptions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListSubscriptions" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-03-31" :: Prelude.ByteString),
        "NextToken" Data.=: nextToken
      ]

-- | Response for ListSubscriptions action
--
-- /See:/ 'newListSubscriptionsResponse' smart constructor.
data ListSubscriptionsResponse = ListSubscriptionsResponse'
  { -- | Token to pass along to the next @ListSubscriptions@ request. This
    -- element is returned if there are more subscriptions to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of subscriptions.
    subscriptions :: Prelude.Maybe [Subscription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSubscriptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSubscriptionsResponse_nextToken' - Token to pass along to the next @ListSubscriptions@ request. This
-- element is returned if there are more subscriptions to retrieve.
--
-- 'subscriptions', 'listSubscriptionsResponse_subscriptions' - A list of subscriptions.
--
-- 'httpStatus', 'listSubscriptionsResponse_httpStatus' - The response's http status code.
newListSubscriptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSubscriptionsResponse
newListSubscriptionsResponse pHttpStatus_ =
  ListSubscriptionsResponse'
    { nextToken =
        Prelude.Nothing,
      subscriptions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token to pass along to the next @ListSubscriptions@ request. This
-- element is returned if there are more subscriptions to retrieve.
listSubscriptionsResponse_nextToken :: Lens.Lens' ListSubscriptionsResponse (Prelude.Maybe Prelude.Text)
listSubscriptionsResponse_nextToken = Lens.lens (\ListSubscriptionsResponse' {nextToken} -> nextToken) (\s@ListSubscriptionsResponse' {} a -> s {nextToken = a} :: ListSubscriptionsResponse)

-- | A list of subscriptions.
listSubscriptionsResponse_subscriptions :: Lens.Lens' ListSubscriptionsResponse (Prelude.Maybe [Subscription])
listSubscriptionsResponse_subscriptions = Lens.lens (\ListSubscriptionsResponse' {subscriptions} -> subscriptions) (\s@ListSubscriptionsResponse' {} a -> s {subscriptions = a} :: ListSubscriptionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSubscriptionsResponse_httpStatus :: Lens.Lens' ListSubscriptionsResponse Prelude.Int
listSubscriptionsResponse_httpStatus = Lens.lens (\ListSubscriptionsResponse' {httpStatus} -> httpStatus) (\s@ListSubscriptionsResponse' {} a -> s {httpStatus = a} :: ListSubscriptionsResponse)

instance Prelude.NFData ListSubscriptionsResponse where
  rnf ListSubscriptionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf subscriptions
      `Prelude.seq` Prelude.rnf httpStatus
