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
-- Module      : Network.AWS.SNS.ListSubscriptions
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.SNS.ListSubscriptions
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SNS.Types

-- | Input for ListSubscriptions action.
--
-- /See:/ 'newListSubscriptions' smart constructor.
data ListSubscriptions = ListSubscriptions'
  { -- | Token returned by the previous @ListSubscriptions@ request.
    nextToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  ListSubscriptions' {nextToken = Core.Nothing}

-- | Token returned by the previous @ListSubscriptions@ request.
listSubscriptions_nextToken :: Lens.Lens' ListSubscriptions (Core.Maybe Core.Text)
listSubscriptions_nextToken = Lens.lens (\ListSubscriptions' {nextToken} -> nextToken) (\s@ListSubscriptions' {} a -> s {nextToken = a} :: ListSubscriptions)

instance Core.AWSPager ListSubscriptions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSubscriptionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listSubscriptionsResponse_subscriptions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listSubscriptions_nextToken
          Lens..~ rs
          Lens.^? listSubscriptionsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListSubscriptions where
  type
    AWSResponse ListSubscriptions =
      ListSubscriptionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListSubscriptionsResult"
      ( \s h x ->
          ListSubscriptionsResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> ( x Core..@? "Subscriptions" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListSubscriptions

instance Core.NFData ListSubscriptions

instance Core.ToHeaders ListSubscriptions where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListSubscriptions where
  toPath = Core.const "/"

instance Core.ToQuery ListSubscriptions where
  toQuery ListSubscriptions' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ListSubscriptions" :: Core.ByteString),
        "Version" Core.=: ("2010-03-31" :: Core.ByteString),
        "NextToken" Core.=: nextToken
      ]

-- | Response for ListSubscriptions action
--
-- /See:/ 'newListSubscriptionsResponse' smart constructor.
data ListSubscriptionsResponse = ListSubscriptionsResponse'
  { -- | Token to pass along to the next @ListSubscriptions@ request. This
    -- element is returned if there are more subscriptions to retrieve.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of subscriptions.
    subscriptions :: Core.Maybe [Subscription],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListSubscriptionsResponse
newListSubscriptionsResponse pHttpStatus_ =
  ListSubscriptionsResponse'
    { nextToken =
        Core.Nothing,
      subscriptions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token to pass along to the next @ListSubscriptions@ request. This
-- element is returned if there are more subscriptions to retrieve.
listSubscriptionsResponse_nextToken :: Lens.Lens' ListSubscriptionsResponse (Core.Maybe Core.Text)
listSubscriptionsResponse_nextToken = Lens.lens (\ListSubscriptionsResponse' {nextToken} -> nextToken) (\s@ListSubscriptionsResponse' {} a -> s {nextToken = a} :: ListSubscriptionsResponse)

-- | A list of subscriptions.
listSubscriptionsResponse_subscriptions :: Lens.Lens' ListSubscriptionsResponse (Core.Maybe [Subscription])
listSubscriptionsResponse_subscriptions = Lens.lens (\ListSubscriptionsResponse' {subscriptions} -> subscriptions) (\s@ListSubscriptionsResponse' {} a -> s {subscriptions = a} :: ListSubscriptionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listSubscriptionsResponse_httpStatus :: Lens.Lens' ListSubscriptionsResponse Core.Int
listSubscriptionsResponse_httpStatus = Lens.lens (\ListSubscriptionsResponse' {httpStatus} -> httpStatus) (\s@ListSubscriptionsResponse' {} a -> s {httpStatus = a} :: ListSubscriptionsResponse)

instance Core.NFData ListSubscriptionsResponse
