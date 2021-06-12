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
-- Module      : Network.AWS.Inspector.ListEventSubscriptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the event subscriptions for the assessment template that is
-- specified by the ARN of the assessment template. For more information,
-- see SubscribeToEvent and UnsubscribeFromEvent.
--
-- This operation returns paginated results.
module Network.AWS.Inspector.ListEventSubscriptions
  ( -- * Creating a Request
    ListEventSubscriptions (..),
    newListEventSubscriptions,

    -- * Request Lenses
    listEventSubscriptions_resourceArn,
    listEventSubscriptions_nextToken,
    listEventSubscriptions_maxResults,

    -- * Destructuring the Response
    ListEventSubscriptionsResponse (..),
    newListEventSubscriptionsResponse,

    -- * Response Lenses
    listEventSubscriptionsResponse_nextToken,
    listEventSubscriptionsResponse_httpStatus,
    listEventSubscriptionsResponse_subscriptions,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListEventSubscriptions' smart constructor.
data ListEventSubscriptions = ListEventSubscriptions'
  { -- | The ARN of the assessment template for which you want to list the
    -- existing event subscriptions.
    resourceArn :: Core.Maybe Core.Text,
    -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the
    -- __ListEventSubscriptions__ action. Subsequent calls to the action fill
    -- __nextToken__ in the request with the value of __NextToken__ from the
    -- previous response to continue listing data.
    nextToken :: Core.Maybe Core.Text,
    -- | You can use this parameter to indicate the maximum number of items you
    -- want in the response. The default value is 10. The maximum value is 500.
    maxResults :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListEventSubscriptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'listEventSubscriptions_resourceArn' - The ARN of the assessment template for which you want to list the
-- existing event subscriptions.
--
-- 'nextToken', 'listEventSubscriptions_nextToken' - You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the
-- __ListEventSubscriptions__ action. Subsequent calls to the action fill
-- __nextToken__ in the request with the value of __NextToken__ from the
-- previous response to continue listing data.
--
-- 'maxResults', 'listEventSubscriptions_maxResults' - You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
newListEventSubscriptions ::
  ListEventSubscriptions
newListEventSubscriptions =
  ListEventSubscriptions'
    { resourceArn = Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The ARN of the assessment template for which you want to list the
-- existing event subscriptions.
listEventSubscriptions_resourceArn :: Lens.Lens' ListEventSubscriptions (Core.Maybe Core.Text)
listEventSubscriptions_resourceArn = Lens.lens (\ListEventSubscriptions' {resourceArn} -> resourceArn) (\s@ListEventSubscriptions' {} a -> s {resourceArn = a} :: ListEventSubscriptions)

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the
-- __ListEventSubscriptions__ action. Subsequent calls to the action fill
-- __nextToken__ in the request with the value of __NextToken__ from the
-- previous response to continue listing data.
listEventSubscriptions_nextToken :: Lens.Lens' ListEventSubscriptions (Core.Maybe Core.Text)
listEventSubscriptions_nextToken = Lens.lens (\ListEventSubscriptions' {nextToken} -> nextToken) (\s@ListEventSubscriptions' {} a -> s {nextToken = a} :: ListEventSubscriptions)

-- | You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
listEventSubscriptions_maxResults :: Lens.Lens' ListEventSubscriptions (Core.Maybe Core.Int)
listEventSubscriptions_maxResults = Lens.lens (\ListEventSubscriptions' {maxResults} -> maxResults) (\s@ListEventSubscriptions' {} a -> s {maxResults = a} :: ListEventSubscriptions)

instance Core.AWSPager ListEventSubscriptions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEventSubscriptionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. listEventSubscriptionsResponse_subscriptions
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listEventSubscriptions_nextToken
          Lens..~ rs
          Lens.^? listEventSubscriptionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListEventSubscriptions where
  type
    AWSResponse ListEventSubscriptions =
      ListEventSubscriptionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEventSubscriptionsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "subscriptions" Core..!@ Core.mempty)
      )

instance Core.Hashable ListEventSubscriptions

instance Core.NFData ListEventSubscriptions

instance Core.ToHeaders ListEventSubscriptions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.ListEventSubscriptions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListEventSubscriptions where
  toJSON ListEventSubscriptions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("resourceArn" Core..=) Core.<$> resourceArn,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListEventSubscriptions where
  toPath = Core.const "/"

instance Core.ToQuery ListEventSubscriptions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListEventSubscriptionsResponse' smart constructor.
data ListEventSubscriptionsResponse = ListEventSubscriptionsResponse'
  { -- | When a response is generated, if there is more data to be listed, this
    -- parameter is present in the response and contains the value to use for
    -- the __nextToken__ parameter in a subsequent pagination request. If there
    -- is no more data to be listed, this parameter is set to null.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Details of the returned event subscriptions.
    subscriptions :: [Subscription]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListEventSubscriptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEventSubscriptionsResponse_nextToken' - When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to null.
--
-- 'httpStatus', 'listEventSubscriptionsResponse_httpStatus' - The response's http status code.
--
-- 'subscriptions', 'listEventSubscriptionsResponse_subscriptions' - Details of the returned event subscriptions.
newListEventSubscriptionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListEventSubscriptionsResponse
newListEventSubscriptionsResponse pHttpStatus_ =
  ListEventSubscriptionsResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      subscriptions = Core.mempty
    }

-- | When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to null.
listEventSubscriptionsResponse_nextToken :: Lens.Lens' ListEventSubscriptionsResponse (Core.Maybe Core.Text)
listEventSubscriptionsResponse_nextToken = Lens.lens (\ListEventSubscriptionsResponse' {nextToken} -> nextToken) (\s@ListEventSubscriptionsResponse' {} a -> s {nextToken = a} :: ListEventSubscriptionsResponse)

-- | The response's http status code.
listEventSubscriptionsResponse_httpStatus :: Lens.Lens' ListEventSubscriptionsResponse Core.Int
listEventSubscriptionsResponse_httpStatus = Lens.lens (\ListEventSubscriptionsResponse' {httpStatus} -> httpStatus) (\s@ListEventSubscriptionsResponse' {} a -> s {httpStatus = a} :: ListEventSubscriptionsResponse)

-- | Details of the returned event subscriptions.
listEventSubscriptionsResponse_subscriptions :: Lens.Lens' ListEventSubscriptionsResponse [Subscription]
listEventSubscriptionsResponse_subscriptions = Lens.lens (\ListEventSubscriptionsResponse' {subscriptions} -> subscriptions) (\s@ListEventSubscriptionsResponse' {} a -> s {subscriptions = a} :: ListEventSubscriptionsResponse) Core.. Lens._Coerce

instance Core.NFData ListEventSubscriptionsResponse
