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
-- Module      : Network.AWS.SNS.ListSubscriptionsByTopic
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the subscriptions to a specific topic. Each call
-- returns a limited list of subscriptions, up to 100. If there are more
-- subscriptions, a @NextToken@ is also returned. Use the @NextToken@
-- parameter in a new @ListSubscriptionsByTopic@ call to get further
-- results.
--
-- This action is throttled at 30 transactions per second (TPS).
--
-- This operation returns paginated results.
module Network.AWS.SNS.ListSubscriptionsByTopic
  ( -- * Creating a Request
    ListSubscriptionsByTopic (..),
    newListSubscriptionsByTopic,

    -- * Request Lenses
    listSubscriptionsByTopic_nextToken,
    listSubscriptionsByTopic_topicArn,

    -- * Destructuring the Response
    ListSubscriptionsByTopicResponse (..),
    newListSubscriptionsByTopicResponse,

    -- * Response Lenses
    listSubscriptionsByTopicResponse_nextToken,
    listSubscriptionsByTopicResponse_subscriptions,
    listSubscriptionsByTopicResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SNS.Types

-- | Input for ListSubscriptionsByTopic action.
--
-- /See:/ 'newListSubscriptionsByTopic' smart constructor.
data ListSubscriptionsByTopic = ListSubscriptionsByTopic'
  { -- | Token returned by the previous @ListSubscriptionsByTopic@ request.
    nextToken :: Core.Maybe Core.Text,
    -- | The ARN of the topic for which you wish to find subscriptions.
    topicArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSubscriptionsByTopic' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSubscriptionsByTopic_nextToken' - Token returned by the previous @ListSubscriptionsByTopic@ request.
--
-- 'topicArn', 'listSubscriptionsByTopic_topicArn' - The ARN of the topic for which you wish to find subscriptions.
newListSubscriptionsByTopic ::
  -- | 'topicArn'
  Core.Text ->
  ListSubscriptionsByTopic
newListSubscriptionsByTopic pTopicArn_ =
  ListSubscriptionsByTopic'
    { nextToken = Core.Nothing,
      topicArn = pTopicArn_
    }

-- | Token returned by the previous @ListSubscriptionsByTopic@ request.
listSubscriptionsByTopic_nextToken :: Lens.Lens' ListSubscriptionsByTopic (Core.Maybe Core.Text)
listSubscriptionsByTopic_nextToken = Lens.lens (\ListSubscriptionsByTopic' {nextToken} -> nextToken) (\s@ListSubscriptionsByTopic' {} a -> s {nextToken = a} :: ListSubscriptionsByTopic)

-- | The ARN of the topic for which you wish to find subscriptions.
listSubscriptionsByTopic_topicArn :: Lens.Lens' ListSubscriptionsByTopic Core.Text
listSubscriptionsByTopic_topicArn = Lens.lens (\ListSubscriptionsByTopic' {topicArn} -> topicArn) (\s@ListSubscriptionsByTopic' {} a -> s {topicArn = a} :: ListSubscriptionsByTopic)

instance Core.AWSPager ListSubscriptionsByTopic where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSubscriptionsByTopicResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listSubscriptionsByTopicResponse_subscriptions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listSubscriptionsByTopic_nextToken
          Lens..~ rs
          Lens.^? listSubscriptionsByTopicResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListSubscriptionsByTopic where
  type
    AWSResponse ListSubscriptionsByTopic =
      ListSubscriptionsByTopicResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListSubscriptionsByTopicResult"
      ( \s h x ->
          ListSubscriptionsByTopicResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> ( x Core..@? "Subscriptions" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListSubscriptionsByTopic

instance Core.NFData ListSubscriptionsByTopic

instance Core.ToHeaders ListSubscriptionsByTopic where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListSubscriptionsByTopic where
  toPath = Core.const "/"

instance Core.ToQuery ListSubscriptionsByTopic where
  toQuery ListSubscriptionsByTopic' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ListSubscriptionsByTopic" :: Core.ByteString),
        "Version" Core.=: ("2010-03-31" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "TopicArn" Core.=: topicArn
      ]

-- | Response for ListSubscriptionsByTopic action.
--
-- /See:/ 'newListSubscriptionsByTopicResponse' smart constructor.
data ListSubscriptionsByTopicResponse = ListSubscriptionsByTopicResponse'
  { -- | Token to pass along to the next @ListSubscriptionsByTopic@ request. This
    -- element is returned if there are more subscriptions to retrieve.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of subscriptions.
    subscriptions :: Core.Maybe [Subscription],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSubscriptionsByTopicResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSubscriptionsByTopicResponse_nextToken' - Token to pass along to the next @ListSubscriptionsByTopic@ request. This
-- element is returned if there are more subscriptions to retrieve.
--
-- 'subscriptions', 'listSubscriptionsByTopicResponse_subscriptions' - A list of subscriptions.
--
-- 'httpStatus', 'listSubscriptionsByTopicResponse_httpStatus' - The response's http status code.
newListSubscriptionsByTopicResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListSubscriptionsByTopicResponse
newListSubscriptionsByTopicResponse pHttpStatus_ =
  ListSubscriptionsByTopicResponse'
    { nextToken =
        Core.Nothing,
      subscriptions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token to pass along to the next @ListSubscriptionsByTopic@ request. This
-- element is returned if there are more subscriptions to retrieve.
listSubscriptionsByTopicResponse_nextToken :: Lens.Lens' ListSubscriptionsByTopicResponse (Core.Maybe Core.Text)
listSubscriptionsByTopicResponse_nextToken = Lens.lens (\ListSubscriptionsByTopicResponse' {nextToken} -> nextToken) (\s@ListSubscriptionsByTopicResponse' {} a -> s {nextToken = a} :: ListSubscriptionsByTopicResponse)

-- | A list of subscriptions.
listSubscriptionsByTopicResponse_subscriptions :: Lens.Lens' ListSubscriptionsByTopicResponse (Core.Maybe [Subscription])
listSubscriptionsByTopicResponse_subscriptions = Lens.lens (\ListSubscriptionsByTopicResponse' {subscriptions} -> subscriptions) (\s@ListSubscriptionsByTopicResponse' {} a -> s {subscriptions = a} :: ListSubscriptionsByTopicResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listSubscriptionsByTopicResponse_httpStatus :: Lens.Lens' ListSubscriptionsByTopicResponse Core.Int
listSubscriptionsByTopicResponse_httpStatus = Lens.lens (\ListSubscriptionsByTopicResponse' {httpStatus} -> httpStatus) (\s@ListSubscriptionsByTopicResponse' {} a -> s {httpStatus = a} :: ListSubscriptionsByTopicResponse)

instance Core.NFData ListSubscriptionsByTopicResponse
