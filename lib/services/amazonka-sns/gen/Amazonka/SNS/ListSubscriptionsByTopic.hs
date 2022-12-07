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
-- Module      : Amazonka.SNS.ListSubscriptionsByTopic
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.SNS.ListSubscriptionsByTopic
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | Input for ListSubscriptionsByTopic action.
--
-- /See:/ 'newListSubscriptionsByTopic' smart constructor.
data ListSubscriptionsByTopic = ListSubscriptionsByTopic'
  { -- | Token returned by the previous @ListSubscriptionsByTopic@ request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the topic for which you wish to find subscriptions.
    topicArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ListSubscriptionsByTopic
newListSubscriptionsByTopic pTopicArn_ =
  ListSubscriptionsByTopic'
    { nextToken =
        Prelude.Nothing,
      topicArn = pTopicArn_
    }

-- | Token returned by the previous @ListSubscriptionsByTopic@ request.
listSubscriptionsByTopic_nextToken :: Lens.Lens' ListSubscriptionsByTopic (Prelude.Maybe Prelude.Text)
listSubscriptionsByTopic_nextToken = Lens.lens (\ListSubscriptionsByTopic' {nextToken} -> nextToken) (\s@ListSubscriptionsByTopic' {} a -> s {nextToken = a} :: ListSubscriptionsByTopic)

-- | The ARN of the topic for which you wish to find subscriptions.
listSubscriptionsByTopic_topicArn :: Lens.Lens' ListSubscriptionsByTopic Prelude.Text
listSubscriptionsByTopic_topicArn = Lens.lens (\ListSubscriptionsByTopic' {topicArn} -> topicArn) (\s@ListSubscriptionsByTopic' {} a -> s {topicArn = a} :: ListSubscriptionsByTopic)

instance Core.AWSPager ListSubscriptionsByTopic where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSubscriptionsByTopicResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSubscriptionsByTopicResponse_subscriptions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSubscriptionsByTopic_nextToken
          Lens..~ rs
          Lens.^? listSubscriptionsByTopicResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListSubscriptionsByTopic where
  type
    AWSResponse ListSubscriptionsByTopic =
      ListSubscriptionsByTopicResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListSubscriptionsByTopicResult"
      ( \s h x ->
          ListSubscriptionsByTopicResponse'
            Prelude.<$> (x Data..@? "NextToken")
            Prelude.<*> ( x Data..@? "Subscriptions" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSubscriptionsByTopic where
  hashWithSalt _salt ListSubscriptionsByTopic' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` topicArn

instance Prelude.NFData ListSubscriptionsByTopic where
  rnf ListSubscriptionsByTopic' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf topicArn

instance Data.ToHeaders ListSubscriptionsByTopic where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListSubscriptionsByTopic where
  toPath = Prelude.const "/"

instance Data.ToQuery ListSubscriptionsByTopic where
  toQuery ListSubscriptionsByTopic' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListSubscriptionsByTopic" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-03-31" :: Prelude.ByteString),
        "NextToken" Data.=: nextToken,
        "TopicArn" Data.=: topicArn
      ]

-- | Response for ListSubscriptionsByTopic action.
--
-- /See:/ 'newListSubscriptionsByTopicResponse' smart constructor.
data ListSubscriptionsByTopicResponse = ListSubscriptionsByTopicResponse'
  { -- | Token to pass along to the next @ListSubscriptionsByTopic@ request. This
    -- element is returned if there are more subscriptions to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of subscriptions.
    subscriptions :: Prelude.Maybe [Subscription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListSubscriptionsByTopicResponse
newListSubscriptionsByTopicResponse pHttpStatus_ =
  ListSubscriptionsByTopicResponse'
    { nextToken =
        Prelude.Nothing,
      subscriptions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token to pass along to the next @ListSubscriptionsByTopic@ request. This
-- element is returned if there are more subscriptions to retrieve.
listSubscriptionsByTopicResponse_nextToken :: Lens.Lens' ListSubscriptionsByTopicResponse (Prelude.Maybe Prelude.Text)
listSubscriptionsByTopicResponse_nextToken = Lens.lens (\ListSubscriptionsByTopicResponse' {nextToken} -> nextToken) (\s@ListSubscriptionsByTopicResponse' {} a -> s {nextToken = a} :: ListSubscriptionsByTopicResponse)

-- | A list of subscriptions.
listSubscriptionsByTopicResponse_subscriptions :: Lens.Lens' ListSubscriptionsByTopicResponse (Prelude.Maybe [Subscription])
listSubscriptionsByTopicResponse_subscriptions = Lens.lens (\ListSubscriptionsByTopicResponse' {subscriptions} -> subscriptions) (\s@ListSubscriptionsByTopicResponse' {} a -> s {subscriptions = a} :: ListSubscriptionsByTopicResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSubscriptionsByTopicResponse_httpStatus :: Lens.Lens' ListSubscriptionsByTopicResponse Prelude.Int
listSubscriptionsByTopicResponse_httpStatus = Lens.lens (\ListSubscriptionsByTopicResponse' {httpStatus} -> httpStatus) (\s@ListSubscriptionsByTopicResponse' {} a -> s {httpStatus = a} :: ListSubscriptionsByTopicResponse)

instance
  Prelude.NFData
    ListSubscriptionsByTopicResponse
  where
  rnf ListSubscriptionsByTopicResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf subscriptions
      `Prelude.seq` Prelude.rnf httpStatus
