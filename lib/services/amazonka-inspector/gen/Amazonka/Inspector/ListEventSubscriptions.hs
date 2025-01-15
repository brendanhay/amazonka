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
-- Module      : Amazonka.Inspector.ListEventSubscriptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the event subscriptions for the assessment template that is
-- specified by the ARN of the assessment template. For more information,
-- see SubscribeToEvent and UnsubscribeFromEvent.
--
-- This operation returns paginated results.
module Amazonka.Inspector.ListEventSubscriptions
  ( -- * Creating a Request
    ListEventSubscriptions (..),
    newListEventSubscriptions,

    -- * Request Lenses
    listEventSubscriptions_maxResults,
    listEventSubscriptions_nextToken,
    listEventSubscriptions_resourceArn,

    -- * Destructuring the Response
    ListEventSubscriptionsResponse (..),
    newListEventSubscriptionsResponse,

    -- * Response Lenses
    listEventSubscriptionsResponse_nextToken,
    listEventSubscriptionsResponse_httpStatus,
    listEventSubscriptionsResponse_subscriptions,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEventSubscriptions' smart constructor.
data ListEventSubscriptions = ListEventSubscriptions'
  { -- | You can use this parameter to indicate the maximum number of items you
    -- want in the response. The default value is 10. The maximum value is 500.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the
    -- __ListEventSubscriptions__ action. Subsequent calls to the action fill
    -- __nextToken__ in the request with the value of __NextToken__ from the
    -- previous response to continue listing data.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the assessment template for which you want to list the
    -- existing event subscriptions.
    resourceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEventSubscriptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listEventSubscriptions_maxResults' - You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
--
-- 'nextToken', 'listEventSubscriptions_nextToken' - You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the
-- __ListEventSubscriptions__ action. Subsequent calls to the action fill
-- __nextToken__ in the request with the value of __NextToken__ from the
-- previous response to continue listing data.
--
-- 'resourceArn', 'listEventSubscriptions_resourceArn' - The ARN of the assessment template for which you want to list the
-- existing event subscriptions.
newListEventSubscriptions ::
  ListEventSubscriptions
newListEventSubscriptions =
  ListEventSubscriptions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceArn = Prelude.Nothing
    }

-- | You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
listEventSubscriptions_maxResults :: Lens.Lens' ListEventSubscriptions (Prelude.Maybe Prelude.Int)
listEventSubscriptions_maxResults = Lens.lens (\ListEventSubscriptions' {maxResults} -> maxResults) (\s@ListEventSubscriptions' {} a -> s {maxResults = a} :: ListEventSubscriptions)

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the
-- __ListEventSubscriptions__ action. Subsequent calls to the action fill
-- __nextToken__ in the request with the value of __NextToken__ from the
-- previous response to continue listing data.
listEventSubscriptions_nextToken :: Lens.Lens' ListEventSubscriptions (Prelude.Maybe Prelude.Text)
listEventSubscriptions_nextToken = Lens.lens (\ListEventSubscriptions' {nextToken} -> nextToken) (\s@ListEventSubscriptions' {} a -> s {nextToken = a} :: ListEventSubscriptions)

-- | The ARN of the assessment template for which you want to list the
-- existing event subscriptions.
listEventSubscriptions_resourceArn :: Lens.Lens' ListEventSubscriptions (Prelude.Maybe Prelude.Text)
listEventSubscriptions_resourceArn = Lens.lens (\ListEventSubscriptions' {resourceArn} -> resourceArn) (\s@ListEventSubscriptions' {} a -> s {resourceArn = a} :: ListEventSubscriptions)

instance Core.AWSPager ListEventSubscriptions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEventSubscriptionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listEventSubscriptionsResponse_subscriptions
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listEventSubscriptions_nextToken
              Lens..~ rs
              Lens.^? listEventSubscriptionsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListEventSubscriptions where
  type
    AWSResponse ListEventSubscriptions =
      ListEventSubscriptionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEventSubscriptionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "subscriptions" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListEventSubscriptions where
  hashWithSalt _salt ListEventSubscriptions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData ListEventSubscriptions where
  rnf ListEventSubscriptions' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf resourceArn

instance Data.ToHeaders ListEventSubscriptions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "InspectorService.ListEventSubscriptions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEventSubscriptions where
  toJSON ListEventSubscriptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("resourceArn" Data..=) Prelude.<$> resourceArn
          ]
      )

instance Data.ToPath ListEventSubscriptions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListEventSubscriptions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEventSubscriptionsResponse' smart constructor.
data ListEventSubscriptionsResponse = ListEventSubscriptionsResponse'
  { -- | When a response is generated, if there is more data to be listed, this
    -- parameter is present in the response and contains the value to use for
    -- the __nextToken__ parameter in a subsequent pagination request. If there
    -- is no more data to be listed, this parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Details of the returned event subscriptions.
    subscriptions :: [Subscription]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListEventSubscriptionsResponse
newListEventSubscriptionsResponse pHttpStatus_ =
  ListEventSubscriptionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      subscriptions = Prelude.mempty
    }

-- | When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to null.
listEventSubscriptionsResponse_nextToken :: Lens.Lens' ListEventSubscriptionsResponse (Prelude.Maybe Prelude.Text)
listEventSubscriptionsResponse_nextToken = Lens.lens (\ListEventSubscriptionsResponse' {nextToken} -> nextToken) (\s@ListEventSubscriptionsResponse' {} a -> s {nextToken = a} :: ListEventSubscriptionsResponse)

-- | The response's http status code.
listEventSubscriptionsResponse_httpStatus :: Lens.Lens' ListEventSubscriptionsResponse Prelude.Int
listEventSubscriptionsResponse_httpStatus = Lens.lens (\ListEventSubscriptionsResponse' {httpStatus} -> httpStatus) (\s@ListEventSubscriptionsResponse' {} a -> s {httpStatus = a} :: ListEventSubscriptionsResponse)

-- | Details of the returned event subscriptions.
listEventSubscriptionsResponse_subscriptions :: Lens.Lens' ListEventSubscriptionsResponse [Subscription]
listEventSubscriptionsResponse_subscriptions = Lens.lens (\ListEventSubscriptionsResponse' {subscriptions} -> subscriptions) (\s@ListEventSubscriptionsResponse' {} a -> s {subscriptions = a} :: ListEventSubscriptionsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListEventSubscriptionsResponse
  where
  rnf ListEventSubscriptionsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf subscriptions
