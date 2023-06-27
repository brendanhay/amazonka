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
-- Module      : Amazonka.SecurityLake.ListSubscribers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all subscribers for the specific Amazon Security Lake account ID.
-- You can retrieve a list of subscriptions associated with a specific
-- organization or Amazon Web Services account.
--
-- This operation returns paginated results.
module Amazonka.SecurityLake.ListSubscribers
  ( -- * Creating a Request
    ListSubscribers (..),
    newListSubscribers,

    -- * Request Lenses
    listSubscribers_maxResults,
    listSubscribers_nextToken,

    -- * Destructuring the Response
    ListSubscribersResponse (..),
    newListSubscribersResponse,

    -- * Response Lenses
    listSubscribersResponse_nextToken,
    listSubscribersResponse_subscribers,
    listSubscribersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newListSubscribers' smart constructor.
data ListSubscribers = ListSubscribers'
  { -- | The maximum number of accounts for which the configuration is displayed.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If nextToken is returned, there are more results available. You can
    -- repeat the call using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSubscribers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSubscribers_maxResults' - The maximum number of accounts for which the configuration is displayed.
--
-- 'nextToken', 'listSubscribers_nextToken' - If nextToken is returned, there are more results available. You can
-- repeat the call using the returned token to retrieve the next page.
newListSubscribers ::
  ListSubscribers
newListSubscribers =
  ListSubscribers'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of accounts for which the configuration is displayed.
listSubscribers_maxResults :: Lens.Lens' ListSubscribers (Prelude.Maybe Prelude.Natural)
listSubscribers_maxResults = Lens.lens (\ListSubscribers' {maxResults} -> maxResults) (\s@ListSubscribers' {} a -> s {maxResults = a} :: ListSubscribers)

-- | If nextToken is returned, there are more results available. You can
-- repeat the call using the returned token to retrieve the next page.
listSubscribers_nextToken :: Lens.Lens' ListSubscribers (Prelude.Maybe Prelude.Text)
listSubscribers_nextToken = Lens.lens (\ListSubscribers' {nextToken} -> nextToken) (\s@ListSubscribers' {} a -> s {nextToken = a} :: ListSubscribers)

instance Core.AWSPager ListSubscribers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSubscribersResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSubscribersResponse_subscribers
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSubscribers_nextToken
          Lens..~ rs
          Lens.^? listSubscribersResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListSubscribers where
  type
    AWSResponse ListSubscribers =
      ListSubscribersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSubscribersResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "subscribers" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSubscribers where
  hashWithSalt _salt ListSubscribers' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListSubscribers where
  rnf ListSubscribers' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListSubscribers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSubscribers where
  toPath = Prelude.const "/v1/subscribers"

instance Data.ToQuery ListSubscribers where
  toQuery ListSubscribers' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListSubscribersResponse' smart constructor.
data ListSubscribersResponse = ListSubscribersResponse'
  { -- | If nextToken is returned, there are more results available. You can
    -- repeat the call using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The subscribers available for the specified Security Lake account ID.
    subscribers :: Prelude.Maybe [SubscriberResource],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSubscribersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSubscribersResponse_nextToken' - If nextToken is returned, there are more results available. You can
-- repeat the call using the returned token to retrieve the next page.
--
-- 'subscribers', 'listSubscribersResponse_subscribers' - The subscribers available for the specified Security Lake account ID.
--
-- 'httpStatus', 'listSubscribersResponse_httpStatus' - The response's http status code.
newListSubscribersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSubscribersResponse
newListSubscribersResponse pHttpStatus_ =
  ListSubscribersResponse'
    { nextToken =
        Prelude.Nothing,
      subscribers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If nextToken is returned, there are more results available. You can
-- repeat the call using the returned token to retrieve the next page.
listSubscribersResponse_nextToken :: Lens.Lens' ListSubscribersResponse (Prelude.Maybe Prelude.Text)
listSubscribersResponse_nextToken = Lens.lens (\ListSubscribersResponse' {nextToken} -> nextToken) (\s@ListSubscribersResponse' {} a -> s {nextToken = a} :: ListSubscribersResponse)

-- | The subscribers available for the specified Security Lake account ID.
listSubscribersResponse_subscribers :: Lens.Lens' ListSubscribersResponse (Prelude.Maybe [SubscriberResource])
listSubscribersResponse_subscribers = Lens.lens (\ListSubscribersResponse' {subscribers} -> subscribers) (\s@ListSubscribersResponse' {} a -> s {subscribers = a} :: ListSubscribersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSubscribersResponse_httpStatus :: Lens.Lens' ListSubscribersResponse Prelude.Int
listSubscribersResponse_httpStatus = Lens.lens (\ListSubscribersResponse' {httpStatus} -> httpStatus) (\s@ListSubscribersResponse' {} a -> s {httpStatus = a} :: ListSubscribersResponse)

instance Prelude.NFData ListSubscribersResponse where
  rnf ListSubscribersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf subscribers
      `Prelude.seq` Prelude.rnf httpStatus
