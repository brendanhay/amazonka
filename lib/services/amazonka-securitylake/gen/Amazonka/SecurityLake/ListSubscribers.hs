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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all subscribers for the specific Security Lake account ID.
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
    listSubscribersResponse_httpStatus,
    listSubscribersResponse_subscribers,
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
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | If nextToken is returned, there are more results available. You can make
    -- the call again using the returned token to retrieve the next page.
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
-- 'nextToken', 'listSubscribers_nextToken' - If nextToken is returned, there are more results available. You can make
-- the call again using the returned token to retrieve the next page.
newListSubscribers ::
  ListSubscribers
newListSubscribers =
  ListSubscribers'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of accounts for which the configuration is displayed.
listSubscribers_maxResults :: Lens.Lens' ListSubscribers (Prelude.Maybe Prelude.Int)
listSubscribers_maxResults = Lens.lens (\ListSubscribers' {maxResults} -> maxResults) (\s@ListSubscribers' {} a -> s {maxResults = a} :: ListSubscribers)

-- | If nextToken is returned, there are more results available. You can make
-- the call again using the returned token to retrieve the next page.
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
        (rs Lens.^. listSubscribersResponse_subscribers) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
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
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "subscribers" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListSubscribers where
  hashWithSalt _salt ListSubscribers' {..} =
    _salt `Prelude.hashWithSalt` maxResults
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
  { -- | If nextToken is returned, there are more results available. You can make
    -- the call again using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The subscribers available in the specified Security Lake account ID.
    subscribers :: [SubscriberResource]
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
-- 'nextToken', 'listSubscribersResponse_nextToken' - If nextToken is returned, there are more results available. You can make
-- the call again using the returned token to retrieve the next page.
--
-- 'httpStatus', 'listSubscribersResponse_httpStatus' - The response's http status code.
--
-- 'subscribers', 'listSubscribersResponse_subscribers' - The subscribers available in the specified Security Lake account ID.
newListSubscribersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSubscribersResponse
newListSubscribersResponse pHttpStatus_ =
  ListSubscribersResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      subscribers = Prelude.mempty
    }

-- | If nextToken is returned, there are more results available. You can make
-- the call again using the returned token to retrieve the next page.
listSubscribersResponse_nextToken :: Lens.Lens' ListSubscribersResponse (Prelude.Maybe Prelude.Text)
listSubscribersResponse_nextToken = Lens.lens (\ListSubscribersResponse' {nextToken} -> nextToken) (\s@ListSubscribersResponse' {} a -> s {nextToken = a} :: ListSubscribersResponse)

-- | The response's http status code.
listSubscribersResponse_httpStatus :: Lens.Lens' ListSubscribersResponse Prelude.Int
listSubscribersResponse_httpStatus = Lens.lens (\ListSubscribersResponse' {httpStatus} -> httpStatus) (\s@ListSubscribersResponse' {} a -> s {httpStatus = a} :: ListSubscribersResponse)

-- | The subscribers available in the specified Security Lake account ID.
listSubscribersResponse_subscribers :: Lens.Lens' ListSubscribersResponse [SubscriberResource]
listSubscribersResponse_subscribers = Lens.lens (\ListSubscribersResponse' {subscribers} -> subscribers) (\s@ListSubscribersResponse' {} a -> s {subscribers = a} :: ListSubscribersResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListSubscribersResponse where
  rnf ListSubscribersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf subscribers
