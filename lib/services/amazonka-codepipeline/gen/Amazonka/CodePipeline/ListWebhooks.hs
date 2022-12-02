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
-- Module      : Amazonka.CodePipeline.ListWebhooks
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a listing of all the webhooks in this AWS Region for this account.
-- The output lists all webhooks and includes the webhook URL and ARN and
-- the configuration for each webhook.
--
-- This operation returns paginated results.
module Amazonka.CodePipeline.ListWebhooks
  ( -- * Creating a Request
    ListWebhooks (..),
    newListWebhooks,

    -- * Request Lenses
    listWebhooks_nextToken,
    listWebhooks_maxResults,

    -- * Destructuring the Response
    ListWebhooksResponse (..),
    newListWebhooksResponse,

    -- * Response Lenses
    listWebhooksResponse_nextToken,
    listWebhooksResponse_webhooks,
    listWebhooksResponse_httpStatus,
  )
where

import Amazonka.CodePipeline.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListWebhooks' smart constructor.
data ListWebhooks = ListWebhooks'
  { -- | The token that was returned from the previous ListWebhooks call, which
    -- can be used to return the next set of webhooks in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned nextToken
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWebhooks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWebhooks_nextToken' - The token that was returned from the previous ListWebhooks call, which
-- can be used to return the next set of webhooks in the list.
--
-- 'maxResults', 'listWebhooks_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned nextToken
-- value.
newListWebhooks ::
  ListWebhooks
newListWebhooks =
  ListWebhooks'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token that was returned from the previous ListWebhooks call, which
-- can be used to return the next set of webhooks in the list.
listWebhooks_nextToken :: Lens.Lens' ListWebhooks (Prelude.Maybe Prelude.Text)
listWebhooks_nextToken = Lens.lens (\ListWebhooks' {nextToken} -> nextToken) (\s@ListWebhooks' {} a -> s {nextToken = a} :: ListWebhooks)

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned nextToken
-- value.
listWebhooks_maxResults :: Lens.Lens' ListWebhooks (Prelude.Maybe Prelude.Natural)
listWebhooks_maxResults = Lens.lens (\ListWebhooks' {maxResults} -> maxResults) (\s@ListWebhooks' {} a -> s {maxResults = a} :: ListWebhooks)

instance Core.AWSPager ListWebhooks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listWebhooksResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listWebhooksResponse_webhooks Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listWebhooks_nextToken
          Lens..~ rs
          Lens.^? listWebhooksResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListWebhooks where
  type AWSResponse ListWebhooks = ListWebhooksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWebhooksResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "webhooks" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListWebhooks where
  hashWithSalt _salt ListWebhooks' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListWebhooks where
  rnf ListWebhooks' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListWebhooks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodePipeline_20150709.ListWebhooks" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListWebhooks where
  toJSON ListWebhooks' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListWebhooks where
  toPath = Prelude.const "/"

instance Data.ToQuery ListWebhooks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListWebhooksResponse' smart constructor.
data ListWebhooksResponse = ListWebhooksResponse'
  { -- | If the amount of returned information is significantly large, an
    -- identifier is also returned and can be used in a subsequent ListWebhooks
    -- call to return the next set of webhooks in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The JSON detail returned for each webhook in the list output for the
    -- ListWebhooks call.
    webhooks :: Prelude.Maybe [ListWebhookItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWebhooksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWebhooksResponse_nextToken' - If the amount of returned information is significantly large, an
-- identifier is also returned and can be used in a subsequent ListWebhooks
-- call to return the next set of webhooks in the list.
--
-- 'webhooks', 'listWebhooksResponse_webhooks' - The JSON detail returned for each webhook in the list output for the
-- ListWebhooks call.
--
-- 'httpStatus', 'listWebhooksResponse_httpStatus' - The response's http status code.
newListWebhooksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWebhooksResponse
newListWebhooksResponse pHttpStatus_ =
  ListWebhooksResponse'
    { nextToken = Prelude.Nothing,
      webhooks = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the amount of returned information is significantly large, an
-- identifier is also returned and can be used in a subsequent ListWebhooks
-- call to return the next set of webhooks in the list.
listWebhooksResponse_nextToken :: Lens.Lens' ListWebhooksResponse (Prelude.Maybe Prelude.Text)
listWebhooksResponse_nextToken = Lens.lens (\ListWebhooksResponse' {nextToken} -> nextToken) (\s@ListWebhooksResponse' {} a -> s {nextToken = a} :: ListWebhooksResponse)

-- | The JSON detail returned for each webhook in the list output for the
-- ListWebhooks call.
listWebhooksResponse_webhooks :: Lens.Lens' ListWebhooksResponse (Prelude.Maybe [ListWebhookItem])
listWebhooksResponse_webhooks = Lens.lens (\ListWebhooksResponse' {webhooks} -> webhooks) (\s@ListWebhooksResponse' {} a -> s {webhooks = a} :: ListWebhooksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listWebhooksResponse_httpStatus :: Lens.Lens' ListWebhooksResponse Prelude.Int
listWebhooksResponse_httpStatus = Lens.lens (\ListWebhooksResponse' {httpStatus} -> httpStatus) (\s@ListWebhooksResponse' {} a -> s {httpStatus = a} :: ListWebhooksResponse)

instance Prelude.NFData ListWebhooksResponse where
  rnf ListWebhooksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf webhooks
      `Prelude.seq` Prelude.rnf httpStatus
