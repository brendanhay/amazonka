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
-- Module      : Amazonka.Amplify.ListWebhooks
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of webhooks for an Amplify app.
module Amazonka.Amplify.ListWebhooks
  ( -- * Creating a Request
    ListWebhooks (..),
    newListWebhooks,

    -- * Request Lenses
    listWebhooks_nextToken,
    listWebhooks_maxResults,
    listWebhooks_appId,

    -- * Destructuring the Response
    ListWebhooksResponse (..),
    newListWebhooksResponse,

    -- * Response Lenses
    listWebhooksResponse_nextToken,
    listWebhooksResponse_httpStatus,
    listWebhooksResponse_webhooks,
  )
where

import Amazonka.Amplify.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request structure for the list webhooks request.
--
-- /See:/ 'newListWebhooks' smart constructor.
data ListWebhooks = ListWebhooks'
  { -- | A pagination token. Set to null to start listing webhooks from the
    -- start. If non-null,the pagination token is returned in a result. Pass
    -- its value in here to list more webhooks.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to list in a single response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The unique ID for an Amplify app.
    appId :: Prelude.Text
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
-- 'nextToken', 'listWebhooks_nextToken' - A pagination token. Set to null to start listing webhooks from the
-- start. If non-null,the pagination token is returned in a result. Pass
-- its value in here to list more webhooks.
--
-- 'maxResults', 'listWebhooks_maxResults' - The maximum number of records to list in a single response.
--
-- 'appId', 'listWebhooks_appId' - The unique ID for an Amplify app.
newListWebhooks ::
  -- | 'appId'
  Prelude.Text ->
  ListWebhooks
newListWebhooks pAppId_ =
  ListWebhooks'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      appId = pAppId_
    }

-- | A pagination token. Set to null to start listing webhooks from the
-- start. If non-null,the pagination token is returned in a result. Pass
-- its value in here to list more webhooks.
listWebhooks_nextToken :: Lens.Lens' ListWebhooks (Prelude.Maybe Prelude.Text)
listWebhooks_nextToken = Lens.lens (\ListWebhooks' {nextToken} -> nextToken) (\s@ListWebhooks' {} a -> s {nextToken = a} :: ListWebhooks)

-- | The maximum number of records to list in a single response.
listWebhooks_maxResults :: Lens.Lens' ListWebhooks (Prelude.Maybe Prelude.Natural)
listWebhooks_maxResults = Lens.lens (\ListWebhooks' {maxResults} -> maxResults) (\s@ListWebhooks' {} a -> s {maxResults = a} :: ListWebhooks)

-- | The unique ID for an Amplify app.
listWebhooks_appId :: Lens.Lens' ListWebhooks Prelude.Text
listWebhooks_appId = Lens.lens (\ListWebhooks' {appId} -> appId) (\s@ListWebhooks' {} a -> s {appId = a} :: ListWebhooks)

instance Core.AWSRequest ListWebhooks where
  type AWSResponse ListWebhooks = ListWebhooksResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWebhooksResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "webhooks" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListWebhooks where
  hashWithSalt _salt ListWebhooks' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` appId

instance Prelude.NFData ListWebhooks where
  rnf ListWebhooks' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf appId

instance Core.ToHeaders ListWebhooks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListWebhooks where
  toPath ListWebhooks' {..} =
    Prelude.mconcat
      ["/apps/", Core.toBS appId, "/webhooks"]

instance Core.ToQuery ListWebhooks where
  toQuery ListWebhooks' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | The result structure for the list webhooks request.
--
-- /See:/ 'newListWebhooksResponse' smart constructor.
data ListWebhooksResponse = ListWebhooksResponse'
  { -- | A pagination token. If non-null, the pagination token is returned in a
    -- result. Pass its value in another request to retrieve more entries.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of webhooks.
    webhooks :: [Webhook]
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
-- 'nextToken', 'listWebhooksResponse_nextToken' - A pagination token. If non-null, the pagination token is returned in a
-- result. Pass its value in another request to retrieve more entries.
--
-- 'httpStatus', 'listWebhooksResponse_httpStatus' - The response's http status code.
--
-- 'webhooks', 'listWebhooksResponse_webhooks' - A list of webhooks.
newListWebhooksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWebhooksResponse
newListWebhooksResponse pHttpStatus_ =
  ListWebhooksResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      webhooks = Prelude.mempty
    }

-- | A pagination token. If non-null, the pagination token is returned in a
-- result. Pass its value in another request to retrieve more entries.
listWebhooksResponse_nextToken :: Lens.Lens' ListWebhooksResponse (Prelude.Maybe Prelude.Text)
listWebhooksResponse_nextToken = Lens.lens (\ListWebhooksResponse' {nextToken} -> nextToken) (\s@ListWebhooksResponse' {} a -> s {nextToken = a} :: ListWebhooksResponse)

-- | The response's http status code.
listWebhooksResponse_httpStatus :: Lens.Lens' ListWebhooksResponse Prelude.Int
listWebhooksResponse_httpStatus = Lens.lens (\ListWebhooksResponse' {httpStatus} -> httpStatus) (\s@ListWebhooksResponse' {} a -> s {httpStatus = a} :: ListWebhooksResponse)

-- | A list of webhooks.
listWebhooksResponse_webhooks :: Lens.Lens' ListWebhooksResponse [Webhook]
listWebhooksResponse_webhooks = Lens.lens (\ListWebhooksResponse' {webhooks} -> webhooks) (\s@ListWebhooksResponse' {} a -> s {webhooks = a} :: ListWebhooksResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListWebhooksResponse where
  rnf ListWebhooksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf webhooks
