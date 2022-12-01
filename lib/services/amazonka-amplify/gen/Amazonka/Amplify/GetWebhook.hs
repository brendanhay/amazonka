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
-- Module      : Amazonka.Amplify.GetWebhook
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the webhook information that corresponds to a specified webhook
-- ID.
module Amazonka.Amplify.GetWebhook
  ( -- * Creating a Request
    GetWebhook (..),
    newGetWebhook,

    -- * Request Lenses
    getWebhook_webhookId,

    -- * Destructuring the Response
    GetWebhookResponse (..),
    newGetWebhookResponse,

    -- * Response Lenses
    getWebhookResponse_httpStatus,
    getWebhookResponse_webhook,
  )
where

import Amazonka.Amplify.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request structure for the get webhook request.
--
-- /See:/ 'newGetWebhook' smart constructor.
data GetWebhook = GetWebhook'
  { -- | The unique ID for a webhook.
    webhookId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWebhook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'webhookId', 'getWebhook_webhookId' - The unique ID for a webhook.
newGetWebhook ::
  -- | 'webhookId'
  Prelude.Text ->
  GetWebhook
newGetWebhook pWebhookId_ =
  GetWebhook' {webhookId = pWebhookId_}

-- | The unique ID for a webhook.
getWebhook_webhookId :: Lens.Lens' GetWebhook Prelude.Text
getWebhook_webhookId = Lens.lens (\GetWebhook' {webhookId} -> webhookId) (\s@GetWebhook' {} a -> s {webhookId = a} :: GetWebhook)

instance Core.AWSRequest GetWebhook where
  type AWSResponse GetWebhook = GetWebhookResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWebhookResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "webhook")
      )

instance Prelude.Hashable GetWebhook where
  hashWithSalt _salt GetWebhook' {..} =
    _salt `Prelude.hashWithSalt` webhookId

instance Prelude.NFData GetWebhook where
  rnf GetWebhook' {..} = Prelude.rnf webhookId

instance Core.ToHeaders GetWebhook where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetWebhook where
  toPath GetWebhook' {..} =
    Prelude.mconcat ["/webhooks/", Core.toBS webhookId]

instance Core.ToQuery GetWebhook where
  toQuery = Prelude.const Prelude.mempty

-- | The result structure for the get webhook request.
--
-- /See:/ 'newGetWebhookResponse' smart constructor.
data GetWebhookResponse = GetWebhookResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Describes the structure of a webhook.
    webhook :: Webhook
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWebhookResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getWebhookResponse_httpStatus' - The response's http status code.
--
-- 'webhook', 'getWebhookResponse_webhook' - Describes the structure of a webhook.
newGetWebhookResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'webhook'
  Webhook ->
  GetWebhookResponse
newGetWebhookResponse pHttpStatus_ pWebhook_ =
  GetWebhookResponse'
    { httpStatus = pHttpStatus_,
      webhook = pWebhook_
    }

-- | The response's http status code.
getWebhookResponse_httpStatus :: Lens.Lens' GetWebhookResponse Prelude.Int
getWebhookResponse_httpStatus = Lens.lens (\GetWebhookResponse' {httpStatus} -> httpStatus) (\s@GetWebhookResponse' {} a -> s {httpStatus = a} :: GetWebhookResponse)

-- | Describes the structure of a webhook.
getWebhookResponse_webhook :: Lens.Lens' GetWebhookResponse Webhook
getWebhookResponse_webhook = Lens.lens (\GetWebhookResponse' {webhook} -> webhook) (\s@GetWebhookResponse' {} a -> s {webhook = a} :: GetWebhookResponse)

instance Prelude.NFData GetWebhookResponse where
  rnf GetWebhookResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf webhook
