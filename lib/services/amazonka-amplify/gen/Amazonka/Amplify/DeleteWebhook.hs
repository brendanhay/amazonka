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
-- Module      : Amazonka.Amplify.DeleteWebhook
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a webhook.
module Amazonka.Amplify.DeleteWebhook
  ( -- * Creating a Request
    DeleteWebhook (..),
    newDeleteWebhook,

    -- * Request Lenses
    deleteWebhook_webhookId,

    -- * Destructuring the Response
    DeleteWebhookResponse (..),
    newDeleteWebhookResponse,

    -- * Response Lenses
    deleteWebhookResponse_httpStatus,
    deleteWebhookResponse_webhook,
  )
where

import Amazonka.Amplify.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request structure for the delete webhook request.
--
-- /See:/ 'newDeleteWebhook' smart constructor.
data DeleteWebhook = DeleteWebhook'
  { -- | The unique ID for a webhook.
    webhookId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWebhook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'webhookId', 'deleteWebhook_webhookId' - The unique ID for a webhook.
newDeleteWebhook ::
  -- | 'webhookId'
  Prelude.Text ->
  DeleteWebhook
newDeleteWebhook pWebhookId_ =
  DeleteWebhook' {webhookId = pWebhookId_}

-- | The unique ID for a webhook.
deleteWebhook_webhookId :: Lens.Lens' DeleteWebhook Prelude.Text
deleteWebhook_webhookId = Lens.lens (\DeleteWebhook' {webhookId} -> webhookId) (\s@DeleteWebhook' {} a -> s {webhookId = a} :: DeleteWebhook)

instance Core.AWSRequest DeleteWebhook where
  type
    AWSResponse DeleteWebhook =
      DeleteWebhookResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteWebhookResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "webhook")
      )

instance Prelude.Hashable DeleteWebhook where
  hashWithSalt _salt DeleteWebhook' {..} =
    _salt `Prelude.hashWithSalt` webhookId

instance Prelude.NFData DeleteWebhook where
  rnf DeleteWebhook' {..} = Prelude.rnf webhookId

instance Data.ToHeaders DeleteWebhook where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteWebhook where
  toPath DeleteWebhook' {..} =
    Prelude.mconcat ["/webhooks/", Data.toBS webhookId]

instance Data.ToQuery DeleteWebhook where
  toQuery = Prelude.const Prelude.mempty

-- | The result structure for the delete webhook request.
--
-- /See:/ 'newDeleteWebhookResponse' smart constructor.
data DeleteWebhookResponse = DeleteWebhookResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Describes a webhook that connects repository events to an Amplify app.
    webhook :: Webhook
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWebhookResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWebhookResponse_httpStatus' - The response's http status code.
--
-- 'webhook', 'deleteWebhookResponse_webhook' - Describes a webhook that connects repository events to an Amplify app.
newDeleteWebhookResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'webhook'
  Webhook ->
  DeleteWebhookResponse
newDeleteWebhookResponse pHttpStatus_ pWebhook_ =
  DeleteWebhookResponse'
    { httpStatus = pHttpStatus_,
      webhook = pWebhook_
    }

-- | The response's http status code.
deleteWebhookResponse_httpStatus :: Lens.Lens' DeleteWebhookResponse Prelude.Int
deleteWebhookResponse_httpStatus = Lens.lens (\DeleteWebhookResponse' {httpStatus} -> httpStatus) (\s@DeleteWebhookResponse' {} a -> s {httpStatus = a} :: DeleteWebhookResponse)

-- | Describes a webhook that connects repository events to an Amplify app.
deleteWebhookResponse_webhook :: Lens.Lens' DeleteWebhookResponse Webhook
deleteWebhookResponse_webhook = Lens.lens (\DeleteWebhookResponse' {webhook} -> webhook) (\s@DeleteWebhookResponse' {} a -> s {webhook = a} :: DeleteWebhookResponse)

instance Prelude.NFData DeleteWebhookResponse where
  rnf DeleteWebhookResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf webhook
