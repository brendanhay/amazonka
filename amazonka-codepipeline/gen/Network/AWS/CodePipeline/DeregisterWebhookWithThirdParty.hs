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
-- Module      : Network.AWS.CodePipeline.DeregisterWebhookWithThirdParty
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the connection between the webhook that was created by
-- CodePipeline and the external tool with events to be detected. Currently
-- supported only for webhooks that target an action type of GitHub.
module Network.AWS.CodePipeline.DeregisterWebhookWithThirdParty
  ( -- * Creating a Request
    DeregisterWebhookWithThirdParty (..),
    newDeregisterWebhookWithThirdParty,

    -- * Request Lenses
    deregisterWebhookWithThirdParty_webhookName,

    -- * Destructuring the Response
    DeregisterWebhookWithThirdPartyResponse (..),
    newDeregisterWebhookWithThirdPartyResponse,

    -- * Response Lenses
    deregisterWebhookWithThirdPartyResponse_httpStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeregisterWebhookWithThirdParty' smart constructor.
data DeregisterWebhookWithThirdParty = DeregisterWebhookWithThirdParty'
  { -- | The name of the webhook you want to deregister.
    webhookName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterWebhookWithThirdParty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'webhookName', 'deregisterWebhookWithThirdParty_webhookName' - The name of the webhook you want to deregister.
newDeregisterWebhookWithThirdParty ::
  DeregisterWebhookWithThirdParty
newDeregisterWebhookWithThirdParty =
  DeregisterWebhookWithThirdParty'
    { webhookName =
        Core.Nothing
    }

-- | The name of the webhook you want to deregister.
deregisterWebhookWithThirdParty_webhookName :: Lens.Lens' DeregisterWebhookWithThirdParty (Core.Maybe Core.Text)
deregisterWebhookWithThirdParty_webhookName = Lens.lens (\DeregisterWebhookWithThirdParty' {webhookName} -> webhookName) (\s@DeregisterWebhookWithThirdParty' {} a -> s {webhookName = a} :: DeregisterWebhookWithThirdParty)

instance
  Core.AWSRequest
    DeregisterWebhookWithThirdParty
  where
  type
    AWSResponse DeregisterWebhookWithThirdParty =
      DeregisterWebhookWithThirdPartyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterWebhookWithThirdPartyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DeregisterWebhookWithThirdParty

instance Core.NFData DeregisterWebhookWithThirdParty

instance
  Core.ToHeaders
    DeregisterWebhookWithThirdParty
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.DeregisterWebhookWithThirdParty" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeregisterWebhookWithThirdParty where
  toJSON DeregisterWebhookWithThirdParty' {..} =
    Core.object
      ( Core.catMaybes
          [("webhookName" Core..=) Core.<$> webhookName]
      )

instance Core.ToPath DeregisterWebhookWithThirdParty where
  toPath = Core.const "/"

instance Core.ToQuery DeregisterWebhookWithThirdParty where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeregisterWebhookWithThirdPartyResponse' smart constructor.
data DeregisterWebhookWithThirdPartyResponse = DeregisterWebhookWithThirdPartyResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterWebhookWithThirdPartyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deregisterWebhookWithThirdPartyResponse_httpStatus' - The response's http status code.
newDeregisterWebhookWithThirdPartyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeregisterWebhookWithThirdPartyResponse
newDeregisterWebhookWithThirdPartyResponse
  pHttpStatus_ =
    DeregisterWebhookWithThirdPartyResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deregisterWebhookWithThirdPartyResponse_httpStatus :: Lens.Lens' DeregisterWebhookWithThirdPartyResponse Core.Int
deregisterWebhookWithThirdPartyResponse_httpStatus = Lens.lens (\DeregisterWebhookWithThirdPartyResponse' {httpStatus} -> httpStatus) (\s@DeregisterWebhookWithThirdPartyResponse' {} a -> s {httpStatus = a} :: DeregisterWebhookWithThirdPartyResponse)

instance
  Core.NFData
    DeregisterWebhookWithThirdPartyResponse
