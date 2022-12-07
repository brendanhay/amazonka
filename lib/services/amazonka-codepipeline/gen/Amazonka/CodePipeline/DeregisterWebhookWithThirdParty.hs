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
-- Module      : Amazonka.CodePipeline.DeregisterWebhookWithThirdParty
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the connection between the webhook that was created by
-- CodePipeline and the external tool with events to be detected. Currently
-- supported only for webhooks that target an action type of GitHub.
module Amazonka.CodePipeline.DeregisterWebhookWithThirdParty
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

import Amazonka.CodePipeline.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeregisterWebhookWithThirdParty' smart constructor.
data DeregisterWebhookWithThirdParty = DeregisterWebhookWithThirdParty'
  { -- | The name of the webhook you want to deregister.
    webhookName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | The name of the webhook you want to deregister.
deregisterWebhookWithThirdParty_webhookName :: Lens.Lens' DeregisterWebhookWithThirdParty (Prelude.Maybe Prelude.Text)
deregisterWebhookWithThirdParty_webhookName = Lens.lens (\DeregisterWebhookWithThirdParty' {webhookName} -> webhookName) (\s@DeregisterWebhookWithThirdParty' {} a -> s {webhookName = a} :: DeregisterWebhookWithThirdParty)

instance
  Core.AWSRequest
    DeregisterWebhookWithThirdParty
  where
  type
    AWSResponse DeregisterWebhookWithThirdParty =
      DeregisterWebhookWithThirdPartyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterWebhookWithThirdPartyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeregisterWebhookWithThirdParty
  where
  hashWithSalt
    _salt
    DeregisterWebhookWithThirdParty' {..} =
      _salt `Prelude.hashWithSalt` webhookName

instance
  Prelude.NFData
    DeregisterWebhookWithThirdParty
  where
  rnf DeregisterWebhookWithThirdParty' {..} =
    Prelude.rnf webhookName

instance
  Data.ToHeaders
    DeregisterWebhookWithThirdParty
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodePipeline_20150709.DeregisterWebhookWithThirdParty" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeregisterWebhookWithThirdParty where
  toJSON DeregisterWebhookWithThirdParty' {..} =
    Data.object
      ( Prelude.catMaybes
          [("webhookName" Data..=) Prelude.<$> webhookName]
      )

instance Data.ToPath DeregisterWebhookWithThirdParty where
  toPath = Prelude.const "/"

instance Data.ToQuery DeregisterWebhookWithThirdParty where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterWebhookWithThirdPartyResponse' smart constructor.
data DeregisterWebhookWithThirdPartyResponse = DeregisterWebhookWithThirdPartyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeregisterWebhookWithThirdPartyResponse
newDeregisterWebhookWithThirdPartyResponse
  pHttpStatus_ =
    DeregisterWebhookWithThirdPartyResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deregisterWebhookWithThirdPartyResponse_httpStatus :: Lens.Lens' DeregisterWebhookWithThirdPartyResponse Prelude.Int
deregisterWebhookWithThirdPartyResponse_httpStatus = Lens.lens (\DeregisterWebhookWithThirdPartyResponse' {httpStatus} -> httpStatus) (\s@DeregisterWebhookWithThirdPartyResponse' {} a -> s {httpStatus = a} :: DeregisterWebhookWithThirdPartyResponse)

instance
  Prelude.NFData
    DeregisterWebhookWithThirdPartyResponse
  where
  rnf DeregisterWebhookWithThirdPartyResponse' {..} =
    Prelude.rnf httpStatus
