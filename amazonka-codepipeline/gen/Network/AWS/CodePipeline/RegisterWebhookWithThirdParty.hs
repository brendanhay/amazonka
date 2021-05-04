{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodePipeline.RegisterWebhookWithThirdParty
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures a connection between the webhook that was created and the
-- external tool with events to be detected.
module Network.AWS.CodePipeline.RegisterWebhookWithThirdParty
  ( -- * Creating a Request
    RegisterWebhookWithThirdParty (..),
    newRegisterWebhookWithThirdParty,

    -- * Request Lenses
    registerWebhookWithThirdParty_webhookName,

    -- * Destructuring the Response
    RegisterWebhookWithThirdPartyResponse (..),
    newRegisterWebhookWithThirdPartyResponse,

    -- * Response Lenses
    registerWebhookWithThirdPartyResponse_httpStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRegisterWebhookWithThirdParty' smart constructor.
data RegisterWebhookWithThirdParty = RegisterWebhookWithThirdParty'
  { -- | The name of an existing webhook created with PutWebhook to register with
    -- a supported third party.
    webhookName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegisterWebhookWithThirdParty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'webhookName', 'registerWebhookWithThirdParty_webhookName' - The name of an existing webhook created with PutWebhook to register with
-- a supported third party.
newRegisterWebhookWithThirdParty ::
  RegisterWebhookWithThirdParty
newRegisterWebhookWithThirdParty =
  RegisterWebhookWithThirdParty'
    { webhookName =
        Prelude.Nothing
    }

-- | The name of an existing webhook created with PutWebhook to register with
-- a supported third party.
registerWebhookWithThirdParty_webhookName :: Lens.Lens' RegisterWebhookWithThirdParty (Prelude.Maybe Prelude.Text)
registerWebhookWithThirdParty_webhookName = Lens.lens (\RegisterWebhookWithThirdParty' {webhookName} -> webhookName) (\s@RegisterWebhookWithThirdParty' {} a -> s {webhookName = a} :: RegisterWebhookWithThirdParty)

instance
  Prelude.AWSRequest
    RegisterWebhookWithThirdParty
  where
  type
    Rs RegisterWebhookWithThirdParty =
      RegisterWebhookWithThirdPartyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RegisterWebhookWithThirdPartyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RegisterWebhookWithThirdParty

instance Prelude.NFData RegisterWebhookWithThirdParty

instance
  Prelude.ToHeaders
    RegisterWebhookWithThirdParty
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodePipeline_20150709.RegisterWebhookWithThirdParty" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RegisterWebhookWithThirdParty where
  toJSON RegisterWebhookWithThirdParty' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("webhookName" Prelude..=) Prelude.<$> webhookName]
      )

instance Prelude.ToPath RegisterWebhookWithThirdParty where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    RegisterWebhookWithThirdParty
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterWebhookWithThirdPartyResponse' smart constructor.
data RegisterWebhookWithThirdPartyResponse = RegisterWebhookWithThirdPartyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegisterWebhookWithThirdPartyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'registerWebhookWithThirdPartyResponse_httpStatus' - The response's http status code.
newRegisterWebhookWithThirdPartyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterWebhookWithThirdPartyResponse
newRegisterWebhookWithThirdPartyResponse pHttpStatus_ =
  RegisterWebhookWithThirdPartyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
registerWebhookWithThirdPartyResponse_httpStatus :: Lens.Lens' RegisterWebhookWithThirdPartyResponse Prelude.Int
registerWebhookWithThirdPartyResponse_httpStatus = Lens.lens (\RegisterWebhookWithThirdPartyResponse' {httpStatus} -> httpStatus) (\s@RegisterWebhookWithThirdPartyResponse' {} a -> s {httpStatus = a} :: RegisterWebhookWithThirdPartyResponse)

instance
  Prelude.NFData
    RegisterWebhookWithThirdPartyResponse
