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
-- Module      : Amazonka.SESV2.PutAccountSendingAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enable or disable the ability of your account to send email.
module Amazonka.SESV2.PutAccountSendingAttributes
  ( -- * Creating a Request
    PutAccountSendingAttributes (..),
    newPutAccountSendingAttributes,

    -- * Request Lenses
    putAccountSendingAttributes_sendingEnabled,

    -- * Destructuring the Response
    PutAccountSendingAttributesResponse (..),
    newPutAccountSendingAttributesResponse,

    -- * Response Lenses
    putAccountSendingAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | A request to change the ability of your account to send email.
--
-- /See:/ 'newPutAccountSendingAttributes' smart constructor.
data PutAccountSendingAttributes = PutAccountSendingAttributes'
  { -- | Enables or disables your account\'s ability to send email. Set to @true@
    -- to enable email sending, or set to @false@ to disable email sending.
    --
    -- If Amazon Web Services paused your account\'s ability to send email, you
    -- can\'t use this operation to resume your account\'s ability to send
    -- email.
    sendingEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAccountSendingAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sendingEnabled', 'putAccountSendingAttributes_sendingEnabled' - Enables or disables your account\'s ability to send email. Set to @true@
-- to enable email sending, or set to @false@ to disable email sending.
--
-- If Amazon Web Services paused your account\'s ability to send email, you
-- can\'t use this operation to resume your account\'s ability to send
-- email.
newPutAccountSendingAttributes ::
  PutAccountSendingAttributes
newPutAccountSendingAttributes =
  PutAccountSendingAttributes'
    { sendingEnabled =
        Prelude.Nothing
    }

-- | Enables or disables your account\'s ability to send email. Set to @true@
-- to enable email sending, or set to @false@ to disable email sending.
--
-- If Amazon Web Services paused your account\'s ability to send email, you
-- can\'t use this operation to resume your account\'s ability to send
-- email.
putAccountSendingAttributes_sendingEnabled :: Lens.Lens' PutAccountSendingAttributes (Prelude.Maybe Prelude.Bool)
putAccountSendingAttributes_sendingEnabled = Lens.lens (\PutAccountSendingAttributes' {sendingEnabled} -> sendingEnabled) (\s@PutAccountSendingAttributes' {} a -> s {sendingEnabled = a} :: PutAccountSendingAttributes)

instance Core.AWSRequest PutAccountSendingAttributes where
  type
    AWSResponse PutAccountSendingAttributes =
      PutAccountSendingAttributesResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutAccountSendingAttributesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutAccountSendingAttributes where
  hashWithSalt _salt PutAccountSendingAttributes' {..} =
    _salt `Prelude.hashWithSalt` sendingEnabled

instance Prelude.NFData PutAccountSendingAttributes where
  rnf PutAccountSendingAttributes' {..} =
    Prelude.rnf sendingEnabled

instance Core.ToHeaders PutAccountSendingAttributes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutAccountSendingAttributes where
  toJSON PutAccountSendingAttributes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SendingEnabled" Core..=)
              Prelude.<$> sendingEnabled
          ]
      )

instance Core.ToPath PutAccountSendingAttributes where
  toPath = Prelude.const "/v2/email/account/sending"

instance Core.ToQuery PutAccountSendingAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newPutAccountSendingAttributesResponse' smart constructor.
data PutAccountSendingAttributesResponse = PutAccountSendingAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAccountSendingAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putAccountSendingAttributesResponse_httpStatus' - The response's http status code.
newPutAccountSendingAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutAccountSendingAttributesResponse
newPutAccountSendingAttributesResponse pHttpStatus_ =
  PutAccountSendingAttributesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putAccountSendingAttributesResponse_httpStatus :: Lens.Lens' PutAccountSendingAttributesResponse Prelude.Int
putAccountSendingAttributesResponse_httpStatus = Lens.lens (\PutAccountSendingAttributesResponse' {httpStatus} -> httpStatus) (\s@PutAccountSendingAttributesResponse' {} a -> s {httpStatus = a} :: PutAccountSendingAttributesResponse)

instance
  Prelude.NFData
    PutAccountSendingAttributesResponse
  where
  rnf PutAccountSendingAttributesResponse' {..} =
    Prelude.rnf httpStatus
