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
-- Module      : Amazonka.RAM.AcceptResourceShareInvitation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts an invitation to a resource share from another Amazon Web
-- Services account.
module Amazonka.RAM.AcceptResourceShareInvitation
  ( -- * Creating a Request
    AcceptResourceShareInvitation (..),
    newAcceptResourceShareInvitation,

    -- * Request Lenses
    acceptResourceShareInvitation_clientToken,
    acceptResourceShareInvitation_resourceShareInvitationArn,

    -- * Destructuring the Response
    AcceptResourceShareInvitationResponse (..),
    newAcceptResourceShareInvitationResponse,

    -- * Response Lenses
    acceptResourceShareInvitationResponse_clientToken,
    acceptResourceShareInvitationResponse_resourceShareInvitation,
    acceptResourceShareInvitationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAcceptResourceShareInvitation' smart constructor.
data AcceptResourceShareInvitation = AcceptResourceShareInvitation'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the invitation.
    resourceShareInvitationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptResourceShareInvitation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'acceptResourceShareInvitation_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'resourceShareInvitationArn', 'acceptResourceShareInvitation_resourceShareInvitationArn' - The Amazon Resource Name (ARN) of the invitation.
newAcceptResourceShareInvitation ::
  -- | 'resourceShareInvitationArn'
  Prelude.Text ->
  AcceptResourceShareInvitation
newAcceptResourceShareInvitation
  pResourceShareInvitationArn_ =
    AcceptResourceShareInvitation'
      { clientToken =
          Prelude.Nothing,
        resourceShareInvitationArn =
          pResourceShareInvitationArn_
      }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
acceptResourceShareInvitation_clientToken :: Lens.Lens' AcceptResourceShareInvitation (Prelude.Maybe Prelude.Text)
acceptResourceShareInvitation_clientToken = Lens.lens (\AcceptResourceShareInvitation' {clientToken} -> clientToken) (\s@AcceptResourceShareInvitation' {} a -> s {clientToken = a} :: AcceptResourceShareInvitation)

-- | The Amazon Resource Name (ARN) of the invitation.
acceptResourceShareInvitation_resourceShareInvitationArn :: Lens.Lens' AcceptResourceShareInvitation Prelude.Text
acceptResourceShareInvitation_resourceShareInvitationArn = Lens.lens (\AcceptResourceShareInvitation' {resourceShareInvitationArn} -> resourceShareInvitationArn) (\s@AcceptResourceShareInvitation' {} a -> s {resourceShareInvitationArn = a} :: AcceptResourceShareInvitation)

instance
  Core.AWSRequest
    AcceptResourceShareInvitation
  where
  type
    AWSResponse AcceptResourceShareInvitation =
      AcceptResourceShareInvitationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AcceptResourceShareInvitationResponse'
            Prelude.<$> (x Core..?> "clientToken")
            Prelude.<*> (x Core..?> "resourceShareInvitation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AcceptResourceShareInvitation
  where
  hashWithSalt _salt AcceptResourceShareInvitation' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` resourceShareInvitationArn

instance Prelude.NFData AcceptResourceShareInvitation where
  rnf AcceptResourceShareInvitation' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf resourceShareInvitationArn

instance Core.ToHeaders AcceptResourceShareInvitation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AcceptResourceShareInvitation where
  toJSON AcceptResourceShareInvitation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientToken" Core..=) Prelude.<$> clientToken,
            Prelude.Just
              ( "resourceShareInvitationArn"
                  Core..= resourceShareInvitationArn
              )
          ]
      )

instance Core.ToPath AcceptResourceShareInvitation where
  toPath =
    Prelude.const "/acceptresourceshareinvitation"

instance Core.ToQuery AcceptResourceShareInvitation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAcceptResourceShareInvitationResponse' smart constructor.
data AcceptResourceShareInvitationResponse = AcceptResourceShareInvitationResponse'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the invitation.
    resourceShareInvitation :: Prelude.Maybe ResourceShareInvitation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptResourceShareInvitationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'acceptResourceShareInvitationResponse_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'resourceShareInvitation', 'acceptResourceShareInvitationResponse_resourceShareInvitation' - Information about the invitation.
--
-- 'httpStatus', 'acceptResourceShareInvitationResponse_httpStatus' - The response's http status code.
newAcceptResourceShareInvitationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AcceptResourceShareInvitationResponse
newAcceptResourceShareInvitationResponse pHttpStatus_ =
  AcceptResourceShareInvitationResponse'
    { clientToken =
        Prelude.Nothing,
      resourceShareInvitation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
acceptResourceShareInvitationResponse_clientToken :: Lens.Lens' AcceptResourceShareInvitationResponse (Prelude.Maybe Prelude.Text)
acceptResourceShareInvitationResponse_clientToken = Lens.lens (\AcceptResourceShareInvitationResponse' {clientToken} -> clientToken) (\s@AcceptResourceShareInvitationResponse' {} a -> s {clientToken = a} :: AcceptResourceShareInvitationResponse)

-- | Information about the invitation.
acceptResourceShareInvitationResponse_resourceShareInvitation :: Lens.Lens' AcceptResourceShareInvitationResponse (Prelude.Maybe ResourceShareInvitation)
acceptResourceShareInvitationResponse_resourceShareInvitation = Lens.lens (\AcceptResourceShareInvitationResponse' {resourceShareInvitation} -> resourceShareInvitation) (\s@AcceptResourceShareInvitationResponse' {} a -> s {resourceShareInvitation = a} :: AcceptResourceShareInvitationResponse)

-- | The response's http status code.
acceptResourceShareInvitationResponse_httpStatus :: Lens.Lens' AcceptResourceShareInvitationResponse Prelude.Int
acceptResourceShareInvitationResponse_httpStatus = Lens.lens (\AcceptResourceShareInvitationResponse' {httpStatus} -> httpStatus) (\s@AcceptResourceShareInvitationResponse' {} a -> s {httpStatus = a} :: AcceptResourceShareInvitationResponse)

instance
  Prelude.NFData
    AcceptResourceShareInvitationResponse
  where
  rnf AcceptResourceShareInvitationResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf resourceShareInvitation
      `Prelude.seq` Prelude.rnf httpStatus
