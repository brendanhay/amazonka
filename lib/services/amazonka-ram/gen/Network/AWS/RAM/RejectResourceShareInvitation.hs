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
-- Module      : Network.AWS.RAM.RejectResourceShareInvitation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects an invitation to a resource share from another Amazon Web
-- Services account.
module Network.AWS.RAM.RejectResourceShareInvitation
  ( -- * Creating a Request
    RejectResourceShareInvitation (..),
    newRejectResourceShareInvitation,

    -- * Request Lenses
    rejectResourceShareInvitation_clientToken,
    rejectResourceShareInvitation_resourceShareInvitationArn,

    -- * Destructuring the Response
    RejectResourceShareInvitationResponse (..),
    newRejectResourceShareInvitationResponse,

    -- * Response Lenses
    rejectResourceShareInvitationResponse_clientToken,
    rejectResourceShareInvitationResponse_resourceShareInvitation,
    rejectResourceShareInvitationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RAM.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRejectResourceShareInvitation' smart constructor.
data RejectResourceShareInvitation = RejectResourceShareInvitation'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the invitation.
    resourceShareInvitationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectResourceShareInvitation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'rejectResourceShareInvitation_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'resourceShareInvitationArn', 'rejectResourceShareInvitation_resourceShareInvitationArn' - The Amazon Resource Name (ARN) of the invitation.
newRejectResourceShareInvitation ::
  -- | 'resourceShareInvitationArn'
  Prelude.Text ->
  RejectResourceShareInvitation
newRejectResourceShareInvitation
  pResourceShareInvitationArn_ =
    RejectResourceShareInvitation'
      { clientToken =
          Prelude.Nothing,
        resourceShareInvitationArn =
          pResourceShareInvitationArn_
      }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
rejectResourceShareInvitation_clientToken :: Lens.Lens' RejectResourceShareInvitation (Prelude.Maybe Prelude.Text)
rejectResourceShareInvitation_clientToken = Lens.lens (\RejectResourceShareInvitation' {clientToken} -> clientToken) (\s@RejectResourceShareInvitation' {} a -> s {clientToken = a} :: RejectResourceShareInvitation)

-- | The Amazon Resource Name (ARN) of the invitation.
rejectResourceShareInvitation_resourceShareInvitationArn :: Lens.Lens' RejectResourceShareInvitation Prelude.Text
rejectResourceShareInvitation_resourceShareInvitationArn = Lens.lens (\RejectResourceShareInvitation' {resourceShareInvitationArn} -> resourceShareInvitationArn) (\s@RejectResourceShareInvitation' {} a -> s {resourceShareInvitationArn = a} :: RejectResourceShareInvitation)

instance
  Core.AWSRequest
    RejectResourceShareInvitation
  where
  type
    AWSResponse RejectResourceShareInvitation =
      RejectResourceShareInvitationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RejectResourceShareInvitationResponse'
            Prelude.<$> (x Core..?> "clientToken")
            Prelude.<*> (x Core..?> "resourceShareInvitation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RejectResourceShareInvitation

instance Prelude.NFData RejectResourceShareInvitation

instance Core.ToHeaders RejectResourceShareInvitation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RejectResourceShareInvitation where
  toJSON RejectResourceShareInvitation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientToken" Core..=) Prelude.<$> clientToken,
            Prelude.Just
              ( "resourceShareInvitationArn"
                  Core..= resourceShareInvitationArn
              )
          ]
      )

instance Core.ToPath RejectResourceShareInvitation where
  toPath =
    Prelude.const "/rejectresourceshareinvitation"

instance Core.ToQuery RejectResourceShareInvitation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRejectResourceShareInvitationResponse' smart constructor.
data RejectResourceShareInvitationResponse = RejectResourceShareInvitationResponse'
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
-- Create a value of 'RejectResourceShareInvitationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'rejectResourceShareInvitationResponse_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'resourceShareInvitation', 'rejectResourceShareInvitationResponse_resourceShareInvitation' - Information about the invitation.
--
-- 'httpStatus', 'rejectResourceShareInvitationResponse_httpStatus' - The response's http status code.
newRejectResourceShareInvitationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RejectResourceShareInvitationResponse
newRejectResourceShareInvitationResponse pHttpStatus_ =
  RejectResourceShareInvitationResponse'
    { clientToken =
        Prelude.Nothing,
      resourceShareInvitation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
rejectResourceShareInvitationResponse_clientToken :: Lens.Lens' RejectResourceShareInvitationResponse (Prelude.Maybe Prelude.Text)
rejectResourceShareInvitationResponse_clientToken = Lens.lens (\RejectResourceShareInvitationResponse' {clientToken} -> clientToken) (\s@RejectResourceShareInvitationResponse' {} a -> s {clientToken = a} :: RejectResourceShareInvitationResponse)

-- | Information about the invitation.
rejectResourceShareInvitationResponse_resourceShareInvitation :: Lens.Lens' RejectResourceShareInvitationResponse (Prelude.Maybe ResourceShareInvitation)
rejectResourceShareInvitationResponse_resourceShareInvitation = Lens.lens (\RejectResourceShareInvitationResponse' {resourceShareInvitation} -> resourceShareInvitation) (\s@RejectResourceShareInvitationResponse' {} a -> s {resourceShareInvitation = a} :: RejectResourceShareInvitationResponse)

-- | The response's http status code.
rejectResourceShareInvitationResponse_httpStatus :: Lens.Lens' RejectResourceShareInvitationResponse Prelude.Int
rejectResourceShareInvitationResponse_httpStatus = Lens.lens (\RejectResourceShareInvitationResponse' {httpStatus} -> httpStatus) (\s@RejectResourceShareInvitationResponse' {} a -> s {httpStatus = a} :: RejectResourceShareInvitationResponse)

instance
  Prelude.NFData
    RejectResourceShareInvitationResponse
