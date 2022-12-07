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
-- Module      : Amazonka.ManagedBlockChain.RejectInvitation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects an invitation to join a network. This action can be called by a
-- principal in an Amazon Web Services account that has received an
-- invitation to create a member and join a network.
--
-- Applies only to Hyperledger Fabric.
module Amazonka.ManagedBlockChain.RejectInvitation
  ( -- * Creating a Request
    RejectInvitation (..),
    newRejectInvitation,

    -- * Request Lenses
    rejectInvitation_invitationId,

    -- * Destructuring the Response
    RejectInvitationResponse (..),
    newRejectInvitationResponse,

    -- * Response Lenses
    rejectInvitationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRejectInvitation' smart constructor.
data RejectInvitation = RejectInvitation'
  { -- | The unique identifier of the invitation to reject.
    invitationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectInvitation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invitationId', 'rejectInvitation_invitationId' - The unique identifier of the invitation to reject.
newRejectInvitation ::
  -- | 'invitationId'
  Prelude.Text ->
  RejectInvitation
newRejectInvitation pInvitationId_ =
  RejectInvitation' {invitationId = pInvitationId_}

-- | The unique identifier of the invitation to reject.
rejectInvitation_invitationId :: Lens.Lens' RejectInvitation Prelude.Text
rejectInvitation_invitationId = Lens.lens (\RejectInvitation' {invitationId} -> invitationId) (\s@RejectInvitation' {} a -> s {invitationId = a} :: RejectInvitation)

instance Core.AWSRequest RejectInvitation where
  type
    AWSResponse RejectInvitation =
      RejectInvitationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RejectInvitationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RejectInvitation where
  hashWithSalt _salt RejectInvitation' {..} =
    _salt `Prelude.hashWithSalt` invitationId

instance Prelude.NFData RejectInvitation where
  rnf RejectInvitation' {..} = Prelude.rnf invitationId

instance Data.ToHeaders RejectInvitation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath RejectInvitation where
  toPath RejectInvitation' {..} =
    Prelude.mconcat
      ["/invitations/", Data.toBS invitationId]

instance Data.ToQuery RejectInvitation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRejectInvitationResponse' smart constructor.
data RejectInvitationResponse = RejectInvitationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectInvitationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'rejectInvitationResponse_httpStatus' - The response's http status code.
newRejectInvitationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RejectInvitationResponse
newRejectInvitationResponse pHttpStatus_ =
  RejectInvitationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
rejectInvitationResponse_httpStatus :: Lens.Lens' RejectInvitationResponse Prelude.Int
rejectInvitationResponse_httpStatus = Lens.lens (\RejectInvitationResponse' {httpStatus} -> httpStatus) (\s@RejectInvitationResponse' {} a -> s {httpStatus = a} :: RejectInvitationResponse)

instance Prelude.NFData RejectInvitationResponse where
  rnf RejectInvitationResponse' {..} =
    Prelude.rnf httpStatus
