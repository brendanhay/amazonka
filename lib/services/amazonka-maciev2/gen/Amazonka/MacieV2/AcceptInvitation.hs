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
-- Module      : Amazonka.MacieV2.AcceptInvitation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts an Amazon Macie membership invitation that was received from a
-- specific account.
module Amazonka.MacieV2.AcceptInvitation
  ( -- * Creating a Request
    AcceptInvitation (..),
    newAcceptInvitation,

    -- * Request Lenses
    acceptInvitation_administratorAccountId,
    acceptInvitation_masterAccount,
    acceptInvitation_invitationId,

    -- * Destructuring the Response
    AcceptInvitationResponse (..),
    newAcceptInvitationResponse,

    -- * Response Lenses
    acceptInvitationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAcceptInvitation' smart constructor.
data AcceptInvitation = AcceptInvitation'
  { -- | The Amazon Web Services account ID for the account that sent the
    -- invitation.
    administratorAccountId :: Prelude.Maybe Prelude.Text,
    -- | (Deprecated) The Amazon Web Services account ID for the account that
    -- sent the invitation. This property has been replaced by the
    -- administratorAccountId property and is retained only for backward
    -- compatibility.
    masterAccount :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the invitation to accept.
    invitationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptInvitation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'administratorAccountId', 'acceptInvitation_administratorAccountId' - The Amazon Web Services account ID for the account that sent the
-- invitation.
--
-- 'masterAccount', 'acceptInvitation_masterAccount' - (Deprecated) The Amazon Web Services account ID for the account that
-- sent the invitation. This property has been replaced by the
-- administratorAccountId property and is retained only for backward
-- compatibility.
--
-- 'invitationId', 'acceptInvitation_invitationId' - The unique identifier for the invitation to accept.
newAcceptInvitation ::
  -- | 'invitationId'
  Prelude.Text ->
  AcceptInvitation
newAcceptInvitation pInvitationId_ =
  AcceptInvitation'
    { administratorAccountId =
        Prelude.Nothing,
      masterAccount = Prelude.Nothing,
      invitationId = pInvitationId_
    }

-- | The Amazon Web Services account ID for the account that sent the
-- invitation.
acceptInvitation_administratorAccountId :: Lens.Lens' AcceptInvitation (Prelude.Maybe Prelude.Text)
acceptInvitation_administratorAccountId = Lens.lens (\AcceptInvitation' {administratorAccountId} -> administratorAccountId) (\s@AcceptInvitation' {} a -> s {administratorAccountId = a} :: AcceptInvitation)

-- | (Deprecated) The Amazon Web Services account ID for the account that
-- sent the invitation. This property has been replaced by the
-- administratorAccountId property and is retained only for backward
-- compatibility.
acceptInvitation_masterAccount :: Lens.Lens' AcceptInvitation (Prelude.Maybe Prelude.Text)
acceptInvitation_masterAccount = Lens.lens (\AcceptInvitation' {masterAccount} -> masterAccount) (\s@AcceptInvitation' {} a -> s {masterAccount = a} :: AcceptInvitation)

-- | The unique identifier for the invitation to accept.
acceptInvitation_invitationId :: Lens.Lens' AcceptInvitation Prelude.Text
acceptInvitation_invitationId = Lens.lens (\AcceptInvitation' {invitationId} -> invitationId) (\s@AcceptInvitation' {} a -> s {invitationId = a} :: AcceptInvitation)

instance Core.AWSRequest AcceptInvitation where
  type
    AWSResponse AcceptInvitation =
      AcceptInvitationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AcceptInvitationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AcceptInvitation where
  hashWithSalt _salt AcceptInvitation' {..} =
    _salt
      `Prelude.hashWithSalt` administratorAccountId
      `Prelude.hashWithSalt` masterAccount
      `Prelude.hashWithSalt` invitationId

instance Prelude.NFData AcceptInvitation where
  rnf AcceptInvitation' {..} =
    Prelude.rnf administratorAccountId
      `Prelude.seq` Prelude.rnf masterAccount
      `Prelude.seq` Prelude.rnf invitationId

instance Data.ToHeaders AcceptInvitation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AcceptInvitation where
  toJSON AcceptInvitation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("administratorAccountId" Data..=)
              Prelude.<$> administratorAccountId,
            ("masterAccount" Data..=) Prelude.<$> masterAccount,
            Prelude.Just ("invitationId" Data..= invitationId)
          ]
      )

instance Data.ToPath AcceptInvitation where
  toPath = Prelude.const "/invitations/accept"

instance Data.ToQuery AcceptInvitation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAcceptInvitationResponse' smart constructor.
data AcceptInvitationResponse = AcceptInvitationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptInvitationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'acceptInvitationResponse_httpStatus' - The response's http status code.
newAcceptInvitationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AcceptInvitationResponse
newAcceptInvitationResponse pHttpStatus_ =
  AcceptInvitationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
acceptInvitationResponse_httpStatus :: Lens.Lens' AcceptInvitationResponse Prelude.Int
acceptInvitationResponse_httpStatus = Lens.lens (\AcceptInvitationResponse' {httpStatus} -> httpStatus) (\s@AcceptInvitationResponse' {} a -> s {httpStatus = a} :: AcceptInvitationResponse)

instance Prelude.NFData AcceptInvitationResponse where
  rnf AcceptInvitationResponse' {..} =
    Prelude.rnf httpStatus
