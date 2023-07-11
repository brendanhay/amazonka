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
-- Module      : Amazonka.AlexaBusiness.SendInvitation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends an enrollment invitation email with a URL to a user. The URL is
-- valid for 30 days or until you call this operation again, whichever
-- comes first.
module Amazonka.AlexaBusiness.SendInvitation
  ( -- * Creating a Request
    SendInvitation (..),
    newSendInvitation,

    -- * Request Lenses
    sendInvitation_userArn,

    -- * Destructuring the Response
    SendInvitationResponse (..),
    newSendInvitationResponse,

    -- * Response Lenses
    sendInvitationResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSendInvitation' smart constructor.
data SendInvitation = SendInvitation'
  { -- | The ARN of the user to whom to send an invitation. Required.
    userArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendInvitation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userArn', 'sendInvitation_userArn' - The ARN of the user to whom to send an invitation. Required.
newSendInvitation ::
  SendInvitation
newSendInvitation =
  SendInvitation' {userArn = Prelude.Nothing}

-- | The ARN of the user to whom to send an invitation. Required.
sendInvitation_userArn :: Lens.Lens' SendInvitation (Prelude.Maybe Prelude.Text)
sendInvitation_userArn = Lens.lens (\SendInvitation' {userArn} -> userArn) (\s@SendInvitation' {} a -> s {userArn = a} :: SendInvitation)

instance Core.AWSRequest SendInvitation where
  type
    AWSResponse SendInvitation =
      SendInvitationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          SendInvitationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendInvitation where
  hashWithSalt _salt SendInvitation' {..} =
    _salt `Prelude.hashWithSalt` userArn

instance Prelude.NFData SendInvitation where
  rnf SendInvitation' {..} = Prelude.rnf userArn

instance Data.ToHeaders SendInvitation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.SendInvitation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SendInvitation where
  toJSON SendInvitation' {..} =
    Data.object
      ( Prelude.catMaybes
          [("UserArn" Data..=) Prelude.<$> userArn]
      )

instance Data.ToPath SendInvitation where
  toPath = Prelude.const "/"

instance Data.ToQuery SendInvitation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendInvitationResponse' smart constructor.
data SendInvitationResponse = SendInvitationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendInvitationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'sendInvitationResponse_httpStatus' - The response's http status code.
newSendInvitationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendInvitationResponse
newSendInvitationResponse pHttpStatus_ =
  SendInvitationResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
sendInvitationResponse_httpStatus :: Lens.Lens' SendInvitationResponse Prelude.Int
sendInvitationResponse_httpStatus = Lens.lens (\SendInvitationResponse' {httpStatus} -> httpStatus) (\s@SendInvitationResponse' {} a -> s {httpStatus = a} :: SendInvitationResponse)

instance Prelude.NFData SendInvitationResponse where
  rnf SendInvitationResponse' {..} =
    Prelude.rnf httpStatus
