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
-- Module      : Network.AWS.AlexaBusiness.SendInvitation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends an enrollment invitation email with a URL to a user. The URL is
-- valid for 30 days or until you call this operation again, whichever
-- comes first.
module Network.AWS.AlexaBusiness.SendInvitation
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

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSendInvitation' smart constructor.
data SendInvitation = SendInvitation'
  { -- | The ARN of the user to whom to send an invitation. Required.
    userArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  SendInvitation' {userArn = Core.Nothing}

-- | The ARN of the user to whom to send an invitation. Required.
sendInvitation_userArn :: Lens.Lens' SendInvitation (Core.Maybe Core.Text)
sendInvitation_userArn = Lens.lens (\SendInvitation' {userArn} -> userArn) (\s@SendInvitation' {} a -> s {userArn = a} :: SendInvitation)

instance Core.AWSRequest SendInvitation where
  type
    AWSResponse SendInvitation =
      SendInvitationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          SendInvitationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SendInvitation

instance Core.NFData SendInvitation

instance Core.ToHeaders SendInvitation where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.SendInvitation" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SendInvitation where
  toJSON SendInvitation' {..} =
    Core.object
      ( Core.catMaybes
          [("UserArn" Core..=) Core.<$> userArn]
      )

instance Core.ToPath SendInvitation where
  toPath = Core.const "/"

instance Core.ToQuery SendInvitation where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSendInvitationResponse' smart constructor.
data SendInvitationResponse = SendInvitationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  SendInvitationResponse
newSendInvitationResponse pHttpStatus_ =
  SendInvitationResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
sendInvitationResponse_httpStatus :: Lens.Lens' SendInvitationResponse Core.Int
sendInvitationResponse_httpStatus = Lens.lens (\SendInvitationResponse' {httpStatus} -> httpStatus) (\s@SendInvitationResponse' {} a -> s {httpStatus = a} :: SendInvitationResponse)

instance Core.NFData SendInvitationResponse
