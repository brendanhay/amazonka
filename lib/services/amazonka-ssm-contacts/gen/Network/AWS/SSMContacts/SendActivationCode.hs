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
-- Module      : Network.AWS.SSMContacts.SendActivationCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends an activation code to a contact channel. The contact can use this
-- code to activate the contact channel in the console or with the
-- @ActivateChannel@ operation. Incident Manager can\'t engage a contact
-- channel until it has been activated.
module Network.AWS.SSMContacts.SendActivationCode
  ( -- * Creating a Request
    SendActivationCode (..),
    newSendActivationCode,

    -- * Request Lenses
    sendActivationCode_contactChannelId,

    -- * Destructuring the Response
    SendActivationCodeResponse (..),
    newSendActivationCodeResponse,

    -- * Response Lenses
    sendActivationCodeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSMContacts.Types

-- | /See:/ 'newSendActivationCode' smart constructor.
data SendActivationCode = SendActivationCode'
  { -- | The Amazon Resource Name (ARN) of the contact channel.
    contactChannelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendActivationCode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactChannelId', 'sendActivationCode_contactChannelId' - The Amazon Resource Name (ARN) of the contact channel.
newSendActivationCode ::
  -- | 'contactChannelId'
  Prelude.Text ->
  SendActivationCode
newSendActivationCode pContactChannelId_ =
  SendActivationCode'
    { contactChannelId =
        pContactChannelId_
    }

-- | The Amazon Resource Name (ARN) of the contact channel.
sendActivationCode_contactChannelId :: Lens.Lens' SendActivationCode Prelude.Text
sendActivationCode_contactChannelId = Lens.lens (\SendActivationCode' {contactChannelId} -> contactChannelId) (\s@SendActivationCode' {} a -> s {contactChannelId = a} :: SendActivationCode)

instance Core.AWSRequest SendActivationCode where
  type
    AWSResponse SendActivationCode =
      SendActivationCodeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          SendActivationCodeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendActivationCode

instance Prelude.NFData SendActivationCode

instance Core.ToHeaders SendActivationCode where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SSMContacts.SendActivationCode" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SendActivationCode where
  toJSON SendActivationCode' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ContactChannelId" Core..= contactChannelId)
          ]
      )

instance Core.ToPath SendActivationCode where
  toPath = Prelude.const "/"

instance Core.ToQuery SendActivationCode where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendActivationCodeResponse' smart constructor.
data SendActivationCodeResponse = SendActivationCodeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendActivationCodeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'sendActivationCodeResponse_httpStatus' - The response's http status code.
newSendActivationCodeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendActivationCodeResponse
newSendActivationCodeResponse pHttpStatus_ =
  SendActivationCodeResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
sendActivationCodeResponse_httpStatus :: Lens.Lens' SendActivationCodeResponse Prelude.Int
sendActivationCodeResponse_httpStatus = Lens.lens (\SendActivationCodeResponse' {httpStatus} -> httpStatus) (\s@SendActivationCodeResponse' {} a -> s {httpStatus = a} :: SendActivationCodeResponse)

instance Prelude.NFData SendActivationCodeResponse
