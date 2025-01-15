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
-- Module      : Amazonka.IVSChat.DisconnectUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disconnects all connections using a specified user ID from a room. This
-- replicates the
-- <https://docs.aws.amazon.com/ivs/latest/chatmsgapireference/actions-disconnectuser-publish.html DisconnectUser>
-- WebSocket operation in the Amazon IVS Chat Messaging API.
module Amazonka.IVSChat.DisconnectUser
  ( -- * Creating a Request
    DisconnectUser (..),
    newDisconnectUser,

    -- * Request Lenses
    disconnectUser_reason,
    disconnectUser_roomIdentifier,
    disconnectUser_userId,

    -- * Destructuring the Response
    DisconnectUserResponse (..),
    newDisconnectUserResponse,

    -- * Response Lenses
    disconnectUserResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSChat.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisconnectUser' smart constructor.
data DisconnectUser = DisconnectUser'
  { -- | Reason for disconnecting the user.
    reason :: Prelude.Maybe Prelude.Text,
    -- | Identifier of the room from which the user\'s clients should be
    -- disconnected. Currently this must be an ARN.
    roomIdentifier :: Prelude.Text,
    -- | ID of the user (connection) to disconnect from the room.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisconnectUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reason', 'disconnectUser_reason' - Reason for disconnecting the user.
--
-- 'roomIdentifier', 'disconnectUser_roomIdentifier' - Identifier of the room from which the user\'s clients should be
-- disconnected. Currently this must be an ARN.
--
-- 'userId', 'disconnectUser_userId' - ID of the user (connection) to disconnect from the room.
newDisconnectUser ::
  -- | 'roomIdentifier'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  DisconnectUser
newDisconnectUser pRoomIdentifier_ pUserId_ =
  DisconnectUser'
    { reason = Prelude.Nothing,
      roomIdentifier = pRoomIdentifier_,
      userId = pUserId_
    }

-- | Reason for disconnecting the user.
disconnectUser_reason :: Lens.Lens' DisconnectUser (Prelude.Maybe Prelude.Text)
disconnectUser_reason = Lens.lens (\DisconnectUser' {reason} -> reason) (\s@DisconnectUser' {} a -> s {reason = a} :: DisconnectUser)

-- | Identifier of the room from which the user\'s clients should be
-- disconnected. Currently this must be an ARN.
disconnectUser_roomIdentifier :: Lens.Lens' DisconnectUser Prelude.Text
disconnectUser_roomIdentifier = Lens.lens (\DisconnectUser' {roomIdentifier} -> roomIdentifier) (\s@DisconnectUser' {} a -> s {roomIdentifier = a} :: DisconnectUser)

-- | ID of the user (connection) to disconnect from the room.
disconnectUser_userId :: Lens.Lens' DisconnectUser Prelude.Text
disconnectUser_userId = Lens.lens (\DisconnectUser' {userId} -> userId) (\s@DisconnectUser' {} a -> s {userId = a} :: DisconnectUser)

instance Core.AWSRequest DisconnectUser where
  type
    AWSResponse DisconnectUser =
      DisconnectUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisconnectUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisconnectUser where
  hashWithSalt _salt DisconnectUser' {..} =
    _salt
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` roomIdentifier
      `Prelude.hashWithSalt` userId

instance Prelude.NFData DisconnectUser where
  rnf DisconnectUser' {..} =
    Prelude.rnf reason `Prelude.seq`
      Prelude.rnf roomIdentifier `Prelude.seq`
        Prelude.rnf userId

instance Data.ToHeaders DisconnectUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisconnectUser where
  toJSON DisconnectUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("reason" Data..=) Prelude.<$> reason,
            Prelude.Just
              ("roomIdentifier" Data..= roomIdentifier),
            Prelude.Just ("userId" Data..= userId)
          ]
      )

instance Data.ToPath DisconnectUser where
  toPath = Prelude.const "/DisconnectUser"

instance Data.ToQuery DisconnectUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisconnectUserResponse' smart constructor.
data DisconnectUserResponse = DisconnectUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisconnectUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disconnectUserResponse_httpStatus' - The response's http status code.
newDisconnectUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisconnectUserResponse
newDisconnectUserResponse pHttpStatus_ =
  DisconnectUserResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
disconnectUserResponse_httpStatus :: Lens.Lens' DisconnectUserResponse Prelude.Int
disconnectUserResponse_httpStatus = Lens.lens (\DisconnectUserResponse' {httpStatus} -> httpStatus) (\s@DisconnectUserResponse' {} a -> s {httpStatus = a} :: DisconnectUserResponse)

instance Prelude.NFData DisconnectUserResponse where
  rnf DisconnectUserResponse' {..} =
    Prelude.rnf httpStatus
