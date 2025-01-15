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
-- Module      : Amazonka.IVSChat.CreateChatToken
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an encrypted token that is used by a chat participant to
-- establish an individual WebSocket chat connection to a room. When the
-- token is used to connect to chat, the connection is valid for the
-- session duration specified in the request. The token becomes invalid at
-- the token-expiration timestamp included in the response.
--
-- Use the @capabilities@ field to permit an end user to send messages or
-- moderate a room.
--
-- The @attributes@ field securely attaches structured data to the chat
-- session; the data is included within each message sent by the end user
-- and received by other participants in the room. Common use cases for
-- attributes include passing end-user profile data like an icon, display
-- name, colors, badges, and other display features.
--
-- Encryption keys are owned by Amazon IVS Chat and never used directly by
-- your application.
module Amazonka.IVSChat.CreateChatToken
  ( -- * Creating a Request
    CreateChatToken (..),
    newCreateChatToken,

    -- * Request Lenses
    createChatToken_attributes,
    createChatToken_capabilities,
    createChatToken_sessionDurationInMinutes,
    createChatToken_roomIdentifier,
    createChatToken_userId,

    -- * Destructuring the Response
    CreateChatTokenResponse (..),
    newCreateChatTokenResponse,

    -- * Response Lenses
    createChatTokenResponse_sessionExpirationTime,
    createChatTokenResponse_token,
    createChatTokenResponse_tokenExpirationTime,
    createChatTokenResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSChat.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateChatToken' smart constructor.
data CreateChatToken = CreateChatToken'
  { -- | Application-provided attributes to encode into the token and attach to a
    -- chat session. Map keys and values can contain UTF-8 encoded text. The
    -- maximum length of this field is 1 KB total.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Set of capabilities that the user is allowed to perform in the room.
    -- Default: None (the capability to view messages is implicitly included in
    -- all requests).
    capabilities :: Prelude.Maybe [ChatTokenCapability],
    -- | Session duration (in minutes), after which the session expires. Default:
    -- 60 (1 hour).
    sessionDurationInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | Identifier of the room that the client is trying to access. Currently
    -- this must be an ARN.
    roomIdentifier :: Prelude.Text,
    -- | Application-provided ID that uniquely identifies the user associated
    -- with this token. This can be any UTF-8 encoded text.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateChatToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'createChatToken_attributes' - Application-provided attributes to encode into the token and attach to a
-- chat session. Map keys and values can contain UTF-8 encoded text. The
-- maximum length of this field is 1 KB total.
--
-- 'capabilities', 'createChatToken_capabilities' - Set of capabilities that the user is allowed to perform in the room.
-- Default: None (the capability to view messages is implicitly included in
-- all requests).
--
-- 'sessionDurationInMinutes', 'createChatToken_sessionDurationInMinutes' - Session duration (in minutes), after which the session expires. Default:
-- 60 (1 hour).
--
-- 'roomIdentifier', 'createChatToken_roomIdentifier' - Identifier of the room that the client is trying to access. Currently
-- this must be an ARN.
--
-- 'userId', 'createChatToken_userId' - Application-provided ID that uniquely identifies the user associated
-- with this token. This can be any UTF-8 encoded text.
newCreateChatToken ::
  -- | 'roomIdentifier'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  CreateChatToken
newCreateChatToken pRoomIdentifier_ pUserId_ =
  CreateChatToken'
    { attributes = Prelude.Nothing,
      capabilities = Prelude.Nothing,
      sessionDurationInMinutes = Prelude.Nothing,
      roomIdentifier = pRoomIdentifier_,
      userId = pUserId_
    }

-- | Application-provided attributes to encode into the token and attach to a
-- chat session. Map keys and values can contain UTF-8 encoded text. The
-- maximum length of this field is 1 KB total.
createChatToken_attributes :: Lens.Lens' CreateChatToken (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createChatToken_attributes = Lens.lens (\CreateChatToken' {attributes} -> attributes) (\s@CreateChatToken' {} a -> s {attributes = a} :: CreateChatToken) Prelude.. Lens.mapping Lens.coerced

-- | Set of capabilities that the user is allowed to perform in the room.
-- Default: None (the capability to view messages is implicitly included in
-- all requests).
createChatToken_capabilities :: Lens.Lens' CreateChatToken (Prelude.Maybe [ChatTokenCapability])
createChatToken_capabilities = Lens.lens (\CreateChatToken' {capabilities} -> capabilities) (\s@CreateChatToken' {} a -> s {capabilities = a} :: CreateChatToken) Prelude.. Lens.mapping Lens.coerced

-- | Session duration (in minutes), after which the session expires. Default:
-- 60 (1 hour).
createChatToken_sessionDurationInMinutes :: Lens.Lens' CreateChatToken (Prelude.Maybe Prelude.Natural)
createChatToken_sessionDurationInMinutes = Lens.lens (\CreateChatToken' {sessionDurationInMinutes} -> sessionDurationInMinutes) (\s@CreateChatToken' {} a -> s {sessionDurationInMinutes = a} :: CreateChatToken)

-- | Identifier of the room that the client is trying to access. Currently
-- this must be an ARN.
createChatToken_roomIdentifier :: Lens.Lens' CreateChatToken Prelude.Text
createChatToken_roomIdentifier = Lens.lens (\CreateChatToken' {roomIdentifier} -> roomIdentifier) (\s@CreateChatToken' {} a -> s {roomIdentifier = a} :: CreateChatToken)

-- | Application-provided ID that uniquely identifies the user associated
-- with this token. This can be any UTF-8 encoded text.
createChatToken_userId :: Lens.Lens' CreateChatToken Prelude.Text
createChatToken_userId = Lens.lens (\CreateChatToken' {userId} -> userId) (\s@CreateChatToken' {} a -> s {userId = a} :: CreateChatToken)

instance Core.AWSRequest CreateChatToken where
  type
    AWSResponse CreateChatToken =
      CreateChatTokenResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateChatTokenResponse'
            Prelude.<$> (x Data..?> "sessionExpirationTime")
            Prelude.<*> (x Data..?> "token")
            Prelude.<*> (x Data..?> "tokenExpirationTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateChatToken where
  hashWithSalt _salt CreateChatToken' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` capabilities
      `Prelude.hashWithSalt` sessionDurationInMinutes
      `Prelude.hashWithSalt` roomIdentifier
      `Prelude.hashWithSalt` userId

instance Prelude.NFData CreateChatToken where
  rnf CreateChatToken' {..} =
    Prelude.rnf attributes `Prelude.seq`
      Prelude.rnf capabilities `Prelude.seq`
        Prelude.rnf sessionDurationInMinutes `Prelude.seq`
          Prelude.rnf roomIdentifier `Prelude.seq`
            Prelude.rnf userId

instance Data.ToHeaders CreateChatToken where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateChatToken where
  toJSON CreateChatToken' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attributes" Data..=) Prelude.<$> attributes,
            ("capabilities" Data..=) Prelude.<$> capabilities,
            ("sessionDurationInMinutes" Data..=)
              Prelude.<$> sessionDurationInMinutes,
            Prelude.Just
              ("roomIdentifier" Data..= roomIdentifier),
            Prelude.Just ("userId" Data..= userId)
          ]
      )

instance Data.ToPath CreateChatToken where
  toPath = Prelude.const "/CreateChatToken"

instance Data.ToQuery CreateChatToken where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateChatTokenResponse' smart constructor.
data CreateChatTokenResponse = CreateChatTokenResponse'
  { -- | Time after which an end user\'s session is no longer valid. This is an
    -- ISO 8601 timestamp; /note that this is returned as a string/.
    sessionExpirationTime :: Prelude.Maybe Data.ISO8601,
    -- | The issued client token, encrypted.
    token :: Prelude.Maybe Prelude.Text,
    -- | Time after which the token is no longer valid and cannot be used to
    -- connect to a room. This is an ISO 8601 timestamp; /note that this is
    -- returned as a string/.
    tokenExpirationTime :: Prelude.Maybe Data.ISO8601,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateChatTokenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionExpirationTime', 'createChatTokenResponse_sessionExpirationTime' - Time after which an end user\'s session is no longer valid. This is an
-- ISO 8601 timestamp; /note that this is returned as a string/.
--
-- 'token', 'createChatTokenResponse_token' - The issued client token, encrypted.
--
-- 'tokenExpirationTime', 'createChatTokenResponse_tokenExpirationTime' - Time after which the token is no longer valid and cannot be used to
-- connect to a room. This is an ISO 8601 timestamp; /note that this is
-- returned as a string/.
--
-- 'httpStatus', 'createChatTokenResponse_httpStatus' - The response's http status code.
newCreateChatTokenResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateChatTokenResponse
newCreateChatTokenResponse pHttpStatus_ =
  CreateChatTokenResponse'
    { sessionExpirationTime =
        Prelude.Nothing,
      token = Prelude.Nothing,
      tokenExpirationTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Time after which an end user\'s session is no longer valid. This is an
-- ISO 8601 timestamp; /note that this is returned as a string/.
createChatTokenResponse_sessionExpirationTime :: Lens.Lens' CreateChatTokenResponse (Prelude.Maybe Prelude.UTCTime)
createChatTokenResponse_sessionExpirationTime = Lens.lens (\CreateChatTokenResponse' {sessionExpirationTime} -> sessionExpirationTime) (\s@CreateChatTokenResponse' {} a -> s {sessionExpirationTime = a} :: CreateChatTokenResponse) Prelude.. Lens.mapping Data._Time

-- | The issued client token, encrypted.
createChatTokenResponse_token :: Lens.Lens' CreateChatTokenResponse (Prelude.Maybe Prelude.Text)
createChatTokenResponse_token = Lens.lens (\CreateChatTokenResponse' {token} -> token) (\s@CreateChatTokenResponse' {} a -> s {token = a} :: CreateChatTokenResponse)

-- | Time after which the token is no longer valid and cannot be used to
-- connect to a room. This is an ISO 8601 timestamp; /note that this is
-- returned as a string/.
createChatTokenResponse_tokenExpirationTime :: Lens.Lens' CreateChatTokenResponse (Prelude.Maybe Prelude.UTCTime)
createChatTokenResponse_tokenExpirationTime = Lens.lens (\CreateChatTokenResponse' {tokenExpirationTime} -> tokenExpirationTime) (\s@CreateChatTokenResponse' {} a -> s {tokenExpirationTime = a} :: CreateChatTokenResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
createChatTokenResponse_httpStatus :: Lens.Lens' CreateChatTokenResponse Prelude.Int
createChatTokenResponse_httpStatus = Lens.lens (\CreateChatTokenResponse' {httpStatus} -> httpStatus) (\s@CreateChatTokenResponse' {} a -> s {httpStatus = a} :: CreateChatTokenResponse)

instance Prelude.NFData CreateChatTokenResponse where
  rnf CreateChatTokenResponse' {..} =
    Prelude.rnf sessionExpirationTime `Prelude.seq`
      Prelude.rnf token `Prelude.seq`
        Prelude.rnf tokenExpirationTime `Prelude.seq`
          Prelude.rnf httpStatus
