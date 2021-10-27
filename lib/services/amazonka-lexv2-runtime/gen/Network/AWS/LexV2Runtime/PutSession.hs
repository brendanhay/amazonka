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
-- Module      : Network.AWS.LexV2Runtime.PutSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new session or modifies an existing session with an Amazon Lex
-- V2 bot. Use this operation to enable your application to set the state
-- of the bot.
module Network.AWS.LexV2Runtime.PutSession
  ( -- * Creating a Request
    PutSession (..),
    newPutSession,

    -- * Request Lenses
    putSession_responseContentType,
    putSession_messages,
    putSession_requestAttributes,
    putSession_botId,
    putSession_botAliasId,
    putSession_localeId,
    putSession_sessionState,
    putSession_sessionId,

    -- * Destructuring the Response
    PutSessionResponse (..),
    newPutSessionResponse,

    -- * Response Lenses
    putSessionResponse_sessionState,
    putSessionResponse_messages,
    putSessionResponse_sessionId,
    putSessionResponse_requestAttributes,
    putSessionResponse_contentType,
    putSessionResponse_httpStatus,
    putSessionResponse_audioStream,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Runtime.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutSession' smart constructor.
data PutSession = PutSession'
  { -- | The message that Amazon Lex V2 returns in the response can be either
    -- text or speech depending on the value of this parameter.
    --
    -- -   If the value is @text\/plain; charset=utf-8@, Amazon Lex V2 returns
    --     text in the response.
    responseContentType :: Prelude.Maybe Prelude.Text,
    -- | A list of messages to send to the user. Messages are sent in the order
    -- that they are defined in the list.
    messages :: Prelude.Maybe [Message],
    -- | Request-specific information passed between Amazon Lex V2 and the client
    -- application.
    --
    -- The namespace @x-amz-lex:@ is reserved for special attributes. Don\'t
    -- create any request attributes with the prefix @x-amz-lex:@.
    requestAttributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The identifier of the bot that receives the session data.
    botId :: Prelude.Text,
    -- | The alias identifier of the bot that receives the session data.
    botAliasId :: Prelude.Text,
    -- | The locale where the session is in use.
    localeId :: Prelude.Text,
    -- | Sets the state of the session with the user. You can use this to set the
    -- current intent, attributes, context, and dialog action. Use the dialog
    -- action to determine the next step that Amazon Lex V2 should use in the
    -- conversation with the user.
    sessionState :: SessionState,
    -- | The identifier of the session that receives the session data.
    sessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'responseContentType', 'putSession_responseContentType' - The message that Amazon Lex V2 returns in the response can be either
-- text or speech depending on the value of this parameter.
--
-- -   If the value is @text\/plain; charset=utf-8@, Amazon Lex V2 returns
--     text in the response.
--
-- 'messages', 'putSession_messages' - A list of messages to send to the user. Messages are sent in the order
-- that they are defined in the list.
--
-- 'requestAttributes', 'putSession_requestAttributes' - Request-specific information passed between Amazon Lex V2 and the client
-- application.
--
-- The namespace @x-amz-lex:@ is reserved for special attributes. Don\'t
-- create any request attributes with the prefix @x-amz-lex:@.
--
-- 'botId', 'putSession_botId' - The identifier of the bot that receives the session data.
--
-- 'botAliasId', 'putSession_botAliasId' - The alias identifier of the bot that receives the session data.
--
-- 'localeId', 'putSession_localeId' - The locale where the session is in use.
--
-- 'sessionState', 'putSession_sessionState' - Sets the state of the session with the user. You can use this to set the
-- current intent, attributes, context, and dialog action. Use the dialog
-- action to determine the next step that Amazon Lex V2 should use in the
-- conversation with the user.
--
-- 'sessionId', 'putSession_sessionId' - The identifier of the session that receives the session data.
newPutSession ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botAliasId'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  -- | 'sessionState'
  SessionState ->
  -- | 'sessionId'
  Prelude.Text ->
  PutSession
newPutSession
  pBotId_
  pBotAliasId_
  pLocaleId_
  pSessionState_
  pSessionId_ =
    PutSession'
      { responseContentType = Prelude.Nothing,
        messages = Prelude.Nothing,
        requestAttributes = Prelude.Nothing,
        botId = pBotId_,
        botAliasId = pBotAliasId_,
        localeId = pLocaleId_,
        sessionState = pSessionState_,
        sessionId = pSessionId_
      }

-- | The message that Amazon Lex V2 returns in the response can be either
-- text or speech depending on the value of this parameter.
--
-- -   If the value is @text\/plain; charset=utf-8@, Amazon Lex V2 returns
--     text in the response.
putSession_responseContentType :: Lens.Lens' PutSession (Prelude.Maybe Prelude.Text)
putSession_responseContentType = Lens.lens (\PutSession' {responseContentType} -> responseContentType) (\s@PutSession' {} a -> s {responseContentType = a} :: PutSession)

-- | A list of messages to send to the user. Messages are sent in the order
-- that they are defined in the list.
putSession_messages :: Lens.Lens' PutSession (Prelude.Maybe [Message])
putSession_messages = Lens.lens (\PutSession' {messages} -> messages) (\s@PutSession' {} a -> s {messages = a} :: PutSession) Prelude.. Lens.mapping Lens.coerced

-- | Request-specific information passed between Amazon Lex V2 and the client
-- application.
--
-- The namespace @x-amz-lex:@ is reserved for special attributes. Don\'t
-- create any request attributes with the prefix @x-amz-lex:@.
putSession_requestAttributes :: Lens.Lens' PutSession (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
putSession_requestAttributes = Lens.lens (\PutSession' {requestAttributes} -> requestAttributes) (\s@PutSession' {} a -> s {requestAttributes = a} :: PutSession) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the bot that receives the session data.
putSession_botId :: Lens.Lens' PutSession Prelude.Text
putSession_botId = Lens.lens (\PutSession' {botId} -> botId) (\s@PutSession' {} a -> s {botId = a} :: PutSession)

-- | The alias identifier of the bot that receives the session data.
putSession_botAliasId :: Lens.Lens' PutSession Prelude.Text
putSession_botAliasId = Lens.lens (\PutSession' {botAliasId} -> botAliasId) (\s@PutSession' {} a -> s {botAliasId = a} :: PutSession)

-- | The locale where the session is in use.
putSession_localeId :: Lens.Lens' PutSession Prelude.Text
putSession_localeId = Lens.lens (\PutSession' {localeId} -> localeId) (\s@PutSession' {} a -> s {localeId = a} :: PutSession)

-- | Sets the state of the session with the user. You can use this to set the
-- current intent, attributes, context, and dialog action. Use the dialog
-- action to determine the next step that Amazon Lex V2 should use in the
-- conversation with the user.
putSession_sessionState :: Lens.Lens' PutSession SessionState
putSession_sessionState = Lens.lens (\PutSession' {sessionState} -> sessionState) (\s@PutSession' {} a -> s {sessionState = a} :: PutSession)

-- | The identifier of the session that receives the session data.
putSession_sessionId :: Lens.Lens' PutSession Prelude.Text
putSession_sessionId = Lens.lens (\PutSession' {sessionId} -> sessionId) (\s@PutSession' {} a -> s {sessionId = a} :: PutSession)

instance Core.AWSRequest PutSession where
  type AWSResponse PutSession = PutSessionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveBody
      ( \s h x ->
          PutSessionResponse'
            Prelude.<$> (h Core..#? "x-amz-lex-session-state")
            Prelude.<*> (h Core..#? "x-amz-lex-messages")
            Prelude.<*> (h Core..#? "x-amz-lex-session-id")
            Prelude.<*> (h Core..#? "x-amz-lex-request-attributes")
            Prelude.<*> (h Core..#? "Content-Type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.pure x)
      )

instance Prelude.Hashable PutSession

instance Prelude.NFData PutSession

instance Core.ToHeaders PutSession where
  toHeaders PutSession' {..} =
    Prelude.mconcat
      [ "ResponseContentType" Core.=# responseContentType,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON PutSession where
  toJSON PutSession' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("messages" Core..=) Prelude.<$> messages,
            ("requestAttributes" Core..=)
              Prelude.<$> requestAttributes,
            Prelude.Just ("sessionState" Core..= sessionState)
          ]
      )

instance Core.ToPath PutSession where
  toPath PutSession' {..} =
    Prelude.mconcat
      [ "/bots/",
        Core.toBS botId,
        "/botAliases/",
        Core.toBS botAliasId,
        "/botLocales/",
        Core.toBS localeId,
        "/sessions/",
        Core.toBS sessionId
      ]

instance Core.ToQuery PutSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutSessionResponse' smart constructor.
data PutSessionResponse = PutSessionResponse'
  { -- | Represents the current state of the dialog between the user and the bot.
    --
    -- Use this to determine the progress of the conversation and what the next
    -- action may be.
    sessionState :: Prelude.Maybe Prelude.Text,
    -- | A list of messages that were last sent to the user. The messages are
    -- ordered based on how you return the messages from you Lambda function or
    -- the order that the messages are defined in the bot.
    messages :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the session that received the data.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | Request-specific information passed between the client application and
    -- Amazon Lex V2. These are the same as the @requestAttribute@ parameter in
    -- the call to the @PutSession@ operation.
    requestAttributes :: Prelude.Maybe Prelude.Text,
    -- | The type of response. Same as the type specified in the
    -- @responseContentType@ field in the request.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | If the requested content type was audio, the audio version of the
    -- message to convey to the user.
    audioStream :: Core.ResponseBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionState', 'putSessionResponse_sessionState' - Represents the current state of the dialog between the user and the bot.
--
-- Use this to determine the progress of the conversation and what the next
-- action may be.
--
-- 'messages', 'putSessionResponse_messages' - A list of messages that were last sent to the user. The messages are
-- ordered based on how you return the messages from you Lambda function or
-- the order that the messages are defined in the bot.
--
-- 'sessionId', 'putSessionResponse_sessionId' - The identifier of the session that received the data.
--
-- 'requestAttributes', 'putSessionResponse_requestAttributes' - Request-specific information passed between the client application and
-- Amazon Lex V2. These are the same as the @requestAttribute@ parameter in
-- the call to the @PutSession@ operation.
--
-- 'contentType', 'putSessionResponse_contentType' - The type of response. Same as the type specified in the
-- @responseContentType@ field in the request.
--
-- 'httpStatus', 'putSessionResponse_httpStatus' - The response's http status code.
--
-- 'audioStream', 'putSessionResponse_audioStream' - If the requested content type was audio, the audio version of the
-- message to convey to the user.
newPutSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'audioStream'
  Core.ResponseBody ->
  PutSessionResponse
newPutSessionResponse pHttpStatus_ pAudioStream_ =
  PutSessionResponse'
    { sessionState = Prelude.Nothing,
      messages = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      requestAttributes = Prelude.Nothing,
      contentType = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      audioStream = pAudioStream_
    }

-- | Represents the current state of the dialog between the user and the bot.
--
-- Use this to determine the progress of the conversation and what the next
-- action may be.
putSessionResponse_sessionState :: Lens.Lens' PutSessionResponse (Prelude.Maybe Prelude.Text)
putSessionResponse_sessionState = Lens.lens (\PutSessionResponse' {sessionState} -> sessionState) (\s@PutSessionResponse' {} a -> s {sessionState = a} :: PutSessionResponse)

-- | A list of messages that were last sent to the user. The messages are
-- ordered based on how you return the messages from you Lambda function or
-- the order that the messages are defined in the bot.
putSessionResponse_messages :: Lens.Lens' PutSessionResponse (Prelude.Maybe Prelude.Text)
putSessionResponse_messages = Lens.lens (\PutSessionResponse' {messages} -> messages) (\s@PutSessionResponse' {} a -> s {messages = a} :: PutSessionResponse)

-- | The identifier of the session that received the data.
putSessionResponse_sessionId :: Lens.Lens' PutSessionResponse (Prelude.Maybe Prelude.Text)
putSessionResponse_sessionId = Lens.lens (\PutSessionResponse' {sessionId} -> sessionId) (\s@PutSessionResponse' {} a -> s {sessionId = a} :: PutSessionResponse)

-- | Request-specific information passed between the client application and
-- Amazon Lex V2. These are the same as the @requestAttribute@ parameter in
-- the call to the @PutSession@ operation.
putSessionResponse_requestAttributes :: Lens.Lens' PutSessionResponse (Prelude.Maybe Prelude.Text)
putSessionResponse_requestAttributes = Lens.lens (\PutSessionResponse' {requestAttributes} -> requestAttributes) (\s@PutSessionResponse' {} a -> s {requestAttributes = a} :: PutSessionResponse)

-- | The type of response. Same as the type specified in the
-- @responseContentType@ field in the request.
putSessionResponse_contentType :: Lens.Lens' PutSessionResponse (Prelude.Maybe Prelude.Text)
putSessionResponse_contentType = Lens.lens (\PutSessionResponse' {contentType} -> contentType) (\s@PutSessionResponse' {} a -> s {contentType = a} :: PutSessionResponse)

-- | The response's http status code.
putSessionResponse_httpStatus :: Lens.Lens' PutSessionResponse Prelude.Int
putSessionResponse_httpStatus = Lens.lens (\PutSessionResponse' {httpStatus} -> httpStatus) (\s@PutSessionResponse' {} a -> s {httpStatus = a} :: PutSessionResponse)

-- | If the requested content type was audio, the audio version of the
-- message to convey to the user.
putSessionResponse_audioStream :: Lens.Lens' PutSessionResponse Core.ResponseBody
putSessionResponse_audioStream = Lens.lens (\PutSessionResponse' {audioStream} -> audioStream) (\s@PutSessionResponse' {} a -> s {audioStream = a} :: PutSessionResponse)
