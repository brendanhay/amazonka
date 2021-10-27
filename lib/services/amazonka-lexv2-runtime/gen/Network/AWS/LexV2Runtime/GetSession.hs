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
-- Module      : Network.AWS.LexV2Runtime.GetSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns session information for a specified bot, alias, and user.
--
-- For example, you can use this operation to retrieve session information
-- for a user that has left a long-running session in use.
--
-- If the bot, alias, or session identifier doesn\'t exist, Amazon Lex V2
-- returns a @BadRequestException@. If the locale doesn\'t exist or is not
-- enabled for the alias, you receive a @BadRequestException@.
module Network.AWS.LexV2Runtime.GetSession
  ( -- * Creating a Request
    GetSession (..),
    newGetSession,

    -- * Request Lenses
    getSession_botId,
    getSession_botAliasId,
    getSession_localeId,
    getSession_sessionId,

    -- * Destructuring the Response
    GetSessionResponse (..),
    newGetSessionResponse,

    -- * Response Lenses
    getSessionResponse_sessionState,
    getSessionResponse_messages,
    getSessionResponse_sessionId,
    getSessionResponse_interpretations,
    getSessionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Runtime.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSession' smart constructor.
data GetSession = GetSession'
  { -- | The identifier of the bot that contains the session data.
    botId :: Prelude.Text,
    -- | The alias identifier in use for the bot that contains the session data.
    botAliasId :: Prelude.Text,
    -- | The locale where the session is in use.
    localeId :: Prelude.Text,
    -- | The identifier of the session to return.
    sessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'getSession_botId' - The identifier of the bot that contains the session data.
--
-- 'botAliasId', 'getSession_botAliasId' - The alias identifier in use for the bot that contains the session data.
--
-- 'localeId', 'getSession_localeId' - The locale where the session is in use.
--
-- 'sessionId', 'getSession_sessionId' - The identifier of the session to return.
newGetSession ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botAliasId'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  -- | 'sessionId'
  Prelude.Text ->
  GetSession
newGetSession
  pBotId_
  pBotAliasId_
  pLocaleId_
  pSessionId_ =
    GetSession'
      { botId = pBotId_,
        botAliasId = pBotAliasId_,
        localeId = pLocaleId_,
        sessionId = pSessionId_
      }

-- | The identifier of the bot that contains the session data.
getSession_botId :: Lens.Lens' GetSession Prelude.Text
getSession_botId = Lens.lens (\GetSession' {botId} -> botId) (\s@GetSession' {} a -> s {botId = a} :: GetSession)

-- | The alias identifier in use for the bot that contains the session data.
getSession_botAliasId :: Lens.Lens' GetSession Prelude.Text
getSession_botAliasId = Lens.lens (\GetSession' {botAliasId} -> botAliasId) (\s@GetSession' {} a -> s {botAliasId = a} :: GetSession)

-- | The locale where the session is in use.
getSession_localeId :: Lens.Lens' GetSession Prelude.Text
getSession_localeId = Lens.lens (\GetSession' {localeId} -> localeId) (\s@GetSession' {} a -> s {localeId = a} :: GetSession)

-- | The identifier of the session to return.
getSession_sessionId :: Lens.Lens' GetSession Prelude.Text
getSession_sessionId = Lens.lens (\GetSession' {sessionId} -> sessionId) (\s@GetSession' {} a -> s {sessionId = a} :: GetSession)

instance Core.AWSRequest GetSession where
  type AWSResponse GetSession = GetSessionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSessionResponse'
            Prelude.<$> (x Core..?> "sessionState")
            Prelude.<*> (x Core..?> "messages" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "sessionId")
            Prelude.<*> ( x Core..?> "interpretations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSession

instance Prelude.NFData GetSession

instance Core.ToHeaders GetSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetSession where
  toPath GetSession' {..} =
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

instance Core.ToQuery GetSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSessionResponse' smart constructor.
data GetSessionResponse = GetSessionResponse'
  { -- | Represents the current state of the dialog between the user and the bot.
    --
    -- You can use this to determine the progress of the conversation and what
    -- the next action might be.
    sessionState :: Prelude.Maybe SessionState,
    -- | A list of messages that were last sent to the user. The messages are
    -- ordered based on the order that your returned the messages from your
    -- Lambda function or the order that messages are defined in the bot.
    messages :: Prelude.Maybe [Message],
    -- | The identifier of the returned session.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | A list of intents that Amazon Lex V2 determined might satisfy the
    -- user\'s utterance.
    --
    -- Each interpretation includes the intent, a score that indicates how
    -- confident Amazon Lex V2 is that the interpretation is the correct one,
    -- and an optional sentiment response that indicates the sentiment
    -- expressed in the utterance.
    interpretations :: Prelude.Maybe [Interpretation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionState', 'getSessionResponse_sessionState' - Represents the current state of the dialog between the user and the bot.
--
-- You can use this to determine the progress of the conversation and what
-- the next action might be.
--
-- 'messages', 'getSessionResponse_messages' - A list of messages that were last sent to the user. The messages are
-- ordered based on the order that your returned the messages from your
-- Lambda function or the order that messages are defined in the bot.
--
-- 'sessionId', 'getSessionResponse_sessionId' - The identifier of the returned session.
--
-- 'interpretations', 'getSessionResponse_interpretations' - A list of intents that Amazon Lex V2 determined might satisfy the
-- user\'s utterance.
--
-- Each interpretation includes the intent, a score that indicates how
-- confident Amazon Lex V2 is that the interpretation is the correct one,
-- and an optional sentiment response that indicates the sentiment
-- expressed in the utterance.
--
-- 'httpStatus', 'getSessionResponse_httpStatus' - The response's http status code.
newGetSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSessionResponse
newGetSessionResponse pHttpStatus_ =
  GetSessionResponse'
    { sessionState = Prelude.Nothing,
      messages = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      interpretations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the current state of the dialog between the user and the bot.
--
-- You can use this to determine the progress of the conversation and what
-- the next action might be.
getSessionResponse_sessionState :: Lens.Lens' GetSessionResponse (Prelude.Maybe SessionState)
getSessionResponse_sessionState = Lens.lens (\GetSessionResponse' {sessionState} -> sessionState) (\s@GetSessionResponse' {} a -> s {sessionState = a} :: GetSessionResponse)

-- | A list of messages that were last sent to the user. The messages are
-- ordered based on the order that your returned the messages from your
-- Lambda function or the order that messages are defined in the bot.
getSessionResponse_messages :: Lens.Lens' GetSessionResponse (Prelude.Maybe [Message])
getSessionResponse_messages = Lens.lens (\GetSessionResponse' {messages} -> messages) (\s@GetSessionResponse' {} a -> s {messages = a} :: GetSessionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the returned session.
getSessionResponse_sessionId :: Lens.Lens' GetSessionResponse (Prelude.Maybe Prelude.Text)
getSessionResponse_sessionId = Lens.lens (\GetSessionResponse' {sessionId} -> sessionId) (\s@GetSessionResponse' {} a -> s {sessionId = a} :: GetSessionResponse)

-- | A list of intents that Amazon Lex V2 determined might satisfy the
-- user\'s utterance.
--
-- Each interpretation includes the intent, a score that indicates how
-- confident Amazon Lex V2 is that the interpretation is the correct one,
-- and an optional sentiment response that indicates the sentiment
-- expressed in the utterance.
getSessionResponse_interpretations :: Lens.Lens' GetSessionResponse (Prelude.Maybe [Interpretation])
getSessionResponse_interpretations = Lens.lens (\GetSessionResponse' {interpretations} -> interpretations) (\s@GetSessionResponse' {} a -> s {interpretations = a} :: GetSessionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getSessionResponse_httpStatus :: Lens.Lens' GetSessionResponse Prelude.Int
getSessionResponse_httpStatus = Lens.lens (\GetSessionResponse' {httpStatus} -> httpStatus) (\s@GetSessionResponse' {} a -> s {httpStatus = a} :: GetSessionResponse)

instance Prelude.NFData GetSessionResponse
