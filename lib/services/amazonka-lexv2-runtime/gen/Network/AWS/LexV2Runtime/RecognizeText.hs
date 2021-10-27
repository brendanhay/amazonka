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
-- Module      : Network.AWS.LexV2Runtime.RecognizeText
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends user input to Amazon Lex V2. Client applications use this API to
-- send requests to Amazon Lex V2 at runtime. Amazon Lex V2 then interprets
-- the user input using the machine learning model that it build for the
-- bot.
--
-- In response, Amazon Lex V2 returns the next message to convey to the
-- user and an optional response card to display.
--
-- If the optional post-fulfillment response is specified, the messages are
-- returned as follows. For more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/API_PostFulfillmentStatusSpecification.html PostFulfillmentStatusSpecification>.
--
-- -   __Success message__ - Returned if the Lambda function completes
--     successfully and the intent state is fulfilled or ready fulfillment
--     if the message is present.
--
-- -   __Failed message__ - The failed message is returned if the Lambda
--     function throws an exception or if the Lambda function returns a
--     failed intent state without a message.
--
-- -   __Timeout message__ - If you don\'t configure a timeout message and
--     a timeout, and the Lambda function doesn\'t return within 30
--     seconds, the timeout message is returned. If you configure a
--     timeout, the timeout message is returned when the period times out.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/streaming-progress.html#progress-complete.html Completion message>.
module Network.AWS.LexV2Runtime.RecognizeText
  ( -- * Creating a Request
    RecognizeText (..),
    newRecognizeText,

    -- * Request Lenses
    recognizeText_sessionState,
    recognizeText_requestAttributes,
    recognizeText_botId,
    recognizeText_botAliasId,
    recognizeText_localeId,
    recognizeText_text,
    recognizeText_sessionId,

    -- * Destructuring the Response
    RecognizeTextResponse (..),
    newRecognizeTextResponse,

    -- * Response Lenses
    recognizeTextResponse_sessionState,
    recognizeTextResponse_messages,
    recognizeTextResponse_sessionId,
    recognizeTextResponse_requestAttributes,
    recognizeTextResponse_interpretations,
    recognizeTextResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Runtime.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRecognizeText' smart constructor.
data RecognizeText = RecognizeText'
  { -- | The current state of the dialog between the user and the bot.
    sessionState :: Prelude.Maybe SessionState,
    -- | Request-specific information passed between the client application and
    -- Amazon Lex V2
    --
    -- The namespace @x-amz-lex:@ is reserved for special attributes. Don\'t
    -- create any request attributes with the prefix @x-amz-lex:@.
    requestAttributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The identifier of the bot that processes the request.
    botId :: Prelude.Text,
    -- | The alias identifier in use for the bot that processes the request.
    botAliasId :: Prelude.Text,
    -- | The locale where the session is in use.
    localeId :: Prelude.Text,
    -- | The text that the user entered. Amazon Lex V2 interprets this text.
    text :: Core.Sensitive Prelude.Text,
    -- | The identifier of the user session that is having the conversation.
    sessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecognizeText' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionState', 'recognizeText_sessionState' - The current state of the dialog between the user and the bot.
--
-- 'requestAttributes', 'recognizeText_requestAttributes' - Request-specific information passed between the client application and
-- Amazon Lex V2
--
-- The namespace @x-amz-lex:@ is reserved for special attributes. Don\'t
-- create any request attributes with the prefix @x-amz-lex:@.
--
-- 'botId', 'recognizeText_botId' - The identifier of the bot that processes the request.
--
-- 'botAliasId', 'recognizeText_botAliasId' - The alias identifier in use for the bot that processes the request.
--
-- 'localeId', 'recognizeText_localeId' - The locale where the session is in use.
--
-- 'text', 'recognizeText_text' - The text that the user entered. Amazon Lex V2 interprets this text.
--
-- 'sessionId', 'recognizeText_sessionId' - The identifier of the user session that is having the conversation.
newRecognizeText ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botAliasId'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  -- | 'text'
  Prelude.Text ->
  -- | 'sessionId'
  Prelude.Text ->
  RecognizeText
newRecognizeText
  pBotId_
  pBotAliasId_
  pLocaleId_
  pText_
  pSessionId_ =
    RecognizeText'
      { sessionState = Prelude.Nothing,
        requestAttributes = Prelude.Nothing,
        botId = pBotId_,
        botAliasId = pBotAliasId_,
        localeId = pLocaleId_,
        text = Core._Sensitive Lens.# pText_,
        sessionId = pSessionId_
      }

-- | The current state of the dialog between the user and the bot.
recognizeText_sessionState :: Lens.Lens' RecognizeText (Prelude.Maybe SessionState)
recognizeText_sessionState = Lens.lens (\RecognizeText' {sessionState} -> sessionState) (\s@RecognizeText' {} a -> s {sessionState = a} :: RecognizeText)

-- | Request-specific information passed between the client application and
-- Amazon Lex V2
--
-- The namespace @x-amz-lex:@ is reserved for special attributes. Don\'t
-- create any request attributes with the prefix @x-amz-lex:@.
recognizeText_requestAttributes :: Lens.Lens' RecognizeText (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
recognizeText_requestAttributes = Lens.lens (\RecognizeText' {requestAttributes} -> requestAttributes) (\s@RecognizeText' {} a -> s {requestAttributes = a} :: RecognizeText) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the bot that processes the request.
recognizeText_botId :: Lens.Lens' RecognizeText Prelude.Text
recognizeText_botId = Lens.lens (\RecognizeText' {botId} -> botId) (\s@RecognizeText' {} a -> s {botId = a} :: RecognizeText)

-- | The alias identifier in use for the bot that processes the request.
recognizeText_botAliasId :: Lens.Lens' RecognizeText Prelude.Text
recognizeText_botAliasId = Lens.lens (\RecognizeText' {botAliasId} -> botAliasId) (\s@RecognizeText' {} a -> s {botAliasId = a} :: RecognizeText)

-- | The locale where the session is in use.
recognizeText_localeId :: Lens.Lens' RecognizeText Prelude.Text
recognizeText_localeId = Lens.lens (\RecognizeText' {localeId} -> localeId) (\s@RecognizeText' {} a -> s {localeId = a} :: RecognizeText)

-- | The text that the user entered. Amazon Lex V2 interprets this text.
recognizeText_text :: Lens.Lens' RecognizeText Prelude.Text
recognizeText_text = Lens.lens (\RecognizeText' {text} -> text) (\s@RecognizeText' {} a -> s {text = a} :: RecognizeText) Prelude.. Core._Sensitive

-- | The identifier of the user session that is having the conversation.
recognizeText_sessionId :: Lens.Lens' RecognizeText Prelude.Text
recognizeText_sessionId = Lens.lens (\RecognizeText' {sessionId} -> sessionId) (\s@RecognizeText' {} a -> s {sessionId = a} :: RecognizeText)

instance Core.AWSRequest RecognizeText where
  type
    AWSResponse RecognizeText =
      RecognizeTextResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RecognizeTextResponse'
            Prelude.<$> (x Core..?> "sessionState")
            Prelude.<*> (x Core..?> "messages" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "sessionId")
            Prelude.<*> ( x Core..?> "requestAttributes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Core..?> "interpretations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RecognizeText

instance Prelude.NFData RecognizeText

instance Core.ToHeaders RecognizeText where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RecognizeText where
  toJSON RecognizeText' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("sessionState" Core..=) Prelude.<$> sessionState,
            ("requestAttributes" Core..=)
              Prelude.<$> requestAttributes,
            Prelude.Just ("text" Core..= text)
          ]
      )

instance Core.ToPath RecognizeText where
  toPath RecognizeText' {..} =
    Prelude.mconcat
      [ "/bots/",
        Core.toBS botId,
        "/botAliases/",
        Core.toBS botAliasId,
        "/botLocales/",
        Core.toBS localeId,
        "/sessions/",
        Core.toBS sessionId,
        "/text"
      ]

instance Core.ToQuery RecognizeText where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRecognizeTextResponse' smart constructor.
data RecognizeTextResponse = RecognizeTextResponse'
  { -- | Represents the current state of the dialog between the user and the bot.
    --
    -- Use this to determine the progress of the conversation and what the next
    -- action may be.
    sessionState :: Prelude.Maybe SessionState,
    -- | A list of messages last sent to the user. The messages are ordered based
    -- on the order that you returned the messages from your Lambda function or
    -- the order that the messages are defined in the bot.
    messages :: Prelude.Maybe [Message],
    -- | The identifier of the session in use.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | The attributes sent in the request.
    requestAttributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of intents that Amazon Lex V2 determined might satisfy the
    -- user\'s utterance.
    --
    -- Each interpretation includes the intent, a score that indicates now
    -- confident Amazon Lex V2 is that the interpretation is the correct one,
    -- and an optional sentiment response that indicates the sentiment
    -- expressed in the utterance.
    interpretations :: Prelude.Maybe [Interpretation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecognizeTextResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionState', 'recognizeTextResponse_sessionState' - Represents the current state of the dialog between the user and the bot.
--
-- Use this to determine the progress of the conversation and what the next
-- action may be.
--
-- 'messages', 'recognizeTextResponse_messages' - A list of messages last sent to the user. The messages are ordered based
-- on the order that you returned the messages from your Lambda function or
-- the order that the messages are defined in the bot.
--
-- 'sessionId', 'recognizeTextResponse_sessionId' - The identifier of the session in use.
--
-- 'requestAttributes', 'recognizeTextResponse_requestAttributes' - The attributes sent in the request.
--
-- 'interpretations', 'recognizeTextResponse_interpretations' - A list of intents that Amazon Lex V2 determined might satisfy the
-- user\'s utterance.
--
-- Each interpretation includes the intent, a score that indicates now
-- confident Amazon Lex V2 is that the interpretation is the correct one,
-- and an optional sentiment response that indicates the sentiment
-- expressed in the utterance.
--
-- 'httpStatus', 'recognizeTextResponse_httpStatus' - The response's http status code.
newRecognizeTextResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RecognizeTextResponse
newRecognizeTextResponse pHttpStatus_ =
  RecognizeTextResponse'
    { sessionState =
        Prelude.Nothing,
      messages = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      requestAttributes = Prelude.Nothing,
      interpretations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the current state of the dialog between the user and the bot.
--
-- Use this to determine the progress of the conversation and what the next
-- action may be.
recognizeTextResponse_sessionState :: Lens.Lens' RecognizeTextResponse (Prelude.Maybe SessionState)
recognizeTextResponse_sessionState = Lens.lens (\RecognizeTextResponse' {sessionState} -> sessionState) (\s@RecognizeTextResponse' {} a -> s {sessionState = a} :: RecognizeTextResponse)

-- | A list of messages last sent to the user. The messages are ordered based
-- on the order that you returned the messages from your Lambda function or
-- the order that the messages are defined in the bot.
recognizeTextResponse_messages :: Lens.Lens' RecognizeTextResponse (Prelude.Maybe [Message])
recognizeTextResponse_messages = Lens.lens (\RecognizeTextResponse' {messages} -> messages) (\s@RecognizeTextResponse' {} a -> s {messages = a} :: RecognizeTextResponse) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the session in use.
recognizeTextResponse_sessionId :: Lens.Lens' RecognizeTextResponse (Prelude.Maybe Prelude.Text)
recognizeTextResponse_sessionId = Lens.lens (\RecognizeTextResponse' {sessionId} -> sessionId) (\s@RecognizeTextResponse' {} a -> s {sessionId = a} :: RecognizeTextResponse)

-- | The attributes sent in the request.
recognizeTextResponse_requestAttributes :: Lens.Lens' RecognizeTextResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
recognizeTextResponse_requestAttributes = Lens.lens (\RecognizeTextResponse' {requestAttributes} -> requestAttributes) (\s@RecognizeTextResponse' {} a -> s {requestAttributes = a} :: RecognizeTextResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of intents that Amazon Lex V2 determined might satisfy the
-- user\'s utterance.
--
-- Each interpretation includes the intent, a score that indicates now
-- confident Amazon Lex V2 is that the interpretation is the correct one,
-- and an optional sentiment response that indicates the sentiment
-- expressed in the utterance.
recognizeTextResponse_interpretations :: Lens.Lens' RecognizeTextResponse (Prelude.Maybe [Interpretation])
recognizeTextResponse_interpretations = Lens.lens (\RecognizeTextResponse' {interpretations} -> interpretations) (\s@RecognizeTextResponse' {} a -> s {interpretations = a} :: RecognizeTextResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
recognizeTextResponse_httpStatus :: Lens.Lens' RecognizeTextResponse Prelude.Int
recognizeTextResponse_httpStatus = Lens.lens (\RecognizeTextResponse' {httpStatus} -> httpStatus) (\s@RecognizeTextResponse' {} a -> s {httpStatus = a} :: RecognizeTextResponse)

instance Prelude.NFData RecognizeTextResponse
