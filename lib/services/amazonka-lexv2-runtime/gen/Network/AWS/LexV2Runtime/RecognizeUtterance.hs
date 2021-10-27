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
-- Module      : Network.AWS.LexV2Runtime.RecognizeUtterance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends user input to Amazon Lex V2. You can send text or speech. Clients
-- use this API to send text and audio requests to Amazon Lex V2 at
-- runtime. Amazon Lex V2 interprets the user input using the machine
-- learning model built for the bot.
--
-- The following request fields must be compressed with gzip and then
-- base64 encoded before you send them to Amazon Lex V2.
--
-- -   requestAttributes
--
-- -   sessionState
--
-- The following response fields are compressed using gzip and then base64
-- encoded by Amazon Lex V2. Before you can use these fields, you must
-- decode and decompress them.
--
-- -   inputTranscript
--
-- -   interpretations
--
-- -   messages
--
-- -   requestAttributes
--
-- -   sessionState
--
-- The example contains a Java application that compresses and encodes a
-- Java object to send to Amazon Lex V2, and a second that decodes and
-- decompresses a response from Amazon Lex V2.
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
module Network.AWS.LexV2Runtime.RecognizeUtterance
  ( -- * Creating a Request
    RecognizeUtterance (..),
    newRecognizeUtterance,

    -- * Request Lenses
    recognizeUtterance_responseContentType,
    recognizeUtterance_sessionState,
    recognizeUtterance_requestAttributes,
    recognizeUtterance_botId,
    recognizeUtterance_botAliasId,
    recognizeUtterance_localeId,
    recognizeUtterance_requestContentType,
    recognizeUtterance_sessionId,
    recognizeUtterance_inputStream,

    -- * Destructuring the Response
    RecognizeUtteranceResponse (..),
    newRecognizeUtteranceResponse,

    -- * Response Lenses
    recognizeUtteranceResponse_sessionState,
    recognizeUtteranceResponse_inputMode,
    recognizeUtteranceResponse_messages,
    recognizeUtteranceResponse_inputTranscript,
    recognizeUtteranceResponse_sessionId,
    recognizeUtteranceResponse_requestAttributes,
    recognizeUtteranceResponse_interpretations,
    recognizeUtteranceResponse_contentType,
    recognizeUtteranceResponse_httpStatus,
    recognizeUtteranceResponse_audioStream,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Runtime.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRecognizeUtterance' smart constructor.
data RecognizeUtterance = RecognizeUtterance'
  { -- | The message that Amazon Lex V2 returns in the response can be either
    -- text or speech based on the @responseContentType@ value.
    --
    -- -   If the value is @text\/plain;charset=utf-8@, Amazon Lex V2 returns
    --     text in the response.
    --
    -- -   If the value begins with @audio\/@, Amazon Lex V2 returns speech in
    --     the response. Amazon Lex V2 uses Amazon Polly to generate the speech
    --     using the configuration that you specified in the
    --     @requestContentType@ parameter. For example, if you specify
    --     @audio\/mpeg@ as the value, Amazon Lex V2 returns speech in the MPEG
    --     format.
    --
    -- -   If the value is @audio\/pcm@, the speech returned is @audio\/pcm@ at
    --     16 KHz in 16-bit, little-endian format.
    --
    -- -   The following are the accepted values:
    --
    --     -   audio\/mpeg
    --
    --     -   audio\/ogg
    --
    --     -   audio\/pcm (16 KHz)
    --
    --     -   audio\/* (defaults to mpeg)
    --
    --     -   text\/plain; charset=utf-8
    responseContentType :: Prelude.Maybe Prelude.Text,
    -- | Sets the state of the session with the user. You can use this to set the
    -- current intent, attributes, context, and dialog action. Use the dialog
    -- action to determine the next step that Amazon Lex V2 should use in the
    -- conversation with the user.
    --
    -- The @sessionState@ field must be compressed using gzip and then base64
    -- encoded before sending to Amazon Lex V2.
    sessionState :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Request-specific information passed between the client application and
    -- Amazon Lex V2
    --
    -- The namespace @x-amz-lex:@ is reserved for special attributes. Don\'t
    -- create any request attributes for prefix @x-amz-lex:@.
    --
    -- The @requestAttributes@ field must be compressed using gzip and then
    -- base64 encoded before sending to Amazon Lex V2.
    requestAttributes :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The identifier of the bot that should receive the request.
    botId :: Prelude.Text,
    -- | The alias identifier in use for the bot that should receive the request.
    botAliasId :: Prelude.Text,
    -- | The locale where the session is in use.
    localeId :: Prelude.Text,
    -- | Indicates the format for audio input or that the content is text. The
    -- header must start with one of the following prefixes:
    --
    -- -   PCM format, audio data must be in little-endian byte order.
    --
    --     -   audio\/l16; rate=16000; channels=1
    --
    --     -   audio\/x-l16; sample-rate=16000; channel-count=1
    --
    --     -   audio\/lpcm; sample-rate=8000; sample-size-bits=16;
    --         channel-count=1; is-big-endian=false
    --
    -- -   Opus format
    --
    --     -   audio\/x-cbr-opus-with-preamble;preamble-size=0;bit-rate=256000;frame-size-milliseconds=4
    --
    -- -   Text format
    --
    --     -   text\/plain; charset=utf-8
    requestContentType :: Prelude.Text,
    -- | The identifier of the session in use.
    sessionId :: Prelude.Text,
    -- | User input in PCM or Opus audio format or text format as described in
    -- the @requestContentType@ parameter.
    inputStream :: Core.HashedBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecognizeUtterance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'responseContentType', 'recognizeUtterance_responseContentType' - The message that Amazon Lex V2 returns in the response can be either
-- text or speech based on the @responseContentType@ value.
--
-- -   If the value is @text\/plain;charset=utf-8@, Amazon Lex V2 returns
--     text in the response.
--
-- -   If the value begins with @audio\/@, Amazon Lex V2 returns speech in
--     the response. Amazon Lex V2 uses Amazon Polly to generate the speech
--     using the configuration that you specified in the
--     @requestContentType@ parameter. For example, if you specify
--     @audio\/mpeg@ as the value, Amazon Lex V2 returns speech in the MPEG
--     format.
--
-- -   If the value is @audio\/pcm@, the speech returned is @audio\/pcm@ at
--     16 KHz in 16-bit, little-endian format.
--
-- -   The following are the accepted values:
--
--     -   audio\/mpeg
--
--     -   audio\/ogg
--
--     -   audio\/pcm (16 KHz)
--
--     -   audio\/* (defaults to mpeg)
--
--     -   text\/plain; charset=utf-8
--
-- 'sessionState', 'recognizeUtterance_sessionState' - Sets the state of the session with the user. You can use this to set the
-- current intent, attributes, context, and dialog action. Use the dialog
-- action to determine the next step that Amazon Lex V2 should use in the
-- conversation with the user.
--
-- The @sessionState@ field must be compressed using gzip and then base64
-- encoded before sending to Amazon Lex V2.
--
-- 'requestAttributes', 'recognizeUtterance_requestAttributes' - Request-specific information passed between the client application and
-- Amazon Lex V2
--
-- The namespace @x-amz-lex:@ is reserved for special attributes. Don\'t
-- create any request attributes for prefix @x-amz-lex:@.
--
-- The @requestAttributes@ field must be compressed using gzip and then
-- base64 encoded before sending to Amazon Lex V2.
--
-- 'botId', 'recognizeUtterance_botId' - The identifier of the bot that should receive the request.
--
-- 'botAliasId', 'recognizeUtterance_botAliasId' - The alias identifier in use for the bot that should receive the request.
--
-- 'localeId', 'recognizeUtterance_localeId' - The locale where the session is in use.
--
-- 'requestContentType', 'recognizeUtterance_requestContentType' - Indicates the format for audio input or that the content is text. The
-- header must start with one of the following prefixes:
--
-- -   PCM format, audio data must be in little-endian byte order.
--
--     -   audio\/l16; rate=16000; channels=1
--
--     -   audio\/x-l16; sample-rate=16000; channel-count=1
--
--     -   audio\/lpcm; sample-rate=8000; sample-size-bits=16;
--         channel-count=1; is-big-endian=false
--
-- -   Opus format
--
--     -   audio\/x-cbr-opus-with-preamble;preamble-size=0;bit-rate=256000;frame-size-milliseconds=4
--
-- -   Text format
--
--     -   text\/plain; charset=utf-8
--
-- 'sessionId', 'recognizeUtterance_sessionId' - The identifier of the session in use.
--
-- 'inputStream', 'recognizeUtterance_inputStream' - User input in PCM or Opus audio format or text format as described in
-- the @requestContentType@ parameter.
newRecognizeUtterance ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botAliasId'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  -- | 'requestContentType'
  Prelude.Text ->
  -- | 'sessionId'
  Prelude.Text ->
  -- | 'inputStream'
  Core.HashedBody ->
  RecognizeUtterance
newRecognizeUtterance
  pBotId_
  pBotAliasId_
  pLocaleId_
  pRequestContentType_
  pSessionId_
  pInputStream_ =
    RecognizeUtterance'
      { responseContentType =
          Prelude.Nothing,
        sessionState = Prelude.Nothing,
        requestAttributes = Prelude.Nothing,
        botId = pBotId_,
        botAliasId = pBotAliasId_,
        localeId = pLocaleId_,
        requestContentType = pRequestContentType_,
        sessionId = pSessionId_,
        inputStream = pInputStream_
      }

-- | The message that Amazon Lex V2 returns in the response can be either
-- text or speech based on the @responseContentType@ value.
--
-- -   If the value is @text\/plain;charset=utf-8@, Amazon Lex V2 returns
--     text in the response.
--
-- -   If the value begins with @audio\/@, Amazon Lex V2 returns speech in
--     the response. Amazon Lex V2 uses Amazon Polly to generate the speech
--     using the configuration that you specified in the
--     @requestContentType@ parameter. For example, if you specify
--     @audio\/mpeg@ as the value, Amazon Lex V2 returns speech in the MPEG
--     format.
--
-- -   If the value is @audio\/pcm@, the speech returned is @audio\/pcm@ at
--     16 KHz in 16-bit, little-endian format.
--
-- -   The following are the accepted values:
--
--     -   audio\/mpeg
--
--     -   audio\/ogg
--
--     -   audio\/pcm (16 KHz)
--
--     -   audio\/* (defaults to mpeg)
--
--     -   text\/plain; charset=utf-8
recognizeUtterance_responseContentType :: Lens.Lens' RecognizeUtterance (Prelude.Maybe Prelude.Text)
recognizeUtterance_responseContentType = Lens.lens (\RecognizeUtterance' {responseContentType} -> responseContentType) (\s@RecognizeUtterance' {} a -> s {responseContentType = a} :: RecognizeUtterance)

-- | Sets the state of the session with the user. You can use this to set the
-- current intent, attributes, context, and dialog action. Use the dialog
-- action to determine the next step that Amazon Lex V2 should use in the
-- conversation with the user.
--
-- The @sessionState@ field must be compressed using gzip and then base64
-- encoded before sending to Amazon Lex V2.
recognizeUtterance_sessionState :: Lens.Lens' RecognizeUtterance (Prelude.Maybe Prelude.Text)
recognizeUtterance_sessionState = Lens.lens (\RecognizeUtterance' {sessionState} -> sessionState) (\s@RecognizeUtterance' {} a -> s {sessionState = a} :: RecognizeUtterance) Prelude.. Lens.mapping Core._Sensitive

-- | Request-specific information passed between the client application and
-- Amazon Lex V2
--
-- The namespace @x-amz-lex:@ is reserved for special attributes. Don\'t
-- create any request attributes for prefix @x-amz-lex:@.
--
-- The @requestAttributes@ field must be compressed using gzip and then
-- base64 encoded before sending to Amazon Lex V2.
recognizeUtterance_requestAttributes :: Lens.Lens' RecognizeUtterance (Prelude.Maybe Prelude.Text)
recognizeUtterance_requestAttributes = Lens.lens (\RecognizeUtterance' {requestAttributes} -> requestAttributes) (\s@RecognizeUtterance' {} a -> s {requestAttributes = a} :: RecognizeUtterance) Prelude.. Lens.mapping Core._Sensitive

-- | The identifier of the bot that should receive the request.
recognizeUtterance_botId :: Lens.Lens' RecognizeUtterance Prelude.Text
recognizeUtterance_botId = Lens.lens (\RecognizeUtterance' {botId} -> botId) (\s@RecognizeUtterance' {} a -> s {botId = a} :: RecognizeUtterance)

-- | The alias identifier in use for the bot that should receive the request.
recognizeUtterance_botAliasId :: Lens.Lens' RecognizeUtterance Prelude.Text
recognizeUtterance_botAliasId = Lens.lens (\RecognizeUtterance' {botAliasId} -> botAliasId) (\s@RecognizeUtterance' {} a -> s {botAliasId = a} :: RecognizeUtterance)

-- | The locale where the session is in use.
recognizeUtterance_localeId :: Lens.Lens' RecognizeUtterance Prelude.Text
recognizeUtterance_localeId = Lens.lens (\RecognizeUtterance' {localeId} -> localeId) (\s@RecognizeUtterance' {} a -> s {localeId = a} :: RecognizeUtterance)

-- | Indicates the format for audio input or that the content is text. The
-- header must start with one of the following prefixes:
--
-- -   PCM format, audio data must be in little-endian byte order.
--
--     -   audio\/l16; rate=16000; channels=1
--
--     -   audio\/x-l16; sample-rate=16000; channel-count=1
--
--     -   audio\/lpcm; sample-rate=8000; sample-size-bits=16;
--         channel-count=1; is-big-endian=false
--
-- -   Opus format
--
--     -   audio\/x-cbr-opus-with-preamble;preamble-size=0;bit-rate=256000;frame-size-milliseconds=4
--
-- -   Text format
--
--     -   text\/plain; charset=utf-8
recognizeUtterance_requestContentType :: Lens.Lens' RecognizeUtterance Prelude.Text
recognizeUtterance_requestContentType = Lens.lens (\RecognizeUtterance' {requestContentType} -> requestContentType) (\s@RecognizeUtterance' {} a -> s {requestContentType = a} :: RecognizeUtterance)

-- | The identifier of the session in use.
recognizeUtterance_sessionId :: Lens.Lens' RecognizeUtterance Prelude.Text
recognizeUtterance_sessionId = Lens.lens (\RecognizeUtterance' {sessionId} -> sessionId) (\s@RecognizeUtterance' {} a -> s {sessionId = a} :: RecognizeUtterance)

-- | User input in PCM or Opus audio format or text format as described in
-- the @requestContentType@ parameter.
recognizeUtterance_inputStream :: Lens.Lens' RecognizeUtterance Core.HashedBody
recognizeUtterance_inputStream = Lens.lens (\RecognizeUtterance' {inputStream} -> inputStream) (\s@RecognizeUtterance' {} a -> s {inputStream = a} :: RecognizeUtterance)

instance Core.AWSRequest RecognizeUtterance where
  type
    AWSResponse RecognizeUtterance =
      RecognizeUtteranceResponse
  request = Request.postBody defaultService
  response =
    Response.receiveBody
      ( \s h x ->
          RecognizeUtteranceResponse'
            Prelude.<$> (h Core..#? "x-amz-lex-session-state")
            Prelude.<*> (h Core..#? "x-amz-lex-input-mode")
            Prelude.<*> (h Core..#? "x-amz-lex-messages")
            Prelude.<*> (h Core..#? "x-amz-lex-input-transcript")
            Prelude.<*> (h Core..#? "x-amz-lex-session-id")
            Prelude.<*> (h Core..#? "x-amz-lex-request-attributes")
            Prelude.<*> (h Core..#? "x-amz-lex-interpretations")
            Prelude.<*> (h Core..#? "Content-Type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.pure x)
      )

instance Core.ToBody RecognizeUtterance where
  toBody RecognizeUtterance' {..} =
    Core.toBody inputStream

instance Core.ToHeaders RecognizeUtterance where
  toHeaders RecognizeUtterance' {..} =
    Prelude.mconcat
      [ "Response-Content-Type" Core.=# responseContentType,
        "x-amz-lex-session-state" Core.=# sessionState,
        "x-amz-lex-request-attributes"
          Core.=# requestAttributes,
        "Content-Type" Core.=# requestContentType
      ]

instance Core.ToPath RecognizeUtterance where
  toPath RecognizeUtterance' {..} =
    Prelude.mconcat
      [ "/bots/",
        Core.toBS botId,
        "/botAliases/",
        Core.toBS botAliasId,
        "/botLocales/",
        Core.toBS localeId,
        "/sessions/",
        Core.toBS sessionId,
        "/utterance"
      ]

instance Core.ToQuery RecognizeUtterance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRecognizeUtteranceResponse' smart constructor.
data RecognizeUtteranceResponse = RecognizeUtteranceResponse'
  { -- | Represents the current state of the dialog between the user and the bot.
    --
    -- Use this to determine the progress of the conversation and what the next
    -- action might be.
    --
    -- The @sessionState@ field is compressed with gzip and then base64
    -- encoded. Before you can use the contents of the field, you must decode
    -- and decompress the contents. See the example for a simple function to
    -- decode and decompress the contents.
    sessionState :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the input mode to the operation was text or speech.
    inputMode :: Prelude.Maybe Prelude.Text,
    -- | A list of messages that were last sent to the user. The messages are
    -- ordered based on the order that you returned the messages from your
    -- Lambda function or the order that the messages are defined in the bot.
    --
    -- The @messages@ field is compressed with gzip and then base64 encoded.
    -- Before you can use the contents of the field, you must decode and
    -- decompress the contents. See the example for a simple function to decode
    -- and decompress the contents.
    messages :: Prelude.Maybe Prelude.Text,
    -- | The text used to process the request.
    --
    -- If the input was an audio stream, the @inputTranscript@ field contains
    -- the text extracted from the audio stream. This is the text that is
    -- actually processed to recognize intents and slot values. You can use
    -- this information to determine if Amazon Lex V2 is correctly processing
    -- the audio that you send.
    --
    -- The @inputTranscript@ field is compressed with gzip and then base64
    -- encoded. Before you can use the contents of the field, you must decode
    -- and decompress the contents. See the example for a simple function to
    -- decode and decompress the contents.
    inputTranscript :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the session in use.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | The attributes sent in the request.
    --
    -- The @requestAttributes@ field is compressed with gzip and then base64
    -- encoded. Before you can use the contents of the field, you must decode
    -- and decompress the contents.
    requestAttributes :: Prelude.Maybe Prelude.Text,
    -- | A list of intents that Amazon Lex V2 determined might satisfy the
    -- user\'s utterance.
    --
    -- Each interpretation includes the intent, a score that indicates how
    -- confident Amazon Lex V2 is that the interpretation is the correct one,
    -- and an optional sentiment response that indicates the sentiment
    -- expressed in the utterance.
    --
    -- The @interpretations@ field is compressed with gzip and then base64
    -- encoded. Before you can use the contents of the field, you must decode
    -- and decompress the contents. See the example for a simple function to
    -- decode and decompress the contents.
    interpretations :: Prelude.Maybe Prelude.Text,
    -- | Content type as specified in the @responseContentType@ in the request.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The prompt or statement to send to the user. This is based on the bot
    -- configuration and context. For example, if Amazon Lex V2 did not
    -- understand the user intent, it sends the @clarificationPrompt@
    -- configured for the bot. If the intent requires confirmation before
    -- taking the fulfillment action, it sends the @confirmationPrompt@.
    -- Another example: Suppose that the Lambda function successfully fulfilled
    -- the intent, and sent a message to convey to the user. Then Amazon Lex V2
    -- sends that message in the response.
    audioStream :: Core.ResponseBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecognizeUtteranceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionState', 'recognizeUtteranceResponse_sessionState' - Represents the current state of the dialog between the user and the bot.
--
-- Use this to determine the progress of the conversation and what the next
-- action might be.
--
-- The @sessionState@ field is compressed with gzip and then base64
-- encoded. Before you can use the contents of the field, you must decode
-- and decompress the contents. See the example for a simple function to
-- decode and decompress the contents.
--
-- 'inputMode', 'recognizeUtteranceResponse_inputMode' - Indicates whether the input mode to the operation was text or speech.
--
-- 'messages', 'recognizeUtteranceResponse_messages' - A list of messages that were last sent to the user. The messages are
-- ordered based on the order that you returned the messages from your
-- Lambda function or the order that the messages are defined in the bot.
--
-- The @messages@ field is compressed with gzip and then base64 encoded.
-- Before you can use the contents of the field, you must decode and
-- decompress the contents. See the example for a simple function to decode
-- and decompress the contents.
--
-- 'inputTranscript', 'recognizeUtteranceResponse_inputTranscript' - The text used to process the request.
--
-- If the input was an audio stream, the @inputTranscript@ field contains
-- the text extracted from the audio stream. This is the text that is
-- actually processed to recognize intents and slot values. You can use
-- this information to determine if Amazon Lex V2 is correctly processing
-- the audio that you send.
--
-- The @inputTranscript@ field is compressed with gzip and then base64
-- encoded. Before you can use the contents of the field, you must decode
-- and decompress the contents. See the example for a simple function to
-- decode and decompress the contents.
--
-- 'sessionId', 'recognizeUtteranceResponse_sessionId' - The identifier of the session in use.
--
-- 'requestAttributes', 'recognizeUtteranceResponse_requestAttributes' - The attributes sent in the request.
--
-- The @requestAttributes@ field is compressed with gzip and then base64
-- encoded. Before you can use the contents of the field, you must decode
-- and decompress the contents.
--
-- 'interpretations', 'recognizeUtteranceResponse_interpretations' - A list of intents that Amazon Lex V2 determined might satisfy the
-- user\'s utterance.
--
-- Each interpretation includes the intent, a score that indicates how
-- confident Amazon Lex V2 is that the interpretation is the correct one,
-- and an optional sentiment response that indicates the sentiment
-- expressed in the utterance.
--
-- The @interpretations@ field is compressed with gzip and then base64
-- encoded. Before you can use the contents of the field, you must decode
-- and decompress the contents. See the example for a simple function to
-- decode and decompress the contents.
--
-- 'contentType', 'recognizeUtteranceResponse_contentType' - Content type as specified in the @responseContentType@ in the request.
--
-- 'httpStatus', 'recognizeUtteranceResponse_httpStatus' - The response's http status code.
--
-- 'audioStream', 'recognizeUtteranceResponse_audioStream' - The prompt or statement to send to the user. This is based on the bot
-- configuration and context. For example, if Amazon Lex V2 did not
-- understand the user intent, it sends the @clarificationPrompt@
-- configured for the bot. If the intent requires confirmation before
-- taking the fulfillment action, it sends the @confirmationPrompt@.
-- Another example: Suppose that the Lambda function successfully fulfilled
-- the intent, and sent a message to convey to the user. Then Amazon Lex V2
-- sends that message in the response.
newRecognizeUtteranceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'audioStream'
  Core.ResponseBody ->
  RecognizeUtteranceResponse
newRecognizeUtteranceResponse
  pHttpStatus_
  pAudioStream_ =
    RecognizeUtteranceResponse'
      { sessionState =
          Prelude.Nothing,
        inputMode = Prelude.Nothing,
        messages = Prelude.Nothing,
        inputTranscript = Prelude.Nothing,
        sessionId = Prelude.Nothing,
        requestAttributes = Prelude.Nothing,
        interpretations = Prelude.Nothing,
        contentType = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        audioStream = pAudioStream_
      }

-- | Represents the current state of the dialog between the user and the bot.
--
-- Use this to determine the progress of the conversation and what the next
-- action might be.
--
-- The @sessionState@ field is compressed with gzip and then base64
-- encoded. Before you can use the contents of the field, you must decode
-- and decompress the contents. See the example for a simple function to
-- decode and decompress the contents.
recognizeUtteranceResponse_sessionState :: Lens.Lens' RecognizeUtteranceResponse (Prelude.Maybe Prelude.Text)
recognizeUtteranceResponse_sessionState = Lens.lens (\RecognizeUtteranceResponse' {sessionState} -> sessionState) (\s@RecognizeUtteranceResponse' {} a -> s {sessionState = a} :: RecognizeUtteranceResponse)

-- | Indicates whether the input mode to the operation was text or speech.
recognizeUtteranceResponse_inputMode :: Lens.Lens' RecognizeUtteranceResponse (Prelude.Maybe Prelude.Text)
recognizeUtteranceResponse_inputMode = Lens.lens (\RecognizeUtteranceResponse' {inputMode} -> inputMode) (\s@RecognizeUtteranceResponse' {} a -> s {inputMode = a} :: RecognizeUtteranceResponse)

-- | A list of messages that were last sent to the user. The messages are
-- ordered based on the order that you returned the messages from your
-- Lambda function or the order that the messages are defined in the bot.
--
-- The @messages@ field is compressed with gzip and then base64 encoded.
-- Before you can use the contents of the field, you must decode and
-- decompress the contents. See the example for a simple function to decode
-- and decompress the contents.
recognizeUtteranceResponse_messages :: Lens.Lens' RecognizeUtteranceResponse (Prelude.Maybe Prelude.Text)
recognizeUtteranceResponse_messages = Lens.lens (\RecognizeUtteranceResponse' {messages} -> messages) (\s@RecognizeUtteranceResponse' {} a -> s {messages = a} :: RecognizeUtteranceResponse)

-- | The text used to process the request.
--
-- If the input was an audio stream, the @inputTranscript@ field contains
-- the text extracted from the audio stream. This is the text that is
-- actually processed to recognize intents and slot values. You can use
-- this information to determine if Amazon Lex V2 is correctly processing
-- the audio that you send.
--
-- The @inputTranscript@ field is compressed with gzip and then base64
-- encoded. Before you can use the contents of the field, you must decode
-- and decompress the contents. See the example for a simple function to
-- decode and decompress the contents.
recognizeUtteranceResponse_inputTranscript :: Lens.Lens' RecognizeUtteranceResponse (Prelude.Maybe Prelude.Text)
recognizeUtteranceResponse_inputTranscript = Lens.lens (\RecognizeUtteranceResponse' {inputTranscript} -> inputTranscript) (\s@RecognizeUtteranceResponse' {} a -> s {inputTranscript = a} :: RecognizeUtteranceResponse)

-- | The identifier of the session in use.
recognizeUtteranceResponse_sessionId :: Lens.Lens' RecognizeUtteranceResponse (Prelude.Maybe Prelude.Text)
recognizeUtteranceResponse_sessionId = Lens.lens (\RecognizeUtteranceResponse' {sessionId} -> sessionId) (\s@RecognizeUtteranceResponse' {} a -> s {sessionId = a} :: RecognizeUtteranceResponse)

-- | The attributes sent in the request.
--
-- The @requestAttributes@ field is compressed with gzip and then base64
-- encoded. Before you can use the contents of the field, you must decode
-- and decompress the contents.
recognizeUtteranceResponse_requestAttributes :: Lens.Lens' RecognizeUtteranceResponse (Prelude.Maybe Prelude.Text)
recognizeUtteranceResponse_requestAttributes = Lens.lens (\RecognizeUtteranceResponse' {requestAttributes} -> requestAttributes) (\s@RecognizeUtteranceResponse' {} a -> s {requestAttributes = a} :: RecognizeUtteranceResponse)

-- | A list of intents that Amazon Lex V2 determined might satisfy the
-- user\'s utterance.
--
-- Each interpretation includes the intent, a score that indicates how
-- confident Amazon Lex V2 is that the interpretation is the correct one,
-- and an optional sentiment response that indicates the sentiment
-- expressed in the utterance.
--
-- The @interpretations@ field is compressed with gzip and then base64
-- encoded. Before you can use the contents of the field, you must decode
-- and decompress the contents. See the example for a simple function to
-- decode and decompress the contents.
recognizeUtteranceResponse_interpretations :: Lens.Lens' RecognizeUtteranceResponse (Prelude.Maybe Prelude.Text)
recognizeUtteranceResponse_interpretations = Lens.lens (\RecognizeUtteranceResponse' {interpretations} -> interpretations) (\s@RecognizeUtteranceResponse' {} a -> s {interpretations = a} :: RecognizeUtteranceResponse)

-- | Content type as specified in the @responseContentType@ in the request.
recognizeUtteranceResponse_contentType :: Lens.Lens' RecognizeUtteranceResponse (Prelude.Maybe Prelude.Text)
recognizeUtteranceResponse_contentType = Lens.lens (\RecognizeUtteranceResponse' {contentType} -> contentType) (\s@RecognizeUtteranceResponse' {} a -> s {contentType = a} :: RecognizeUtteranceResponse)

-- | The response's http status code.
recognizeUtteranceResponse_httpStatus :: Lens.Lens' RecognizeUtteranceResponse Prelude.Int
recognizeUtteranceResponse_httpStatus = Lens.lens (\RecognizeUtteranceResponse' {httpStatus} -> httpStatus) (\s@RecognizeUtteranceResponse' {} a -> s {httpStatus = a} :: RecognizeUtteranceResponse)

-- | The prompt or statement to send to the user. This is based on the bot
-- configuration and context. For example, if Amazon Lex V2 did not
-- understand the user intent, it sends the @clarificationPrompt@
-- configured for the bot. If the intent requires confirmation before
-- taking the fulfillment action, it sends the @confirmationPrompt@.
-- Another example: Suppose that the Lambda function successfully fulfilled
-- the intent, and sent a message to convey to the user. Then Amazon Lex V2
-- sends that message in the response.
recognizeUtteranceResponse_audioStream :: Lens.Lens' RecognizeUtteranceResponse Core.ResponseBody
recognizeUtteranceResponse_audioStream = Lens.lens (\RecognizeUtteranceResponse' {audioStream} -> audioStream) (\s@RecognizeUtteranceResponse' {} a -> s {audioStream = a} :: RecognizeUtteranceResponse)
