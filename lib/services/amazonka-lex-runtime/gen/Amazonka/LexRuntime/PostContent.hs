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
-- Module      : Amazonka.LexRuntime.PostContent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends user input (text or speech) to Amazon Lex. Clients use this API to
-- send text and audio requests to Amazon Lex at runtime. Amazon Lex
-- interprets the user input using the machine learning model that it built
-- for the bot.
--
-- The @PostContent@ operation supports audio input at 8kHz and 16kHz. You
-- can use 8kHz audio to achieve higher speech recognition accuracy in
-- telephone audio applications.
--
-- In response, Amazon Lex returns the next message to convey to the user.
-- Consider the following example messages:
--
-- -   For a user input \"I would like a pizza,\" Amazon Lex might return a
--     response with a message eliciting slot data (for example,
--     @PizzaSize@): \"What size pizza would you like?\".
--
-- -   After the user provides all of the pizza order information, Amazon
--     Lex might return a response with a message to get user confirmation:
--     \"Order the pizza?\".
--
-- -   After the user replies \"Yes\" to the confirmation prompt, Amazon
--     Lex might return a conclusion statement: \"Thank you, your cheese
--     pizza has been ordered.\".
--
-- Not all Amazon Lex messages require a response from the user. For
-- example, conclusion statements do not require a response. Some messages
-- require only a yes or no response. In addition to the @message@, Amazon
-- Lex provides additional context about the message in the response that
-- you can use to enhance client behavior, such as displaying the
-- appropriate client user interface. Consider the following examples:
--
-- -   If the message is to elicit slot data, Amazon Lex returns the
--     following context information:
--
--     -   @x-amz-lex-dialog-state@ header set to @ElicitSlot@
--
--     -   @x-amz-lex-intent-name@ header set to the intent name in the
--         current context
--
--     -   @x-amz-lex-slot-to-elicit@ header set to the slot name for which
--         the @message@ is eliciting information
--
--     -   @x-amz-lex-slots@ header set to a map of slots configured for
--         the intent with their current values
--
-- -   If the message is a confirmation prompt, the
--     @x-amz-lex-dialog-state@ header is set to @Confirmation@ and the
--     @x-amz-lex-slot-to-elicit@ header is omitted.
--
-- -   If the message is a clarification prompt configured for the intent,
--     indicating that the user intent is not understood, the
--     @x-amz-dialog-state@ header is set to @ElicitIntent@ and the
--     @x-amz-slot-to-elicit@ header is omitted.
--
-- In addition, Amazon Lex also returns your application-specific
-- @sessionAttributes@. For more information, see
-- <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html Managing Conversation Context>.
module Amazonka.LexRuntime.PostContent
  ( -- * Creating a Request
    PostContent (..),
    newPostContent,

    -- * Request Lenses
    postContent_sessionAttributes,
    postContent_accept,
    postContent_requestAttributes,
    postContent_activeContexts,
    postContent_botName,
    postContent_botAlias,
    postContent_userId,
    postContent_contentType,
    postContent_inputStream,

    -- * Destructuring the Response
    PostContentResponse (..),
    newPostContentResponse,

    -- * Response Lenses
    postContentResponse_message,
    postContentResponse_botVersion,
    postContentResponse_slotToElicit,
    postContentResponse_dialogState,
    postContentResponse_sessionAttributes,
    postContentResponse_messageFormat,
    postContentResponse_encodedMessage,
    postContentResponse_sentimentResponse,
    postContentResponse_sessionId,
    postContentResponse_intentName,
    postContentResponse_alternativeIntents,
    postContentResponse_inputTranscript,
    postContentResponse_activeContexts,
    postContentResponse_nluIntentConfidence,
    postContentResponse_slots,
    postContentResponse_encodedInputTranscript,
    postContentResponse_contentType,
    postContentResponse_httpStatus,
    postContentResponse_audioStream,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexRuntime.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPostContent' smart constructor.
data PostContent = PostContent'
  { -- | You pass this value as the @x-amz-lex-session-attributes@ HTTP header.
    --
    -- Application-specific information passed between Amazon Lex and a client
    -- application. The value must be a JSON serialized and base64 encoded map
    -- with string keys and values. The total size of the @sessionAttributes@
    -- and @requestAttributes@ headers is limited to 12 KB.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-session-attribs Setting Session Attributes>.
    sessionAttributes :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | You pass this value as the @Accept@ HTTP header.
    --
    -- The message Amazon Lex returns in the response can be either text or
    -- speech based on the @Accept@ HTTP header value in the request.
    --
    -- -   If the value is @text\/plain; charset=utf-8@, Amazon Lex returns
    --     text in the response.
    --
    -- -   If the value begins with @audio\/@, Amazon Lex returns speech in the
    --     response. Amazon Lex uses Amazon Polly to generate the speech (using
    --     the configuration you specified in the @Accept@ header). For
    --     example, if you specify @audio\/mpeg@ as the value, Amazon Lex
    --     returns speech in the MPEG format.
    --
    -- -   If the value is @audio\/pcm@, the speech returned is @audio\/pcm@ in
    --     16-bit, little endian format.
    --
    -- -   The following are the accepted values:
    --
    --     -   audio\/mpeg
    --
    --     -   audio\/ogg
    --
    --     -   audio\/pcm
    --
    --     -   text\/plain; charset=utf-8
    --
    --     -   audio\/* (defaults to mpeg)
    accept :: Prelude.Maybe Prelude.Text,
    -- | You pass this value as the @x-amz-lex-request-attributes@ HTTP header.
    --
    -- Request-specific information passed between Amazon Lex and a client
    -- application. The value must be a JSON serialized and base64 encoded map
    -- with string keys and values. The total size of the @requestAttributes@
    -- and @sessionAttributes@ headers is limited to 12 KB.
    --
    -- The namespace @x-amz-lex:@ is reserved for special attributes. Don\'t
    -- create any request attributes with the prefix @x-amz-lex:@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-request-attribs Setting Request Attributes>.
    requestAttributes :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A list of contexts active for the request. A context can be activated
    -- when a previous intent is fulfilled, or by including the context in the
    -- request,
    --
    -- If you don\'t specify a list of contexts, Amazon Lex will use the
    -- current list of contexts for the session. If you specify an empty list,
    -- all contexts for the session are cleared.
    activeContexts :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Name of the Amazon Lex bot.
    botName :: Prelude.Text,
    -- | Alias of the Amazon Lex bot.
    botAlias :: Prelude.Text,
    -- | The ID of the client application user. Amazon Lex uses this to identify
    -- a user\'s conversation with your bot. At runtime, each request must
    -- contain the @userID@ field.
    --
    -- To decide the user ID to use for your application, consider the
    -- following factors.
    --
    -- -   The @userID@ field must not contain any personally identifiable
    --     information of the user, for example, name, personal identification
    --     numbers, or other end user personal information.
    --
    -- -   If you want a user to start a conversation on one device and
    --     continue on another device, use a user-specific identifier.
    --
    -- -   If you want the same user to be able to have two independent
    --     conversations on two different devices, choose a device-specific
    --     identifier.
    --
    -- -   A user can\'t have two independent conversations with two different
    --     versions of the same bot. For example, a user can\'t have a
    --     conversation with the PROD and BETA versions of the same bot. If you
    --     anticipate that a user will need to have conversation with two
    --     different versions, for example, while testing, include the bot
    --     alias in the user ID to separate the two conversations.
    userId :: Prelude.Text,
    -- | You pass this value as the @Content-Type@ HTTP header.
    --
    -- Indicates the audio format or text. The header value must start with one
    -- of the following prefixes:
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
    --     -   audio\/x-cbr-opus-with-preamble; preamble-size=0;
    --         bit-rate=256000; frame-size-milliseconds=4
    --
    -- -   Text format
    --
    --     -   text\/plain; charset=utf-8
    contentType :: Prelude.Text,
    -- | User input in PCM or Opus audio format or text format as described in
    -- the @Content-Type@ HTTP header.
    --
    -- You can stream audio data to Amazon Lex or you can create a local buffer
    -- that captures all of the audio data before sending. In general, you get
    -- better performance if you stream audio data rather than buffering the
    -- data locally.
    inputStream :: Data.HashedBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PostContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionAttributes', 'postContent_sessionAttributes' - You pass this value as the @x-amz-lex-session-attributes@ HTTP header.
--
-- Application-specific information passed between Amazon Lex and a client
-- application. The value must be a JSON serialized and base64 encoded map
-- with string keys and values. The total size of the @sessionAttributes@
-- and @requestAttributes@ headers is limited to 12 KB.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-session-attribs Setting Session Attributes>.
--
-- 'accept', 'postContent_accept' - You pass this value as the @Accept@ HTTP header.
--
-- The message Amazon Lex returns in the response can be either text or
-- speech based on the @Accept@ HTTP header value in the request.
--
-- -   If the value is @text\/plain; charset=utf-8@, Amazon Lex returns
--     text in the response.
--
-- -   If the value begins with @audio\/@, Amazon Lex returns speech in the
--     response. Amazon Lex uses Amazon Polly to generate the speech (using
--     the configuration you specified in the @Accept@ header). For
--     example, if you specify @audio\/mpeg@ as the value, Amazon Lex
--     returns speech in the MPEG format.
--
-- -   If the value is @audio\/pcm@, the speech returned is @audio\/pcm@ in
--     16-bit, little endian format.
--
-- -   The following are the accepted values:
--
--     -   audio\/mpeg
--
--     -   audio\/ogg
--
--     -   audio\/pcm
--
--     -   text\/plain; charset=utf-8
--
--     -   audio\/* (defaults to mpeg)
--
-- 'requestAttributes', 'postContent_requestAttributes' - You pass this value as the @x-amz-lex-request-attributes@ HTTP header.
--
-- Request-specific information passed between Amazon Lex and a client
-- application. The value must be a JSON serialized and base64 encoded map
-- with string keys and values. The total size of the @requestAttributes@
-- and @sessionAttributes@ headers is limited to 12 KB.
--
-- The namespace @x-amz-lex:@ is reserved for special attributes. Don\'t
-- create any request attributes with the prefix @x-amz-lex:@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-request-attribs Setting Request Attributes>.
--
-- 'activeContexts', 'postContent_activeContexts' - A list of contexts active for the request. A context can be activated
-- when a previous intent is fulfilled, or by including the context in the
-- request,
--
-- If you don\'t specify a list of contexts, Amazon Lex will use the
-- current list of contexts for the session. If you specify an empty list,
-- all contexts for the session are cleared.
--
-- 'botName', 'postContent_botName' - Name of the Amazon Lex bot.
--
-- 'botAlias', 'postContent_botAlias' - Alias of the Amazon Lex bot.
--
-- 'userId', 'postContent_userId' - The ID of the client application user. Amazon Lex uses this to identify
-- a user\'s conversation with your bot. At runtime, each request must
-- contain the @userID@ field.
--
-- To decide the user ID to use for your application, consider the
-- following factors.
--
-- -   The @userID@ field must not contain any personally identifiable
--     information of the user, for example, name, personal identification
--     numbers, or other end user personal information.
--
-- -   If you want a user to start a conversation on one device and
--     continue on another device, use a user-specific identifier.
--
-- -   If you want the same user to be able to have two independent
--     conversations on two different devices, choose a device-specific
--     identifier.
--
-- -   A user can\'t have two independent conversations with two different
--     versions of the same bot. For example, a user can\'t have a
--     conversation with the PROD and BETA versions of the same bot. If you
--     anticipate that a user will need to have conversation with two
--     different versions, for example, while testing, include the bot
--     alias in the user ID to separate the two conversations.
--
-- 'contentType', 'postContent_contentType' - You pass this value as the @Content-Type@ HTTP header.
--
-- Indicates the audio format or text. The header value must start with one
-- of the following prefixes:
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
--     -   audio\/x-cbr-opus-with-preamble; preamble-size=0;
--         bit-rate=256000; frame-size-milliseconds=4
--
-- -   Text format
--
--     -   text\/plain; charset=utf-8
--
-- 'inputStream', 'postContent_inputStream' - User input in PCM or Opus audio format or text format as described in
-- the @Content-Type@ HTTP header.
--
-- You can stream audio data to Amazon Lex or you can create a local buffer
-- that captures all of the audio data before sending. In general, you get
-- better performance if you stream audio data rather than buffering the
-- data locally.
newPostContent ::
  -- | 'botName'
  Prelude.Text ->
  -- | 'botAlias'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  -- | 'contentType'
  Prelude.Text ->
  -- | 'inputStream'
  Data.HashedBody ->
  PostContent
newPostContent
  pBotName_
  pBotAlias_
  pUserId_
  pContentType_
  pInputStream_ =
    PostContent'
      { sessionAttributes = Prelude.Nothing,
        accept = Prelude.Nothing,
        requestAttributes = Prelude.Nothing,
        activeContexts = Prelude.Nothing,
        botName = pBotName_,
        botAlias = pBotAlias_,
        userId = pUserId_,
        contentType = pContentType_,
        inputStream = pInputStream_
      }

-- | You pass this value as the @x-amz-lex-session-attributes@ HTTP header.
--
-- Application-specific information passed between Amazon Lex and a client
-- application. The value must be a JSON serialized and base64 encoded map
-- with string keys and values. The total size of the @sessionAttributes@
-- and @requestAttributes@ headers is limited to 12 KB.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-session-attribs Setting Session Attributes>.
postContent_sessionAttributes :: Lens.Lens' PostContent (Prelude.Maybe Prelude.Text)
postContent_sessionAttributes = Lens.lens (\PostContent' {sessionAttributes} -> sessionAttributes) (\s@PostContent' {} a -> s {sessionAttributes = a} :: PostContent) Prelude.. Lens.mapping Data._Sensitive

-- | You pass this value as the @Accept@ HTTP header.
--
-- The message Amazon Lex returns in the response can be either text or
-- speech based on the @Accept@ HTTP header value in the request.
--
-- -   If the value is @text\/plain; charset=utf-8@, Amazon Lex returns
--     text in the response.
--
-- -   If the value begins with @audio\/@, Amazon Lex returns speech in the
--     response. Amazon Lex uses Amazon Polly to generate the speech (using
--     the configuration you specified in the @Accept@ header). For
--     example, if you specify @audio\/mpeg@ as the value, Amazon Lex
--     returns speech in the MPEG format.
--
-- -   If the value is @audio\/pcm@, the speech returned is @audio\/pcm@ in
--     16-bit, little endian format.
--
-- -   The following are the accepted values:
--
--     -   audio\/mpeg
--
--     -   audio\/ogg
--
--     -   audio\/pcm
--
--     -   text\/plain; charset=utf-8
--
--     -   audio\/* (defaults to mpeg)
postContent_accept :: Lens.Lens' PostContent (Prelude.Maybe Prelude.Text)
postContent_accept = Lens.lens (\PostContent' {accept} -> accept) (\s@PostContent' {} a -> s {accept = a} :: PostContent)

-- | You pass this value as the @x-amz-lex-request-attributes@ HTTP header.
--
-- Request-specific information passed between Amazon Lex and a client
-- application. The value must be a JSON serialized and base64 encoded map
-- with string keys and values. The total size of the @requestAttributes@
-- and @sessionAttributes@ headers is limited to 12 KB.
--
-- The namespace @x-amz-lex:@ is reserved for special attributes. Don\'t
-- create any request attributes with the prefix @x-amz-lex:@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-request-attribs Setting Request Attributes>.
postContent_requestAttributes :: Lens.Lens' PostContent (Prelude.Maybe Prelude.Text)
postContent_requestAttributes = Lens.lens (\PostContent' {requestAttributes} -> requestAttributes) (\s@PostContent' {} a -> s {requestAttributes = a} :: PostContent) Prelude.. Lens.mapping Data._Sensitive

-- | A list of contexts active for the request. A context can be activated
-- when a previous intent is fulfilled, or by including the context in the
-- request,
--
-- If you don\'t specify a list of contexts, Amazon Lex will use the
-- current list of contexts for the session. If you specify an empty list,
-- all contexts for the session are cleared.
postContent_activeContexts :: Lens.Lens' PostContent (Prelude.Maybe Prelude.Text)
postContent_activeContexts = Lens.lens (\PostContent' {activeContexts} -> activeContexts) (\s@PostContent' {} a -> s {activeContexts = a} :: PostContent) Prelude.. Lens.mapping Data._Sensitive

-- | Name of the Amazon Lex bot.
postContent_botName :: Lens.Lens' PostContent Prelude.Text
postContent_botName = Lens.lens (\PostContent' {botName} -> botName) (\s@PostContent' {} a -> s {botName = a} :: PostContent)

-- | Alias of the Amazon Lex bot.
postContent_botAlias :: Lens.Lens' PostContent Prelude.Text
postContent_botAlias = Lens.lens (\PostContent' {botAlias} -> botAlias) (\s@PostContent' {} a -> s {botAlias = a} :: PostContent)

-- | The ID of the client application user. Amazon Lex uses this to identify
-- a user\'s conversation with your bot. At runtime, each request must
-- contain the @userID@ field.
--
-- To decide the user ID to use for your application, consider the
-- following factors.
--
-- -   The @userID@ field must not contain any personally identifiable
--     information of the user, for example, name, personal identification
--     numbers, or other end user personal information.
--
-- -   If you want a user to start a conversation on one device and
--     continue on another device, use a user-specific identifier.
--
-- -   If you want the same user to be able to have two independent
--     conversations on two different devices, choose a device-specific
--     identifier.
--
-- -   A user can\'t have two independent conversations with two different
--     versions of the same bot. For example, a user can\'t have a
--     conversation with the PROD and BETA versions of the same bot. If you
--     anticipate that a user will need to have conversation with two
--     different versions, for example, while testing, include the bot
--     alias in the user ID to separate the two conversations.
postContent_userId :: Lens.Lens' PostContent Prelude.Text
postContent_userId = Lens.lens (\PostContent' {userId} -> userId) (\s@PostContent' {} a -> s {userId = a} :: PostContent)

-- | You pass this value as the @Content-Type@ HTTP header.
--
-- Indicates the audio format or text. The header value must start with one
-- of the following prefixes:
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
--     -   audio\/x-cbr-opus-with-preamble; preamble-size=0;
--         bit-rate=256000; frame-size-milliseconds=4
--
-- -   Text format
--
--     -   text\/plain; charset=utf-8
postContent_contentType :: Lens.Lens' PostContent Prelude.Text
postContent_contentType = Lens.lens (\PostContent' {contentType} -> contentType) (\s@PostContent' {} a -> s {contentType = a} :: PostContent)

-- | User input in PCM or Opus audio format or text format as described in
-- the @Content-Type@ HTTP header.
--
-- You can stream audio data to Amazon Lex or you can create a local buffer
-- that captures all of the audio data before sending. In general, you get
-- better performance if you stream audio data rather than buffering the
-- data locally.
postContent_inputStream :: Lens.Lens' PostContent Data.HashedBody
postContent_inputStream = Lens.lens (\PostContent' {inputStream} -> inputStream) (\s@PostContent' {} a -> s {inputStream = a} :: PostContent)

instance Core.AWSRequest PostContent where
  type AWSResponse PostContent = PostContentResponse
  request overrides =
    Request.postBody (overrides defaultService)
  response =
    Response.receiveBody
      ( \s h x ->
          PostContentResponse'
            Prelude.<$> (h Data..#? "x-amz-lex-message")
            Prelude.<*> (h Data..#? "x-amz-lex-bot-version")
            Prelude.<*> (h Data..#? "x-amz-lex-slot-to-elicit")
            Prelude.<*> (h Data..#? "x-amz-lex-dialog-state")
            Prelude.<*> (h Data..#? "x-amz-lex-session-attributes")
            Prelude.<*> (h Data..#? "x-amz-lex-message-format")
            Prelude.<*> (h Data..#? "x-amz-lex-encoded-message")
            Prelude.<*> (h Data..#? "x-amz-lex-sentiment")
            Prelude.<*> (h Data..#? "x-amz-lex-session-id")
            Prelude.<*> (h Data..#? "x-amz-lex-intent-name")
            Prelude.<*> (h Data..#? "x-amz-lex-alternative-intents")
            Prelude.<*> (h Data..#? "x-amz-lex-input-transcript")
            Prelude.<*> (h Data..#? "x-amz-lex-active-contexts")
            Prelude.<*> (h Data..#? "x-amz-lex-nlu-intent-confidence")
            Prelude.<*> (h Data..#? "x-amz-lex-slots")
            Prelude.<*> (h Data..#? "x-amz-lex-encoded-input-transcript")
            Prelude.<*> (h Data..#? "Content-Type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.pure x)
      )

instance Data.ToBody PostContent where
  toBody PostContent' {..} = Data.toBody inputStream

instance Data.ToHeaders PostContent where
  toHeaders PostContent' {..} =
    Prelude.mconcat
      [ "x-amz-lex-session-attributes"
          Data.=# sessionAttributes,
        "Accept" Data.=# accept,
        "x-amz-lex-request-attributes"
          Data.=# requestAttributes,
        "x-amz-lex-active-contexts" Data.=# activeContexts,
        "Content-Type" Data.=# contentType
      ]

instance Data.ToPath PostContent where
  toPath PostContent' {..} =
    Prelude.mconcat
      [ "/bot/",
        Data.toBS botName,
        "/alias/",
        Data.toBS botAlias,
        "/user/",
        Data.toBS userId,
        "/content"
      ]

instance Data.ToQuery PostContent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPostContentResponse' smart constructor.
data PostContentResponse = PostContentResponse'
  { -- | You can only use this field in the de-DE, en-AU, en-GB, en-US, es-419,
    -- es-ES, es-US, fr-CA, fr-FR, and it-IT locales. In all other locales, the
    -- @message@ field is null. You should use the @encodedMessage@ field
    -- instead.
    --
    -- The message to convey to the user. The message can come from the bot\'s
    -- configuration or from a Lambda function.
    --
    -- If the intent is not configured with a Lambda function, or if the Lambda
    -- function returned @Delegate@ as the @dialogAction.type@ in its response,
    -- Amazon Lex decides on the next course of action and selects an
    -- appropriate message from the bot\'s configuration based on the current
    -- interaction context. For example, if Amazon Lex isn\'t able to
    -- understand user input, it uses a clarification prompt message.
    --
    -- When you create an intent you can assign messages to groups. When
    -- messages are assigned to groups Amazon Lex returns one message from each
    -- group in the response. The message field is an escaped JSON string
    -- containing the messages. For more information about the structure of the
    -- JSON string returned, see msg-prompts-formats.
    --
    -- If the Lambda function returns a message, Amazon Lex passes it to the
    -- client in its response.
    message :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The version of the bot that responded to the conversation. You can use
    -- this information to help determine if one version of a bot is performing
    -- better than another version.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | If the @dialogState@ value is @ElicitSlot@, returns the name of the slot
    -- for which Amazon Lex is eliciting a value.
    slotToElicit :: Prelude.Maybe Prelude.Text,
    -- | Identifies the current state of the user interaction. Amazon Lex returns
    -- one of the following values as @dialogState@. The client can optionally
    -- use this information to customize the user interface.
    --
    -- -   @ElicitIntent@ - Amazon Lex wants to elicit the user\'s intent.
    --     Consider the following examples:
    --
    --     For example, a user might utter an intent (\"I want to order a
    --     pizza\"). If Amazon Lex cannot infer the user intent from this
    --     utterance, it will return this dialog state.
    --
    -- -   @ConfirmIntent@ - Amazon Lex is expecting a \"yes\" or \"no\"
    --     response.
    --
    --     For example, Amazon Lex wants user confirmation before fulfilling an
    --     intent. Instead of a simple \"yes\" or \"no\" response, a user might
    --     respond with additional information. For example, \"yes, but make it
    --     a thick crust pizza\" or \"no, I want to order a drink.\" Amazon Lex
    --     can process such additional information (in these examples, update
    --     the crust type slot or change the intent from OrderPizza to
    --     OrderDrink).
    --
    -- -   @ElicitSlot@ - Amazon Lex is expecting the value of a slot for the
    --     current intent.
    --
    --     For example, suppose that in the response Amazon Lex sends this
    --     message: \"What size pizza would you like?\". A user might reply
    --     with the slot value (e.g., \"medium\"). The user might also provide
    --     additional information in the response (e.g., \"medium thick crust
    --     pizza\"). Amazon Lex can process such additional information
    --     appropriately.
    --
    -- -   @Fulfilled@ - Conveys that the Lambda function has successfully
    --     fulfilled the intent.
    --
    -- -   @ReadyForFulfillment@ - Conveys that the client has to fulfill the
    --     request.
    --
    -- -   @Failed@ - Conveys that the conversation with the user failed.
    --
    --     This can happen for various reasons, including that the user does
    --     not provide an appropriate response to prompts from the service (you
    --     can configure how many times Amazon Lex can prompt a user for
    --     specific information), or if the Lambda function fails to fulfill
    --     the intent.
    dialogState :: Prelude.Maybe DialogState,
    -- | Map of key\/value pairs representing the session-specific context
    -- information.
    sessionAttributes :: Prelude.Maybe Prelude.Text,
    -- | The format of the response message. One of the following values:
    --
    -- -   @PlainText@ - The message contains plain UTF-8 text.
    --
    -- -   @CustomPayload@ - The message is a custom format for the client.
    --
    -- -   @SSML@ - The message contains text formatted for voice output.
    --
    -- -   @Composite@ - The message contains an escaped JSON object containing
    --     one or more messages from the groups that messages were assigned to
    --     when the intent was created.
    messageFormat :: Prelude.Maybe MessageFormatType,
    -- | The message to convey to the user. The message can come from the bot\'s
    -- configuration or from a Lambda function.
    --
    -- If the intent is not configured with a Lambda function, or if the Lambda
    -- function returned @Delegate@ as the @dialogAction.type@ in its response,
    -- Amazon Lex decides on the next course of action and selects an
    -- appropriate message from the bot\'s configuration based on the current
    -- interaction context. For example, if Amazon Lex isn\'t able to
    -- understand user input, it uses a clarification prompt message.
    --
    -- When you create an intent you can assign messages to groups. When
    -- messages are assigned to groups Amazon Lex returns one message from each
    -- group in the response. The message field is an escaped JSON string
    -- containing the messages. For more information about the structure of the
    -- JSON string returned, see msg-prompts-formats.
    --
    -- If the Lambda function returns a message, Amazon Lex passes it to the
    -- client in its response.
    --
    -- The @encodedMessage@ field is base-64 encoded. You must decode the field
    -- before you can use the value.
    encodedMessage :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The sentiment expressed in an utterance.
    --
    -- When the bot is configured to send utterances to Amazon Comprehend for
    -- sentiment analysis, this field contains the result of the analysis.
    sentimentResponse :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the session.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | Current user intent that Amazon Lex is aware of.
    intentName :: Prelude.Maybe Prelude.Text,
    -- | One to four alternative intents that may be applicable to the user\'s
    -- intent.
    --
    -- Each alternative includes a score that indicates how confident Amazon
    -- Lex is that the intent matches the user\'s intent. The intents are
    -- sorted by the confidence score.
    alternativeIntents :: Prelude.Maybe Prelude.Text,
    -- | The text used to process the request.
    --
    -- You can use this field only in the de-DE, en-AU, en-GB, en-US, es-419,
    -- es-ES, es-US, fr-CA, fr-FR, and it-IT locales. In all other locales, the
    -- @inputTranscript@ field is null. You should use the
    -- @encodedInputTranscript@ field instead.
    --
    -- If the input was an audio stream, the @inputTranscript@ field contains
    -- the text extracted from the audio stream. This is the text that is
    -- actually processed to recognize intents and slot values. You can use
    -- this information to determine if Amazon Lex is correctly processing the
    -- audio that you send.
    inputTranscript :: Prelude.Maybe Prelude.Text,
    -- | A list of active contexts for the session. A context can be set when an
    -- intent is fulfilled or by calling the @PostContent@, @PostText@, or
    -- @PutSession@ operation.
    --
    -- You can use a context to control the intents that can follow up an
    -- intent, or to modify the operation of your application.
    activeContexts :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Provides a score that indicates how confident Amazon Lex is that the
    -- returned intent is the one that matches the user\'s intent. The score is
    -- between 0.0 and 1.0.
    --
    -- The score is a relative score, not an absolute score. The score may
    -- change based on improvements to Amazon Lex.
    nluIntentConfidence :: Prelude.Maybe Prelude.Text,
    -- | Map of zero or more intent slots (name\/value pairs) Amazon Lex detected
    -- from the user input during the conversation. The field is base-64
    -- encoded.
    --
    -- Amazon Lex creates a resolution list containing likely values for a
    -- slot. The value that it returns is determined by the
    -- @valueSelectionStrategy@ selected when the slot type was created or
    -- updated. If @valueSelectionStrategy@ is set to @ORIGINAL_VALUE@, the
    -- value provided by the user is returned, if the user value is similar to
    -- the slot values. If @valueSelectionStrategy@ is set to @TOP_RESOLUTION@
    -- Amazon Lex returns the first value in the resolution list or, if there
    -- is no resolution list, null. If you don\'t specify a
    -- @valueSelectionStrategy@, the default is @ORIGINAL_VALUE@.
    slots :: Prelude.Maybe Prelude.Text,
    -- | The text used to process the request.
    --
    -- If the input was an audio stream, the @encodedInputTranscript@ field
    -- contains the text extracted from the audio stream. This is the text that
    -- is actually processed to recognize intents and slot values. You can use
    -- this information to determine if Amazon Lex is correctly processing the
    -- audio that you send.
    --
    -- The @encodedInputTranscript@ field is base-64 encoded. You must decode
    -- the field before you can use the value.
    encodedInputTranscript :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Content type as specified in the @Accept@ HTTP header in the request.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The prompt (or statement) to convey to the user. This is based on the
    -- bot configuration and context. For example, if Amazon Lex did not
    -- understand the user intent, it sends the @clarificationPrompt@
    -- configured for the bot. If the intent requires confirmation before
    -- taking the fulfillment action, it sends the @confirmationPrompt@.
    -- Another example: Suppose that the Lambda function successfully fulfilled
    -- the intent, and sent a message to convey to the user. Then Amazon Lex
    -- sends that message in the response.
    audioStream :: Data.ResponseBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PostContentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'postContentResponse_message' - You can only use this field in the de-DE, en-AU, en-GB, en-US, es-419,
-- es-ES, es-US, fr-CA, fr-FR, and it-IT locales. In all other locales, the
-- @message@ field is null. You should use the @encodedMessage@ field
-- instead.
--
-- The message to convey to the user. The message can come from the bot\'s
-- configuration or from a Lambda function.
--
-- If the intent is not configured with a Lambda function, or if the Lambda
-- function returned @Delegate@ as the @dialogAction.type@ in its response,
-- Amazon Lex decides on the next course of action and selects an
-- appropriate message from the bot\'s configuration based on the current
-- interaction context. For example, if Amazon Lex isn\'t able to
-- understand user input, it uses a clarification prompt message.
--
-- When you create an intent you can assign messages to groups. When
-- messages are assigned to groups Amazon Lex returns one message from each
-- group in the response. The message field is an escaped JSON string
-- containing the messages. For more information about the structure of the
-- JSON string returned, see msg-prompts-formats.
--
-- If the Lambda function returns a message, Amazon Lex passes it to the
-- client in its response.
--
-- 'botVersion', 'postContentResponse_botVersion' - The version of the bot that responded to the conversation. You can use
-- this information to help determine if one version of a bot is performing
-- better than another version.
--
-- 'slotToElicit', 'postContentResponse_slotToElicit' - If the @dialogState@ value is @ElicitSlot@, returns the name of the slot
-- for which Amazon Lex is eliciting a value.
--
-- 'dialogState', 'postContentResponse_dialogState' - Identifies the current state of the user interaction. Amazon Lex returns
-- one of the following values as @dialogState@. The client can optionally
-- use this information to customize the user interface.
--
-- -   @ElicitIntent@ - Amazon Lex wants to elicit the user\'s intent.
--     Consider the following examples:
--
--     For example, a user might utter an intent (\"I want to order a
--     pizza\"). If Amazon Lex cannot infer the user intent from this
--     utterance, it will return this dialog state.
--
-- -   @ConfirmIntent@ - Amazon Lex is expecting a \"yes\" or \"no\"
--     response.
--
--     For example, Amazon Lex wants user confirmation before fulfilling an
--     intent. Instead of a simple \"yes\" or \"no\" response, a user might
--     respond with additional information. For example, \"yes, but make it
--     a thick crust pizza\" or \"no, I want to order a drink.\" Amazon Lex
--     can process such additional information (in these examples, update
--     the crust type slot or change the intent from OrderPizza to
--     OrderDrink).
--
-- -   @ElicitSlot@ - Amazon Lex is expecting the value of a slot for the
--     current intent.
--
--     For example, suppose that in the response Amazon Lex sends this
--     message: \"What size pizza would you like?\". A user might reply
--     with the slot value (e.g., \"medium\"). The user might also provide
--     additional information in the response (e.g., \"medium thick crust
--     pizza\"). Amazon Lex can process such additional information
--     appropriately.
--
-- -   @Fulfilled@ - Conveys that the Lambda function has successfully
--     fulfilled the intent.
--
-- -   @ReadyForFulfillment@ - Conveys that the client has to fulfill the
--     request.
--
-- -   @Failed@ - Conveys that the conversation with the user failed.
--
--     This can happen for various reasons, including that the user does
--     not provide an appropriate response to prompts from the service (you
--     can configure how many times Amazon Lex can prompt a user for
--     specific information), or if the Lambda function fails to fulfill
--     the intent.
--
-- 'sessionAttributes', 'postContentResponse_sessionAttributes' - Map of key\/value pairs representing the session-specific context
-- information.
--
-- 'messageFormat', 'postContentResponse_messageFormat' - The format of the response message. One of the following values:
--
-- -   @PlainText@ - The message contains plain UTF-8 text.
--
-- -   @CustomPayload@ - The message is a custom format for the client.
--
-- -   @SSML@ - The message contains text formatted for voice output.
--
-- -   @Composite@ - The message contains an escaped JSON object containing
--     one or more messages from the groups that messages were assigned to
--     when the intent was created.
--
-- 'encodedMessage', 'postContentResponse_encodedMessage' - The message to convey to the user. The message can come from the bot\'s
-- configuration or from a Lambda function.
--
-- If the intent is not configured with a Lambda function, or if the Lambda
-- function returned @Delegate@ as the @dialogAction.type@ in its response,
-- Amazon Lex decides on the next course of action and selects an
-- appropriate message from the bot\'s configuration based on the current
-- interaction context. For example, if Amazon Lex isn\'t able to
-- understand user input, it uses a clarification prompt message.
--
-- When you create an intent you can assign messages to groups. When
-- messages are assigned to groups Amazon Lex returns one message from each
-- group in the response. The message field is an escaped JSON string
-- containing the messages. For more information about the structure of the
-- JSON string returned, see msg-prompts-formats.
--
-- If the Lambda function returns a message, Amazon Lex passes it to the
-- client in its response.
--
-- The @encodedMessage@ field is base-64 encoded. You must decode the field
-- before you can use the value.
--
-- 'sentimentResponse', 'postContentResponse_sentimentResponse' - The sentiment expressed in an utterance.
--
-- When the bot is configured to send utterances to Amazon Comprehend for
-- sentiment analysis, this field contains the result of the analysis.
--
-- 'sessionId', 'postContentResponse_sessionId' - The unique identifier for the session.
--
-- 'intentName', 'postContentResponse_intentName' - Current user intent that Amazon Lex is aware of.
--
-- 'alternativeIntents', 'postContentResponse_alternativeIntents' - One to four alternative intents that may be applicable to the user\'s
-- intent.
--
-- Each alternative includes a score that indicates how confident Amazon
-- Lex is that the intent matches the user\'s intent. The intents are
-- sorted by the confidence score.
--
-- 'inputTranscript', 'postContentResponse_inputTranscript' - The text used to process the request.
--
-- You can use this field only in the de-DE, en-AU, en-GB, en-US, es-419,
-- es-ES, es-US, fr-CA, fr-FR, and it-IT locales. In all other locales, the
-- @inputTranscript@ field is null. You should use the
-- @encodedInputTranscript@ field instead.
--
-- If the input was an audio stream, the @inputTranscript@ field contains
-- the text extracted from the audio stream. This is the text that is
-- actually processed to recognize intents and slot values. You can use
-- this information to determine if Amazon Lex is correctly processing the
-- audio that you send.
--
-- 'activeContexts', 'postContentResponse_activeContexts' - A list of active contexts for the session. A context can be set when an
-- intent is fulfilled or by calling the @PostContent@, @PostText@, or
-- @PutSession@ operation.
--
-- You can use a context to control the intents that can follow up an
-- intent, or to modify the operation of your application.
--
-- 'nluIntentConfidence', 'postContentResponse_nluIntentConfidence' - Provides a score that indicates how confident Amazon Lex is that the
-- returned intent is the one that matches the user\'s intent. The score is
-- between 0.0 and 1.0.
--
-- The score is a relative score, not an absolute score. The score may
-- change based on improvements to Amazon Lex.
--
-- 'slots', 'postContentResponse_slots' - Map of zero or more intent slots (name\/value pairs) Amazon Lex detected
-- from the user input during the conversation. The field is base-64
-- encoded.
--
-- Amazon Lex creates a resolution list containing likely values for a
-- slot. The value that it returns is determined by the
-- @valueSelectionStrategy@ selected when the slot type was created or
-- updated. If @valueSelectionStrategy@ is set to @ORIGINAL_VALUE@, the
-- value provided by the user is returned, if the user value is similar to
-- the slot values. If @valueSelectionStrategy@ is set to @TOP_RESOLUTION@
-- Amazon Lex returns the first value in the resolution list or, if there
-- is no resolution list, null. If you don\'t specify a
-- @valueSelectionStrategy@, the default is @ORIGINAL_VALUE@.
--
-- 'encodedInputTranscript', 'postContentResponse_encodedInputTranscript' - The text used to process the request.
--
-- If the input was an audio stream, the @encodedInputTranscript@ field
-- contains the text extracted from the audio stream. This is the text that
-- is actually processed to recognize intents and slot values. You can use
-- this information to determine if Amazon Lex is correctly processing the
-- audio that you send.
--
-- The @encodedInputTranscript@ field is base-64 encoded. You must decode
-- the field before you can use the value.
--
-- 'contentType', 'postContentResponse_contentType' - Content type as specified in the @Accept@ HTTP header in the request.
--
-- 'httpStatus', 'postContentResponse_httpStatus' - The response's http status code.
--
-- 'audioStream', 'postContentResponse_audioStream' - The prompt (or statement) to convey to the user. This is based on the
-- bot configuration and context. For example, if Amazon Lex did not
-- understand the user intent, it sends the @clarificationPrompt@
-- configured for the bot. If the intent requires confirmation before
-- taking the fulfillment action, it sends the @confirmationPrompt@.
-- Another example: Suppose that the Lambda function successfully fulfilled
-- the intent, and sent a message to convey to the user. Then Amazon Lex
-- sends that message in the response.
newPostContentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'audioStream'
  Data.ResponseBody ->
  PostContentResponse
newPostContentResponse pHttpStatus_ pAudioStream_ =
  PostContentResponse'
    { message = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      slotToElicit = Prelude.Nothing,
      dialogState = Prelude.Nothing,
      sessionAttributes = Prelude.Nothing,
      messageFormat = Prelude.Nothing,
      encodedMessage = Prelude.Nothing,
      sentimentResponse = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      intentName = Prelude.Nothing,
      alternativeIntents = Prelude.Nothing,
      inputTranscript = Prelude.Nothing,
      activeContexts = Prelude.Nothing,
      nluIntentConfidence = Prelude.Nothing,
      slots = Prelude.Nothing,
      encodedInputTranscript = Prelude.Nothing,
      contentType = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      audioStream = pAudioStream_
    }

-- | You can only use this field in the de-DE, en-AU, en-GB, en-US, es-419,
-- es-ES, es-US, fr-CA, fr-FR, and it-IT locales. In all other locales, the
-- @message@ field is null. You should use the @encodedMessage@ field
-- instead.
--
-- The message to convey to the user. The message can come from the bot\'s
-- configuration or from a Lambda function.
--
-- If the intent is not configured with a Lambda function, or if the Lambda
-- function returned @Delegate@ as the @dialogAction.type@ in its response,
-- Amazon Lex decides on the next course of action and selects an
-- appropriate message from the bot\'s configuration based on the current
-- interaction context. For example, if Amazon Lex isn\'t able to
-- understand user input, it uses a clarification prompt message.
--
-- When you create an intent you can assign messages to groups. When
-- messages are assigned to groups Amazon Lex returns one message from each
-- group in the response. The message field is an escaped JSON string
-- containing the messages. For more information about the structure of the
-- JSON string returned, see msg-prompts-formats.
--
-- If the Lambda function returns a message, Amazon Lex passes it to the
-- client in its response.
postContentResponse_message :: Lens.Lens' PostContentResponse (Prelude.Maybe Prelude.Text)
postContentResponse_message = Lens.lens (\PostContentResponse' {message} -> message) (\s@PostContentResponse' {} a -> s {message = a} :: PostContentResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The version of the bot that responded to the conversation. You can use
-- this information to help determine if one version of a bot is performing
-- better than another version.
postContentResponse_botVersion :: Lens.Lens' PostContentResponse (Prelude.Maybe Prelude.Text)
postContentResponse_botVersion = Lens.lens (\PostContentResponse' {botVersion} -> botVersion) (\s@PostContentResponse' {} a -> s {botVersion = a} :: PostContentResponse)

-- | If the @dialogState@ value is @ElicitSlot@, returns the name of the slot
-- for which Amazon Lex is eliciting a value.
postContentResponse_slotToElicit :: Lens.Lens' PostContentResponse (Prelude.Maybe Prelude.Text)
postContentResponse_slotToElicit = Lens.lens (\PostContentResponse' {slotToElicit} -> slotToElicit) (\s@PostContentResponse' {} a -> s {slotToElicit = a} :: PostContentResponse)

-- | Identifies the current state of the user interaction. Amazon Lex returns
-- one of the following values as @dialogState@. The client can optionally
-- use this information to customize the user interface.
--
-- -   @ElicitIntent@ - Amazon Lex wants to elicit the user\'s intent.
--     Consider the following examples:
--
--     For example, a user might utter an intent (\"I want to order a
--     pizza\"). If Amazon Lex cannot infer the user intent from this
--     utterance, it will return this dialog state.
--
-- -   @ConfirmIntent@ - Amazon Lex is expecting a \"yes\" or \"no\"
--     response.
--
--     For example, Amazon Lex wants user confirmation before fulfilling an
--     intent. Instead of a simple \"yes\" or \"no\" response, a user might
--     respond with additional information. For example, \"yes, but make it
--     a thick crust pizza\" or \"no, I want to order a drink.\" Amazon Lex
--     can process such additional information (in these examples, update
--     the crust type slot or change the intent from OrderPizza to
--     OrderDrink).
--
-- -   @ElicitSlot@ - Amazon Lex is expecting the value of a slot for the
--     current intent.
--
--     For example, suppose that in the response Amazon Lex sends this
--     message: \"What size pizza would you like?\". A user might reply
--     with the slot value (e.g., \"medium\"). The user might also provide
--     additional information in the response (e.g., \"medium thick crust
--     pizza\"). Amazon Lex can process such additional information
--     appropriately.
--
-- -   @Fulfilled@ - Conveys that the Lambda function has successfully
--     fulfilled the intent.
--
-- -   @ReadyForFulfillment@ - Conveys that the client has to fulfill the
--     request.
--
-- -   @Failed@ - Conveys that the conversation with the user failed.
--
--     This can happen for various reasons, including that the user does
--     not provide an appropriate response to prompts from the service (you
--     can configure how many times Amazon Lex can prompt a user for
--     specific information), or if the Lambda function fails to fulfill
--     the intent.
postContentResponse_dialogState :: Lens.Lens' PostContentResponse (Prelude.Maybe DialogState)
postContentResponse_dialogState = Lens.lens (\PostContentResponse' {dialogState} -> dialogState) (\s@PostContentResponse' {} a -> s {dialogState = a} :: PostContentResponse)

-- | Map of key\/value pairs representing the session-specific context
-- information.
postContentResponse_sessionAttributes :: Lens.Lens' PostContentResponse (Prelude.Maybe Prelude.Text)
postContentResponse_sessionAttributes = Lens.lens (\PostContentResponse' {sessionAttributes} -> sessionAttributes) (\s@PostContentResponse' {} a -> s {sessionAttributes = a} :: PostContentResponse)

-- | The format of the response message. One of the following values:
--
-- -   @PlainText@ - The message contains plain UTF-8 text.
--
-- -   @CustomPayload@ - The message is a custom format for the client.
--
-- -   @SSML@ - The message contains text formatted for voice output.
--
-- -   @Composite@ - The message contains an escaped JSON object containing
--     one or more messages from the groups that messages were assigned to
--     when the intent was created.
postContentResponse_messageFormat :: Lens.Lens' PostContentResponse (Prelude.Maybe MessageFormatType)
postContentResponse_messageFormat = Lens.lens (\PostContentResponse' {messageFormat} -> messageFormat) (\s@PostContentResponse' {} a -> s {messageFormat = a} :: PostContentResponse)

-- | The message to convey to the user. The message can come from the bot\'s
-- configuration or from a Lambda function.
--
-- If the intent is not configured with a Lambda function, or if the Lambda
-- function returned @Delegate@ as the @dialogAction.type@ in its response,
-- Amazon Lex decides on the next course of action and selects an
-- appropriate message from the bot\'s configuration based on the current
-- interaction context. For example, if Amazon Lex isn\'t able to
-- understand user input, it uses a clarification prompt message.
--
-- When you create an intent you can assign messages to groups. When
-- messages are assigned to groups Amazon Lex returns one message from each
-- group in the response. The message field is an escaped JSON string
-- containing the messages. For more information about the structure of the
-- JSON string returned, see msg-prompts-formats.
--
-- If the Lambda function returns a message, Amazon Lex passes it to the
-- client in its response.
--
-- The @encodedMessage@ field is base-64 encoded. You must decode the field
-- before you can use the value.
postContentResponse_encodedMessage :: Lens.Lens' PostContentResponse (Prelude.Maybe Prelude.Text)
postContentResponse_encodedMessage = Lens.lens (\PostContentResponse' {encodedMessage} -> encodedMessage) (\s@PostContentResponse' {} a -> s {encodedMessage = a} :: PostContentResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The sentiment expressed in an utterance.
--
-- When the bot is configured to send utterances to Amazon Comprehend for
-- sentiment analysis, this field contains the result of the analysis.
postContentResponse_sentimentResponse :: Lens.Lens' PostContentResponse (Prelude.Maybe Prelude.Text)
postContentResponse_sentimentResponse = Lens.lens (\PostContentResponse' {sentimentResponse} -> sentimentResponse) (\s@PostContentResponse' {} a -> s {sentimentResponse = a} :: PostContentResponse)

-- | The unique identifier for the session.
postContentResponse_sessionId :: Lens.Lens' PostContentResponse (Prelude.Maybe Prelude.Text)
postContentResponse_sessionId = Lens.lens (\PostContentResponse' {sessionId} -> sessionId) (\s@PostContentResponse' {} a -> s {sessionId = a} :: PostContentResponse)

-- | Current user intent that Amazon Lex is aware of.
postContentResponse_intentName :: Lens.Lens' PostContentResponse (Prelude.Maybe Prelude.Text)
postContentResponse_intentName = Lens.lens (\PostContentResponse' {intentName} -> intentName) (\s@PostContentResponse' {} a -> s {intentName = a} :: PostContentResponse)

-- | One to four alternative intents that may be applicable to the user\'s
-- intent.
--
-- Each alternative includes a score that indicates how confident Amazon
-- Lex is that the intent matches the user\'s intent. The intents are
-- sorted by the confidence score.
postContentResponse_alternativeIntents :: Lens.Lens' PostContentResponse (Prelude.Maybe Prelude.Text)
postContentResponse_alternativeIntents = Lens.lens (\PostContentResponse' {alternativeIntents} -> alternativeIntents) (\s@PostContentResponse' {} a -> s {alternativeIntents = a} :: PostContentResponse)

-- | The text used to process the request.
--
-- You can use this field only in the de-DE, en-AU, en-GB, en-US, es-419,
-- es-ES, es-US, fr-CA, fr-FR, and it-IT locales. In all other locales, the
-- @inputTranscript@ field is null. You should use the
-- @encodedInputTranscript@ field instead.
--
-- If the input was an audio stream, the @inputTranscript@ field contains
-- the text extracted from the audio stream. This is the text that is
-- actually processed to recognize intents and slot values. You can use
-- this information to determine if Amazon Lex is correctly processing the
-- audio that you send.
postContentResponse_inputTranscript :: Lens.Lens' PostContentResponse (Prelude.Maybe Prelude.Text)
postContentResponse_inputTranscript = Lens.lens (\PostContentResponse' {inputTranscript} -> inputTranscript) (\s@PostContentResponse' {} a -> s {inputTranscript = a} :: PostContentResponse)

-- | A list of active contexts for the session. A context can be set when an
-- intent is fulfilled or by calling the @PostContent@, @PostText@, or
-- @PutSession@ operation.
--
-- You can use a context to control the intents that can follow up an
-- intent, or to modify the operation of your application.
postContentResponse_activeContexts :: Lens.Lens' PostContentResponse (Prelude.Maybe Prelude.Text)
postContentResponse_activeContexts = Lens.lens (\PostContentResponse' {activeContexts} -> activeContexts) (\s@PostContentResponse' {} a -> s {activeContexts = a} :: PostContentResponse) Prelude.. Lens.mapping Data._Sensitive

-- | Provides a score that indicates how confident Amazon Lex is that the
-- returned intent is the one that matches the user\'s intent. The score is
-- between 0.0 and 1.0.
--
-- The score is a relative score, not an absolute score. The score may
-- change based on improvements to Amazon Lex.
postContentResponse_nluIntentConfidence :: Lens.Lens' PostContentResponse (Prelude.Maybe Prelude.Text)
postContentResponse_nluIntentConfidence = Lens.lens (\PostContentResponse' {nluIntentConfidence} -> nluIntentConfidence) (\s@PostContentResponse' {} a -> s {nluIntentConfidence = a} :: PostContentResponse)

-- | Map of zero or more intent slots (name\/value pairs) Amazon Lex detected
-- from the user input during the conversation. The field is base-64
-- encoded.
--
-- Amazon Lex creates a resolution list containing likely values for a
-- slot. The value that it returns is determined by the
-- @valueSelectionStrategy@ selected when the slot type was created or
-- updated. If @valueSelectionStrategy@ is set to @ORIGINAL_VALUE@, the
-- value provided by the user is returned, if the user value is similar to
-- the slot values. If @valueSelectionStrategy@ is set to @TOP_RESOLUTION@
-- Amazon Lex returns the first value in the resolution list or, if there
-- is no resolution list, null. If you don\'t specify a
-- @valueSelectionStrategy@, the default is @ORIGINAL_VALUE@.
postContentResponse_slots :: Lens.Lens' PostContentResponse (Prelude.Maybe Prelude.Text)
postContentResponse_slots = Lens.lens (\PostContentResponse' {slots} -> slots) (\s@PostContentResponse' {} a -> s {slots = a} :: PostContentResponse)

-- | The text used to process the request.
--
-- If the input was an audio stream, the @encodedInputTranscript@ field
-- contains the text extracted from the audio stream. This is the text that
-- is actually processed to recognize intents and slot values. You can use
-- this information to determine if Amazon Lex is correctly processing the
-- audio that you send.
--
-- The @encodedInputTranscript@ field is base-64 encoded. You must decode
-- the field before you can use the value.
postContentResponse_encodedInputTranscript :: Lens.Lens' PostContentResponse (Prelude.Maybe Prelude.Text)
postContentResponse_encodedInputTranscript = Lens.lens (\PostContentResponse' {encodedInputTranscript} -> encodedInputTranscript) (\s@PostContentResponse' {} a -> s {encodedInputTranscript = a} :: PostContentResponse) Prelude.. Lens.mapping Data._Sensitive

-- | Content type as specified in the @Accept@ HTTP header in the request.
postContentResponse_contentType :: Lens.Lens' PostContentResponse (Prelude.Maybe Prelude.Text)
postContentResponse_contentType = Lens.lens (\PostContentResponse' {contentType} -> contentType) (\s@PostContentResponse' {} a -> s {contentType = a} :: PostContentResponse)

-- | The response's http status code.
postContentResponse_httpStatus :: Lens.Lens' PostContentResponse Prelude.Int
postContentResponse_httpStatus = Lens.lens (\PostContentResponse' {httpStatus} -> httpStatus) (\s@PostContentResponse' {} a -> s {httpStatus = a} :: PostContentResponse)

-- | The prompt (or statement) to convey to the user. This is based on the
-- bot configuration and context. For example, if Amazon Lex did not
-- understand the user intent, it sends the @clarificationPrompt@
-- configured for the bot. If the intent requires confirmation before
-- taking the fulfillment action, it sends the @confirmationPrompt@.
-- Another example: Suppose that the Lambda function successfully fulfilled
-- the intent, and sent a message to convey to the user. Then Amazon Lex
-- sends that message in the response.
postContentResponse_audioStream :: Lens.Lens' PostContentResponse Data.ResponseBody
postContentResponse_audioStream = Lens.lens (\PostContentResponse' {audioStream} -> audioStream) (\s@PostContentResponse' {} a -> s {audioStream = a} :: PostContentResponse)
