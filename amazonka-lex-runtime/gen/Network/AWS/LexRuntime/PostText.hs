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
-- Module      : Network.AWS.LexRuntime.PostText
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends user input to Amazon Lex. Client applications can use this API to
-- send requests to Amazon Lex at runtime. Amazon Lex then interprets the
-- user input using the machine learning model it built for the bot.
--
-- In response, Amazon Lex returns the next @message@ to convey to the user
-- an optional @responseCard@ to display. Consider the following example
-- messages:
--
-- -   For a user input \"I would like a pizza\", Amazon Lex might return a
--     response with a message eliciting slot data (for example,
--     PizzaSize): \"What size pizza would you like?\"
--
-- -   After the user provides all of the pizza order information, Amazon
--     Lex might return a response with a message to obtain user
--     confirmation \"Proceed with the pizza order?\".
--
-- -   After the user replies to a confirmation prompt with a \"yes\",
--     Amazon Lex might return a conclusion statement: \"Thank you, your
--     cheese pizza has been ordered.\".
--
-- Not all Amazon Lex messages require a user response. For example, a
-- conclusion statement does not require a response. Some messages require
-- only a \"yes\" or \"no\" user response. In addition to the @message@,
-- Amazon Lex provides additional context about the message in the response
-- that you might use to enhance client behavior, for example, to display
-- the appropriate client user interface. These are the @slotToElicit@,
-- @dialogState@, @intentName@, and @slots@ fields in the response.
-- Consider the following examples:
--
-- -   If the message is to elicit slot data, Amazon Lex returns the
--     following context information:
--
--     -   @dialogState@ set to ElicitSlot
--
--     -   @intentName@ set to the intent name in the current context
--
--     -   @slotToElicit@ set to the slot name for which the @message@ is
--         eliciting information
--
--     -   @slots@ set to a map of slots, configured for the intent, with
--         currently known values
--
-- -   If the message is a confirmation prompt, the @dialogState@ is set to
--     ConfirmIntent and @SlotToElicit@ is set to null.
--
-- -   If the message is a clarification prompt (configured for the intent)
--     that indicates that user intent is not understood, the @dialogState@
--     is set to ElicitIntent and @slotToElicit@ is set to null.
--
-- In addition, Amazon Lex also returns your application-specific
-- @sessionAttributes@. For more information, see
-- <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html Managing Conversation Context>.
module Network.AWS.LexRuntime.PostText
  ( -- * Creating a Request
    PostText (..),
    newPostText,

    -- * Request Lenses
    postText_sessionAttributes,
    postText_requestAttributes,
    postText_activeContexts,
    postText_botName,
    postText_botAlias,
    postText_userId,
    postText_inputText,

    -- * Destructuring the Response
    PostTextResponse (..),
    newPostTextResponse,

    -- * Response Lenses
    postTextResponse_responseCard,
    postTextResponse_dialogState,
    postTextResponse_sessionAttributes,
    postTextResponse_message,
    postTextResponse_sessionId,
    postTextResponse_intentName,
    postTextResponse_botVersion,
    postTextResponse_messageFormat,
    postTextResponse_slots,
    postTextResponse_nluIntentConfidence,
    postTextResponse_sentimentResponse,
    postTextResponse_slotToElicit,
    postTextResponse_activeContexts,
    postTextResponse_alternativeIntents,
    postTextResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexRuntime.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPostText' smart constructor.
data PostText = PostText'
  { -- | Application-specific information passed between Amazon Lex and a client
    -- application.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-session-attribs Setting Session Attributes>.
    sessionAttributes :: Core.Maybe (Core.Sensitive (Core.HashMap Core.Text Core.Text)),
    -- | Request-specific information passed between Amazon Lex and a client
    -- application.
    --
    -- The namespace @x-amz-lex:@ is reserved for special attributes. Don\'t
    -- create any request attributes with the prefix @x-amz-lex:@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-request-attribs Setting Request Attributes>.
    requestAttributes :: Core.Maybe (Core.Sensitive (Core.HashMap Core.Text Core.Text)),
    -- | A list of contexts active for the request. A context can be activated
    -- when a previous intent is fulfilled, or by including the context in the
    -- request,
    --
    -- If you don\'t specify a list of contexts, Amazon Lex will use the
    -- current list of contexts for the session. If you specify an empty list,
    -- all contexts for the session are cleared.
    activeContexts :: Core.Maybe (Core.Sensitive [ActiveContext]),
    -- | The name of the Amazon Lex bot.
    botName :: Core.Text,
    -- | The alias of the Amazon Lex bot.
    botAlias :: Core.Text,
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
    userId :: Core.Text,
    -- | The text that the user entered (Amazon Lex interprets this text).
    inputText :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'PostText' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionAttributes', 'postText_sessionAttributes' - Application-specific information passed between Amazon Lex and a client
-- application.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-session-attribs Setting Session Attributes>.
--
-- 'requestAttributes', 'postText_requestAttributes' - Request-specific information passed between Amazon Lex and a client
-- application.
--
-- The namespace @x-amz-lex:@ is reserved for special attributes. Don\'t
-- create any request attributes with the prefix @x-amz-lex:@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-request-attribs Setting Request Attributes>.
--
-- 'activeContexts', 'postText_activeContexts' - A list of contexts active for the request. A context can be activated
-- when a previous intent is fulfilled, or by including the context in the
-- request,
--
-- If you don\'t specify a list of contexts, Amazon Lex will use the
-- current list of contexts for the session. If you specify an empty list,
-- all contexts for the session are cleared.
--
-- 'botName', 'postText_botName' - The name of the Amazon Lex bot.
--
-- 'botAlias', 'postText_botAlias' - The alias of the Amazon Lex bot.
--
-- 'userId', 'postText_userId' - The ID of the client application user. Amazon Lex uses this to identify
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
-- 'inputText', 'postText_inputText' - The text that the user entered (Amazon Lex interprets this text).
newPostText ::
  -- | 'botName'
  Core.Text ->
  -- | 'botAlias'
  Core.Text ->
  -- | 'userId'
  Core.Text ->
  -- | 'inputText'
  Core.Text ->
  PostText
newPostText pBotName_ pBotAlias_ pUserId_ pInputText_ =
  PostText'
    { sessionAttributes = Core.Nothing,
      requestAttributes = Core.Nothing,
      activeContexts = Core.Nothing,
      botName = pBotName_,
      botAlias = pBotAlias_,
      userId = pUserId_,
      inputText = Core._Sensitive Lens.# pInputText_
    }

-- | Application-specific information passed between Amazon Lex and a client
-- application.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-session-attribs Setting Session Attributes>.
postText_sessionAttributes :: Lens.Lens' PostText (Core.Maybe (Core.HashMap Core.Text Core.Text))
postText_sessionAttributes = Lens.lens (\PostText' {sessionAttributes} -> sessionAttributes) (\s@PostText' {} a -> s {sessionAttributes = a} :: PostText) Core.. Lens.mapping (Core._Sensitive Core.. Lens._Coerce)

-- | Request-specific information passed between Amazon Lex and a client
-- application.
--
-- The namespace @x-amz-lex:@ is reserved for special attributes. Don\'t
-- create any request attributes with the prefix @x-amz-lex:@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-request-attribs Setting Request Attributes>.
postText_requestAttributes :: Lens.Lens' PostText (Core.Maybe (Core.HashMap Core.Text Core.Text))
postText_requestAttributes = Lens.lens (\PostText' {requestAttributes} -> requestAttributes) (\s@PostText' {} a -> s {requestAttributes = a} :: PostText) Core.. Lens.mapping (Core._Sensitive Core.. Lens._Coerce)

-- | A list of contexts active for the request. A context can be activated
-- when a previous intent is fulfilled, or by including the context in the
-- request,
--
-- If you don\'t specify a list of contexts, Amazon Lex will use the
-- current list of contexts for the session. If you specify an empty list,
-- all contexts for the session are cleared.
postText_activeContexts :: Lens.Lens' PostText (Core.Maybe [ActiveContext])
postText_activeContexts = Lens.lens (\PostText' {activeContexts} -> activeContexts) (\s@PostText' {} a -> s {activeContexts = a} :: PostText) Core.. Lens.mapping (Core._Sensitive Core.. Lens._Coerce)

-- | The name of the Amazon Lex bot.
postText_botName :: Lens.Lens' PostText Core.Text
postText_botName = Lens.lens (\PostText' {botName} -> botName) (\s@PostText' {} a -> s {botName = a} :: PostText)

-- | The alias of the Amazon Lex bot.
postText_botAlias :: Lens.Lens' PostText Core.Text
postText_botAlias = Lens.lens (\PostText' {botAlias} -> botAlias) (\s@PostText' {} a -> s {botAlias = a} :: PostText)

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
postText_userId :: Lens.Lens' PostText Core.Text
postText_userId = Lens.lens (\PostText' {userId} -> userId) (\s@PostText' {} a -> s {userId = a} :: PostText)

-- | The text that the user entered (Amazon Lex interprets this text).
postText_inputText :: Lens.Lens' PostText Core.Text
postText_inputText = Lens.lens (\PostText' {inputText} -> inputText) (\s@PostText' {} a -> s {inputText = a} :: PostText) Core.. Core._Sensitive

instance Core.AWSRequest PostText where
  type AWSResponse PostText = PostTextResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PostTextResponse'
            Core.<$> (x Core..?> "responseCard")
            Core.<*> (x Core..?> "dialogState")
            Core.<*> (x Core..?> "sessionAttributes" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "message")
            Core.<*> (x Core..?> "sessionId")
            Core.<*> (x Core..?> "intentName")
            Core.<*> (x Core..?> "botVersion")
            Core.<*> (x Core..?> "messageFormat")
            Core.<*> (x Core..?> "slots" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "nluIntentConfidence")
            Core.<*> (x Core..?> "sentimentResponse")
            Core.<*> (x Core..?> "slotToElicit")
            Core.<*> (x Core..?> "activeContexts" Core..!@ Core.mempty)
            Core.<*> ( x Core..?> "alternativeIntents"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PostText

instance Core.NFData PostText

instance Core.ToHeaders PostText where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PostText where
  toJSON PostText' {..} =
    Core.object
      ( Core.catMaybes
          [ ("sessionAttributes" Core..=)
              Core.<$> sessionAttributes,
            ("requestAttributes" Core..=)
              Core.<$> requestAttributes,
            ("activeContexts" Core..=) Core.<$> activeContexts,
            Core.Just ("inputText" Core..= inputText)
          ]
      )

instance Core.ToPath PostText where
  toPath PostText' {..} =
    Core.mconcat
      [ "/bot/",
        Core.toBS botName,
        "/alias/",
        Core.toBS botAlias,
        "/user/",
        Core.toBS userId,
        "/text"
      ]

instance Core.ToQuery PostText where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPostTextResponse' smart constructor.
data PostTextResponse = PostTextResponse'
  { -- | Represents the options that the user has to respond to the current
    -- prompt. Response Card can come from the bot configuration (in the Amazon
    -- Lex console, choose the settings button next to a slot) or from a code
    -- hook (Lambda function).
    responseCard :: Core.Maybe ResponseCard,
    -- | Identifies the current state of the user interaction. Amazon Lex returns
    -- one of the following values as @dialogState@. The client can optionally
    -- use this information to customize the user interface.
    --
    -- -   @ElicitIntent@ - Amazon Lex wants to elicit user intent.
    --
    --     For example, a user might utter an intent (\"I want to order a
    --     pizza\"). If Amazon Lex cannot infer the user intent from this
    --     utterance, it will return this dialogState.
    --
    -- -   @ConfirmIntent@ - Amazon Lex is expecting a \"yes\" or \"no\"
    --     response.
    --
    --     For example, Amazon Lex wants user confirmation before fulfilling an
    --     intent.
    --
    --     Instead of a simple \"yes\" or \"no,\" a user might respond with
    --     additional information. For example, \"yes, but make it thick crust
    --     pizza\" or \"no, I want to order a drink\". Amazon Lex can process
    --     such additional information (in these examples, update the crust
    --     type slot value, or change intent from OrderPizza to OrderDrink).
    --
    -- -   @ElicitSlot@ - Amazon Lex is expecting a slot value for the current
    --     intent.
    --
    --     For example, suppose that in the response Amazon Lex sends this
    --     message: \"What size pizza would you like?\". A user might reply
    --     with the slot value (e.g., \"medium\"). The user might also provide
    --     additional information in the response (e.g., \"medium thick crust
    --     pizza\"). Amazon Lex can process such additional information
    --     appropriately.
    --
    -- -   @Fulfilled@ - Conveys that the Lambda function configured for the
    --     intent has successfully fulfilled the intent.
    --
    -- -   @ReadyForFulfillment@ - Conveys that the client has to fulfill the
    --     intent.
    --
    -- -   @Failed@ - Conveys that the conversation with the user failed.
    --
    --     This can happen for various reasons including that the user did not
    --     provide an appropriate response to prompts from the service (you can
    --     configure how many times Amazon Lex can prompt a user for specific
    --     information), or the Lambda function failed to fulfill the intent.
    dialogState :: Core.Maybe DialogState,
    -- | A map of key-value pairs representing the session-specific context
    -- information.
    sessionAttributes :: Core.Maybe (Core.Sensitive (Core.HashMap Core.Text Core.Text)),
    -- | The message to convey to the user. The message can come from the bot\'s
    -- configuration or from a Lambda function.
    --
    -- If the intent is not configured with a Lambda function, or if the Lambda
    -- function returned @Delegate@ as the @dialogAction.type@ its response,
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
    message :: Core.Maybe (Core.Sensitive Core.Text),
    -- | A unique identifier for the session.
    sessionId :: Core.Maybe Core.Text,
    -- | The current user intent that Amazon Lex is aware of.
    intentName :: Core.Maybe Core.Text,
    -- | The version of the bot that responded to the conversation. You can use
    -- this information to help determine if one version of a bot is performing
    -- better than another version.
    botVersion :: Core.Maybe Core.Text,
    -- | The format of the response message. One of the following values:
    --
    -- -   @PlainText@ - The message contains plain UTF-8 text.
    --
    -- -   @CustomPayload@ - The message is a custom format defined by the
    --     Lambda function.
    --
    -- -   @SSML@ - The message contains text formatted for voice output.
    --
    -- -   @Composite@ - The message contains an escaped JSON object containing
    --     one or more messages from the groups that messages were assigned to
    --     when the intent was created.
    messageFormat :: Core.Maybe MessageFormatType,
    -- | The intent slots that Amazon Lex detected from the user input in the
    -- conversation.
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
    slots :: Core.Maybe (Core.Sensitive (Core.HashMap Core.Text Core.Text)),
    -- | Provides a score that indicates how confident Amazon Lex is that the
    -- returned intent is the one that matches the user\'s intent. The score is
    -- between 0.0 and 1.0. For more information, see
    -- <https://docs.aws.amazon.com/lex/latest/dg/confidence-scores.html Confidence Scores>.
    --
    -- The score is a relative score, not an absolute score. The score may
    -- change based on improvements to Amazon Lex.
    nluIntentConfidence :: Core.Maybe IntentConfidence,
    -- | The sentiment expressed in and utterance.
    --
    -- When the bot is configured to send utterances to Amazon Comprehend for
    -- sentiment analysis, this field contains the result of the analysis.
    sentimentResponse :: Core.Maybe SentimentResponse,
    -- | If the @dialogState@ value is @ElicitSlot@, returns the name of the slot
    -- for which Amazon Lex is eliciting a value.
    slotToElicit :: Core.Maybe Core.Text,
    -- | A list of active contexts for the session. A context can be set when an
    -- intent is fulfilled or by calling the @PostContent@, @PostText@, or
    -- @PutSession@ operation.
    --
    -- You can use a context to control the intents that can follow up an
    -- intent, or to modify the operation of your application.
    activeContexts :: Core.Maybe (Core.Sensitive [ActiveContext]),
    -- | One to four alternative intents that may be applicable to the user\'s
    -- intent.
    --
    -- Each alternative includes a score that indicates how confident Amazon
    -- Lex is that the intent matches the user\'s intent. The intents are
    -- sorted by the confidence score.
    alternativeIntents :: Core.Maybe [PredictedIntent],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'PostTextResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'responseCard', 'postTextResponse_responseCard' - Represents the options that the user has to respond to the current
-- prompt. Response Card can come from the bot configuration (in the Amazon
-- Lex console, choose the settings button next to a slot) or from a code
-- hook (Lambda function).
--
-- 'dialogState', 'postTextResponse_dialogState' - Identifies the current state of the user interaction. Amazon Lex returns
-- one of the following values as @dialogState@. The client can optionally
-- use this information to customize the user interface.
--
-- -   @ElicitIntent@ - Amazon Lex wants to elicit user intent.
--
--     For example, a user might utter an intent (\"I want to order a
--     pizza\"). If Amazon Lex cannot infer the user intent from this
--     utterance, it will return this dialogState.
--
-- -   @ConfirmIntent@ - Amazon Lex is expecting a \"yes\" or \"no\"
--     response.
--
--     For example, Amazon Lex wants user confirmation before fulfilling an
--     intent.
--
--     Instead of a simple \"yes\" or \"no,\" a user might respond with
--     additional information. For example, \"yes, but make it thick crust
--     pizza\" or \"no, I want to order a drink\". Amazon Lex can process
--     such additional information (in these examples, update the crust
--     type slot value, or change intent from OrderPizza to OrderDrink).
--
-- -   @ElicitSlot@ - Amazon Lex is expecting a slot value for the current
--     intent.
--
--     For example, suppose that in the response Amazon Lex sends this
--     message: \"What size pizza would you like?\". A user might reply
--     with the slot value (e.g., \"medium\"). The user might also provide
--     additional information in the response (e.g., \"medium thick crust
--     pizza\"). Amazon Lex can process such additional information
--     appropriately.
--
-- -   @Fulfilled@ - Conveys that the Lambda function configured for the
--     intent has successfully fulfilled the intent.
--
-- -   @ReadyForFulfillment@ - Conveys that the client has to fulfill the
--     intent.
--
-- -   @Failed@ - Conveys that the conversation with the user failed.
--
--     This can happen for various reasons including that the user did not
--     provide an appropriate response to prompts from the service (you can
--     configure how many times Amazon Lex can prompt a user for specific
--     information), or the Lambda function failed to fulfill the intent.
--
-- 'sessionAttributes', 'postTextResponse_sessionAttributes' - A map of key-value pairs representing the session-specific context
-- information.
--
-- 'message', 'postTextResponse_message' - The message to convey to the user. The message can come from the bot\'s
-- configuration or from a Lambda function.
--
-- If the intent is not configured with a Lambda function, or if the Lambda
-- function returned @Delegate@ as the @dialogAction.type@ its response,
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
-- 'sessionId', 'postTextResponse_sessionId' - A unique identifier for the session.
--
-- 'intentName', 'postTextResponse_intentName' - The current user intent that Amazon Lex is aware of.
--
-- 'botVersion', 'postTextResponse_botVersion' - The version of the bot that responded to the conversation. You can use
-- this information to help determine if one version of a bot is performing
-- better than another version.
--
-- 'messageFormat', 'postTextResponse_messageFormat' - The format of the response message. One of the following values:
--
-- -   @PlainText@ - The message contains plain UTF-8 text.
--
-- -   @CustomPayload@ - The message is a custom format defined by the
--     Lambda function.
--
-- -   @SSML@ - The message contains text formatted for voice output.
--
-- -   @Composite@ - The message contains an escaped JSON object containing
--     one or more messages from the groups that messages were assigned to
--     when the intent was created.
--
-- 'slots', 'postTextResponse_slots' - The intent slots that Amazon Lex detected from the user input in the
-- conversation.
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
-- 'nluIntentConfidence', 'postTextResponse_nluIntentConfidence' - Provides a score that indicates how confident Amazon Lex is that the
-- returned intent is the one that matches the user\'s intent. The score is
-- between 0.0 and 1.0. For more information, see
-- <https://docs.aws.amazon.com/lex/latest/dg/confidence-scores.html Confidence Scores>.
--
-- The score is a relative score, not an absolute score. The score may
-- change based on improvements to Amazon Lex.
--
-- 'sentimentResponse', 'postTextResponse_sentimentResponse' - The sentiment expressed in and utterance.
--
-- When the bot is configured to send utterances to Amazon Comprehend for
-- sentiment analysis, this field contains the result of the analysis.
--
-- 'slotToElicit', 'postTextResponse_slotToElicit' - If the @dialogState@ value is @ElicitSlot@, returns the name of the slot
-- for which Amazon Lex is eliciting a value.
--
-- 'activeContexts', 'postTextResponse_activeContexts' - A list of active contexts for the session. A context can be set when an
-- intent is fulfilled or by calling the @PostContent@, @PostText@, or
-- @PutSession@ operation.
--
-- You can use a context to control the intents that can follow up an
-- intent, or to modify the operation of your application.
--
-- 'alternativeIntents', 'postTextResponse_alternativeIntents' - One to four alternative intents that may be applicable to the user\'s
-- intent.
--
-- Each alternative includes a score that indicates how confident Amazon
-- Lex is that the intent matches the user\'s intent. The intents are
-- sorted by the confidence score.
--
-- 'httpStatus', 'postTextResponse_httpStatus' - The response's http status code.
newPostTextResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PostTextResponse
newPostTextResponse pHttpStatus_ =
  PostTextResponse'
    { responseCard = Core.Nothing,
      dialogState = Core.Nothing,
      sessionAttributes = Core.Nothing,
      message = Core.Nothing,
      sessionId = Core.Nothing,
      intentName = Core.Nothing,
      botVersion = Core.Nothing,
      messageFormat = Core.Nothing,
      slots = Core.Nothing,
      nluIntentConfidence = Core.Nothing,
      sentimentResponse = Core.Nothing,
      slotToElicit = Core.Nothing,
      activeContexts = Core.Nothing,
      alternativeIntents = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the options that the user has to respond to the current
-- prompt. Response Card can come from the bot configuration (in the Amazon
-- Lex console, choose the settings button next to a slot) or from a code
-- hook (Lambda function).
postTextResponse_responseCard :: Lens.Lens' PostTextResponse (Core.Maybe ResponseCard)
postTextResponse_responseCard = Lens.lens (\PostTextResponse' {responseCard} -> responseCard) (\s@PostTextResponse' {} a -> s {responseCard = a} :: PostTextResponse)

-- | Identifies the current state of the user interaction. Amazon Lex returns
-- one of the following values as @dialogState@. The client can optionally
-- use this information to customize the user interface.
--
-- -   @ElicitIntent@ - Amazon Lex wants to elicit user intent.
--
--     For example, a user might utter an intent (\"I want to order a
--     pizza\"). If Amazon Lex cannot infer the user intent from this
--     utterance, it will return this dialogState.
--
-- -   @ConfirmIntent@ - Amazon Lex is expecting a \"yes\" or \"no\"
--     response.
--
--     For example, Amazon Lex wants user confirmation before fulfilling an
--     intent.
--
--     Instead of a simple \"yes\" or \"no,\" a user might respond with
--     additional information. For example, \"yes, but make it thick crust
--     pizza\" or \"no, I want to order a drink\". Amazon Lex can process
--     such additional information (in these examples, update the crust
--     type slot value, or change intent from OrderPizza to OrderDrink).
--
-- -   @ElicitSlot@ - Amazon Lex is expecting a slot value for the current
--     intent.
--
--     For example, suppose that in the response Amazon Lex sends this
--     message: \"What size pizza would you like?\". A user might reply
--     with the slot value (e.g., \"medium\"). The user might also provide
--     additional information in the response (e.g., \"medium thick crust
--     pizza\"). Amazon Lex can process such additional information
--     appropriately.
--
-- -   @Fulfilled@ - Conveys that the Lambda function configured for the
--     intent has successfully fulfilled the intent.
--
-- -   @ReadyForFulfillment@ - Conveys that the client has to fulfill the
--     intent.
--
-- -   @Failed@ - Conveys that the conversation with the user failed.
--
--     This can happen for various reasons including that the user did not
--     provide an appropriate response to prompts from the service (you can
--     configure how many times Amazon Lex can prompt a user for specific
--     information), or the Lambda function failed to fulfill the intent.
postTextResponse_dialogState :: Lens.Lens' PostTextResponse (Core.Maybe DialogState)
postTextResponse_dialogState = Lens.lens (\PostTextResponse' {dialogState} -> dialogState) (\s@PostTextResponse' {} a -> s {dialogState = a} :: PostTextResponse)

-- | A map of key-value pairs representing the session-specific context
-- information.
postTextResponse_sessionAttributes :: Lens.Lens' PostTextResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
postTextResponse_sessionAttributes = Lens.lens (\PostTextResponse' {sessionAttributes} -> sessionAttributes) (\s@PostTextResponse' {} a -> s {sessionAttributes = a} :: PostTextResponse) Core.. Lens.mapping (Core._Sensitive Core.. Lens._Coerce)

-- | The message to convey to the user. The message can come from the bot\'s
-- configuration or from a Lambda function.
--
-- If the intent is not configured with a Lambda function, or if the Lambda
-- function returned @Delegate@ as the @dialogAction.type@ its response,
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
postTextResponse_message :: Lens.Lens' PostTextResponse (Core.Maybe Core.Text)
postTextResponse_message = Lens.lens (\PostTextResponse' {message} -> message) (\s@PostTextResponse' {} a -> s {message = a} :: PostTextResponse) Core.. Lens.mapping Core._Sensitive

-- | A unique identifier for the session.
postTextResponse_sessionId :: Lens.Lens' PostTextResponse (Core.Maybe Core.Text)
postTextResponse_sessionId = Lens.lens (\PostTextResponse' {sessionId} -> sessionId) (\s@PostTextResponse' {} a -> s {sessionId = a} :: PostTextResponse)

-- | The current user intent that Amazon Lex is aware of.
postTextResponse_intentName :: Lens.Lens' PostTextResponse (Core.Maybe Core.Text)
postTextResponse_intentName = Lens.lens (\PostTextResponse' {intentName} -> intentName) (\s@PostTextResponse' {} a -> s {intentName = a} :: PostTextResponse)

-- | The version of the bot that responded to the conversation. You can use
-- this information to help determine if one version of a bot is performing
-- better than another version.
postTextResponse_botVersion :: Lens.Lens' PostTextResponse (Core.Maybe Core.Text)
postTextResponse_botVersion = Lens.lens (\PostTextResponse' {botVersion} -> botVersion) (\s@PostTextResponse' {} a -> s {botVersion = a} :: PostTextResponse)

-- | The format of the response message. One of the following values:
--
-- -   @PlainText@ - The message contains plain UTF-8 text.
--
-- -   @CustomPayload@ - The message is a custom format defined by the
--     Lambda function.
--
-- -   @SSML@ - The message contains text formatted for voice output.
--
-- -   @Composite@ - The message contains an escaped JSON object containing
--     one or more messages from the groups that messages were assigned to
--     when the intent was created.
postTextResponse_messageFormat :: Lens.Lens' PostTextResponse (Core.Maybe MessageFormatType)
postTextResponse_messageFormat = Lens.lens (\PostTextResponse' {messageFormat} -> messageFormat) (\s@PostTextResponse' {} a -> s {messageFormat = a} :: PostTextResponse)

-- | The intent slots that Amazon Lex detected from the user input in the
-- conversation.
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
postTextResponse_slots :: Lens.Lens' PostTextResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
postTextResponse_slots = Lens.lens (\PostTextResponse' {slots} -> slots) (\s@PostTextResponse' {} a -> s {slots = a} :: PostTextResponse) Core.. Lens.mapping (Core._Sensitive Core.. Lens._Coerce)

-- | Provides a score that indicates how confident Amazon Lex is that the
-- returned intent is the one that matches the user\'s intent. The score is
-- between 0.0 and 1.0. For more information, see
-- <https://docs.aws.amazon.com/lex/latest/dg/confidence-scores.html Confidence Scores>.
--
-- The score is a relative score, not an absolute score. The score may
-- change based on improvements to Amazon Lex.
postTextResponse_nluIntentConfidence :: Lens.Lens' PostTextResponse (Core.Maybe IntentConfidence)
postTextResponse_nluIntentConfidence = Lens.lens (\PostTextResponse' {nluIntentConfidence} -> nluIntentConfidence) (\s@PostTextResponse' {} a -> s {nluIntentConfidence = a} :: PostTextResponse)

-- | The sentiment expressed in and utterance.
--
-- When the bot is configured to send utterances to Amazon Comprehend for
-- sentiment analysis, this field contains the result of the analysis.
postTextResponse_sentimentResponse :: Lens.Lens' PostTextResponse (Core.Maybe SentimentResponse)
postTextResponse_sentimentResponse = Lens.lens (\PostTextResponse' {sentimentResponse} -> sentimentResponse) (\s@PostTextResponse' {} a -> s {sentimentResponse = a} :: PostTextResponse)

-- | If the @dialogState@ value is @ElicitSlot@, returns the name of the slot
-- for which Amazon Lex is eliciting a value.
postTextResponse_slotToElicit :: Lens.Lens' PostTextResponse (Core.Maybe Core.Text)
postTextResponse_slotToElicit = Lens.lens (\PostTextResponse' {slotToElicit} -> slotToElicit) (\s@PostTextResponse' {} a -> s {slotToElicit = a} :: PostTextResponse)

-- | A list of active contexts for the session. A context can be set when an
-- intent is fulfilled or by calling the @PostContent@, @PostText@, or
-- @PutSession@ operation.
--
-- You can use a context to control the intents that can follow up an
-- intent, or to modify the operation of your application.
postTextResponse_activeContexts :: Lens.Lens' PostTextResponse (Core.Maybe [ActiveContext])
postTextResponse_activeContexts = Lens.lens (\PostTextResponse' {activeContexts} -> activeContexts) (\s@PostTextResponse' {} a -> s {activeContexts = a} :: PostTextResponse) Core.. Lens.mapping (Core._Sensitive Core.. Lens._Coerce)

-- | One to four alternative intents that may be applicable to the user\'s
-- intent.
--
-- Each alternative includes a score that indicates how confident Amazon
-- Lex is that the intent matches the user\'s intent. The intents are
-- sorted by the confidence score.
postTextResponse_alternativeIntents :: Lens.Lens' PostTextResponse (Core.Maybe [PredictedIntent])
postTextResponse_alternativeIntents = Lens.lens (\PostTextResponse' {alternativeIntents} -> alternativeIntents) (\s@PostTextResponse' {} a -> s {alternativeIntents = a} :: PostTextResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
postTextResponse_httpStatus :: Lens.Lens' PostTextResponse Core.Int
postTextResponse_httpStatus = Lens.lens (\PostTextResponse' {httpStatus} -> httpStatus) (\s@PostTextResponse' {} a -> s {httpStatus = a} :: PostTextResponse)

instance Core.NFData PostTextResponse
