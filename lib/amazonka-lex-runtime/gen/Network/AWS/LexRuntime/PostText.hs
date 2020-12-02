{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.PostText
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends user input to Amazon Lex. Client applications can use this API to send requests to Amazon Lex at runtime. Amazon Lex then interprets the user input using the machine learning model it built for the bot.
--
--
-- In response, Amazon Lex returns the next @message@ to convey to the user an optional @responseCard@ to display. Consider the following example messages:
--
--     * For a user input "I would like a pizza", Amazon Lex might return a response with a message eliciting slot data (for example, PizzaSize): "What size pizza would you like?"
--
--     * After the user provides all of the pizza order information, Amazon Lex might return a response with a message to obtain user confirmation "Proceed with the pizza order?".
--
--     * After the user replies to a confirmation prompt with a "yes", Amazon Lex might return a conclusion statement: "Thank you, your cheese pizza has been ordered.".
--
--
--
-- Not all Amazon Lex messages require a user response. For example, a conclusion statement does not require a response. Some messages require only a "yes" or "no" user response. In addition to the @message@ , Amazon Lex provides additional context about the message in the response that you might use to enhance client behavior, for example, to display the appropriate client user interface. These are the @slotToElicit@ , @dialogState@ , @intentName@ , and @slots@ fields in the response. Consider the following examples:
--
--     * If the message is to elicit slot data, Amazon Lex returns the following context information:
--
--     * @dialogState@ set to ElicitSlot
--
--     * @intentName@ set to the intent name in the current context
--
--     * @slotToElicit@ set to the slot name for which the @message@ is eliciting information
--
--     * @slots@ set to a map of slots, configured for the intent, with currently known values
--
--
--
--     * If the message is a confirmation prompt, the @dialogState@ is set to ConfirmIntent and @SlotToElicit@ is set to null.
--
--     * If the message is a clarification prompt (configured for the intent) that indicates that user intent is not understood, the @dialogState@ is set to ElicitIntent and @slotToElicit@ is set to null.
--
--
--
-- In addition, Amazon Lex also returns your application-specific @sessionAttributes@ . For more information, see <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html Managing Conversation Context> .
module Network.AWS.LexRuntime.PostText
  ( -- * Creating a Request
    postText,
    PostText,

    -- * Request Lenses
    ptActiveContexts,
    ptRequestAttributes,
    ptSessionAttributes,
    ptBotName,
    ptBotAlias,
    ptUserId,
    ptInputText,

    -- * Destructuring the Response
    postTextResponse,
    PostTextResponse,

    -- * Response Lenses
    ptrsSentimentResponse,
    ptrsNluIntentConfidence,
    ptrsSlots,
    ptrsResponseCard,
    ptrsIntentName,
    ptrsBotVersion,
    ptrsDialogState,
    ptrsActiveContexts,
    ptrsAlternativeIntents,
    ptrsMessageFormat,
    ptrsMessage,
    ptrsSessionId,
    ptrsSlotToElicit,
    ptrsSessionAttributes,
    ptrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.LexRuntime.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'postText' smart constructor.
data PostText = PostText'
  { _ptActiveContexts ::
      !(Maybe (Sensitive [ActiveContext])),
    _ptRequestAttributes :: !(Maybe (Sensitive (Map Text (Text)))),
    _ptSessionAttributes :: !(Maybe (Sensitive (Map Text (Text)))),
    _ptBotName :: !Text,
    _ptBotAlias :: !Text,
    _ptUserId :: !Text,
    _ptInputText :: !(Sensitive Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'PostText' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptActiveContexts' - A list of contexts active for the request. A context can be activated when a previous intent is fulfilled, or by including the context in the request, If you don't specify a list of contexts, Amazon Lex will use the current list of contexts for the session. If you specify an empty list, all contexts for the session are cleared.
--
-- * 'ptRequestAttributes' - Request-specific information passed between Amazon Lex and a client application. The namespace @x-amz-lex:@ is reserved for special attributes. Don't create any request attributes with the prefix @x-amz-lex:@ . For more information, see <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-request-attribs Setting Request Attributes> .
--
-- * 'ptSessionAttributes' - Application-specific information passed between Amazon Lex and a client application. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-session-attribs Setting Session Attributes> .
--
-- * 'ptBotName' - The name of the Amazon Lex bot.
--
-- * 'ptBotAlias' - The alias of the Amazon Lex bot.
--
-- * 'ptUserId' - The ID of the client application user. Amazon Lex uses this to identify a user's conversation with your bot. At runtime, each request must contain the @userID@ field. To decide the user ID to use for your application, consider the following factors.     * The @userID@ field must not contain any personally identifiable information of the user, for example, name, personal identification numbers, or other end user personal information.     * If you want a user to start a conversation on one device and continue on another device, use a user-specific identifier.     * If you want the same user to be able to have two independent conversations on two different devices, choose a device-specific identifier.     * A user can't have two independent conversations with two different versions of the same bot. For example, a user can't have a conversation with the PROD and BETA versions of the same bot. If you anticipate that a user will need to have conversation with two different versions, for example, while testing, include the bot alias in the user ID to separate the two conversations.
--
-- * 'ptInputText' - The text that the user entered (Amazon Lex interprets this text).
postText ::
  -- | 'ptBotName'
  Text ->
  -- | 'ptBotAlias'
  Text ->
  -- | 'ptUserId'
  Text ->
  -- | 'ptInputText'
  Text ->
  PostText
postText pBotName_ pBotAlias_ pUserId_ pInputText_ =
  PostText'
    { _ptActiveContexts = Nothing,
      _ptRequestAttributes = Nothing,
      _ptSessionAttributes = Nothing,
      _ptBotName = pBotName_,
      _ptBotAlias = pBotAlias_,
      _ptUserId = pUserId_,
      _ptInputText = _Sensitive # pInputText_
    }

-- | A list of contexts active for the request. A context can be activated when a previous intent is fulfilled, or by including the context in the request, If you don't specify a list of contexts, Amazon Lex will use the current list of contexts for the session. If you specify an empty list, all contexts for the session are cleared.
ptActiveContexts :: Lens' PostText (Maybe [ActiveContext])
ptActiveContexts = lens _ptActiveContexts (\s a -> s {_ptActiveContexts = a}) . mapping (_Sensitive . _Coerce)

-- | Request-specific information passed between Amazon Lex and a client application. The namespace @x-amz-lex:@ is reserved for special attributes. Don't create any request attributes with the prefix @x-amz-lex:@ . For more information, see <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-request-attribs Setting Request Attributes> .
ptRequestAttributes :: Lens' PostText (Maybe (HashMap Text (Text)))
ptRequestAttributes = lens _ptRequestAttributes (\s a -> s {_ptRequestAttributes = a}) . mapping (_Sensitive . _Map)

-- | Application-specific information passed between Amazon Lex and a client application. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html#context-mgmt-session-attribs Setting Session Attributes> .
ptSessionAttributes :: Lens' PostText (Maybe (HashMap Text (Text)))
ptSessionAttributes = lens _ptSessionAttributes (\s a -> s {_ptSessionAttributes = a}) . mapping (_Sensitive . _Map)

-- | The name of the Amazon Lex bot.
ptBotName :: Lens' PostText Text
ptBotName = lens _ptBotName (\s a -> s {_ptBotName = a})

-- | The alias of the Amazon Lex bot.
ptBotAlias :: Lens' PostText Text
ptBotAlias = lens _ptBotAlias (\s a -> s {_ptBotAlias = a})

-- | The ID of the client application user. Amazon Lex uses this to identify a user's conversation with your bot. At runtime, each request must contain the @userID@ field. To decide the user ID to use for your application, consider the following factors.     * The @userID@ field must not contain any personally identifiable information of the user, for example, name, personal identification numbers, or other end user personal information.     * If you want a user to start a conversation on one device and continue on another device, use a user-specific identifier.     * If you want the same user to be able to have two independent conversations on two different devices, choose a device-specific identifier.     * A user can't have two independent conversations with two different versions of the same bot. For example, a user can't have a conversation with the PROD and BETA versions of the same bot. If you anticipate that a user will need to have conversation with two different versions, for example, while testing, include the bot alias in the user ID to separate the two conversations.
ptUserId :: Lens' PostText Text
ptUserId = lens _ptUserId (\s a -> s {_ptUserId = a})

-- | The text that the user entered (Amazon Lex interprets this text).
ptInputText :: Lens' PostText Text
ptInputText = lens _ptInputText (\s a -> s {_ptInputText = a}) . _Sensitive

instance AWSRequest PostText where
  type Rs PostText = PostTextResponse
  request = postJSON lexRuntime
  response =
    receiveJSON
      ( \s h x ->
          PostTextResponse'
            <$> (x .?> "sentimentResponse")
            <*> (x .?> "nluIntentConfidence")
            <*> (x .?> "slots" .!@ mempty)
            <*> (x .?> "responseCard")
            <*> (x .?> "intentName")
            <*> (x .?> "botVersion")
            <*> (x .?> "dialogState")
            <*> (x .?> "activeContexts" .!@ mempty)
            <*> (x .?> "alternativeIntents" .!@ mempty)
            <*> (x .?> "messageFormat")
            <*> (x .?> "message")
            <*> (x .?> "sessionId")
            <*> (x .?> "slotToElicit")
            <*> (x .?> "sessionAttributes" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable PostText

instance NFData PostText

instance ToHeaders PostText where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON PostText where
  toJSON PostText' {..} =
    object
      ( catMaybes
          [ ("activeContexts" .=) <$> _ptActiveContexts,
            ("requestAttributes" .=) <$> _ptRequestAttributes,
            ("sessionAttributes" .=) <$> _ptSessionAttributes,
            Just ("inputText" .= _ptInputText)
          ]
      )

instance ToPath PostText where
  toPath PostText' {..} =
    mconcat
      [ "/bot/",
        toBS _ptBotName,
        "/alias/",
        toBS _ptBotAlias,
        "/user/",
        toBS _ptUserId,
        "/text"
      ]

instance ToQuery PostText where
  toQuery = const mempty

-- | /See:/ 'postTextResponse' smart constructor.
data PostTextResponse = PostTextResponse'
  { _ptrsSentimentResponse ::
      !(Maybe SentimentResponse),
    _ptrsNluIntentConfidence :: !(Maybe IntentConfidence),
    _ptrsSlots :: !(Maybe (Sensitive (Map Text (Text)))),
    _ptrsResponseCard :: !(Maybe ResponseCard),
    _ptrsIntentName :: !(Maybe Text),
    _ptrsBotVersion :: !(Maybe Text),
    _ptrsDialogState :: !(Maybe DialogState),
    _ptrsActiveContexts ::
      !(Maybe (Sensitive [ActiveContext])),
    _ptrsAlternativeIntents :: !(Maybe [PredictedIntent]),
    _ptrsMessageFormat :: !(Maybe MessageFormatType),
    _ptrsMessage :: !(Maybe (Sensitive Text)),
    _ptrsSessionId :: !(Maybe Text),
    _ptrsSlotToElicit :: !(Maybe Text),
    _ptrsSessionAttributes ::
      !(Maybe (Sensitive (Map Text (Text)))),
    _ptrsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'PostTextResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptrsSentimentResponse' - The sentiment expressed in and utterance. When the bot is configured to send utterances to Amazon Comprehend for sentiment analysis, this field contains the result of the analysis.
--
-- * 'ptrsNluIntentConfidence' - Provides a score that indicates how confident Amazon Lex is that the returned intent is the one that matches the user's intent. The score is between 0.0 and 1.0. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/confidence-scores.html Confidence Scores> . The score is a relative score, not an absolute score. The score may change based on improvements to Amazon Lex.
--
-- * 'ptrsSlots' - The intent slots that Amazon Lex detected from the user input in the conversation.  Amazon Lex creates a resolution list containing likely values for a slot. The value that it returns is determined by the @valueSelectionStrategy@ selected when the slot type was created or updated. If @valueSelectionStrategy@ is set to @ORIGINAL_VALUE@ , the value provided by the user is returned, if the user value is similar to the slot values. If @valueSelectionStrategy@ is set to @TOP_RESOLUTION@ Amazon Lex returns the first value in the resolution list or, if there is no resolution list, null. If you don't specify a @valueSelectionStrategy@ , the default is @ORIGINAL_VALUE@ .
--
-- * 'ptrsResponseCard' - Represents the options that the user has to respond to the current prompt. Response Card can come from the bot configuration (in the Amazon Lex console, choose the settings button next to a slot) or from a code hook (Lambda function).
--
-- * 'ptrsIntentName' - The current user intent that Amazon Lex is aware of.
--
-- * 'ptrsBotVersion' - The version of the bot that responded to the conversation. You can use this information to help determine if one version of a bot is performing better than another version.
--
-- * 'ptrsDialogState' - Identifies the current state of the user interaction. Amazon Lex returns one of the following values as @dialogState@ . The client can optionally use this information to customize the user interface.      * @ElicitIntent@ - Amazon Lex wants to elicit user intent.  For example, a user might utter an intent ("I want to order a pizza"). If Amazon Lex cannot infer the user intent from this utterance, it will return this dialogState.     * @ConfirmIntent@ - Amazon Lex is expecting a "yes" or "no" response.  For example, Amazon Lex wants user confirmation before fulfilling an intent.  Instead of a simple "yes" or "no," a user might respond with additional information. For example, "yes, but make it thick crust pizza" or "no, I want to order a drink". Amazon Lex can process such additional information (in these examples, update the crust type slot value, or change intent from OrderPizza to OrderDrink).     * @ElicitSlot@ - Amazon Lex is expecting a slot value for the current intent.  For example, suppose that in the response Amazon Lex sends this message: "What size pizza would you like?". A user might reply with the slot value (e.g., "medium"). The user might also provide additional information in the response (e.g., "medium thick crust pizza"). Amazon Lex can process such additional information appropriately.      * @Fulfilled@ - Conveys that the Lambda function configured for the intent has successfully fulfilled the intent.      * @ReadyForFulfillment@ - Conveys that the client has to fulfill the intent.      * @Failed@ - Conveys that the conversation with the user failed.  This can happen for various reasons including that the user did not provide an appropriate response to prompts from the service (you can configure how many times Amazon Lex can prompt a user for specific information), or the Lambda function failed to fulfill the intent.
--
-- * 'ptrsActiveContexts' - A list of active contexts for the session. A context can be set when an intent is fulfilled or by calling the @PostContent@ , @PostText@ , or @PutSession@ operation. You can use a context to control the intents that can follow up an intent, or to modify the operation of your application.
--
-- * 'ptrsAlternativeIntents' - One to four alternative intents that may be applicable to the user's intent. Each alternative includes a score that indicates how confident Amazon Lex is that the intent matches the user's intent. The intents are sorted by the confidence score.
--
-- * 'ptrsMessageFormat' - The format of the response message. One of the following values:     * @PlainText@ - The message contains plain UTF-8 text.     * @CustomPayload@ - The message is a custom format defined by the Lambda function.     * @SSML@ - The message contains text formatted for voice output.     * @Composite@ - The message contains an escaped JSON object containing one or more messages from the groups that messages were assigned to when the intent was created.
--
-- * 'ptrsMessage' - The message to convey to the user. The message can come from the bot's configuration or from a Lambda function. If the intent is not configured with a Lambda function, or if the Lambda function returned @Delegate@ as the @dialogAction.type@ its response, Amazon Lex decides on the next course of action and selects an appropriate message from the bot's configuration based on the current interaction context. For example, if Amazon Lex isn't able to understand user input, it uses a clarification prompt message. When you create an intent you can assign messages to groups. When messages are assigned to groups Amazon Lex returns one message from each group in the response. The message field is an escaped JSON string containing the messages. For more information about the structure of the JSON string returned, see 'msg-prompts-formats' . If the Lambda function returns a message, Amazon Lex passes it to the client in its response.
--
-- * 'ptrsSessionId' - A unique identifier for the session.
--
-- * 'ptrsSlotToElicit' - If the @dialogState@ value is @ElicitSlot@ , returns the name of the slot for which Amazon Lex is eliciting a value.
--
-- * 'ptrsSessionAttributes' - A map of key-value pairs representing the session-specific context information.
--
-- * 'ptrsResponseStatus' - -- | The response status code.
postTextResponse ::
  -- | 'ptrsResponseStatus'
  Int ->
  PostTextResponse
postTextResponse pResponseStatus_ =
  PostTextResponse'
    { _ptrsSentimentResponse = Nothing,
      _ptrsNluIntentConfidence = Nothing,
      _ptrsSlots = Nothing,
      _ptrsResponseCard = Nothing,
      _ptrsIntentName = Nothing,
      _ptrsBotVersion = Nothing,
      _ptrsDialogState = Nothing,
      _ptrsActiveContexts = Nothing,
      _ptrsAlternativeIntents = Nothing,
      _ptrsMessageFormat = Nothing,
      _ptrsMessage = Nothing,
      _ptrsSessionId = Nothing,
      _ptrsSlotToElicit = Nothing,
      _ptrsSessionAttributes = Nothing,
      _ptrsResponseStatus = pResponseStatus_
    }

-- | The sentiment expressed in and utterance. When the bot is configured to send utterances to Amazon Comprehend for sentiment analysis, this field contains the result of the analysis.
ptrsSentimentResponse :: Lens' PostTextResponse (Maybe SentimentResponse)
ptrsSentimentResponse = lens _ptrsSentimentResponse (\s a -> s {_ptrsSentimentResponse = a})

-- | Provides a score that indicates how confident Amazon Lex is that the returned intent is the one that matches the user's intent. The score is between 0.0 and 1.0. For more information, see <https://docs.aws.amazon.com/lex/latest/dg/confidence-scores.html Confidence Scores> . The score is a relative score, not an absolute score. The score may change based on improvements to Amazon Lex.
ptrsNluIntentConfidence :: Lens' PostTextResponse (Maybe IntentConfidence)
ptrsNluIntentConfidence = lens _ptrsNluIntentConfidence (\s a -> s {_ptrsNluIntentConfidence = a})

-- | The intent slots that Amazon Lex detected from the user input in the conversation.  Amazon Lex creates a resolution list containing likely values for a slot. The value that it returns is determined by the @valueSelectionStrategy@ selected when the slot type was created or updated. If @valueSelectionStrategy@ is set to @ORIGINAL_VALUE@ , the value provided by the user is returned, if the user value is similar to the slot values. If @valueSelectionStrategy@ is set to @TOP_RESOLUTION@ Amazon Lex returns the first value in the resolution list or, if there is no resolution list, null. If you don't specify a @valueSelectionStrategy@ , the default is @ORIGINAL_VALUE@ .
ptrsSlots :: Lens' PostTextResponse (Maybe (HashMap Text (Text)))
ptrsSlots = lens _ptrsSlots (\s a -> s {_ptrsSlots = a}) . mapping (_Sensitive . _Map)

-- | Represents the options that the user has to respond to the current prompt. Response Card can come from the bot configuration (in the Amazon Lex console, choose the settings button next to a slot) or from a code hook (Lambda function).
ptrsResponseCard :: Lens' PostTextResponse (Maybe ResponseCard)
ptrsResponseCard = lens _ptrsResponseCard (\s a -> s {_ptrsResponseCard = a})

-- | The current user intent that Amazon Lex is aware of.
ptrsIntentName :: Lens' PostTextResponse (Maybe Text)
ptrsIntentName = lens _ptrsIntentName (\s a -> s {_ptrsIntentName = a})

-- | The version of the bot that responded to the conversation. You can use this information to help determine if one version of a bot is performing better than another version.
ptrsBotVersion :: Lens' PostTextResponse (Maybe Text)
ptrsBotVersion = lens _ptrsBotVersion (\s a -> s {_ptrsBotVersion = a})

-- | Identifies the current state of the user interaction. Amazon Lex returns one of the following values as @dialogState@ . The client can optionally use this information to customize the user interface.      * @ElicitIntent@ - Amazon Lex wants to elicit user intent.  For example, a user might utter an intent ("I want to order a pizza"). If Amazon Lex cannot infer the user intent from this utterance, it will return this dialogState.     * @ConfirmIntent@ - Amazon Lex is expecting a "yes" or "no" response.  For example, Amazon Lex wants user confirmation before fulfilling an intent.  Instead of a simple "yes" or "no," a user might respond with additional information. For example, "yes, but make it thick crust pizza" or "no, I want to order a drink". Amazon Lex can process such additional information (in these examples, update the crust type slot value, or change intent from OrderPizza to OrderDrink).     * @ElicitSlot@ - Amazon Lex is expecting a slot value for the current intent.  For example, suppose that in the response Amazon Lex sends this message: "What size pizza would you like?". A user might reply with the slot value (e.g., "medium"). The user might also provide additional information in the response (e.g., "medium thick crust pizza"). Amazon Lex can process such additional information appropriately.      * @Fulfilled@ - Conveys that the Lambda function configured for the intent has successfully fulfilled the intent.      * @ReadyForFulfillment@ - Conveys that the client has to fulfill the intent.      * @Failed@ - Conveys that the conversation with the user failed.  This can happen for various reasons including that the user did not provide an appropriate response to prompts from the service (you can configure how many times Amazon Lex can prompt a user for specific information), or the Lambda function failed to fulfill the intent.
ptrsDialogState :: Lens' PostTextResponse (Maybe DialogState)
ptrsDialogState = lens _ptrsDialogState (\s a -> s {_ptrsDialogState = a})

-- | A list of active contexts for the session. A context can be set when an intent is fulfilled or by calling the @PostContent@ , @PostText@ , or @PutSession@ operation. You can use a context to control the intents that can follow up an intent, or to modify the operation of your application.
ptrsActiveContexts :: Lens' PostTextResponse (Maybe [ActiveContext])
ptrsActiveContexts = lens _ptrsActiveContexts (\s a -> s {_ptrsActiveContexts = a}) . mapping (_Sensitive . _Coerce)

-- | One to four alternative intents that may be applicable to the user's intent. Each alternative includes a score that indicates how confident Amazon Lex is that the intent matches the user's intent. The intents are sorted by the confidence score.
ptrsAlternativeIntents :: Lens' PostTextResponse [PredictedIntent]
ptrsAlternativeIntents = lens _ptrsAlternativeIntents (\s a -> s {_ptrsAlternativeIntents = a}) . _Default . _Coerce

-- | The format of the response message. One of the following values:     * @PlainText@ - The message contains plain UTF-8 text.     * @CustomPayload@ - The message is a custom format defined by the Lambda function.     * @SSML@ - The message contains text formatted for voice output.     * @Composite@ - The message contains an escaped JSON object containing one or more messages from the groups that messages were assigned to when the intent was created.
ptrsMessageFormat :: Lens' PostTextResponse (Maybe MessageFormatType)
ptrsMessageFormat = lens _ptrsMessageFormat (\s a -> s {_ptrsMessageFormat = a})

-- | The message to convey to the user. The message can come from the bot's configuration or from a Lambda function. If the intent is not configured with a Lambda function, or if the Lambda function returned @Delegate@ as the @dialogAction.type@ its response, Amazon Lex decides on the next course of action and selects an appropriate message from the bot's configuration based on the current interaction context. For example, if Amazon Lex isn't able to understand user input, it uses a clarification prompt message. When you create an intent you can assign messages to groups. When messages are assigned to groups Amazon Lex returns one message from each group in the response. The message field is an escaped JSON string containing the messages. For more information about the structure of the JSON string returned, see 'msg-prompts-formats' . If the Lambda function returns a message, Amazon Lex passes it to the client in its response.
ptrsMessage :: Lens' PostTextResponse (Maybe Text)
ptrsMessage = lens _ptrsMessage (\s a -> s {_ptrsMessage = a}) . mapping _Sensitive

-- | A unique identifier for the session.
ptrsSessionId :: Lens' PostTextResponse (Maybe Text)
ptrsSessionId = lens _ptrsSessionId (\s a -> s {_ptrsSessionId = a})

-- | If the @dialogState@ value is @ElicitSlot@ , returns the name of the slot for which Amazon Lex is eliciting a value.
ptrsSlotToElicit :: Lens' PostTextResponse (Maybe Text)
ptrsSlotToElicit = lens _ptrsSlotToElicit (\s a -> s {_ptrsSlotToElicit = a})

-- | A map of key-value pairs representing the session-specific context information.
ptrsSessionAttributes :: Lens' PostTextResponse (Maybe (HashMap Text (Text)))
ptrsSessionAttributes = lens _ptrsSessionAttributes (\s a -> s {_ptrsSessionAttributes = a}) . mapping (_Sensitive . _Map)

-- | -- | The response status code.
ptrsResponseStatus :: Lens' PostTextResponse Int
ptrsResponseStatus = lens _ptrsResponseStatus (\s a -> s {_ptrsResponseStatus = a})

instance NFData PostTextResponse
