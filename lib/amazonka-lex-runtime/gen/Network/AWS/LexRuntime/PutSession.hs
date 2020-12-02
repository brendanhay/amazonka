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
-- Module      : Network.AWS.LexRuntime.PutSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new session or modifies an existing session with an Amazon Lex bot. Use this operation to enable your application to set the state of the bot.
--
--
-- For more information, see <https://docs.aws.amazon.com/lex/latest/dg/how-session-api.html Managing Sessions> .
module Network.AWS.LexRuntime.PutSession
  ( -- * Creating a Request
    putSession,
    PutSession,

    -- * Request Lenses
    psAccept,
    psActiveContexts,
    psRecentIntentSummaryView,
    psDialogAction,
    psSessionAttributes,
    psBotName,
    psBotAlias,
    psUserId,

    -- * Destructuring the Response
    putSessionResponse,
    PutSessionResponse,

    -- * Response Lenses
    psrsSlots,
    psrsIntentName,
    psrsDialogState,
    psrsActiveContexts,
    psrsMessageFormat,
    psrsMessage,
    psrsSessionId,
    psrsSlotToElicit,
    psrsContentType,
    psrsSessionAttributes,
    psrsResponseStatus,
    psrsAudioStream,
  )
where

import Network.AWS.Lens
import Network.AWS.LexRuntime.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putSession' smart constructor.
data PutSession = PutSession'
  { _psAccept :: !(Maybe Text),
    _psActiveContexts :: !(Maybe (Sensitive [ActiveContext])),
    _psRecentIntentSummaryView :: !(Maybe [IntentSummary]),
    _psDialogAction :: !(Maybe DialogAction),
    _psSessionAttributes :: !(Maybe (Sensitive (Map Text (Text)))),
    _psBotName :: !Text,
    _psBotAlias :: !Text,
    _psUserId :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psAccept' - The message that Amazon Lex returns in the response can be either text or speech based depending on the value of this field.     * If the value is @text/plain; charset=utf-8@ , Amazon Lex returns text in the response.     * If the value begins with @audio/@ , Amazon Lex returns speech in the response. Amazon Lex uses Amazon Polly to generate the speech in the configuration that you specify. For example, if you specify @audio/mpeg@ as the value, Amazon Lex returns speech in the MPEG format.     * If the value is @audio/pcm@ , the speech is returned as @audio/pcm@ in 16-bit, little endian format.     * The following are the accepted values:     * @audio/mpeg@      * @audio/ogg@      * @audio/pcm@      * @audio/*@ (defaults to mpeg)     * @text/plain; charset=utf-8@
--
-- * 'psActiveContexts' - A list of contexts active for the request. A context can be activated when a previous intent is fulfilled, or by including the context in the request, If you don't specify a list of contexts, Amazon Lex will use the current list of contexts for the session. If you specify an empty list, all contexts for the session are cleared.
--
-- * 'psRecentIntentSummaryView' - A summary of the recent intents for the bot. You can use the intent summary view to set a checkpoint label on an intent and modify attributes of intents. You can also use it to remove or add intent summary objects to the list. An intent that you modify or add to the list must make sense for the bot. For example, the intent name must be valid for the bot. You must provide valid values for:     * @intentName@      * slot names     * @slotToElict@  If you send the @recentIntentSummaryView@ parameter in a @PutSession@ request, the contents of the new summary view replaces the old summary view. For example, if a @GetSession@ request returns three intents in the summary view and you call @PutSession@ with one intent in the summary view, the next call to @GetSession@ will only return one intent.
--
-- * 'psDialogAction' - Sets the next action that the bot should take to fulfill the conversation.
--
-- * 'psSessionAttributes' - Map of key/value pairs representing the session-specific context information. It contains application information passed between Amazon Lex and a client application.
--
-- * 'psBotName' - The name of the bot that contains the session data.
--
-- * 'psBotAlias' - The alias in use for the bot that contains the session data.
--
-- * 'psUserId' - The ID of the client application user. Amazon Lex uses this to identify a user's conversation with your bot.
putSession ::
  -- | 'psBotName'
  Text ->
  -- | 'psBotAlias'
  Text ->
  -- | 'psUserId'
  Text ->
  PutSession
putSession pBotName_ pBotAlias_ pUserId_ =
  PutSession'
    { _psAccept = Nothing,
      _psActiveContexts = Nothing,
      _psRecentIntentSummaryView = Nothing,
      _psDialogAction = Nothing,
      _psSessionAttributes = Nothing,
      _psBotName = pBotName_,
      _psBotAlias = pBotAlias_,
      _psUserId = pUserId_
    }

-- | The message that Amazon Lex returns in the response can be either text or speech based depending on the value of this field.     * If the value is @text/plain; charset=utf-8@ , Amazon Lex returns text in the response.     * If the value begins with @audio/@ , Amazon Lex returns speech in the response. Amazon Lex uses Amazon Polly to generate the speech in the configuration that you specify. For example, if you specify @audio/mpeg@ as the value, Amazon Lex returns speech in the MPEG format.     * If the value is @audio/pcm@ , the speech is returned as @audio/pcm@ in 16-bit, little endian format.     * The following are the accepted values:     * @audio/mpeg@      * @audio/ogg@      * @audio/pcm@      * @audio/*@ (defaults to mpeg)     * @text/plain; charset=utf-8@
psAccept :: Lens' PutSession (Maybe Text)
psAccept = lens _psAccept (\s a -> s {_psAccept = a})

-- | A list of contexts active for the request. A context can be activated when a previous intent is fulfilled, or by including the context in the request, If you don't specify a list of contexts, Amazon Lex will use the current list of contexts for the session. If you specify an empty list, all contexts for the session are cleared.
psActiveContexts :: Lens' PutSession (Maybe [ActiveContext])
psActiveContexts = lens _psActiveContexts (\s a -> s {_psActiveContexts = a}) . mapping (_Sensitive . _Coerce)

-- | A summary of the recent intents for the bot. You can use the intent summary view to set a checkpoint label on an intent and modify attributes of intents. You can also use it to remove or add intent summary objects to the list. An intent that you modify or add to the list must make sense for the bot. For example, the intent name must be valid for the bot. You must provide valid values for:     * @intentName@      * slot names     * @slotToElict@  If you send the @recentIntentSummaryView@ parameter in a @PutSession@ request, the contents of the new summary view replaces the old summary view. For example, if a @GetSession@ request returns three intents in the summary view and you call @PutSession@ with one intent in the summary view, the next call to @GetSession@ will only return one intent.
psRecentIntentSummaryView :: Lens' PutSession [IntentSummary]
psRecentIntentSummaryView = lens _psRecentIntentSummaryView (\s a -> s {_psRecentIntentSummaryView = a}) . _Default . _Coerce

-- | Sets the next action that the bot should take to fulfill the conversation.
psDialogAction :: Lens' PutSession (Maybe DialogAction)
psDialogAction = lens _psDialogAction (\s a -> s {_psDialogAction = a})

-- | Map of key/value pairs representing the session-specific context information. It contains application information passed between Amazon Lex and a client application.
psSessionAttributes :: Lens' PutSession (Maybe (HashMap Text (Text)))
psSessionAttributes = lens _psSessionAttributes (\s a -> s {_psSessionAttributes = a}) . mapping (_Sensitive . _Map)

-- | The name of the bot that contains the session data.
psBotName :: Lens' PutSession Text
psBotName = lens _psBotName (\s a -> s {_psBotName = a})

-- | The alias in use for the bot that contains the session data.
psBotAlias :: Lens' PutSession Text
psBotAlias = lens _psBotAlias (\s a -> s {_psBotAlias = a})

-- | The ID of the client application user. Amazon Lex uses this to identify a user's conversation with your bot.
psUserId :: Lens' PutSession Text
psUserId = lens _psUserId (\s a -> s {_psUserId = a})

instance AWSRequest PutSession where
  type Rs PutSession = PutSessionResponse
  request = postJSON lexRuntime
  response =
    receiveBody
      ( \s h x ->
          PutSessionResponse'
            <$> (h .#? "x-amz-lex-slots")
            <*> (h .#? "x-amz-lex-intent-name")
            <*> (h .#? "x-amz-lex-dialog-state")
            <*> (h .#? "x-amz-lex-active-contexts")
            <*> (h .#? "x-amz-lex-message-format")
            <*> (h .#? "x-amz-lex-message")
            <*> (h .#? "x-amz-lex-session-id")
            <*> (h .#? "x-amz-lex-slot-to-elicit")
            <*> (h .#? "Content-Type")
            <*> (h .#? "x-amz-lex-session-attributes")
            <*> (pure (fromEnum s))
            <*> (pure x)
      )

instance Hashable PutSession

instance NFData PutSession

instance ToHeaders PutSession where
  toHeaders PutSession' {..} =
    mconcat
      [ "Accept" =# _psAccept,
        "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
      ]

instance ToJSON PutSession where
  toJSON PutSession' {..} =
    object
      ( catMaybes
          [ ("activeContexts" .=) <$> _psActiveContexts,
            ("recentIntentSummaryView" .=) <$> _psRecentIntentSummaryView,
            ("dialogAction" .=) <$> _psDialogAction,
            ("sessionAttributes" .=) <$> _psSessionAttributes
          ]
      )

instance ToPath PutSession where
  toPath PutSession' {..} =
    mconcat
      [ "/bot/",
        toBS _psBotName,
        "/alias/",
        toBS _psBotAlias,
        "/user/",
        toBS _psUserId,
        "/session"
      ]

instance ToQuery PutSession where
  toQuery = const mempty

-- | /See:/ 'putSessionResponse' smart constructor.
data PutSessionResponse = PutSessionResponse'
  { _psrsSlots ::
      !(Maybe Text),
    _psrsIntentName :: !(Maybe Text),
    _psrsDialogState :: !(Maybe DialogState),
    _psrsActiveContexts :: !(Maybe (Sensitive Text)),
    _psrsMessageFormat :: !(Maybe MessageFormatType),
    _psrsMessage :: !(Maybe (Sensitive Text)),
    _psrsSessionId :: !(Maybe Text),
    _psrsSlotToElicit :: !(Maybe Text),
    _psrsContentType :: !(Maybe Text),
    _psrsSessionAttributes :: !(Maybe Text),
    _psrsResponseStatus :: !Int,
    _psrsAudioStream :: !RsBody
  }
  deriving (Show, Generic)

-- | Creates a value of 'PutSessionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psrsSlots' - Map of zero or more intent slots Amazon Lex detected from the user input during the conversation. Amazon Lex creates a resolution list containing likely values for a slot. The value that it returns is determined by the @valueSelectionStrategy@ selected when the slot type was created or updated. If @valueSelectionStrategy@ is set to @ORIGINAL_VALUE@ , the value provided by the user is returned, if the user value is similar to the slot values. If @valueSelectionStrategy@ is set to @TOP_RESOLUTION@ Amazon Lex returns the first value in the resolution list or, if there is no resolution list, null. If you don't specify a @valueSelectionStrategy@ the default is @ORIGINAL_VALUE@ .
--
-- * 'psrsIntentName' - The name of the current intent.
--
-- * 'psrsDialogState' -      * @ConfirmIntent@ - Amazon Lex is expecting a "yes" or "no" response to confirm the intent before fulfilling an intent.     * @ElicitIntent@ - Amazon Lex wants to elicit the user's intent.     * @ElicitSlot@ - Amazon Lex is expecting the value of a slot for the current intent.     * @Failed@ - Conveys that the conversation with the user has failed. This can happen for various reasons, including the user does not provide an appropriate response to prompts from the service, or if the Lambda function fails to fulfill the intent.     * @Fulfilled@ - Conveys that the Lambda function has sucessfully fulfilled the intent.     * @ReadyForFulfillment@ - Conveys that the client has to fulfill the intent.
--
-- * 'psrsActiveContexts' - A list of active contexts for the session.
--
-- * 'psrsMessageFormat' - The format of the response message. One of the following values:     * @PlainText@ - The message contains plain UTF-8 text.     * @CustomPayload@ - The message is a custom format for the client.     * @SSML@ - The message contains text formatted for voice output.     * @Composite@ - The message contains an escaped JSON object containing one or more messages from the groups that messages were assigned to when the intent was created.
--
-- * 'psrsMessage' - The next message that should be presented to the user.
--
-- * 'psrsSessionId' - A unique identifier for the session.
--
-- * 'psrsSlotToElicit' - If the @dialogState@ is @ElicitSlot@ , returns the name of the slot for which Amazon Lex is eliciting a value.
--
-- * 'psrsContentType' - Content type as specified in the @Accept@ HTTP header in the request.
--
-- * 'psrsSessionAttributes' - Map of key/value pairs representing session-specific context information.
--
-- * 'psrsResponseStatus' - -- | The response status code.
--
-- * 'psrsAudioStream' - The audio version of the message to convey to the user.
putSessionResponse ::
  -- | 'psrsResponseStatus'
  Int ->
  -- | 'psrsAudioStream'
  RsBody ->
  PutSessionResponse
putSessionResponse pResponseStatus_ pAudioStream_ =
  PutSessionResponse'
    { _psrsSlots = Nothing,
      _psrsIntentName = Nothing,
      _psrsDialogState = Nothing,
      _psrsActiveContexts = Nothing,
      _psrsMessageFormat = Nothing,
      _psrsMessage = Nothing,
      _psrsSessionId = Nothing,
      _psrsSlotToElicit = Nothing,
      _psrsContentType = Nothing,
      _psrsSessionAttributes = Nothing,
      _psrsResponseStatus = pResponseStatus_,
      _psrsAudioStream = pAudioStream_
    }

-- | Map of zero or more intent slots Amazon Lex detected from the user input during the conversation. Amazon Lex creates a resolution list containing likely values for a slot. The value that it returns is determined by the @valueSelectionStrategy@ selected when the slot type was created or updated. If @valueSelectionStrategy@ is set to @ORIGINAL_VALUE@ , the value provided by the user is returned, if the user value is similar to the slot values. If @valueSelectionStrategy@ is set to @TOP_RESOLUTION@ Amazon Lex returns the first value in the resolution list or, if there is no resolution list, null. If you don't specify a @valueSelectionStrategy@ the default is @ORIGINAL_VALUE@ .
psrsSlots :: Lens' PutSessionResponse (Maybe Text)
psrsSlots = lens _psrsSlots (\s a -> s {_psrsSlots = a})

-- | The name of the current intent.
psrsIntentName :: Lens' PutSessionResponse (Maybe Text)
psrsIntentName = lens _psrsIntentName (\s a -> s {_psrsIntentName = a})

-- |      * @ConfirmIntent@ - Amazon Lex is expecting a "yes" or "no" response to confirm the intent before fulfilling an intent.     * @ElicitIntent@ - Amazon Lex wants to elicit the user's intent.     * @ElicitSlot@ - Amazon Lex is expecting the value of a slot for the current intent.     * @Failed@ - Conveys that the conversation with the user has failed. This can happen for various reasons, including the user does not provide an appropriate response to prompts from the service, or if the Lambda function fails to fulfill the intent.     * @Fulfilled@ - Conveys that the Lambda function has sucessfully fulfilled the intent.     * @ReadyForFulfillment@ - Conveys that the client has to fulfill the intent.
psrsDialogState :: Lens' PutSessionResponse (Maybe DialogState)
psrsDialogState = lens _psrsDialogState (\s a -> s {_psrsDialogState = a})

-- | A list of active contexts for the session.
psrsActiveContexts :: Lens' PutSessionResponse (Maybe Text)
psrsActiveContexts = lens _psrsActiveContexts (\s a -> s {_psrsActiveContexts = a}) . mapping _Sensitive

-- | The format of the response message. One of the following values:     * @PlainText@ - The message contains plain UTF-8 text.     * @CustomPayload@ - The message is a custom format for the client.     * @SSML@ - The message contains text formatted for voice output.     * @Composite@ - The message contains an escaped JSON object containing one or more messages from the groups that messages were assigned to when the intent was created.
psrsMessageFormat :: Lens' PutSessionResponse (Maybe MessageFormatType)
psrsMessageFormat = lens _psrsMessageFormat (\s a -> s {_psrsMessageFormat = a})

-- | The next message that should be presented to the user.
psrsMessage :: Lens' PutSessionResponse (Maybe Text)
psrsMessage = lens _psrsMessage (\s a -> s {_psrsMessage = a}) . mapping _Sensitive

-- | A unique identifier for the session.
psrsSessionId :: Lens' PutSessionResponse (Maybe Text)
psrsSessionId = lens _psrsSessionId (\s a -> s {_psrsSessionId = a})

-- | If the @dialogState@ is @ElicitSlot@ , returns the name of the slot for which Amazon Lex is eliciting a value.
psrsSlotToElicit :: Lens' PutSessionResponse (Maybe Text)
psrsSlotToElicit = lens _psrsSlotToElicit (\s a -> s {_psrsSlotToElicit = a})

-- | Content type as specified in the @Accept@ HTTP header in the request.
psrsContentType :: Lens' PutSessionResponse (Maybe Text)
psrsContentType = lens _psrsContentType (\s a -> s {_psrsContentType = a})

-- | Map of key/value pairs representing session-specific context information.
psrsSessionAttributes :: Lens' PutSessionResponse (Maybe Text)
psrsSessionAttributes = lens _psrsSessionAttributes (\s a -> s {_psrsSessionAttributes = a})

-- | -- | The response status code.
psrsResponseStatus :: Lens' PutSessionResponse Int
psrsResponseStatus = lens _psrsResponseStatus (\s a -> s {_psrsResponseStatus = a})

-- | The audio version of the message to convey to the user.
psrsAudioStream :: Lens' PutSessionResponse RsBody
psrsAudioStream = lens _psrsAudioStream (\s a -> s {_psrsAudioStream = a})
