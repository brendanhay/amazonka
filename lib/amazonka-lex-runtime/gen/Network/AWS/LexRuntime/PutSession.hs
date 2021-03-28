{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
-- For more information, see <https://docs.aws.amazon.com/lex/latest/dg/how-session-api.html Managing Sessions> .
module Network.AWS.LexRuntime.PutSession
    (
    -- * Creating a request
      PutSession (..)
    , mkPutSession
    -- ** Request lenses
    , psBotName
    , psBotAlias
    , psUserId
    , psAccept
    , psActiveContexts
    , psDialogAction
    , psRecentIntentSummaryView
    , psSessionAttributes

    -- * Destructuring the response
    , PutSessionResponse (..)
    , mkPutSessionResponse
    -- ** Response lenses
    , psrrsActiveContexts
    , psrrsAudioStream
    , psrrsContentType
    , psrrsDialogState
    , psrrsIntentName
    , psrrsMessage
    , psrrsMessageFormat
    , psrrsSessionAttributes
    , psrrsSessionId
    , psrrsSlotToElicit
    , psrrsSlots
    , psrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexRuntime.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutSession' smart constructor.
data PutSession = PutSession'
  { botName :: Types.BotName
    -- ^ The name of the bot that contains the session data.
  , botAlias :: Types.BotAlias
    -- ^ The alias in use for the bot that contains the session data.
  , userId :: Types.UserId
    -- ^ The ID of the client application user. Amazon Lex uses this to identify a user's conversation with your bot. 
  , accept :: Core.Maybe Types.Accept
    -- ^ The message that Amazon Lex returns in the response can be either text or speech based depending on the value of this field.
--
--
--     * If the value is @text/plain; charset=utf-8@ , Amazon Lex returns text in the response.
--
--
--     * If the value begins with @audio/@ , Amazon Lex returns speech in the response. Amazon Lex uses Amazon Polly to generate the speech in the configuration that you specify. For example, if you specify @audio/mpeg@ as the value, Amazon Lex returns speech in the MPEG format.
--
--
--     * If the value is @audio/pcm@ , the speech is returned as @audio/pcm@ in 16-bit, little endian format.
--
--
--     * The following are the accepted values:
--
--     * @audio/mpeg@ 
--
--
--     * @audio/ogg@ 
--
--
--     * @audio/pcm@ 
--
--
--     * @audio/*@ (defaults to mpeg)
--
--
--     * @text/plain; charset=utf-8@ 
--
--
--
--
  , activeContexts :: Core.Maybe [Types.ActiveContext]
    -- ^ A list of contexts active for the request. A context can be activated when a previous intent is fulfilled, or by including the context in the request,
--
-- If you don't specify a list of contexts, Amazon Lex will use the current list of contexts for the session. If you specify an empty list, all contexts for the session are cleared.
  , dialogAction :: Core.Maybe Types.DialogAction
    -- ^ Sets the next action that the bot should take to fulfill the conversation.
  , recentIntentSummaryView :: Core.Maybe [Types.IntentSummary]
    -- ^ A summary of the recent intents for the bot. You can use the intent summary view to set a checkpoint label on an intent and modify attributes of intents. You can also use it to remove or add intent summary objects to the list.
--
-- An intent that you modify or add to the list must make sense for the bot. For example, the intent name must be valid for the bot. You must provide valid values for:
--
--     * @intentName@ 
--
--
--     * slot names
--
--
--     * @slotToElict@ 
--
--
-- If you send the @recentIntentSummaryView@ parameter in a @PutSession@ request, the contents of the new summary view replaces the old summary view. For example, if a @GetSession@ request returns three intents in the summary view and you call @PutSession@ with one intent in the summary view, the next call to @GetSession@ will only return one intent.
  , sessionAttributes :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Map of key/value pairs representing the session-specific context information. It contains application information passed between Amazon Lex and a client application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutSession' value with any optional fields omitted.
mkPutSession
    :: Types.BotName -- ^ 'botName'
    -> Types.BotAlias -- ^ 'botAlias'
    -> Types.UserId -- ^ 'userId'
    -> PutSession
mkPutSession botName botAlias userId
  = PutSession'{botName, botAlias, userId, accept = Core.Nothing,
                activeContexts = Core.Nothing, dialogAction = Core.Nothing,
                recentIntentSummaryView = Core.Nothing,
                sessionAttributes = Core.Nothing}

-- | The name of the bot that contains the session data.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psBotName :: Lens.Lens' PutSession Types.BotName
psBotName = Lens.field @"botName"
{-# INLINEABLE psBotName #-}
{-# DEPRECATED botName "Use generic-lens or generic-optics with 'botName' instead"  #-}

-- | The alias in use for the bot that contains the session data.
--
-- /Note:/ Consider using 'botAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psBotAlias :: Lens.Lens' PutSession Types.BotAlias
psBotAlias = Lens.field @"botAlias"
{-# INLINEABLE psBotAlias #-}
{-# DEPRECATED botAlias "Use generic-lens or generic-optics with 'botAlias' instead"  #-}

-- | The ID of the client application user. Amazon Lex uses this to identify a user's conversation with your bot. 
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psUserId :: Lens.Lens' PutSession Types.UserId
psUserId = Lens.field @"userId"
{-# INLINEABLE psUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

-- | The message that Amazon Lex returns in the response can be either text or speech based depending on the value of this field.
--
--
--     * If the value is @text/plain; charset=utf-8@ , Amazon Lex returns text in the response.
--
--
--     * If the value begins with @audio/@ , Amazon Lex returns speech in the response. Amazon Lex uses Amazon Polly to generate the speech in the configuration that you specify. For example, if you specify @audio/mpeg@ as the value, Amazon Lex returns speech in the MPEG format.
--
--
--     * If the value is @audio/pcm@ , the speech is returned as @audio/pcm@ in 16-bit, little endian format.
--
--
--     * The following are the accepted values:
--
--     * @audio/mpeg@ 
--
--
--     * @audio/ogg@ 
--
--
--     * @audio/pcm@ 
--
--
--     * @audio/*@ (defaults to mpeg)
--
--
--     * @text/plain; charset=utf-8@ 
--
--
--
--
--
-- /Note:/ Consider using 'accept' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psAccept :: Lens.Lens' PutSession (Core.Maybe Types.Accept)
psAccept = Lens.field @"accept"
{-# INLINEABLE psAccept #-}
{-# DEPRECATED accept "Use generic-lens or generic-optics with 'accept' instead"  #-}

-- | A list of contexts active for the request. A context can be activated when a previous intent is fulfilled, or by including the context in the request,
--
-- If you don't specify a list of contexts, Amazon Lex will use the current list of contexts for the session. If you specify an empty list, all contexts for the session are cleared.
--
-- /Note:/ Consider using 'activeContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psActiveContexts :: Lens.Lens' PutSession (Core.Maybe [Types.ActiveContext])
psActiveContexts = Lens.field @"activeContexts"
{-# INLINEABLE psActiveContexts #-}
{-# DEPRECATED activeContexts "Use generic-lens or generic-optics with 'activeContexts' instead"  #-}

-- | Sets the next action that the bot should take to fulfill the conversation.
--
-- /Note:/ Consider using 'dialogAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psDialogAction :: Lens.Lens' PutSession (Core.Maybe Types.DialogAction)
psDialogAction = Lens.field @"dialogAction"
{-# INLINEABLE psDialogAction #-}
{-# DEPRECATED dialogAction "Use generic-lens or generic-optics with 'dialogAction' instead"  #-}

-- | A summary of the recent intents for the bot. You can use the intent summary view to set a checkpoint label on an intent and modify attributes of intents. You can also use it to remove or add intent summary objects to the list.
--
-- An intent that you modify or add to the list must make sense for the bot. For example, the intent name must be valid for the bot. You must provide valid values for:
--
--     * @intentName@ 
--
--
--     * slot names
--
--
--     * @slotToElict@ 
--
--
-- If you send the @recentIntentSummaryView@ parameter in a @PutSession@ request, the contents of the new summary view replaces the old summary view. For example, if a @GetSession@ request returns three intents in the summary view and you call @PutSession@ with one intent in the summary view, the next call to @GetSession@ will only return one intent.
--
-- /Note:/ Consider using 'recentIntentSummaryView' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psRecentIntentSummaryView :: Lens.Lens' PutSession (Core.Maybe [Types.IntentSummary])
psRecentIntentSummaryView = Lens.field @"recentIntentSummaryView"
{-# INLINEABLE psRecentIntentSummaryView #-}
{-# DEPRECATED recentIntentSummaryView "Use generic-lens or generic-optics with 'recentIntentSummaryView' instead"  #-}

-- | Map of key/value pairs representing the session-specific context information. It contains application information passed between Amazon Lex and a client application.
--
-- /Note:/ Consider using 'sessionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psSessionAttributes :: Lens.Lens' PutSession (Core.Maybe (Core.HashMap Core.Text Core.Text))
psSessionAttributes = Lens.field @"sessionAttributes"
{-# INLINEABLE psSessionAttributes #-}
{-# DEPRECATED sessionAttributes "Use generic-lens or generic-optics with 'sessionAttributes' instead"  #-}

instance Core.ToQuery PutSession where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutSession where
        toHeaders PutSession{..}
          = Core.toHeaders "Accept" accept Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutSession where
        toJSON PutSession{..}
          = Core.object
              (Core.catMaybes
                 [("activeContexts" Core..=) Core.<$> activeContexts,
                  ("dialogAction" Core..=) Core.<$> dialogAction,
                  ("recentIntentSummaryView" Core..=) Core.<$>
                    recentIntentSummaryView,
                  ("sessionAttributes" Core..=) Core.<$> sessionAttributes])

instance Core.AWSRequest PutSession where
        type Rs PutSession = PutSessionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/bot/" Core.<> Core.toText botName Core.<> "/alias/" Core.<>
                             Core.toText botAlias
                             Core.<> "/user/"
                             Core.<> Core.toText userId
                             Core.<> "/session",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveBody
              (\ s h x ->
                 PutSessionResponse' Core.<$>
                   (Core.parseHeaderMaybe "x-amz-lex-active-contexts" h) Core.<*>
                     Core.pure x
                     Core.<*> Core.parseHeaderMaybe "Content-Type" h
                     Core.<*> Core.parseHeaderMaybe "x-amz-lex-dialog-state" h
                     Core.<*> Core.parseHeaderMaybe "x-amz-lex-intent-name" h
                     Core.<*> Core.parseHeaderMaybe "x-amz-lex-message" h
                     Core.<*> Core.parseHeaderMaybe "x-amz-lex-message-format" h
                     Core.<*> Core.parseHeaderMaybe "x-amz-lex-session-attributes" h
                     Core.<*> Core.parseHeaderMaybe "x-amz-lex-session-id" h
                     Core.<*> Core.parseHeaderMaybe "x-amz-lex-slot-to-elicit" h
                     Core.<*> Core.parseHeaderMaybe "x-amz-lex-slots" h
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutSessionResponse' smart constructor.
data PutSessionResponse = PutSessionResponse'
  { activeContexts :: Core.Maybe Types.ActiveContexts
    -- ^ A list of active contexts for the session.
  , audioStream :: Core.RsBody
    -- ^ The audio version of the message to convey to the user.
  , contentType :: Core.Maybe Types.HttpContentType
    -- ^ Content type as specified in the @Accept@ HTTP header in the request.
  , dialogState :: Core.Maybe Types.DialogState
    -- ^ 
--
--
--     * @ConfirmIntent@ - Amazon Lex is expecting a "yes" or "no" response to confirm the intent before fulfilling an intent.
--
--
--     * @ElicitIntent@ - Amazon Lex wants to elicit the user's intent.
--
--
--     * @ElicitSlot@ - Amazon Lex is expecting the value of a slot for the current intent.
--
--
--     * @Failed@ - Conveys that the conversation with the user has failed. This can happen for various reasons, including the user does not provide an appropriate response to prompts from the service, or if the Lambda function fails to fulfill the intent.
--
--
--     * @Fulfilled@ - Conveys that the Lambda function has sucessfully fulfilled the intent.
--
--
--     * @ReadyForFulfillment@ - Conveys that the client has to fulfill the intent.
--
--
  , intentName :: Core.Maybe Types.IntentName
    -- ^ The name of the current intent.
  , message :: Core.Maybe Types.Message
    -- ^ The next message that should be presented to the user.
  , messageFormat :: Core.Maybe Types.MessageFormatType
    -- ^ The format of the response message. One of the following values:
--
--
--     * @PlainText@ - The message contains plain UTF-8 text.
--
--
--     * @CustomPayload@ - The message is a custom format for the client.
--
--
--     * @SSML@ - The message contains text formatted for voice output.
--
--
--     * @Composite@ - The message contains an escaped JSON object containing one or more messages from the groups that messages were assigned to when the intent was created.
--
--
  , sessionAttributes :: Core.Maybe Core.Text
    -- ^ Map of key/value pairs representing session-specific context information.
  , sessionId :: Core.Maybe Core.Text
    -- ^ A unique identifier for the session.
  , slotToElicit :: Core.Maybe Core.Text
    -- ^ If the @dialogState@ is @ElicitSlot@ , returns the name of the slot for which Amazon Lex is eliciting a value.
  , slots :: Core.Maybe Core.Text
    -- ^ Map of zero or more intent slots Amazon Lex detected from the user input during the conversation.
--
-- Amazon Lex creates a resolution list containing likely values for a slot. The value that it returns is determined by the @valueSelectionStrategy@ selected when the slot type was created or updated. If @valueSelectionStrategy@ is set to @ORIGINAL_VALUE@ , the value provided by the user is returned, if the user value is similar to the slot values. If @valueSelectionStrategy@ is set to @TOP_RESOLUTION@ Amazon Lex returns the first value in the resolution list or, if there is no resolution list, null. If you don't specify a @valueSelectionStrategy@ the default is @ORIGINAL_VALUE@ . 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Show, Core.Generic)

-- | Creates a 'PutSessionResponse' value with any optional fields omitted.
mkPutSessionResponse
    :: Core.RsBody -- ^ 'audioStream'
    -> Core.Int -- ^ 'responseStatus'
    -> PutSessionResponse
mkPutSessionResponse audioStream responseStatus
  = PutSessionResponse'{activeContexts = Core.Nothing, audioStream,
                        contentType = Core.Nothing, dialogState = Core.Nothing,
                        intentName = Core.Nothing, message = Core.Nothing,
                        messageFormat = Core.Nothing, sessionAttributes = Core.Nothing,
                        sessionId = Core.Nothing, slotToElicit = Core.Nothing,
                        slots = Core.Nothing, responseStatus}

-- | A list of active contexts for the session.
--
-- /Note:/ Consider using 'activeContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrrsActiveContexts :: Lens.Lens' PutSessionResponse (Core.Maybe Types.ActiveContexts)
psrrsActiveContexts = Lens.field @"activeContexts"
{-# INLINEABLE psrrsActiveContexts #-}
{-# DEPRECATED activeContexts "Use generic-lens or generic-optics with 'activeContexts' instead"  #-}

-- | The audio version of the message to convey to the user.
--
-- /Note:/ Consider using 'audioStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrrsAudioStream :: Lens.Lens' PutSessionResponse Core.RsBody
psrrsAudioStream = Lens.field @"audioStream"
{-# INLINEABLE psrrsAudioStream #-}
{-# DEPRECATED audioStream "Use generic-lens or generic-optics with 'audioStream' instead"  #-}

-- | Content type as specified in the @Accept@ HTTP header in the request.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrrsContentType :: Lens.Lens' PutSessionResponse (Core.Maybe Types.HttpContentType)
psrrsContentType = Lens.field @"contentType"
{-# INLINEABLE psrrsContentType #-}
{-# DEPRECATED contentType "Use generic-lens or generic-optics with 'contentType' instead"  #-}

-- | 
--
--
--     * @ConfirmIntent@ - Amazon Lex is expecting a "yes" or "no" response to confirm the intent before fulfilling an intent.
--
--
--     * @ElicitIntent@ - Amazon Lex wants to elicit the user's intent.
--
--
--     * @ElicitSlot@ - Amazon Lex is expecting the value of a slot for the current intent.
--
--
--     * @Failed@ - Conveys that the conversation with the user has failed. This can happen for various reasons, including the user does not provide an appropriate response to prompts from the service, or if the Lambda function fails to fulfill the intent.
--
--
--     * @Fulfilled@ - Conveys that the Lambda function has sucessfully fulfilled the intent.
--
--
--     * @ReadyForFulfillment@ - Conveys that the client has to fulfill the intent.
--
--
--
-- /Note:/ Consider using 'dialogState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrrsDialogState :: Lens.Lens' PutSessionResponse (Core.Maybe Types.DialogState)
psrrsDialogState = Lens.field @"dialogState"
{-# INLINEABLE psrrsDialogState #-}
{-# DEPRECATED dialogState "Use generic-lens or generic-optics with 'dialogState' instead"  #-}

-- | The name of the current intent.
--
-- /Note:/ Consider using 'intentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrrsIntentName :: Lens.Lens' PutSessionResponse (Core.Maybe Types.IntentName)
psrrsIntentName = Lens.field @"intentName"
{-# INLINEABLE psrrsIntentName #-}
{-# DEPRECATED intentName "Use generic-lens or generic-optics with 'intentName' instead"  #-}

-- | The next message that should be presented to the user.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrrsMessage :: Lens.Lens' PutSessionResponse (Core.Maybe Types.Message)
psrrsMessage = Lens.field @"message"
{-# INLINEABLE psrrsMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The format of the response message. One of the following values:
--
--
--     * @PlainText@ - The message contains plain UTF-8 text.
--
--
--     * @CustomPayload@ - The message is a custom format for the client.
--
--
--     * @SSML@ - The message contains text formatted for voice output.
--
--
--     * @Composite@ - The message contains an escaped JSON object containing one or more messages from the groups that messages were assigned to when the intent was created.
--
--
--
-- /Note:/ Consider using 'messageFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrrsMessageFormat :: Lens.Lens' PutSessionResponse (Core.Maybe Types.MessageFormatType)
psrrsMessageFormat = Lens.field @"messageFormat"
{-# INLINEABLE psrrsMessageFormat #-}
{-# DEPRECATED messageFormat "Use generic-lens or generic-optics with 'messageFormat' instead"  #-}

-- | Map of key/value pairs representing session-specific context information.
--
-- /Note:/ Consider using 'sessionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrrsSessionAttributes :: Lens.Lens' PutSessionResponse (Core.Maybe Core.Text)
psrrsSessionAttributes = Lens.field @"sessionAttributes"
{-# INLINEABLE psrrsSessionAttributes #-}
{-# DEPRECATED sessionAttributes "Use generic-lens or generic-optics with 'sessionAttributes' instead"  #-}

-- | A unique identifier for the session.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrrsSessionId :: Lens.Lens' PutSessionResponse (Core.Maybe Core.Text)
psrrsSessionId = Lens.field @"sessionId"
{-# INLINEABLE psrrsSessionId #-}
{-# DEPRECATED sessionId "Use generic-lens or generic-optics with 'sessionId' instead"  #-}

-- | If the @dialogState@ is @ElicitSlot@ , returns the name of the slot for which Amazon Lex is eliciting a value.
--
-- /Note:/ Consider using 'slotToElicit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrrsSlotToElicit :: Lens.Lens' PutSessionResponse (Core.Maybe Core.Text)
psrrsSlotToElicit = Lens.field @"slotToElicit"
{-# INLINEABLE psrrsSlotToElicit #-}
{-# DEPRECATED slotToElicit "Use generic-lens or generic-optics with 'slotToElicit' instead"  #-}

-- | Map of zero or more intent slots Amazon Lex detected from the user input during the conversation.
--
-- Amazon Lex creates a resolution list containing likely values for a slot. The value that it returns is determined by the @valueSelectionStrategy@ selected when the slot type was created or updated. If @valueSelectionStrategy@ is set to @ORIGINAL_VALUE@ , the value provided by the user is returned, if the user value is similar to the slot values. If @valueSelectionStrategy@ is set to @TOP_RESOLUTION@ Amazon Lex returns the first value in the resolution list or, if there is no resolution list, null. If you don't specify a @valueSelectionStrategy@ the default is @ORIGINAL_VALUE@ . 
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrrsSlots :: Lens.Lens' PutSessionResponse (Core.Maybe Core.Text)
psrrsSlots = Lens.field @"slots"
{-# INLINEABLE psrrsSlots #-}
{-# DEPRECATED slots "Use generic-lens or generic-optics with 'slots' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psrrsResponseStatus :: Lens.Lens' PutSessionResponse Core.Int
psrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE psrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
