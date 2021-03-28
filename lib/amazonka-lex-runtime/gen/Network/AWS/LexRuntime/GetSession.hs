{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.GetSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns session information for a specified bot, alias, and user ID.
module Network.AWS.LexRuntime.GetSession
    (
    -- * Creating a request
      GetSession (..)
    , mkGetSession
    -- ** Request lenses
    , gsBotName
    , gsBotAlias
    , gsUserId
    , gsCheckpointLabelFilter

    -- * Destructuring the response
    , GetSessionResponse (..)
    , mkGetSessionResponse
    -- ** Response lenses
    , gsrrsActiveContexts
    , gsrrsDialogAction
    , gsrrsRecentIntentSummaryView
    , gsrrsSessionAttributes
    , gsrrsSessionId
    , gsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexRuntime.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSession' smart constructor.
data GetSession = GetSession'
  { botName :: Types.BotName
    -- ^ The name of the bot that contains the session data.
  , botAlias :: Types.BotAlias
    -- ^ The alias in use for the bot that contains the session data.
  , userId :: Types.UserId
    -- ^ The ID of the client application user. Amazon Lex uses this to identify a user's conversation with your bot. 
  , checkpointLabelFilter :: Core.Maybe Types.CheckpointLabelFilter
    -- ^ A string used to filter the intents returned in the @recentIntentSummaryView@ structure. 
--
-- When you specify a filter, only intents with their @checkpointLabel@ field set to that string are returned.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSession' value with any optional fields omitted.
mkGetSession
    :: Types.BotName -- ^ 'botName'
    -> Types.BotAlias -- ^ 'botAlias'
    -> Types.UserId -- ^ 'userId'
    -> GetSession
mkGetSession botName botAlias userId
  = GetSession'{botName, botAlias, userId,
                checkpointLabelFilter = Core.Nothing}

-- | The name of the bot that contains the session data.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsBotName :: Lens.Lens' GetSession Types.BotName
gsBotName = Lens.field @"botName"
{-# INLINEABLE gsBotName #-}
{-# DEPRECATED botName "Use generic-lens or generic-optics with 'botName' instead"  #-}

-- | The alias in use for the bot that contains the session data.
--
-- /Note:/ Consider using 'botAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsBotAlias :: Lens.Lens' GetSession Types.BotAlias
gsBotAlias = Lens.field @"botAlias"
{-# INLINEABLE gsBotAlias #-}
{-# DEPRECATED botAlias "Use generic-lens or generic-optics with 'botAlias' instead"  #-}

-- | The ID of the client application user. Amazon Lex uses this to identify a user's conversation with your bot. 
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsUserId :: Lens.Lens' GetSession Types.UserId
gsUserId = Lens.field @"userId"
{-# INLINEABLE gsUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

-- | A string used to filter the intents returned in the @recentIntentSummaryView@ structure. 
--
-- When you specify a filter, only intents with their @checkpointLabel@ field set to that string are returned.
--
-- /Note:/ Consider using 'checkpointLabelFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsCheckpointLabelFilter :: Lens.Lens' GetSession (Core.Maybe Types.CheckpointLabelFilter)
gsCheckpointLabelFilter = Lens.field @"checkpointLabelFilter"
{-# INLINEABLE gsCheckpointLabelFilter #-}
{-# DEPRECATED checkpointLabelFilter "Use generic-lens or generic-optics with 'checkpointLabelFilter' instead"  #-}

instance Core.ToQuery GetSession where
        toQuery GetSession{..}
          = Core.maybe Core.mempty (Core.toQueryPair "checkpointLabelFilter")
              checkpointLabelFilter

instance Core.ToHeaders GetSession where
        toHeaders GetSession{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetSession where
        type Rs GetSession = GetSessionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/bot/" Core.<> Core.toText botName Core.<> "/alias/" Core.<>
                             Core.toText botAlias
                             Core.<> "/user/"
                             Core.<> Core.toText userId
                             Core.<> "/session/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSessionResponse' Core.<$>
                   (x Core..:? "activeContexts") Core.<*> x Core..:? "dialogAction"
                     Core.<*> x Core..:? "recentIntentSummaryView"
                     Core.<*> x Core..:? "sessionAttributes"
                     Core.<*> x Core..:? "sessionId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetSessionResponse' smart constructor.
data GetSessionResponse = GetSessionResponse'
  { activeContexts :: Core.Maybe [Types.ActiveContext]
    -- ^ A list of active contexts for the session. A context can be set when an intent is fulfilled or by calling the @PostContent@ , @PostText@ , or @PutSession@ operation.
--
-- You can use a context to control the intents that can follow up an intent, or to modify the operation of your application.
  , dialogAction :: Core.Maybe Types.DialogAction
    -- ^ Describes the current state of the bot.
  , recentIntentSummaryView :: Core.Maybe [Types.IntentSummary]
    -- ^ An array of information about the intents used in the session. The array can contain a maximum of three summaries. If more than three intents are used in the session, the @recentIntentSummaryView@ operation contains information about the last three intents used.
--
-- If you set the @checkpointLabelFilter@ parameter in the request, the array contains only the intents with the specified label.
  , sessionAttributes :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Map of key/value pairs representing the session-specific context information. It contains application information passed between Amazon Lex and a client application.
  , sessionId :: Core.Maybe Core.Text
    -- ^ A unique identifier for the session.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSessionResponse' value with any optional fields omitted.
mkGetSessionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSessionResponse
mkGetSessionResponse responseStatus
  = GetSessionResponse'{activeContexts = Core.Nothing,
                        dialogAction = Core.Nothing,
                        recentIntentSummaryView = Core.Nothing,
                        sessionAttributes = Core.Nothing, sessionId = Core.Nothing,
                        responseStatus}

-- | A list of active contexts for the session. A context can be set when an intent is fulfilled or by calling the @PostContent@ , @PostText@ , or @PutSession@ operation.
--
-- You can use a context to control the intents that can follow up an intent, or to modify the operation of your application.
--
-- /Note:/ Consider using 'activeContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsActiveContexts :: Lens.Lens' GetSessionResponse (Core.Maybe [Types.ActiveContext])
gsrrsActiveContexts = Lens.field @"activeContexts"
{-# INLINEABLE gsrrsActiveContexts #-}
{-# DEPRECATED activeContexts "Use generic-lens or generic-optics with 'activeContexts' instead"  #-}

-- | Describes the current state of the bot.
--
-- /Note:/ Consider using 'dialogAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsDialogAction :: Lens.Lens' GetSessionResponse (Core.Maybe Types.DialogAction)
gsrrsDialogAction = Lens.field @"dialogAction"
{-# INLINEABLE gsrrsDialogAction #-}
{-# DEPRECATED dialogAction "Use generic-lens or generic-optics with 'dialogAction' instead"  #-}

-- | An array of information about the intents used in the session. The array can contain a maximum of three summaries. If more than three intents are used in the session, the @recentIntentSummaryView@ operation contains information about the last three intents used.
--
-- If you set the @checkpointLabelFilter@ parameter in the request, the array contains only the intents with the specified label.
--
-- /Note:/ Consider using 'recentIntentSummaryView' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsRecentIntentSummaryView :: Lens.Lens' GetSessionResponse (Core.Maybe [Types.IntentSummary])
gsrrsRecentIntentSummaryView = Lens.field @"recentIntentSummaryView"
{-# INLINEABLE gsrrsRecentIntentSummaryView #-}
{-# DEPRECATED recentIntentSummaryView "Use generic-lens or generic-optics with 'recentIntentSummaryView' instead"  #-}

-- | Map of key/value pairs representing the session-specific context information. It contains application information passed between Amazon Lex and a client application.
--
-- /Note:/ Consider using 'sessionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsSessionAttributes :: Lens.Lens' GetSessionResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
gsrrsSessionAttributes = Lens.field @"sessionAttributes"
{-# INLINEABLE gsrrsSessionAttributes #-}
{-# DEPRECATED sessionAttributes "Use generic-lens or generic-optics with 'sessionAttributes' instead"  #-}

-- | A unique identifier for the session.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsSessionId :: Lens.Lens' GetSessionResponse (Core.Maybe Core.Text)
gsrrsSessionId = Lens.field @"sessionId"
{-# INLINEABLE gsrrsSessionId #-}
{-# DEPRECATED sessionId "Use generic-lens or generic-optics with 'sessionId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsResponseStatus :: Lens.Lens' GetSessionResponse Core.Int
gsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
