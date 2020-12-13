{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetSession (..),
    mkGetSession,

    -- ** Request lenses
    gsBotAlias,
    gsBotName,
    gsUserId,
    gsCheckpointLabelFilter,

    -- * Destructuring the response
    GetSessionResponse (..),
    mkGetSessionResponse,

    -- ** Response lenses
    gsrsActiveContexts,
    gsrsSessionId,
    gsrsRecentIntentSummaryView,
    gsrsDialogAction,
    gsrsSessionAttributes,
    gsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexRuntime.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSession' smart constructor.
data GetSession = GetSession'
  { -- | The alias in use for the bot that contains the session data.
    botAlias :: Lude.Text,
    -- | The name of the bot that contains the session data.
    botName :: Lude.Text,
    -- | The ID of the client application user. Amazon Lex uses this to identify a user's conversation with your bot.
    userId :: Lude.Text,
    -- | A string used to filter the intents returned in the @recentIntentSummaryView@ structure.
    --
    -- When you specify a filter, only intents with their @checkpointLabel@ field set to that string are returned.
    checkpointLabelFilter :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSession' with the minimum fields required to make a request.
--
-- * 'botAlias' - The alias in use for the bot that contains the session data.
-- * 'botName' - The name of the bot that contains the session data.
-- * 'userId' - The ID of the client application user. Amazon Lex uses this to identify a user's conversation with your bot.
-- * 'checkpointLabelFilter' - A string used to filter the intents returned in the @recentIntentSummaryView@ structure.
--
-- When you specify a filter, only intents with their @checkpointLabel@ field set to that string are returned.
mkGetSession ::
  -- | 'botAlias'
  Lude.Text ->
  -- | 'botName'
  Lude.Text ->
  -- | 'userId'
  Lude.Text ->
  GetSession
mkGetSession pBotAlias_ pBotName_ pUserId_ =
  GetSession'
    { botAlias = pBotAlias_,
      botName = pBotName_,
      userId = pUserId_,
      checkpointLabelFilter = Lude.Nothing
    }

-- | The alias in use for the bot that contains the session data.
--
-- /Note:/ Consider using 'botAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsBotAlias :: Lens.Lens' GetSession Lude.Text
gsBotAlias = Lens.lens (botAlias :: GetSession -> Lude.Text) (\s a -> s {botAlias = a} :: GetSession)
{-# DEPRECATED gsBotAlias "Use generic-lens or generic-optics with 'botAlias' instead." #-}

-- | The name of the bot that contains the session data.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsBotName :: Lens.Lens' GetSession Lude.Text
gsBotName = Lens.lens (botName :: GetSession -> Lude.Text) (\s a -> s {botName = a} :: GetSession)
{-# DEPRECATED gsBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | The ID of the client application user. Amazon Lex uses this to identify a user's conversation with your bot.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsUserId :: Lens.Lens' GetSession Lude.Text
gsUserId = Lens.lens (userId :: GetSession -> Lude.Text) (\s a -> s {userId = a} :: GetSession)
{-# DEPRECATED gsUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | A string used to filter the intents returned in the @recentIntentSummaryView@ structure.
--
-- When you specify a filter, only intents with their @checkpointLabel@ field set to that string are returned.
--
-- /Note:/ Consider using 'checkpointLabelFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsCheckpointLabelFilter :: Lens.Lens' GetSession (Lude.Maybe Lude.Text)
gsCheckpointLabelFilter = Lens.lens (checkpointLabelFilter :: GetSession -> Lude.Maybe Lude.Text) (\s a -> s {checkpointLabelFilter = a} :: GetSession)
{-# DEPRECATED gsCheckpointLabelFilter "Use generic-lens or generic-optics with 'checkpointLabelFilter' instead." #-}

instance Lude.AWSRequest GetSession where
  type Rs GetSession = GetSessionResponse
  request = Req.get lexRuntimeService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSessionResponse'
            Lude.<$> (x Lude..?> "activeContexts" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "sessionId")
            Lude.<*> (x Lude..?> "recentIntentSummaryView" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "dialogAction")
            Lude.<*> (x Lude..?> "sessionAttributes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSession where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetSession where
  toPath GetSession' {..} =
    Lude.mconcat
      [ "/bot/",
        Lude.toBS botName,
        "/alias/",
        Lude.toBS botAlias,
        "/user/",
        Lude.toBS userId,
        "/session/"
      ]

instance Lude.ToQuery GetSession where
  toQuery GetSession' {..} =
    Lude.mconcat
      ["checkpointLabelFilter" Lude.=: checkpointLabelFilter]

-- | /See:/ 'mkGetSessionResponse' smart constructor.
data GetSessionResponse = GetSessionResponse'
  { -- | A list of active contexts for the session. A context can be set when an intent is fulfilled or by calling the @PostContent@ , @PostText@ , or @PutSession@ operation.
    --
    -- You can use a context to control the intents that can follow up an intent, or to modify the operation of your application.
    activeContexts :: Lude.Maybe [ActiveContext],
    -- | A unique identifier for the session.
    sessionId :: Lude.Maybe Lude.Text,
    -- | An array of information about the intents used in the session. The array can contain a maximum of three summaries. If more than three intents are used in the session, the @recentIntentSummaryView@ operation contains information about the last three intents used.
    --
    -- If you set the @checkpointLabelFilter@ parameter in the request, the array contains only the intents with the specified label.
    recentIntentSummaryView :: Lude.Maybe [IntentSummary],
    -- | Describes the current state of the bot.
    dialogAction :: Lude.Maybe DialogAction,
    -- | Map of key/value pairs representing the session-specific context information. It contains application information passed between Amazon Lex and a client application.
    sessionAttributes :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSessionResponse' with the minimum fields required to make a request.
--
-- * 'activeContexts' - A list of active contexts for the session. A context can be set when an intent is fulfilled or by calling the @PostContent@ , @PostText@ , or @PutSession@ operation.
--
-- You can use a context to control the intents that can follow up an intent, or to modify the operation of your application.
-- * 'sessionId' - A unique identifier for the session.
-- * 'recentIntentSummaryView' - An array of information about the intents used in the session. The array can contain a maximum of three summaries. If more than three intents are used in the session, the @recentIntentSummaryView@ operation contains information about the last three intents used.
--
-- If you set the @checkpointLabelFilter@ parameter in the request, the array contains only the intents with the specified label.
-- * 'dialogAction' - Describes the current state of the bot.
-- * 'sessionAttributes' - Map of key/value pairs representing the session-specific context information. It contains application information passed between Amazon Lex and a client application.
-- * 'responseStatus' - The response status code.
mkGetSessionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSessionResponse
mkGetSessionResponse pResponseStatus_ =
  GetSessionResponse'
    { activeContexts = Lude.Nothing,
      sessionId = Lude.Nothing,
      recentIntentSummaryView = Lude.Nothing,
      dialogAction = Lude.Nothing,
      sessionAttributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of active contexts for the session. A context can be set when an intent is fulfilled or by calling the @PostContent@ , @PostText@ , or @PutSession@ operation.
--
-- You can use a context to control the intents that can follow up an intent, or to modify the operation of your application.
--
-- /Note:/ Consider using 'activeContexts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsActiveContexts :: Lens.Lens' GetSessionResponse (Lude.Maybe [ActiveContext])
gsrsActiveContexts = Lens.lens (activeContexts :: GetSessionResponse -> Lude.Maybe [ActiveContext]) (\s a -> s {activeContexts = a} :: GetSessionResponse)
{-# DEPRECATED gsrsActiveContexts "Use generic-lens or generic-optics with 'activeContexts' instead." #-}

-- | A unique identifier for the session.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsSessionId :: Lens.Lens' GetSessionResponse (Lude.Maybe Lude.Text)
gsrsSessionId = Lens.lens (sessionId :: GetSessionResponse -> Lude.Maybe Lude.Text) (\s a -> s {sessionId = a} :: GetSessionResponse)
{-# DEPRECATED gsrsSessionId "Use generic-lens or generic-optics with 'sessionId' instead." #-}

-- | An array of information about the intents used in the session. The array can contain a maximum of three summaries. If more than three intents are used in the session, the @recentIntentSummaryView@ operation contains information about the last three intents used.
--
-- If you set the @checkpointLabelFilter@ parameter in the request, the array contains only the intents with the specified label.
--
-- /Note:/ Consider using 'recentIntentSummaryView' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsRecentIntentSummaryView :: Lens.Lens' GetSessionResponse (Lude.Maybe [IntentSummary])
gsrsRecentIntentSummaryView = Lens.lens (recentIntentSummaryView :: GetSessionResponse -> Lude.Maybe [IntentSummary]) (\s a -> s {recentIntentSummaryView = a} :: GetSessionResponse)
{-# DEPRECATED gsrsRecentIntentSummaryView "Use generic-lens or generic-optics with 'recentIntentSummaryView' instead." #-}

-- | Describes the current state of the bot.
--
-- /Note:/ Consider using 'dialogAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsDialogAction :: Lens.Lens' GetSessionResponse (Lude.Maybe DialogAction)
gsrsDialogAction = Lens.lens (dialogAction :: GetSessionResponse -> Lude.Maybe DialogAction) (\s a -> s {dialogAction = a} :: GetSessionResponse)
{-# DEPRECATED gsrsDialogAction "Use generic-lens or generic-optics with 'dialogAction' instead." #-}

-- | Map of key/value pairs representing the session-specific context information. It contains application information passed between Amazon Lex and a client application.
--
-- /Note:/ Consider using 'sessionAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsSessionAttributes :: Lens.Lens' GetSessionResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gsrsSessionAttributes = Lens.lens (sessionAttributes :: GetSessionResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {sessionAttributes = a} :: GetSessionResponse)
{-# DEPRECATED gsrsSessionAttributes "Use generic-lens or generic-optics with 'sessionAttributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsResponseStatus :: Lens.Lens' GetSessionResponse Lude.Int
gsrsResponseStatus = Lens.lens (responseStatus :: GetSessionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSessionResponse)
{-# DEPRECATED gsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
