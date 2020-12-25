{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetUtterancesView
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use the @GetUtterancesView@ operation to get information about the utterances that your users have made to your bot. You can use this list to tune the utterances that your bot responds to.
--
-- For example, say that you have created a bot to order flowers. After your users have used your bot for a while, use the @GetUtterancesView@ operation to see the requests that they have made and whether they have been successful. You might find that the utterance "I want flowers" is not being recognized. You could add this utterance to the @OrderFlowers@ intent so that your bot recognizes that utterance.
-- After you publish a new version of a bot, you can get information about the old version and the new so that you can compare the performance across the two versions.
-- Utterance statistics are generated once a day. Data is available for the last 15 days. You can request information for up to 5 versions of your bot in each request. Amazon Lex returns the most frequent utterances received by the bot in the last 15 days. The response contains information about a maximum of 100 utterances for each version.
-- If you set @childDirected@ field to true when you created your bot, or if you opted out of participating in improving Amazon Lex, utterances are not available.
-- This operation requires permissions for the @lex:GetUtterancesView@ action.
module Network.AWS.LexModels.GetUtterancesView
  ( -- * Creating a request
    GetUtterancesView (..),
    mkGetUtterancesView,

    -- ** Request lenses
    guvBotName,
    guvBotVersions,
    guvStatusType,

    -- * Destructuring the response
    GetUtterancesViewResponse (..),
    mkGetUtterancesViewResponse,

    -- ** Response lenses
    guvrrsBotName,
    guvrrsUtterances,
    guvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetUtterancesView' smart constructor.
data GetUtterancesView = GetUtterancesView'
  { -- | The name of the bot for which utterance information should be returned.
    botName :: Types.BotName,
    -- | An array of bot versions for which utterance information should be returned. The limit is 5 versions per request.
    botVersions :: Core.NonEmpty Types.Version,
    -- | To return utterances that were recognized and handled, use @Detected@ . To return utterances that were not recognized, use @Missed@ .
    statusType :: Types.StatusType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetUtterancesView' value with any optional fields omitted.
mkGetUtterancesView ::
  -- | 'botName'
  Types.BotName ->
  -- | 'botVersions'
  Core.NonEmpty Types.Version ->
  -- | 'statusType'
  Types.StatusType ->
  GetUtterancesView
mkGetUtterancesView botName botVersions statusType =
  GetUtterancesView' {botName, botVersions, statusType}

-- | The name of the bot for which utterance information should be returned.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guvBotName :: Lens.Lens' GetUtterancesView Types.BotName
guvBotName = Lens.field @"botName"
{-# DEPRECATED guvBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | An array of bot versions for which utterance information should be returned. The limit is 5 versions per request.
--
-- /Note:/ Consider using 'botVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guvBotVersions :: Lens.Lens' GetUtterancesView (Core.NonEmpty Types.Version)
guvBotVersions = Lens.field @"botVersions"
{-# DEPRECATED guvBotVersions "Use generic-lens or generic-optics with 'botVersions' instead." #-}

-- | To return utterances that were recognized and handled, use @Detected@ . To return utterances that were not recognized, use @Missed@ .
--
-- /Note:/ Consider using 'statusType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guvStatusType :: Lens.Lens' GetUtterancesView Types.StatusType
guvStatusType = Lens.field @"statusType"
{-# DEPRECATED guvStatusType "Use generic-lens or generic-optics with 'statusType' instead." #-}

instance Core.AWSRequest GetUtterancesView where
  type Rs GetUtterancesView = GetUtterancesViewResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/bots/" Core.<> (Core.toText botName) Core.<> ("/utterances")),
        Core._rqQuery =
          Core.toQueryValue
            "bot_versions"
            (Core.toQueryList "member" botVersions)
            Core.<> (Core.toQueryValue "status_type" statusType)
            Core.<> (Core.pure ("view=aggregation", "")),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUtterancesViewResponse'
            Core.<$> (x Core..:? "botName")
            Core.<*> (x Core..:? "utterances")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetUtterancesViewResponse' smart constructor.
data GetUtterancesViewResponse = GetUtterancesViewResponse'
  { -- | The name of the bot for which utterance information was returned.
    botName :: Core.Maybe Types.BotName,
    -- | An array of 'UtteranceList' objects, each containing a list of 'UtteranceData' objects describing the utterances that were processed by your bot. The response contains a maximum of 100 @UtteranceData@ objects for each version. Amazon Lex returns the most frequent utterances received by the bot in the last 15 days.
    utterances :: Core.Maybe [Types.UtteranceList],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetUtterancesViewResponse' value with any optional fields omitted.
mkGetUtterancesViewResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetUtterancesViewResponse
mkGetUtterancesViewResponse responseStatus =
  GetUtterancesViewResponse'
    { botName = Core.Nothing,
      utterances = Core.Nothing,
      responseStatus
    }

-- | The name of the bot for which utterance information was returned.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guvrrsBotName :: Lens.Lens' GetUtterancesViewResponse (Core.Maybe Types.BotName)
guvrrsBotName = Lens.field @"botName"
{-# DEPRECATED guvrrsBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | An array of 'UtteranceList' objects, each containing a list of 'UtteranceData' objects describing the utterances that were processed by your bot. The response contains a maximum of 100 @UtteranceData@ objects for each version. Amazon Lex returns the most frequent utterances received by the bot in the last 15 days.
--
-- /Note:/ Consider using 'utterances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guvrrsUtterances :: Lens.Lens' GetUtterancesViewResponse (Core.Maybe [Types.UtteranceList])
guvrrsUtterances = Lens.field @"utterances"
{-# DEPRECATED guvrrsUtterances "Use generic-lens or generic-optics with 'utterances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guvrrsResponseStatus :: Lens.Lens' GetUtterancesViewResponse Core.Int
guvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED guvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
