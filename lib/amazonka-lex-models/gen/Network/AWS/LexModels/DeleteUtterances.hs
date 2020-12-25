{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.DeleteUtterances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes stored utterances.
--
-- Amazon Lex stores the utterances that users send to your bot. Utterances are stored for 15 days for use with the 'GetUtterancesView' operation, and then stored indefinitely for use in improving the ability of your bot to respond to user input.
-- Use the @DeleteUtterances@ operation to manually delete stored utterances for a specific user. When you use the @DeleteUtterances@ operation, utterances stored for improving your bot's ability to respond to user input are deleted immediately. Utterances stored for use with the @GetUtterancesView@ operation are deleted after 15 days.
-- This operation requires permissions for the @lex:DeleteUtterances@ action.
module Network.AWS.LexModels.DeleteUtterances
  ( -- * Creating a request
    DeleteUtterances (..),
    mkDeleteUtterances,

    -- ** Request lenses
    duBotName,
    duUserId,

    -- * Destructuring the response
    DeleteUtterancesResponse (..),
    mkDeleteUtterancesResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteUtterances' smart constructor.
data DeleteUtterances = DeleteUtterances'
  { -- | The name of the bot that stored the utterances.
    botName :: Types.BotName,
    -- | The unique identifier for the user that made the utterances. This is the user ID that was sent in the <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent> or <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> operation request that contained the utterance.
    userId :: Types.UserId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUtterances' value with any optional fields omitted.
mkDeleteUtterances ::
  -- | 'botName'
  Types.BotName ->
  -- | 'userId'
  Types.UserId ->
  DeleteUtterances
mkDeleteUtterances botName userId =
  DeleteUtterances' {botName, userId}

-- | The name of the bot that stored the utterances.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duBotName :: Lens.Lens' DeleteUtterances Types.BotName
duBotName = Lens.field @"botName"
{-# DEPRECATED duBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | The unique identifier for the user that made the utterances. This is the user ID that was sent in the <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostContent.html PostContent> or <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> operation request that contained the utterance.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duUserId :: Lens.Lens' DeleteUtterances Types.UserId
duUserId = Lens.field @"userId"
{-# DEPRECATED duUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Core.AWSRequest DeleteUtterances where
  type Rs DeleteUtterances = DeleteUtterancesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/bots/" Core.<> (Core.toText botName) Core.<> ("/utterances/")
                Core.<> (Core.toText userId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response = Response.receiveNull DeleteUtterancesResponse'

-- | /See:/ 'mkDeleteUtterancesResponse' smart constructor.
data DeleteUtterancesResponse = DeleteUtterancesResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUtterancesResponse' value with any optional fields omitted.
mkDeleteUtterancesResponse ::
  DeleteUtterancesResponse
mkDeleteUtterancesResponse = DeleteUtterancesResponse'
