{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DisassociateLexBot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes authorization from the specified instance to access the specified Amazon Lex bot.
module Network.AWS.Connect.DisassociateLexBot
  ( -- * Creating a request
    DisassociateLexBot (..),
    mkDisassociateLexBot,

    -- ** Request lenses
    dlbInstanceId,
    dlbBotName,
    dlbLexRegion,

    -- * Destructuring the response
    DisassociateLexBotResponse (..),
    mkDisassociateLexBotResponse,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateLexBot' smart constructor.
data DisassociateLexBot = DisassociateLexBot'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The name of the Amazon Lex bot. Maximum character limit of 50.
    botName :: Types.BotName,
    -- | The Region in which the Amazon Lex bot has been created.
    lexRegion :: Types.LexRegion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateLexBot' value with any optional fields omitted.
mkDisassociateLexBot ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'botName'
  Types.BotName ->
  -- | 'lexRegion'
  Types.LexRegion ->
  DisassociateLexBot
mkDisassociateLexBot instanceId botName lexRegion =
  DisassociateLexBot' {instanceId, botName, lexRegion}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbInstanceId :: Lens.Lens' DisassociateLexBot Types.InstanceId
dlbInstanceId = Lens.field @"instanceId"
{-# DEPRECATED dlbInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The name of the Amazon Lex bot. Maximum character limit of 50.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbBotName :: Lens.Lens' DisassociateLexBot Types.BotName
dlbBotName = Lens.field @"botName"
{-# DEPRECATED dlbBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | The Region in which the Amazon Lex bot has been created.
--
-- /Note:/ Consider using 'lexRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbLexRegion :: Lens.Lens' DisassociateLexBot Types.LexRegion
dlbLexRegion = Lens.field @"lexRegion"
{-# DEPRECATED dlbLexRegion "Use generic-lens or generic-optics with 'lexRegion' instead." #-}

instance Core.AWSRequest DisassociateLexBot where
  type Rs DisassociateLexBot = DisassociateLexBotResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/instance/" Core.<> (Core.toText instanceId)
                Core.<> ("/lex-bot")
            ),
        Core._rqQuery =
          Core.toQueryValue "botName" botName
            Core.<> (Core.toQueryValue "lexRegion" lexRegion),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response = Response.receiveNull DisassociateLexBotResponse'

-- | /See:/ 'mkDisassociateLexBotResponse' smart constructor.
data DisassociateLexBotResponse = DisassociateLexBotResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateLexBotResponse' value with any optional fields omitted.
mkDisassociateLexBotResponse ::
  DisassociateLexBotResponse
mkDisassociateLexBotResponse = DisassociateLexBotResponse'
