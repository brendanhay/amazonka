{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.DeleteSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes session information for a specified bot, alias, and user ID.
module Network.AWS.LexRuntime.DeleteSession
  ( -- * Creating a request
    DeleteSession (..),
    mkDeleteSession,

    -- ** Request lenses
    dsBotName,
    dsBotAlias,
    dsUserId,

    -- * Destructuring the response
    DeleteSessionResponse (..),
    mkDeleteSessionResponse,

    -- ** Response lenses
    dsrrsBotAlias,
    dsrrsBotName,
    dsrrsSessionId,
    dsrrsUserId,
    dsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexRuntime.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSession' smart constructor.
data DeleteSession = DeleteSession'
  { -- | The name of the bot that contains the session data.
    botName :: Types.BotName,
    -- | The alias in use for the bot that contains the session data.
    botAlias :: Types.BotAlias,
    -- | The identifier of the user associated with the session data.
    userId :: Types.UserId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSession' value with any optional fields omitted.
mkDeleteSession ::
  -- | 'botName'
  Types.BotName ->
  -- | 'botAlias'
  Types.BotAlias ->
  -- | 'userId'
  Types.UserId ->
  DeleteSession
mkDeleteSession botName botAlias userId =
  DeleteSession' {botName, botAlias, userId}

-- | The name of the bot that contains the session data.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsBotName :: Lens.Lens' DeleteSession Types.BotName
dsBotName = Lens.field @"botName"
{-# DEPRECATED dsBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | The alias in use for the bot that contains the session data.
--
-- /Note:/ Consider using 'botAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsBotAlias :: Lens.Lens' DeleteSession Types.BotAlias
dsBotAlias = Lens.field @"botAlias"
{-# DEPRECATED dsBotAlias "Use generic-lens or generic-optics with 'botAlias' instead." #-}

-- | The identifier of the user associated with the session data.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsUserId :: Lens.Lens' DeleteSession Types.UserId
dsUserId = Lens.field @"userId"
{-# DEPRECATED dsUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Core.AWSRequest DeleteSession where
  type Rs DeleteSession = DeleteSessionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/bot/" Core.<> (Core.toText botName) Core.<> ("/alias/")
                Core.<> (Core.toText botAlias)
                Core.<> ("/user/")
                Core.<> (Core.toText userId)
                Core.<> ("/session")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSessionResponse'
            Core.<$> (x Core..:? "botAlias")
            Core.<*> (x Core..:? "botName")
            Core.<*> (x Core..:? "sessionId")
            Core.<*> (x Core..:? "userId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteSessionResponse' smart constructor.
data DeleteSessionResponse = DeleteSessionResponse'
  { -- | The alias in use for the bot associated with the session data.
    botAlias :: Core.Maybe Types.BotAlias,
    -- | The name of the bot associated with the session data.
    botName :: Core.Maybe Types.BotName,
    -- | The unique identifier for the session.
    sessionId :: Core.Maybe Types.SessionId,
    -- | The ID of the client application user.
    userId :: Core.Maybe Types.UserId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSessionResponse' value with any optional fields omitted.
mkDeleteSessionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteSessionResponse
mkDeleteSessionResponse responseStatus =
  DeleteSessionResponse'
    { botAlias = Core.Nothing,
      botName = Core.Nothing,
      sessionId = Core.Nothing,
      userId = Core.Nothing,
      responseStatus
    }

-- | The alias in use for the bot associated with the session data.
--
-- /Note:/ Consider using 'botAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsBotAlias :: Lens.Lens' DeleteSessionResponse (Core.Maybe Types.BotAlias)
dsrrsBotAlias = Lens.field @"botAlias"
{-# DEPRECATED dsrrsBotAlias "Use generic-lens or generic-optics with 'botAlias' instead." #-}

-- | The name of the bot associated with the session data.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsBotName :: Lens.Lens' DeleteSessionResponse (Core.Maybe Types.BotName)
dsrrsBotName = Lens.field @"botName"
{-# DEPRECATED dsrrsBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | The unique identifier for the session.
--
-- /Note:/ Consider using 'sessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsSessionId :: Lens.Lens' DeleteSessionResponse (Core.Maybe Types.SessionId)
dsrrsSessionId = Lens.field @"sessionId"
{-# DEPRECATED dsrrsSessionId "Use generic-lens or generic-optics with 'sessionId' instead." #-}

-- | The ID of the client application user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsUserId :: Lens.Lens' DeleteSessionResponse (Core.Maybe Types.UserId)
dsrrsUserId = Lens.field @"userId"
{-# DEPRECATED dsrrsUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DeleteSessionResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
