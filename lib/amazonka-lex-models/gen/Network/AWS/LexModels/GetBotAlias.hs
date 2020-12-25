{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetBotAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an Amazon Lex bot alias. For more information about aliases, see 'versioning-aliases' .
--
-- This operation requires permissions for the @lex:GetBotAlias@ action.
module Network.AWS.LexModels.GetBotAlias
  ( -- * Creating a request
    GetBotAlias (..),
    mkGetBotAlias,

    -- ** Request lenses
    gbasName,
    gbasBotName,

    -- * Destructuring the response
    GetBotAliasResponse (..),
    mkGetBotAliasResponse,

    -- ** Response lenses
    gbarfrsBotName,
    gbarfrsBotVersion,
    gbarfrsChecksum,
    gbarfrsConversationLogs,
    gbarfrsCreatedDate,
    gbarfrsDescription,
    gbarfrsLastUpdatedDate,
    gbarfrsName,
    gbarfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetBotAlias' smart constructor.
data GetBotAlias = GetBotAlias'
  { -- | The name of the bot alias. The name is case sensitive.
    name :: Types.AliasName,
    -- | The name of the bot.
    botName :: Types.BotName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBotAlias' value with any optional fields omitted.
mkGetBotAlias ::
  -- | 'name'
  Types.AliasName ->
  -- | 'botName'
  Types.BotName ->
  GetBotAlias
mkGetBotAlias name botName = GetBotAlias' {name, botName}

-- | The name of the bot alias. The name is case sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbasName :: Lens.Lens' GetBotAlias Types.AliasName
gbasName = Lens.field @"name"
{-# DEPRECATED gbasName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name of the bot.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbasBotName :: Lens.Lens' GetBotAlias Types.BotName
gbasBotName = Lens.field @"botName"
{-# DEPRECATED gbasBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

instance Core.AWSRequest GetBotAlias where
  type Rs GetBotAlias = GetBotAliasResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/bots/" Core.<> (Core.toText botName) Core.<> ("/aliases/")
                Core.<> (Core.toText name)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBotAliasResponse'
            Core.<$> (x Core..:? "botName")
            Core.<*> (x Core..:? "botVersion")
            Core.<*> (x Core..:? "checksum")
            Core.<*> (x Core..:? "conversationLogs")
            Core.<*> (x Core..:? "createdDate")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "lastUpdatedDate")
            Core.<*> (x Core..:? "name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetBotAliasResponse' smart constructor.
data GetBotAliasResponse = GetBotAliasResponse'
  { -- | The name of the bot that the alias points to.
    botName :: Core.Maybe Types.BotName,
    -- | The version of the bot that the alias points to.
    botVersion :: Core.Maybe Types.Version,
    -- | Checksum of the bot alias.
    checksum :: Core.Maybe Types.String,
    -- | The settings that determine how Amazon Lex uses conversation logs for the alias.
    conversationLogs :: Core.Maybe Types.ConversationLogsResponse,
    -- | The date that the bot alias was created.
    createdDate :: Core.Maybe Core.NominalDiffTime,
    -- | A description of the bot alias.
    description :: Core.Maybe Types.Description,
    -- | The date that the bot alias was updated. When you create a resource, the creation date and the last updated date are the same.
    lastUpdatedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the bot alias.
    name :: Core.Maybe Types.AliasName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetBotAliasResponse' value with any optional fields omitted.
mkGetBotAliasResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetBotAliasResponse
mkGetBotAliasResponse responseStatus =
  GetBotAliasResponse'
    { botName = Core.Nothing,
      botVersion = Core.Nothing,
      checksum = Core.Nothing,
      conversationLogs = Core.Nothing,
      createdDate = Core.Nothing,
      description = Core.Nothing,
      lastUpdatedDate = Core.Nothing,
      name = Core.Nothing,
      responseStatus
    }

-- | The name of the bot that the alias points to.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarfrsBotName :: Lens.Lens' GetBotAliasResponse (Core.Maybe Types.BotName)
gbarfrsBotName = Lens.field @"botName"
{-# DEPRECATED gbarfrsBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | The version of the bot that the alias points to.
--
-- /Note:/ Consider using 'botVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarfrsBotVersion :: Lens.Lens' GetBotAliasResponse (Core.Maybe Types.Version)
gbarfrsBotVersion = Lens.field @"botVersion"
{-# DEPRECATED gbarfrsBotVersion "Use generic-lens or generic-optics with 'botVersion' instead." #-}

-- | Checksum of the bot alias.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarfrsChecksum :: Lens.Lens' GetBotAliasResponse (Core.Maybe Types.String)
gbarfrsChecksum = Lens.field @"checksum"
{-# DEPRECATED gbarfrsChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | The settings that determine how Amazon Lex uses conversation logs for the alias.
--
-- /Note:/ Consider using 'conversationLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarfrsConversationLogs :: Lens.Lens' GetBotAliasResponse (Core.Maybe Types.ConversationLogsResponse)
gbarfrsConversationLogs = Lens.field @"conversationLogs"
{-# DEPRECATED gbarfrsConversationLogs "Use generic-lens or generic-optics with 'conversationLogs' instead." #-}

-- | The date that the bot alias was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarfrsCreatedDate :: Lens.Lens' GetBotAliasResponse (Core.Maybe Core.NominalDiffTime)
gbarfrsCreatedDate = Lens.field @"createdDate"
{-# DEPRECATED gbarfrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | A description of the bot alias.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarfrsDescription :: Lens.Lens' GetBotAliasResponse (Core.Maybe Types.Description)
gbarfrsDescription = Lens.field @"description"
{-# DEPRECATED gbarfrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The date that the bot alias was updated. When you create a resource, the creation date and the last updated date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarfrsLastUpdatedDate :: Lens.Lens' GetBotAliasResponse (Core.Maybe Core.NominalDiffTime)
gbarfrsLastUpdatedDate = Lens.field @"lastUpdatedDate"
{-# DEPRECATED gbarfrsLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | The name of the bot alias.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarfrsName :: Lens.Lens' GetBotAliasResponse (Core.Maybe Types.AliasName)
gbarfrsName = Lens.field @"name"
{-# DEPRECATED gbarfrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarfrsResponseStatus :: Lens.Lens' GetBotAliasResponse Core.Int
gbarfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gbarfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
