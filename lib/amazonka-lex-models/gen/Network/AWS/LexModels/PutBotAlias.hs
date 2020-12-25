{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.PutBotAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an alias for the specified version of the bot or replaces an alias for the specified bot. To change the version of the bot that the alias points to, replace the alias. For more information about aliases, see 'versioning-aliases' .
--
-- This operation requires permissions for the @lex:PutBotAlias@ action.
module Network.AWS.LexModels.PutBotAlias
  ( -- * Creating a request
    PutBotAlias (..),
    mkPutBotAlias,

    -- ** Request lenses
    pbaName,
    pbaBotVersion,
    pbaBotName,
    pbaChecksum,
    pbaConversationLogs,
    pbaDescription,
    pbaTags,

    -- * Destructuring the response
    PutBotAliasResponse (..),
    mkPutBotAliasResponse,

    -- ** Response lenses
    pbarrsBotName,
    pbarrsBotVersion,
    pbarrsChecksum,
    pbarrsConversationLogs,
    pbarrsCreatedDate,
    pbarrsDescription,
    pbarrsLastUpdatedDate,
    pbarrsName,
    pbarrsTags,
    pbarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutBotAlias' smart constructor.
data PutBotAlias = PutBotAlias'
  { -- | The name of the alias. The name is /not/ case sensitive.
    name :: Types.AliasName,
    -- | The version of the bot.
    botVersion :: Types.BotVersion,
    -- | The name of the bot.
    botName :: Types.BotName,
    -- | Identifies a specific revision of the @> LATEST@ version.
    --
    -- When you create a new bot alias, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception.
    -- When you want to update a bot alias, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
    checksum :: Core.Maybe Types.Checksum,
    -- | Settings for conversation logs for the alias.
    conversationLogs :: Core.Maybe Types.ConversationLogsRequest,
    -- | A description of the alias.
    description :: Core.Maybe Types.Description,
    -- | A list of tags to add to the bot alias. You can only add tags when you create an alias, you can't use the @PutBotAlias@ operation to update the tags on a bot alias. To update tags, use the @TagResource@ operation.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBotAlias' value with any optional fields omitted.
mkPutBotAlias ::
  -- | 'name'
  Types.AliasName ->
  -- | 'botVersion'
  Types.BotVersion ->
  -- | 'botName'
  Types.BotName ->
  PutBotAlias
mkPutBotAlias name botVersion botName =
  PutBotAlias'
    { name,
      botVersion,
      botName,
      checksum = Core.Nothing,
      conversationLogs = Core.Nothing,
      description = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the alias. The name is /not/ case sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaName :: Lens.Lens' PutBotAlias Types.AliasName
pbaName = Lens.field @"name"
{-# DEPRECATED pbaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the bot.
--
-- /Note:/ Consider using 'botVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaBotVersion :: Lens.Lens' PutBotAlias Types.BotVersion
pbaBotVersion = Lens.field @"botVersion"
{-# DEPRECATED pbaBotVersion "Use generic-lens or generic-optics with 'botVersion' instead." #-}

-- | The name of the bot.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaBotName :: Lens.Lens' PutBotAlias Types.BotName
pbaBotName = Lens.field @"botName"
{-# DEPRECATED pbaBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | Identifies a specific revision of the @> LATEST@ version.
--
-- When you create a new bot alias, leave the @checksum@ field blank. If you specify a checksum you get a @BadRequestException@ exception.
-- When you want to update a bot alias, set the @checksum@ field to the checksum of the most recent revision of the @> LATEST@ version. If you don't specify the @checksum@ field, or if the checksum does not match the @> LATEST@ version, you get a @PreconditionFailedException@ exception.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaChecksum :: Lens.Lens' PutBotAlias (Core.Maybe Types.Checksum)
pbaChecksum = Lens.field @"checksum"
{-# DEPRECATED pbaChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | Settings for conversation logs for the alias.
--
-- /Note:/ Consider using 'conversationLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaConversationLogs :: Lens.Lens' PutBotAlias (Core.Maybe Types.ConversationLogsRequest)
pbaConversationLogs = Lens.field @"conversationLogs"
{-# DEPRECATED pbaConversationLogs "Use generic-lens or generic-optics with 'conversationLogs' instead." #-}

-- | A description of the alias.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaDescription :: Lens.Lens' PutBotAlias (Core.Maybe Types.Description)
pbaDescription = Lens.field @"description"
{-# DEPRECATED pbaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of tags to add to the bot alias. You can only add tags when you create an alias, you can't use the @PutBotAlias@ operation to update the tags on a bot alias. To update tags, use the @TagResource@ operation.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaTags :: Lens.Lens' PutBotAlias (Core.Maybe [Types.Tag])
pbaTags = Lens.field @"tags"
{-# DEPRECATED pbaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON PutBotAlias where
  toJSON PutBotAlias {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("botVersion" Core..= botVersion),
            ("checksum" Core..=) Core.<$> checksum,
            ("conversationLogs" Core..=) Core.<$> conversationLogs,
            ("description" Core..=) Core.<$> description,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest PutBotAlias where
  type Rs PutBotAlias = PutBotAliasResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/bots/" Core.<> (Core.toText botName) Core.<> ("/aliases/")
                Core.<> (Core.toText name)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutBotAliasResponse'
            Core.<$> (x Core..:? "botName")
            Core.<*> (x Core..:? "botVersion")
            Core.<*> (x Core..:? "checksum")
            Core.<*> (x Core..:? "conversationLogs")
            Core.<*> (x Core..:? "createdDate")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "lastUpdatedDate")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutBotAliasResponse' smart constructor.
data PutBotAliasResponse = PutBotAliasResponse'
  { -- | The name of the bot that the alias points to.
    botName :: Core.Maybe Types.BotName,
    -- | The version of the bot that the alias points to.
    botVersion :: Core.Maybe Types.Version,
    -- | The checksum for the current version of the alias.
    checksum :: Core.Maybe Types.String,
    -- | The settings that determine how Amazon Lex uses conversation logs for the alias.
    conversationLogs :: Core.Maybe Types.ConversationLogsResponse,
    -- | The date that the bot alias was created.
    createdDate :: Core.Maybe Core.NominalDiffTime,
    -- | A description of the alias.
    description :: Core.Maybe Types.Description,
    -- | The date that the bot alias was updated. When you create a resource, the creation date and the last updated date are the same.
    lastUpdatedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the alias.
    name :: Core.Maybe Types.AliasName,
    -- | A list of tags associated with a bot.
    tags :: Core.Maybe [Types.Tag],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PutBotAliasResponse' value with any optional fields omitted.
mkPutBotAliasResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutBotAliasResponse
mkPutBotAliasResponse responseStatus =
  PutBotAliasResponse'
    { botName = Core.Nothing,
      botVersion = Core.Nothing,
      checksum = Core.Nothing,
      conversationLogs = Core.Nothing,
      createdDate = Core.Nothing,
      description = Core.Nothing,
      lastUpdatedDate = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | The name of the bot that the alias points to.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbarrsBotName :: Lens.Lens' PutBotAliasResponse (Core.Maybe Types.BotName)
pbarrsBotName = Lens.field @"botName"
{-# DEPRECATED pbarrsBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | The version of the bot that the alias points to.
--
-- /Note:/ Consider using 'botVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbarrsBotVersion :: Lens.Lens' PutBotAliasResponse (Core.Maybe Types.Version)
pbarrsBotVersion = Lens.field @"botVersion"
{-# DEPRECATED pbarrsBotVersion "Use generic-lens or generic-optics with 'botVersion' instead." #-}

-- | The checksum for the current version of the alias.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbarrsChecksum :: Lens.Lens' PutBotAliasResponse (Core.Maybe Types.String)
pbarrsChecksum = Lens.field @"checksum"
{-# DEPRECATED pbarrsChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | The settings that determine how Amazon Lex uses conversation logs for the alias.
--
-- /Note:/ Consider using 'conversationLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbarrsConversationLogs :: Lens.Lens' PutBotAliasResponse (Core.Maybe Types.ConversationLogsResponse)
pbarrsConversationLogs = Lens.field @"conversationLogs"
{-# DEPRECATED pbarrsConversationLogs "Use generic-lens or generic-optics with 'conversationLogs' instead." #-}

-- | The date that the bot alias was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbarrsCreatedDate :: Lens.Lens' PutBotAliasResponse (Core.Maybe Core.NominalDiffTime)
pbarrsCreatedDate = Lens.field @"createdDate"
{-# DEPRECATED pbarrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | A description of the alias.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbarrsDescription :: Lens.Lens' PutBotAliasResponse (Core.Maybe Types.Description)
pbarrsDescription = Lens.field @"description"
{-# DEPRECATED pbarrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The date that the bot alias was updated. When you create a resource, the creation date and the last updated date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbarrsLastUpdatedDate :: Lens.Lens' PutBotAliasResponse (Core.Maybe Core.NominalDiffTime)
pbarrsLastUpdatedDate = Lens.field @"lastUpdatedDate"
{-# DEPRECATED pbarrsLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | The name of the alias.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbarrsName :: Lens.Lens' PutBotAliasResponse (Core.Maybe Types.AliasName)
pbarrsName = Lens.field @"name"
{-# DEPRECATED pbarrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of tags associated with a bot.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbarrsTags :: Lens.Lens' PutBotAliasResponse (Core.Maybe [Types.Tag])
pbarrsTags = Lens.field @"tags"
{-# DEPRECATED pbarrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbarrsResponseStatus :: Lens.Lens' PutBotAliasResponse Core.Int
pbarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pbarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
