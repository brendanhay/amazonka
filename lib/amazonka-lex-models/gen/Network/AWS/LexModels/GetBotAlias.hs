{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetBotAlias (..)
    , mkGetBotAlias
    -- ** Request lenses
    , gbasName
    , gbasBotName

    -- * Destructuring the response
    , GetBotAliasResponse (..)
    , mkGetBotAliasResponse
    -- ** Response lenses
    , gbarfrsBotName
    , gbarfrsBotVersion
    , gbarfrsChecksum
    , gbarfrsConversationLogs
    , gbarfrsCreatedDate
    , gbarfrsDescription
    , gbarfrsLastUpdatedDate
    , gbarfrsName
    , gbarfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetBotAlias' smart constructor.
data GetBotAlias = GetBotAlias'
  { name :: Types.AliasName
    -- ^ The name of the bot alias. The name is case sensitive.
  , botName :: Types.BotName
    -- ^ The name of the bot.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBotAlias' value with any optional fields omitted.
mkGetBotAlias
    :: Types.AliasName -- ^ 'name'
    -> Types.BotName -- ^ 'botName'
    -> GetBotAlias
mkGetBotAlias name botName = GetBotAlias'{name, botName}

-- | The name of the bot alias. The name is case sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbasName :: Lens.Lens' GetBotAlias Types.AliasName
gbasName = Lens.field @"name"
{-# INLINEABLE gbasName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The name of the bot.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbasBotName :: Lens.Lens' GetBotAlias Types.BotName
gbasBotName = Lens.field @"botName"
{-# INLINEABLE gbasBotName #-}
{-# DEPRECATED botName "Use generic-lens or generic-optics with 'botName' instead"  #-}

instance Core.ToQuery GetBotAlias where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetBotAlias where
        toHeaders GetBotAlias{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetBotAlias where
        type Rs GetBotAlias = GetBotAliasResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/bots/" Core.<> Core.toText botName Core.<> "/aliases/" Core.<>
                             Core.toText name,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetBotAliasResponse' Core.<$>
                   (x Core..:? "botName") Core.<*> x Core..:? "botVersion" Core.<*>
                     x Core..:? "checksum"
                     Core.<*> x Core..:? "conversationLogs"
                     Core.<*> x Core..:? "createdDate"
                     Core.<*> x Core..:? "description"
                     Core.<*> x Core..:? "lastUpdatedDate"
                     Core.<*> x Core..:? "name"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetBotAliasResponse' smart constructor.
data GetBotAliasResponse = GetBotAliasResponse'
  { botName :: Core.Maybe Types.BotName
    -- ^ The name of the bot that the alias points to.
  , botVersion :: Core.Maybe Types.Version
    -- ^ The version of the bot that the alias points to.
  , checksum :: Core.Maybe Core.Text
    -- ^ Checksum of the bot alias.
  , conversationLogs :: Core.Maybe Types.ConversationLogsResponse
    -- ^ The settings that determine how Amazon Lex uses conversation logs for the alias.
  , createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the bot alias was created.
  , description :: Core.Maybe Types.Description
    -- ^ A description of the bot alias.
  , lastUpdatedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the bot alias was updated. When you create a resource, the creation date and the last updated date are the same.
  , name :: Core.Maybe Types.AliasName
    -- ^ The name of the bot alias.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetBotAliasResponse' value with any optional fields omitted.
mkGetBotAliasResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetBotAliasResponse
mkGetBotAliasResponse responseStatus
  = GetBotAliasResponse'{botName = Core.Nothing,
                         botVersion = Core.Nothing, checksum = Core.Nothing,
                         conversationLogs = Core.Nothing, createdDate = Core.Nothing,
                         description = Core.Nothing, lastUpdatedDate = Core.Nothing,
                         name = Core.Nothing, responseStatus}

-- | The name of the bot that the alias points to.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarfrsBotName :: Lens.Lens' GetBotAliasResponse (Core.Maybe Types.BotName)
gbarfrsBotName = Lens.field @"botName"
{-# INLINEABLE gbarfrsBotName #-}
{-# DEPRECATED botName "Use generic-lens or generic-optics with 'botName' instead"  #-}

-- | The version of the bot that the alias points to.
--
-- /Note:/ Consider using 'botVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarfrsBotVersion :: Lens.Lens' GetBotAliasResponse (Core.Maybe Types.Version)
gbarfrsBotVersion = Lens.field @"botVersion"
{-# INLINEABLE gbarfrsBotVersion #-}
{-# DEPRECATED botVersion "Use generic-lens or generic-optics with 'botVersion' instead"  #-}

-- | Checksum of the bot alias.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarfrsChecksum :: Lens.Lens' GetBotAliasResponse (Core.Maybe Core.Text)
gbarfrsChecksum = Lens.field @"checksum"
{-# INLINEABLE gbarfrsChecksum #-}
{-# DEPRECATED checksum "Use generic-lens or generic-optics with 'checksum' instead"  #-}

-- | The settings that determine how Amazon Lex uses conversation logs for the alias.
--
-- /Note:/ Consider using 'conversationLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarfrsConversationLogs :: Lens.Lens' GetBotAliasResponse (Core.Maybe Types.ConversationLogsResponse)
gbarfrsConversationLogs = Lens.field @"conversationLogs"
{-# INLINEABLE gbarfrsConversationLogs #-}
{-# DEPRECATED conversationLogs "Use generic-lens or generic-optics with 'conversationLogs' instead"  #-}

-- | The date that the bot alias was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarfrsCreatedDate :: Lens.Lens' GetBotAliasResponse (Core.Maybe Core.NominalDiffTime)
gbarfrsCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE gbarfrsCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | A description of the bot alias.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarfrsDescription :: Lens.Lens' GetBotAliasResponse (Core.Maybe Types.Description)
gbarfrsDescription = Lens.field @"description"
{-# INLINEABLE gbarfrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The date that the bot alias was updated. When you create a resource, the creation date and the last updated date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarfrsLastUpdatedDate :: Lens.Lens' GetBotAliasResponse (Core.Maybe Core.NominalDiffTime)
gbarfrsLastUpdatedDate = Lens.field @"lastUpdatedDate"
{-# INLINEABLE gbarfrsLastUpdatedDate #-}
{-# DEPRECATED lastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead"  #-}

-- | The name of the bot alias.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarfrsName :: Lens.Lens' GetBotAliasResponse (Core.Maybe Types.AliasName)
gbarfrsName = Lens.field @"name"
{-# INLINEABLE gbarfrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarfrsResponseStatus :: Lens.Lens' GetBotAliasResponse Core.Int
gbarfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gbarfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
