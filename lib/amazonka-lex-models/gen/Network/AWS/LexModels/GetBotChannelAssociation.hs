{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetBotChannelAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the association between an Amazon Lex bot and a messaging platform.
--
-- This operation requires permissions for the @lex:GetBotChannelAssociation@ action.
module Network.AWS.LexModels.GetBotChannelAssociation
  ( -- * Creating a request
    GetBotChannelAssociation (..),
    mkGetBotChannelAssociation,

    -- ** Request lenses
    gName,
    gBotName,
    gBotAlias,

    -- * Destructuring the response
    GetBotChannelAssociationResponse (..),
    mkGetBotChannelAssociationResponse,

    -- ** Response lenses
    gbcarrsBotAlias,
    gbcarrsBotConfiguration,
    gbcarrsBotName,
    gbcarrsCreatedDate,
    gbcarrsDescription,
    gbcarrsFailureReason,
    gbcarrsName,
    gbcarrsStatus,
    gbcarrsType,
    gbcarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetBotChannelAssociation' smart constructor.
data GetBotChannelAssociation = GetBotChannelAssociation'
  { -- | The name of the association between the bot and the channel. The name is case sensitive.
    name :: Types.BotChannelName,
    -- | The name of the Amazon Lex bot.
    botName :: Types.BotName,
    -- | An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
    botAlias :: Types.AliasName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBotChannelAssociation' value with any optional fields omitted.
mkGetBotChannelAssociation ::
  -- | 'name'
  Types.BotChannelName ->
  -- | 'botName'
  Types.BotName ->
  -- | 'botAlias'
  Types.AliasName ->
  GetBotChannelAssociation
mkGetBotChannelAssociation name botName botAlias =
  GetBotChannelAssociation' {name, botName, botAlias}

-- | The name of the association between the bot and the channel. The name is case sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gName :: Lens.Lens' GetBotChannelAssociation Types.BotChannelName
gName = Lens.field @"name"
{-# DEPRECATED gName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name of the Amazon Lex bot.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gBotName :: Lens.Lens' GetBotChannelAssociation Types.BotName
gBotName = Lens.field @"botName"
{-# DEPRECATED gBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
--
-- /Note:/ Consider using 'botAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gBotAlias :: Lens.Lens' GetBotChannelAssociation Types.AliasName
gBotAlias = Lens.field @"botAlias"
{-# DEPRECATED gBotAlias "Use generic-lens or generic-optics with 'botAlias' instead." #-}

instance Core.AWSRequest GetBotChannelAssociation where
  type Rs GetBotChannelAssociation = GetBotChannelAssociationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/bots/" Core.<> (Core.toText botName) Core.<> ("/aliases/")
                Core.<> (Core.toText botAlias)
                Core.<> ("/channels/")
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
          GetBotChannelAssociationResponse'
            Core.<$> (x Core..:? "botAlias")
            Core.<*> (x Core..:? "botConfiguration")
            Core.<*> (x Core..:? "botName")
            Core.<*> (x Core..:? "createdDate")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "failureReason")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "type")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetBotChannelAssociationResponse' smart constructor.
data GetBotChannelAssociationResponse = GetBotChannelAssociationResponse'
  { -- | An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
    botAlias :: Core.Maybe Types.BotAlias,
    -- | Provides information that the messaging platform needs to communicate with the Amazon Lex bot.
    botConfiguration :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | The name of the Amazon Lex bot.
    botName :: Core.Maybe Types.BotName,
    -- | The date that the association between the bot and the channel was created.
    createdDate :: Core.Maybe Core.NominalDiffTime,
    -- | A description of the association between the bot and the channel.
    description :: Core.Maybe Types.Description,
    -- | If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to create the association.
    failureReason :: Core.Maybe Types.String,
    -- | The name of the association between the bot and the channel.
    name :: Core.Maybe Types.BotChannelName,
    -- | The status of the bot channel.
    --
    --
    --     * @CREATED@ - The channel has been created and is ready for use.
    --
    --
    --     * @IN_PROGRESS@ - Channel creation is in progress.
    --
    --
    --     * @FAILED@ - There was an error creating the channel. For information about the reason for the failure, see the @failureReason@ field.
    status :: Core.Maybe Types.ChannelStatus,
    -- | The type of the messaging platform.
    type' :: Core.Maybe Types.ChannelType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetBotChannelAssociationResponse' value with any optional fields omitted.
mkGetBotChannelAssociationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetBotChannelAssociationResponse
mkGetBotChannelAssociationResponse responseStatus =
  GetBotChannelAssociationResponse'
    { botAlias = Core.Nothing,
      botConfiguration = Core.Nothing,
      botName = Core.Nothing,
      createdDate = Core.Nothing,
      description = Core.Nothing,
      failureReason = Core.Nothing,
      name = Core.Nothing,
      status = Core.Nothing,
      type' = Core.Nothing,
      responseStatus
    }

-- | An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
--
-- /Note:/ Consider using 'botAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarrsBotAlias :: Lens.Lens' GetBotChannelAssociationResponse (Core.Maybe Types.BotAlias)
gbcarrsBotAlias = Lens.field @"botAlias"
{-# DEPRECATED gbcarrsBotAlias "Use generic-lens or generic-optics with 'botAlias' instead." #-}

-- | Provides information that the messaging platform needs to communicate with the Amazon Lex bot.
--
-- /Note:/ Consider using 'botConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarrsBotConfiguration :: Lens.Lens' GetBotChannelAssociationResponse (Core.Maybe (Core.HashMap Types.String Types.String))
gbcarrsBotConfiguration = Lens.field @"botConfiguration"
{-# DEPRECATED gbcarrsBotConfiguration "Use generic-lens or generic-optics with 'botConfiguration' instead." #-}

-- | The name of the Amazon Lex bot.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarrsBotName :: Lens.Lens' GetBotChannelAssociationResponse (Core.Maybe Types.BotName)
gbcarrsBotName = Lens.field @"botName"
{-# DEPRECATED gbcarrsBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | The date that the association between the bot and the channel was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarrsCreatedDate :: Lens.Lens' GetBotChannelAssociationResponse (Core.Maybe Core.NominalDiffTime)
gbcarrsCreatedDate = Lens.field @"createdDate"
{-# DEPRECATED gbcarrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | A description of the association between the bot and the channel.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarrsDescription :: Lens.Lens' GetBotChannelAssociationResponse (Core.Maybe Types.Description)
gbcarrsDescription = Lens.field @"description"
{-# DEPRECATED gbcarrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to create the association.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarrsFailureReason :: Lens.Lens' GetBotChannelAssociationResponse (Core.Maybe Types.String)
gbcarrsFailureReason = Lens.field @"failureReason"
{-# DEPRECATED gbcarrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The name of the association between the bot and the channel.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarrsName :: Lens.Lens' GetBotChannelAssociationResponse (Core.Maybe Types.BotChannelName)
gbcarrsName = Lens.field @"name"
{-# DEPRECATED gbcarrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The status of the bot channel.
--
--
--     * @CREATED@ - The channel has been created and is ready for use.
--
--
--     * @IN_PROGRESS@ - Channel creation is in progress.
--
--
--     * @FAILED@ - There was an error creating the channel. For information about the reason for the failure, see the @failureReason@ field.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarrsStatus :: Lens.Lens' GetBotChannelAssociationResponse (Core.Maybe Types.ChannelStatus)
gbcarrsStatus = Lens.field @"status"
{-# DEPRECATED gbcarrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The type of the messaging platform.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarrsType :: Lens.Lens' GetBotChannelAssociationResponse (Core.Maybe Types.ChannelType)
gbcarrsType = Lens.field @"type'"
{-# DEPRECATED gbcarrsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcarrsResponseStatus :: Lens.Lens' GetBotChannelAssociationResponse Core.Int
gbcarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gbcarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
