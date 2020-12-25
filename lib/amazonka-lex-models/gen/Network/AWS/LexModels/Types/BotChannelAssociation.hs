{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.BotChannelAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.BotChannelAssociation
  ( BotChannelAssociation (..),

    -- * Smart constructor
    mkBotChannelAssociation,

    -- * Lenses
    bcaBotAlias,
    bcaBotConfiguration,
    bcaBotName,
    bcaCreatedDate,
    bcaDescription,
    bcaFailureReason,
    bcaName,
    bcaStatus,
    bcaType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.BotAlias as Types
import qualified Network.AWS.LexModels.Types.BotChannelName as Types
import qualified Network.AWS.LexModels.Types.BotName as Types
import qualified Network.AWS.LexModels.Types.ChannelStatus as Types
import qualified Network.AWS.LexModels.Types.ChannelType as Types
import qualified Network.AWS.LexModels.Types.Description as Types
import qualified Network.AWS.LexModels.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Represents an association between an Amazon Lex bot and an external messaging platform.
--
-- /See:/ 'mkBotChannelAssociation' smart constructor.
data BotChannelAssociation = BotChannelAssociation'
  { -- | An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
    botAlias :: Core.Maybe Types.BotAlias,
    -- | Provides information necessary to communicate with the messaging platform.
    botConfiguration :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | The name of the Amazon Lex bot to which this association is being made.
    botName :: Core.Maybe Types.BotName,
    -- | The date that the association between the Amazon Lex bot and the channel was created.
    createdDate :: Core.Maybe Core.NominalDiffTime,
    -- | A text description of the association you are creating.
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
    -- | Specifies the type of association by indicating the type of channel being established between the Amazon Lex bot and the external messaging platform.
    type' :: Core.Maybe Types.ChannelType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BotChannelAssociation' value with any optional fields omitted.
mkBotChannelAssociation ::
  BotChannelAssociation
mkBotChannelAssociation =
  BotChannelAssociation'
    { botAlias = Core.Nothing,
      botConfiguration = Core.Nothing,
      botName = Core.Nothing,
      createdDate = Core.Nothing,
      description = Core.Nothing,
      failureReason = Core.Nothing,
      name = Core.Nothing,
      status = Core.Nothing,
      type' = Core.Nothing
    }

-- | An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
--
-- /Note:/ Consider using 'botAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcaBotAlias :: Lens.Lens' BotChannelAssociation (Core.Maybe Types.BotAlias)
bcaBotAlias = Lens.field @"botAlias"
{-# DEPRECATED bcaBotAlias "Use generic-lens or generic-optics with 'botAlias' instead." #-}

-- | Provides information necessary to communicate with the messaging platform.
--
-- /Note:/ Consider using 'botConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcaBotConfiguration :: Lens.Lens' BotChannelAssociation (Core.Maybe (Core.HashMap Types.String Types.String))
bcaBotConfiguration = Lens.field @"botConfiguration"
{-# DEPRECATED bcaBotConfiguration "Use generic-lens or generic-optics with 'botConfiguration' instead." #-}

-- | The name of the Amazon Lex bot to which this association is being made.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcaBotName :: Lens.Lens' BotChannelAssociation (Core.Maybe Types.BotName)
bcaBotName = Lens.field @"botName"
{-# DEPRECATED bcaBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | The date that the association between the Amazon Lex bot and the channel was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcaCreatedDate :: Lens.Lens' BotChannelAssociation (Core.Maybe Core.NominalDiffTime)
bcaCreatedDate = Lens.field @"createdDate"
{-# DEPRECATED bcaCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | A text description of the association you are creating.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcaDescription :: Lens.Lens' BotChannelAssociation (Core.Maybe Types.Description)
bcaDescription = Lens.field @"description"
{-# DEPRECATED bcaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to create the association.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcaFailureReason :: Lens.Lens' BotChannelAssociation (Core.Maybe Types.String)
bcaFailureReason = Lens.field @"failureReason"
{-# DEPRECATED bcaFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The name of the association between the bot and the channel.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcaName :: Lens.Lens' BotChannelAssociation (Core.Maybe Types.BotChannelName)
bcaName = Lens.field @"name"
{-# DEPRECATED bcaName "Use generic-lens or generic-optics with 'name' instead." #-}

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
bcaStatus :: Lens.Lens' BotChannelAssociation (Core.Maybe Types.ChannelStatus)
bcaStatus = Lens.field @"status"
{-# DEPRECATED bcaStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies the type of association by indicating the type of channel being established between the Amazon Lex bot and the external messaging platform.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcaType :: Lens.Lens' BotChannelAssociation (Core.Maybe Types.ChannelType)
bcaType = Lens.field @"type'"
{-# DEPRECATED bcaType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON BotChannelAssociation where
  parseJSON =
    Core.withObject "BotChannelAssociation" Core.$
      \x ->
        BotChannelAssociation'
          Core.<$> (x Core..:? "botAlias")
          Core.<*> (x Core..:? "botConfiguration")
          Core.<*> (x Core..:? "botName")
          Core.<*> (x Core..:? "createdDate")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "failureReason")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "type")
