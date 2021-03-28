{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.BotChannelAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.BotChannelAssociation
  ( BotChannelAssociation (..)
  -- * Smart constructor
  , mkBotChannelAssociation
  -- * Lenses
  , bcaBotAlias
  , bcaBotConfiguration
  , bcaBotName
  , bcaCreatedDate
  , bcaDescription
  , bcaFailureReason
  , bcaName
  , bcaStatus
  , bcaType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.BotAlias as Types
import qualified Network.AWS.LexModels.Types.BotChannelName as Types
import qualified Network.AWS.LexModels.Types.BotName as Types
import qualified Network.AWS.LexModels.Types.ChannelStatus as Types
import qualified Network.AWS.LexModels.Types.ChannelType as Types
import qualified Network.AWS.LexModels.Types.Description as Types
import qualified Network.AWS.Prelude as Core

-- | Represents an association between an Amazon Lex bot and an external messaging platform.
--
-- /See:/ 'mkBotChannelAssociation' smart constructor.
data BotChannelAssociation = BotChannelAssociation'
  { botAlias :: Core.Maybe Types.BotAlias
    -- ^ An alias pointing to the specific version of the Amazon Lex bot to which this association is being made. 
  , botConfiguration :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Provides information necessary to communicate with the messaging platform. 
  , botName :: Core.Maybe Types.BotName
    -- ^ The name of the Amazon Lex bot to which this association is being made. 
  , createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the association between the Amazon Lex bot and the channel was created. 
  , description :: Core.Maybe Types.Description
    -- ^ A text description of the association you are creating. 
  , failureReason :: Core.Maybe Core.Text
    -- ^ If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to create the association.
  , name :: Core.Maybe Types.BotChannelName
    -- ^ The name of the association between the bot and the channel. 
  , status :: Core.Maybe Types.ChannelStatus
    -- ^ The status of the bot channel. 
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
  , type' :: Core.Maybe Types.ChannelType
    -- ^ Specifies the type of association by indicating the type of channel being established between the Amazon Lex bot and the external messaging platform.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BotChannelAssociation' value with any optional fields omitted.
mkBotChannelAssociation
    :: BotChannelAssociation
mkBotChannelAssociation
  = BotChannelAssociation'{botAlias = Core.Nothing,
                           botConfiguration = Core.Nothing, botName = Core.Nothing,
                           createdDate = Core.Nothing, description = Core.Nothing,
                           failureReason = Core.Nothing, name = Core.Nothing,
                           status = Core.Nothing, type' = Core.Nothing}

-- | An alias pointing to the specific version of the Amazon Lex bot to which this association is being made. 
--
-- /Note:/ Consider using 'botAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcaBotAlias :: Lens.Lens' BotChannelAssociation (Core.Maybe Types.BotAlias)
bcaBotAlias = Lens.field @"botAlias"
{-# INLINEABLE bcaBotAlias #-}
{-# DEPRECATED botAlias "Use generic-lens or generic-optics with 'botAlias' instead"  #-}

-- | Provides information necessary to communicate with the messaging platform. 
--
-- /Note:/ Consider using 'botConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcaBotConfiguration :: Lens.Lens' BotChannelAssociation (Core.Maybe (Core.HashMap Core.Text Core.Text))
bcaBotConfiguration = Lens.field @"botConfiguration"
{-# INLINEABLE bcaBotConfiguration #-}
{-# DEPRECATED botConfiguration "Use generic-lens or generic-optics with 'botConfiguration' instead"  #-}

-- | The name of the Amazon Lex bot to which this association is being made. 
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcaBotName :: Lens.Lens' BotChannelAssociation (Core.Maybe Types.BotName)
bcaBotName = Lens.field @"botName"
{-# INLINEABLE bcaBotName #-}
{-# DEPRECATED botName "Use generic-lens or generic-optics with 'botName' instead"  #-}

-- | The date that the association between the Amazon Lex bot and the channel was created. 
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcaCreatedDate :: Lens.Lens' BotChannelAssociation (Core.Maybe Core.NominalDiffTime)
bcaCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE bcaCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | A text description of the association you are creating. 
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcaDescription :: Lens.Lens' BotChannelAssociation (Core.Maybe Types.Description)
bcaDescription = Lens.field @"description"
{-# INLINEABLE bcaDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to create the association.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcaFailureReason :: Lens.Lens' BotChannelAssociation (Core.Maybe Core.Text)
bcaFailureReason = Lens.field @"failureReason"
{-# INLINEABLE bcaFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | The name of the association between the bot and the channel. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcaName :: Lens.Lens' BotChannelAssociation (Core.Maybe Types.BotChannelName)
bcaName = Lens.field @"name"
{-# INLINEABLE bcaName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

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
{-# INLINEABLE bcaStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Specifies the type of association by indicating the type of channel being established between the Amazon Lex bot and the external messaging platform.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcaType :: Lens.Lens' BotChannelAssociation (Core.Maybe Types.ChannelType)
bcaType = Lens.field @"type'"
{-# INLINEABLE bcaType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON BotChannelAssociation where
        parseJSON
          = Core.withObject "BotChannelAssociation" Core.$
              \ x ->
                BotChannelAssociation' Core.<$>
                  (x Core..:? "botAlias") Core.<*> x Core..:? "botConfiguration"
                    Core.<*> x Core..:? "botName"
                    Core.<*> x Core..:? "createdDate"
                    Core.<*> x Core..:? "description"
                    Core.<*> x Core..:? "failureReason"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "type"
