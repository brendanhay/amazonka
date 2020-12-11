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
    bcaFailureReason,
    bcaStatus,
    bcaBotAlias,
    bcaBotName,
    bcaBotConfiguration,
    bcaCreatedDate,
    bcaName,
    bcaType,
    bcaDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.ChannelStatus
import Network.AWS.LexModels.Types.ChannelType
import qualified Network.AWS.Prelude as Lude

-- | Represents an association between an Amazon Lex bot and an external messaging platform.
--
-- /See:/ 'mkBotChannelAssociation' smart constructor.
data BotChannelAssociation = BotChannelAssociation'
  { failureReason ::
      Lude.Maybe Lude.Text,
    status :: Lude.Maybe ChannelStatus,
    botAlias :: Lude.Maybe Lude.Text,
    botName :: Lude.Maybe Lude.Text,
    botConfiguration ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    createdDate :: Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe ChannelType,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BotChannelAssociation' with the minimum fields required to make a request.
--
-- * 'botAlias' - An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
-- * 'botConfiguration' - Provides information necessary to communicate with the messaging platform.
-- * 'botName' - The name of the Amazon Lex bot to which this association is being made.
-- * 'createdDate' - The date that the association between the Amazon Lex bot and the channel was created.
-- * 'description' - A text description of the association you are creating.
-- * 'failureReason' - If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to create the association.
-- * 'name' - The name of the association between the bot and the channel.
-- * 'status' - The status of the bot channel.
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
-- * 'type'' - Specifies the type of association by indicating the type of channel being established between the Amazon Lex bot and the external messaging platform.
mkBotChannelAssociation ::
  BotChannelAssociation
mkBotChannelAssociation =
  BotChannelAssociation'
    { failureReason = Lude.Nothing,
      status = Lude.Nothing,
      botAlias = Lude.Nothing,
      botName = Lude.Nothing,
      botConfiguration = Lude.Nothing,
      createdDate = Lude.Nothing,
      name = Lude.Nothing,
      type' = Lude.Nothing,
      description = Lude.Nothing
    }

-- | If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to create the association.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcaFailureReason :: Lens.Lens' BotChannelAssociation (Lude.Maybe Lude.Text)
bcaFailureReason = Lens.lens (failureReason :: BotChannelAssociation -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: BotChannelAssociation)
{-# DEPRECATED bcaFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

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
bcaStatus :: Lens.Lens' BotChannelAssociation (Lude.Maybe ChannelStatus)
bcaStatus = Lens.lens (status :: BotChannelAssociation -> Lude.Maybe ChannelStatus) (\s a -> s {status = a} :: BotChannelAssociation)
{-# DEPRECATED bcaStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | An alias pointing to the specific version of the Amazon Lex bot to which this association is being made.
--
-- /Note:/ Consider using 'botAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcaBotAlias :: Lens.Lens' BotChannelAssociation (Lude.Maybe Lude.Text)
bcaBotAlias = Lens.lens (botAlias :: BotChannelAssociation -> Lude.Maybe Lude.Text) (\s a -> s {botAlias = a} :: BotChannelAssociation)
{-# DEPRECATED bcaBotAlias "Use generic-lens or generic-optics with 'botAlias' instead." #-}

-- | The name of the Amazon Lex bot to which this association is being made.
--
-- /Note:/ Consider using 'botName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcaBotName :: Lens.Lens' BotChannelAssociation (Lude.Maybe Lude.Text)
bcaBotName = Lens.lens (botName :: BotChannelAssociation -> Lude.Maybe Lude.Text) (\s a -> s {botName = a} :: BotChannelAssociation)
{-# DEPRECATED bcaBotName "Use generic-lens or generic-optics with 'botName' instead." #-}

-- | Provides information necessary to communicate with the messaging platform.
--
-- /Note:/ Consider using 'botConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcaBotConfiguration :: Lens.Lens' BotChannelAssociation (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
bcaBotConfiguration = Lens.lens (botConfiguration :: BotChannelAssociation -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {botConfiguration = a} :: BotChannelAssociation)
{-# DEPRECATED bcaBotConfiguration "Use generic-lens or generic-optics with 'botConfiguration' instead." #-}

-- | The date that the association between the Amazon Lex bot and the channel was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcaCreatedDate :: Lens.Lens' BotChannelAssociation (Lude.Maybe Lude.Timestamp)
bcaCreatedDate = Lens.lens (createdDate :: BotChannelAssociation -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: BotChannelAssociation)
{-# DEPRECATED bcaCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The name of the association between the bot and the channel.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcaName :: Lens.Lens' BotChannelAssociation (Lude.Maybe Lude.Text)
bcaName = Lens.lens (name :: BotChannelAssociation -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: BotChannelAssociation)
{-# DEPRECATED bcaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specifies the type of association by indicating the type of channel being established between the Amazon Lex bot and the external messaging platform.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcaType :: Lens.Lens' BotChannelAssociation (Lude.Maybe ChannelType)
bcaType = Lens.lens (type' :: BotChannelAssociation -> Lude.Maybe ChannelType) (\s a -> s {type' = a} :: BotChannelAssociation)
{-# DEPRECATED bcaType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A text description of the association you are creating.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcaDescription :: Lens.Lens' BotChannelAssociation (Lude.Maybe Lude.Text)
bcaDescription = Lens.lens (description :: BotChannelAssociation -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: BotChannelAssociation)
{-# DEPRECATED bcaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON BotChannelAssociation where
  parseJSON =
    Lude.withObject
      "BotChannelAssociation"
      ( \x ->
          BotChannelAssociation'
            Lude.<$> (x Lude..:? "failureReason")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "botAlias")
            Lude.<*> (x Lude..:? "botName")
            Lude.<*> (x Lude..:? "botConfiguration" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "createdDate")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "description")
      )
