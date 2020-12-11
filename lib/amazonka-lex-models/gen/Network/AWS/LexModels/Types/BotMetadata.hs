-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.BotMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.BotMetadata
  ( BotMetadata (..),

    -- * Smart constructor
    mkBotMetadata,

    -- * Lenses
    bmStatus,
    bmCreatedDate,
    bmName,
    bmVersion,
    bmLastUpdatedDate,
    bmDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.LexStatus
import qualified Network.AWS.Prelude as Lude

-- | Provides information about a bot. .
--
-- /See:/ 'mkBotMetadata' smart constructor.
data BotMetadata = BotMetadata'
  { status :: Lude.Maybe LexStatus,
    createdDate :: Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Text,
    lastUpdatedDate :: Lude.Maybe Lude.Timestamp,
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

-- | Creates a value of 'BotMetadata' with the minimum fields required to make a request.
--
-- * 'createdDate' - The date that the bot was created.
-- * 'description' - A description of the bot.
-- * 'lastUpdatedDate' - The date that the bot was updated. When you create a bot, the creation date and last updated date are the same.
-- * 'name' - The name of the bot.
-- * 'status' - The status of the bot.
-- * 'version' - The version of the bot. For a new bot, the version is always @> LATEST@ .
mkBotMetadata ::
  BotMetadata
mkBotMetadata =
  BotMetadata'
    { status = Lude.Nothing,
      createdDate = Lude.Nothing,
      name = Lude.Nothing,
      version = Lude.Nothing,
      lastUpdatedDate = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The status of the bot.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmStatus :: Lens.Lens' BotMetadata (Lude.Maybe LexStatus)
bmStatus = Lens.lens (status :: BotMetadata -> Lude.Maybe LexStatus) (\s a -> s {status = a} :: BotMetadata)
{-# DEPRECATED bmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date that the bot was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmCreatedDate :: Lens.Lens' BotMetadata (Lude.Maybe Lude.Timestamp)
bmCreatedDate = Lens.lens (createdDate :: BotMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: BotMetadata)
{-# DEPRECATED bmCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The name of the bot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmName :: Lens.Lens' BotMetadata (Lude.Maybe Lude.Text)
bmName = Lens.lens (name :: BotMetadata -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: BotMetadata)
{-# DEPRECATED bmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the bot. For a new bot, the version is always @> LATEST@ .
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmVersion :: Lens.Lens' BotMetadata (Lude.Maybe Lude.Text)
bmVersion = Lens.lens (version :: BotMetadata -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: BotMetadata)
{-# DEPRECATED bmVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The date that the bot was updated. When you create a bot, the creation date and last updated date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmLastUpdatedDate :: Lens.Lens' BotMetadata (Lude.Maybe Lude.Timestamp)
bmLastUpdatedDate = Lens.lens (lastUpdatedDate :: BotMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDate = a} :: BotMetadata)
{-# DEPRECATED bmLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | A description of the bot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmDescription :: Lens.Lens' BotMetadata (Lude.Maybe Lude.Text)
bmDescription = Lens.lens (description :: BotMetadata -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: BotMetadata)
{-# DEPRECATED bmDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON BotMetadata where
  parseJSON =
    Lude.withObject
      "BotMetadata"
      ( \x ->
          BotMetadata'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "createdDate")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "version")
            Lude.<*> (x Lude..:? "lastUpdatedDate")
            Lude.<*> (x Lude..:? "description")
      )
