{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.IntentMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.IntentMetadata
  ( IntentMetadata (..),

    -- * Smart constructor
    mkIntentMetadata,

    -- * Lenses
    imCreatedDate,
    imName,
    imVersion,
    imLastUpdatedDate,
    imDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about an intent.
--
-- /See:/ 'mkIntentMetadata' smart constructor.
data IntentMetadata = IntentMetadata'
  { -- | The date that the intent was created.
    createdDate :: Lude.Maybe Lude.Timestamp,
    -- | The name of the intent.
    name :: Lude.Maybe Lude.Text,
    -- | The version of the intent.
    version :: Lude.Maybe Lude.Text,
    -- | The date that the intent was updated. When you create an intent, the creation date and last updated date are the same.
    lastUpdatedDate :: Lude.Maybe Lude.Timestamp,
    -- | A description of the intent.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IntentMetadata' with the minimum fields required to make a request.
--
-- * 'createdDate' - The date that the intent was created.
-- * 'name' - The name of the intent.
-- * 'version' - The version of the intent.
-- * 'lastUpdatedDate' - The date that the intent was updated. When you create an intent, the creation date and last updated date are the same.
-- * 'description' - A description of the intent.
mkIntentMetadata ::
  IntentMetadata
mkIntentMetadata =
  IntentMetadata'
    { createdDate = Lude.Nothing,
      name = Lude.Nothing,
      version = Lude.Nothing,
      lastUpdatedDate = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The date that the intent was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imCreatedDate :: Lens.Lens' IntentMetadata (Lude.Maybe Lude.Timestamp)
imCreatedDate = Lens.lens (createdDate :: IntentMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: IntentMetadata)
{-# DEPRECATED imCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The name of the intent.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imName :: Lens.Lens' IntentMetadata (Lude.Maybe Lude.Text)
imName = Lens.lens (name :: IntentMetadata -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: IntentMetadata)
{-# DEPRECATED imName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the intent.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imVersion :: Lens.Lens' IntentMetadata (Lude.Maybe Lude.Text)
imVersion = Lens.lens (version :: IntentMetadata -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: IntentMetadata)
{-# DEPRECATED imVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The date that the intent was updated. When you create an intent, the creation date and last updated date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imLastUpdatedDate :: Lens.Lens' IntentMetadata (Lude.Maybe Lude.Timestamp)
imLastUpdatedDate = Lens.lens (lastUpdatedDate :: IntentMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDate = a} :: IntentMetadata)
{-# DEPRECATED imLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | A description of the intent.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imDescription :: Lens.Lens' IntentMetadata (Lude.Maybe Lude.Text)
imDescription = Lens.lens (description :: IntentMetadata -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: IntentMetadata)
{-# DEPRECATED imDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON IntentMetadata where
  parseJSON =
    Lude.withObject
      "IntentMetadata"
      ( \x ->
          IntentMetadata'
            Lude.<$> (x Lude..:? "createdDate")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "version")
            Lude.<*> (x Lude..:? "lastUpdatedDate")
            Lude.<*> (x Lude..:? "description")
      )
