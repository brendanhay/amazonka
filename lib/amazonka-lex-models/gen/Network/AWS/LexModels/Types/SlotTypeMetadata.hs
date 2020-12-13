{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.SlotTypeMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.SlotTypeMetadata
  ( SlotTypeMetadata (..),

    -- * Smart constructor
    mkSlotTypeMetadata,

    -- * Lenses
    stmCreatedDate,
    stmName,
    stmVersion,
    stmLastUpdatedDate,
    stmDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about a slot type..
--
-- /See:/ 'mkSlotTypeMetadata' smart constructor.
data SlotTypeMetadata = SlotTypeMetadata'
  { -- | The date that the slot type was created.
    createdDate :: Lude.Maybe Lude.Timestamp,
    -- | The name of the slot type.
    name :: Lude.Maybe Lude.Text,
    -- | The version of the slot type.
    version :: Lude.Maybe Lude.Text,
    -- | The date that the slot type was updated. When you create a resource, the creation date and last updated date are the same.
    lastUpdatedDate :: Lude.Maybe Lude.Timestamp,
    -- | A description of the slot type.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SlotTypeMetadata' with the minimum fields required to make a request.
--
-- * 'createdDate' - The date that the slot type was created.
-- * 'name' - The name of the slot type.
-- * 'version' - The version of the slot type.
-- * 'lastUpdatedDate' - The date that the slot type was updated. When you create a resource, the creation date and last updated date are the same.
-- * 'description' - A description of the slot type.
mkSlotTypeMetadata ::
  SlotTypeMetadata
mkSlotTypeMetadata =
  SlotTypeMetadata'
    { createdDate = Lude.Nothing,
      name = Lude.Nothing,
      version = Lude.Nothing,
      lastUpdatedDate = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The date that the slot type was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stmCreatedDate :: Lens.Lens' SlotTypeMetadata (Lude.Maybe Lude.Timestamp)
stmCreatedDate = Lens.lens (createdDate :: SlotTypeMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: SlotTypeMetadata)
{-# DEPRECATED stmCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The name of the slot type.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stmName :: Lens.Lens' SlotTypeMetadata (Lude.Maybe Lude.Text)
stmName = Lens.lens (name :: SlotTypeMetadata -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: SlotTypeMetadata)
{-# DEPRECATED stmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the slot type.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stmVersion :: Lens.Lens' SlotTypeMetadata (Lude.Maybe Lude.Text)
stmVersion = Lens.lens (version :: SlotTypeMetadata -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: SlotTypeMetadata)
{-# DEPRECATED stmVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The date that the slot type was updated. When you create a resource, the creation date and last updated date are the same.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stmLastUpdatedDate :: Lens.Lens' SlotTypeMetadata (Lude.Maybe Lude.Timestamp)
stmLastUpdatedDate = Lens.lens (lastUpdatedDate :: SlotTypeMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDate = a} :: SlotTypeMetadata)
{-# DEPRECATED stmLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | A description of the slot type.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stmDescription :: Lens.Lens' SlotTypeMetadata (Lude.Maybe Lude.Text)
stmDescription = Lens.lens (description :: SlotTypeMetadata -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: SlotTypeMetadata)
{-# DEPRECATED stmDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON SlotTypeMetadata where
  parseJSON =
    Lude.withObject
      "SlotTypeMetadata"
      ( \x ->
          SlotTypeMetadata'
            Lude.<$> (x Lude..:? "createdDate")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "version")
            Lude.<*> (x Lude..:? "lastUpdatedDate")
            Lude.<*> (x Lude..:? "description")
      )
