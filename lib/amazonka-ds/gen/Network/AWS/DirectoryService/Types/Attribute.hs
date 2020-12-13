{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.Attribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.Attribute
  ( Attribute (..),

    -- * Smart constructor
    mkAttribute,

    -- * Lenses
    aValue,
    aName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a named directory attribute.
--
-- /See:/ 'mkAttribute' smart constructor.
data Attribute = Attribute'
  { -- | The value of the attribute.
    value :: Lude.Maybe Lude.Text,
    -- | The name of the attribute.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Attribute' with the minimum fields required to make a request.
--
-- * 'value' - The value of the attribute.
-- * 'name' - The name of the attribute.
mkAttribute ::
  Attribute
mkAttribute = Attribute' {value = Lude.Nothing, name = Lude.Nothing}

-- | The value of the attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aValue :: Lens.Lens' Attribute (Lude.Maybe Lude.Text)
aValue = Lens.lens (value :: Attribute -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: Attribute)
{-# DEPRECATED aValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of the attribute.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Attribute (Lude.Maybe Lude.Text)
aName = Lens.lens (name :: Attribute -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Attribute)
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON Attribute where
  parseJSON =
    Lude.withObject
      "Attribute"
      ( \x ->
          Attribute'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..:? "Name")
      )

instance Lude.ToJSON Attribute where
  toJSON Attribute' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Value" Lude..=) Lude.<$> value, ("Name" Lude..=) Lude.<$> name]
      )
