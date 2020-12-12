{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.Alias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.Alias
  ( Alias (..),

    -- * Smart constructor
    mkAlias,

    -- * Lenses
    aNames,
    aName,
    aType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An alias for an edge.
--
-- /See:/ 'mkAlias' smart constructor.
data Alias = Alias'
  { names :: Lude.Maybe [Lude.Text],
    name :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Alias' with the minimum fields required to make a request.
--
-- * 'name' - The canonical name of the alias.
-- * 'names' - A list of names for the alias, including the canonical name.
-- * 'type'' - The type of the alias.
mkAlias ::
  Alias
mkAlias =
  Alias'
    { names = Lude.Nothing,
      name = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | A list of names for the alias, including the canonical name.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNames :: Lens.Lens' Alias (Lude.Maybe [Lude.Text])
aNames = Lens.lens (names :: Alias -> Lude.Maybe [Lude.Text]) (\s a -> s {names = a} :: Alias)
{-# DEPRECATED aNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The canonical name of the alias.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Alias (Lude.Maybe Lude.Text)
aName = Lens.lens (name :: Alias -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Alias)
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of the alias.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aType :: Lens.Lens' Alias (Lude.Maybe Lude.Text)
aType = Lens.lens (type' :: Alias -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: Alias)
{-# DEPRECATED aType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON Alias where
  parseJSON =
    Lude.withObject
      "Alias"
      ( \x ->
          Alias'
            Lude.<$> (x Lude..:? "Names" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Type")
      )
