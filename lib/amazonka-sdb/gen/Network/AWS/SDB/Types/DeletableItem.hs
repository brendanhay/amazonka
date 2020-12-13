{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types.DeletableItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types.DeletableItem
  ( DeletableItem (..),

    -- * Smart constructor
    mkDeletableItem,

    -- * Lenses
    diAttributes,
    diName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SDB.Types.Attribute

-- | /See:/ 'mkDeletableItem' smart constructor.
data DeletableItem = DeletableItem'
  { attributes :: Lude.Maybe [Attribute],
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletableItem' with the minimum fields required to make a request.
--
-- * 'attributes' -
-- * 'name' -
mkDeletableItem ::
  -- | 'name'
  Lude.Text ->
  DeletableItem
mkDeletableItem pName_ =
  DeletableItem' {attributes = Lude.Nothing, name = pName_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diAttributes :: Lens.Lens' DeletableItem (Lude.Maybe [Attribute])
diAttributes = Lens.lens (attributes :: DeletableItem -> Lude.Maybe [Attribute]) (\s a -> s {attributes = a} :: DeletableItem)
{-# DEPRECATED diAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diName :: Lens.Lens' DeletableItem Lude.Text
diName = Lens.lens (name :: DeletableItem -> Lude.Text) (\s a -> s {name = a} :: DeletableItem)
{-# DEPRECATED diName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToQuery DeletableItem where
  toQuery DeletableItem' {..} =
    Lude.mconcat
      [ Lude.toQuery (Lude.toQueryList "Attribute" Lude.<$> attributes),
        "ItemName" Lude.=: name
      ]
