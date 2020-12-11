-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types.Item
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types.Item
  ( Item (..),

    -- * Smart constructor
    mkItem,

    -- * Lenses
    iAlternateNameEncoding,
    iName,
    iAttributes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SDB.Types.Attribute

-- |
--
-- /See:/ 'mkItem' smart constructor.
data Item = Item'
  { alternateNameEncoding :: Lude.Maybe Lude.Text,
    name :: Lude.Text,
    attributes :: [Attribute]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Item' with the minimum fields required to make a request.
--
-- * 'alternateNameEncoding' -
-- * 'attributes' - A list of attributes.
-- * 'name' - The name of the item.
mkItem ::
  -- | 'name'
  Lude.Text ->
  Item
mkItem pName_ =
  Item'
    { alternateNameEncoding = Lude.Nothing,
      name = pName_,
      attributes = Lude.mempty
    }

-- |
--
-- /Note:/ Consider using 'alternateNameEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAlternateNameEncoding :: Lens.Lens' Item (Lude.Maybe Lude.Text)
iAlternateNameEncoding = Lens.lens (alternateNameEncoding :: Item -> Lude.Maybe Lude.Text) (\s a -> s {alternateNameEncoding = a} :: Item)
{-# DEPRECATED iAlternateNameEncoding "Use generic-lens or generic-optics with 'alternateNameEncoding' instead." #-}

-- | The name of the item.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iName :: Lens.Lens' Item Lude.Text
iName = Lens.lens (name :: Item -> Lude.Text) (\s a -> s {name = a} :: Item)
{-# DEPRECATED iName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAttributes :: Lens.Lens' Item [Attribute]
iAttributes = Lens.lens (attributes :: Item -> [Attribute]) (\s a -> s {attributes = a} :: Item)
{-# DEPRECATED iAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.FromXML Item where
  parseXML x =
    Item'
      Lude.<$> (x Lude..@? "AlternateNameEncoding")
      Lude.<*> (x Lude..@ "Name")
      Lude.<*> (Lude.parseXMLList "Attribute" x)
