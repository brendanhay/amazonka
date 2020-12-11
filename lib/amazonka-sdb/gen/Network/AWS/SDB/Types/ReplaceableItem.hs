-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types.ReplaceableItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types.ReplaceableItem
  ( ReplaceableItem (..),

    -- * Smart constructor
    mkReplaceableItem,

    -- * Lenses
    riName,
    riAttributes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SDB.Types.ReplaceableAttribute

-- |
--
-- /See:/ 'mkReplaceableItem' smart constructor.
data ReplaceableItem = ReplaceableItem'
  { name :: Lude.Text,
    attributes :: [ReplaceableAttribute]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplaceableItem' with the minimum fields required to make a request.
--
-- * 'attributes' - The list of attributes for a replaceable item.
-- * 'name' - The name of the replaceable item.
mkReplaceableItem ::
  -- | 'name'
  Lude.Text ->
  ReplaceableItem
mkReplaceableItem pName_ =
  ReplaceableItem' {name = pName_, attributes = Lude.mempty}

-- | The name of the replaceable item.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riName :: Lens.Lens' ReplaceableItem Lude.Text
riName = Lens.lens (name :: ReplaceableItem -> Lude.Text) (\s a -> s {name = a} :: ReplaceableItem)
{-# DEPRECATED riName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The list of attributes for a replaceable item.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riAttributes :: Lens.Lens' ReplaceableItem [ReplaceableAttribute]
riAttributes = Lens.lens (attributes :: ReplaceableItem -> [ReplaceableAttribute]) (\s a -> s {attributes = a} :: ReplaceableItem)
{-# DEPRECATED riAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.ToQuery ReplaceableItem where
  toQuery ReplaceableItem' {..} =
    Lude.mconcat
      ["ItemName" Lude.=: name, Lude.toQueryList "Attribute" attributes]
