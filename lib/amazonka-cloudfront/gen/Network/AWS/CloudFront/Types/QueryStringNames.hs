-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.QueryStringNames
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.QueryStringNames
  ( QueryStringNames (..),

    -- * Smart constructor
    mkQueryStringNames,

    -- * Lenses
    qsnItems,
    qsnQuantity,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains a list of query string names.
--
-- /See:/ 'mkQueryStringNames' smart constructor.
data QueryStringNames = QueryStringNames'
  { items ::
      Lude.Maybe [Lude.Text],
    quantity :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QueryStringNames' with the minimum fields required to make a request.
--
-- * 'items' - A list of query string names.
-- * 'quantity' - The number of query string names in the @Items@ list.
mkQueryStringNames ::
  -- | 'quantity'
  Lude.Int ->
  QueryStringNames
mkQueryStringNames pQuantity_ =
  QueryStringNames' {items = Lude.Nothing, quantity = pQuantity_}

-- | A list of query string names.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsnItems :: Lens.Lens' QueryStringNames (Lude.Maybe [Lude.Text])
qsnItems = Lens.lens (items :: QueryStringNames -> Lude.Maybe [Lude.Text]) (\s a -> s {items = a} :: QueryStringNames)
{-# DEPRECATED qsnItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | The number of query string names in the @Items@ list.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsnQuantity :: Lens.Lens' QueryStringNames Lude.Int
qsnQuantity = Lens.lens (quantity :: QueryStringNames -> Lude.Int) (\s a -> s {quantity = a} :: QueryStringNames)
{-# DEPRECATED qsnQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

instance Lude.FromXML QueryStringNames where
  parseXML x =
    QueryStringNames'
      Lude.<$> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Name")
               )
      Lude.<*> (x Lude..@ "Quantity")

instance Lude.ToXML QueryStringNames where
  toXML QueryStringNames' {..} =
    Lude.mconcat
      [ "Items" Lude.@= Lude.toXML (Lude.toXMLList "Name" Lude.<$> items),
        "Quantity" Lude.@= quantity
      ]
