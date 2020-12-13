{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Paths
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Paths
  ( Paths (..),

    -- * Smart constructor
    mkPaths,

    -- * Lenses
    pQuantity,
    pItems,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex type that contains information about the objects that you want to invalidate. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html#invalidation-specifying-objects Specifying the Objects to Invalidate> in the /Amazon CloudFront Developer Guide/ .
--
-- /See:/ 'mkPaths' smart constructor.
data Paths = Paths'
  { -- | The number of invalidation paths specified for the objects that you want to invalidate.
    quantity :: Lude.Int,
    -- | A complex type that contains a list of the paths that you want to invalidate.
    items :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Paths' with the minimum fields required to make a request.
--
-- * 'quantity' - The number of invalidation paths specified for the objects that you want to invalidate.
-- * 'items' - A complex type that contains a list of the paths that you want to invalidate.
mkPaths ::
  -- | 'quantity'
  Lude.Int ->
  Paths
mkPaths pQuantity_ =
  Paths' {quantity = pQuantity_, items = Lude.Nothing}

-- | The number of invalidation paths specified for the objects that you want to invalidate.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pQuantity :: Lens.Lens' Paths Lude.Int
pQuantity = Lens.lens (quantity :: Paths -> Lude.Int) (\s a -> s {quantity = a} :: Paths)
{-# DEPRECATED pQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A complex type that contains a list of the paths that you want to invalidate.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pItems :: Lens.Lens' Paths (Lude.Maybe [Lude.Text])
pItems = Lens.lens (items :: Paths -> Lude.Maybe [Lude.Text]) (\s a -> s {items = a} :: Paths)
{-# DEPRECATED pItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Lude.FromXML Paths where
  parseXML x =
    Paths'
      Lude.<$> (x Lude..@ "Quantity")
      Lude.<*> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Path")
               )

instance Lude.ToXML Paths where
  toXML Paths' {..} =
    Lude.mconcat
      [ "Quantity" Lude.@= quantity,
        "Items" Lude.@= Lude.toXML (Lude.toXMLList "Path" Lude.<$> items)
      ]
