-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CacheBehaviors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CacheBehaviors
  ( CacheBehaviors (..),

    -- * Smart constructor
    mkCacheBehaviors,

    -- * Lenses
    cbItems,
    cbQuantity,
  )
where

import Network.AWS.CloudFront.Types.CacheBehavior
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex type that contains zero or more @CacheBehavior@ elements.
--
-- /See:/ 'mkCacheBehaviors' smart constructor.
data CacheBehaviors = CacheBehaviors'
  { items ::
      Lude.Maybe [CacheBehavior],
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

-- | Creates a value of 'CacheBehaviors' with the minimum fields required to make a request.
--
-- * 'items' - Optional: A complex type that contains cache behaviors for this distribution. If @Quantity@ is @0@ , you can omit @Items@ .
-- * 'quantity' - The number of cache behaviors for this distribution.
mkCacheBehaviors ::
  -- | 'quantity'
  Lude.Int ->
  CacheBehaviors
mkCacheBehaviors pQuantity_ =
  CacheBehaviors' {items = Lude.Nothing, quantity = pQuantity_}

-- | Optional: A complex type that contains cache behaviors for this distribution. If @Quantity@ is @0@ , you can omit @Items@ .
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbItems :: Lens.Lens' CacheBehaviors (Lude.Maybe [CacheBehavior])
cbItems = Lens.lens (items :: CacheBehaviors -> Lude.Maybe [CacheBehavior]) (\s a -> s {items = a} :: CacheBehaviors)
{-# DEPRECATED cbItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | The number of cache behaviors for this distribution.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbQuantity :: Lens.Lens' CacheBehaviors Lude.Int
cbQuantity = Lens.lens (quantity :: CacheBehaviors -> Lude.Int) (\s a -> s {quantity = a} :: CacheBehaviors)
{-# DEPRECATED cbQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

instance Lude.FromXML CacheBehaviors where
  parseXML x =
    CacheBehaviors'
      Lude.<$> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "CacheBehavior")
               )
      Lude.<*> (x Lude..@ "Quantity")

instance Lude.ToXML CacheBehaviors where
  toXML CacheBehaviors' {..} =
    Lude.mconcat
      [ "Items"
          Lude.@= Lude.toXML (Lude.toXMLList "CacheBehavior" Lude.<$> items),
        "Quantity" Lude.@= quantity
      ]
