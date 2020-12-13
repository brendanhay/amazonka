{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    cbQuantity,
    cbItems,
  )
where

import Network.AWS.CloudFront.Types.CacheBehavior
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex type that contains zero or more @CacheBehavior@ elements.
--
-- /See:/ 'mkCacheBehaviors' smart constructor.
data CacheBehaviors = CacheBehaviors'
  { -- | The number of cache behaviors for this distribution.
    quantity :: Lude.Int,
    -- | Optional: A complex type that contains cache behaviors for this distribution. If @Quantity@ is @0@ , you can omit @Items@ .
    items :: Lude.Maybe [CacheBehavior]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CacheBehaviors' with the minimum fields required to make a request.
--
-- * 'quantity' - The number of cache behaviors for this distribution.
-- * 'items' - Optional: A complex type that contains cache behaviors for this distribution. If @Quantity@ is @0@ , you can omit @Items@ .
mkCacheBehaviors ::
  -- | 'quantity'
  Lude.Int ->
  CacheBehaviors
mkCacheBehaviors pQuantity_ =
  CacheBehaviors' {quantity = pQuantity_, items = Lude.Nothing}

-- | The number of cache behaviors for this distribution.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbQuantity :: Lens.Lens' CacheBehaviors Lude.Int
cbQuantity = Lens.lens (quantity :: CacheBehaviors -> Lude.Int) (\s a -> s {quantity = a} :: CacheBehaviors)
{-# DEPRECATED cbQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | Optional: A complex type that contains cache behaviors for this distribution. If @Quantity@ is @0@ , you can omit @Items@ .
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbItems :: Lens.Lens' CacheBehaviors (Lude.Maybe [CacheBehavior])
cbItems = Lens.lens (items :: CacheBehaviors -> Lude.Maybe [CacheBehavior]) (\s a -> s {items = a} :: CacheBehaviors)
{-# DEPRECATED cbItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Lude.FromXML CacheBehaviors where
  parseXML x =
    CacheBehaviors'
      Lude.<$> (x Lude..@ "Quantity")
      Lude.<*> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "CacheBehavior")
               )

instance Lude.ToXML CacheBehaviors where
  toXML CacheBehaviors' {..} =
    Lude.mconcat
      [ "Quantity" Lude.@= quantity,
        "Items"
          Lude.@= Lude.toXML (Lude.toXMLList "CacheBehavior" Lude.<$> items)
      ]
