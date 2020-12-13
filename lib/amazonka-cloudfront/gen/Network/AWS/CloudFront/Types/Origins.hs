{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Origins
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Origins
  ( Origins (..),

    -- * Smart constructor
    mkOrigins,

    -- * Lenses
    oQuantity,
    oItems,
  )
where

import Network.AWS.CloudFront.Types.Origin
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the origins for this distribution.
--
-- /See:/ 'mkOrigins' smart constructor.
data Origins = Origins'
  { -- | The number of origins for this distribution.
    quantity :: Lude.Int,
    -- | A list of origins.
    items :: Lude.NonEmpty Origin
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Origins' with the minimum fields required to make a request.
--
-- * 'quantity' - The number of origins for this distribution.
-- * 'items' - A list of origins.
mkOrigins ::
  -- | 'quantity'
  Lude.Int ->
  -- | 'items'
  Lude.NonEmpty Origin ->
  Origins
mkOrigins pQuantity_ pItems_ =
  Origins' {quantity = pQuantity_, items = pItems_}

-- | The number of origins for this distribution.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oQuantity :: Lens.Lens' Origins Lude.Int
oQuantity = Lens.lens (quantity :: Origins -> Lude.Int) (\s a -> s {quantity = a} :: Origins)
{-# DEPRECATED oQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A list of origins.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oItems :: Lens.Lens' Origins (Lude.NonEmpty Origin)
oItems = Lens.lens (items :: Origins -> Lude.NonEmpty Origin) (\s a -> s {items = a} :: Origins)
{-# DEPRECATED oItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Lude.FromXML Origins where
  parseXML x =
    Origins'
      Lude.<$> (x Lude..@ "Quantity")
      Lude.<*> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.parseXMLNonEmpty "Origin"
               )

instance Lude.ToXML Origins where
  toXML Origins' {..} =
    Lude.mconcat
      [ "Quantity" Lude.@= quantity,
        "Items" Lude.@= Lude.toXMLList "Origin" items
      ]
