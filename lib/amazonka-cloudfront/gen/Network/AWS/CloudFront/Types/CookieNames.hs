{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CookieNames
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CookieNames
  ( CookieNames (..),

    -- * Smart constructor
    mkCookieNames,

    -- * Lenses
    cnQuantity,
    cnItems,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains a list of cookie names.
--
-- /See:/ 'mkCookieNames' smart constructor.
data CookieNames = CookieNames'
  { -- | The number of cookie names in the @Items@ list.
    quantity :: Lude.Int,
    -- | A list of cookie names.
    items :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CookieNames' with the minimum fields required to make a request.
--
-- * 'quantity' - The number of cookie names in the @Items@ list.
-- * 'items' - A list of cookie names.
mkCookieNames ::
  -- | 'quantity'
  Lude.Int ->
  CookieNames
mkCookieNames pQuantity_ =
  CookieNames' {quantity = pQuantity_, items = Lude.Nothing}

-- | The number of cookie names in the @Items@ list.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnQuantity :: Lens.Lens' CookieNames Lude.Int
cnQuantity = Lens.lens (quantity :: CookieNames -> Lude.Int) (\s a -> s {quantity = a} :: CookieNames)
{-# DEPRECATED cnQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A list of cookie names.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnItems :: Lens.Lens' CookieNames (Lude.Maybe [Lude.Text])
cnItems = Lens.lens (items :: CookieNames -> Lude.Maybe [Lude.Text]) (\s a -> s {items = a} :: CookieNames)
{-# DEPRECATED cnItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Lude.FromXML CookieNames where
  parseXML x =
    CookieNames'
      Lude.<$> (x Lude..@ "Quantity")
      Lude.<*> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Name")
               )

instance Lude.ToXML CookieNames where
  toXML CookieNames' {..} =
    Lude.mconcat
      [ "Quantity" Lude.@= quantity,
        "Items" Lude.@= Lude.toXML (Lude.toXMLList "Name" Lude.<$> items)
      ]
