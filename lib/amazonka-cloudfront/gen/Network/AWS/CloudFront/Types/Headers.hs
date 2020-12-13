{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Headers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Headers
  ( Headers (..),

    -- * Smart constructor
    mkHeaders,

    -- * Lenses
    hQuantity,
    hItems,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains a list of HTTP header names.
--
-- /See:/ 'mkHeaders' smart constructor.
data Headers = Headers'
  { -- | The number of header names in the @Items@ list.
    quantity :: Lude.Int,
    -- | A list of HTTP header names.
    items :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Headers' with the minimum fields required to make a request.
--
-- * 'quantity' - The number of header names in the @Items@ list.
-- * 'items' - A list of HTTP header names.
mkHeaders ::
  -- | 'quantity'
  Lude.Int ->
  Headers
mkHeaders pQuantity_ =
  Headers' {quantity = pQuantity_, items = Lude.Nothing}

-- | The number of header names in the @Items@ list.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hQuantity :: Lens.Lens' Headers Lude.Int
hQuantity = Lens.lens (quantity :: Headers -> Lude.Int) (\s a -> s {quantity = a} :: Headers)
{-# DEPRECATED hQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | A list of HTTP header names.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hItems :: Lens.Lens' Headers (Lude.Maybe [Lude.Text])
hItems = Lens.lens (items :: Headers -> Lude.Maybe [Lude.Text]) (\s a -> s {items = a} :: Headers)
{-# DEPRECATED hItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Lude.FromXML Headers where
  parseXML x =
    Headers'
      Lude.<$> (x Lude..@ "Quantity")
      Lude.<*> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Name")
               )

instance Lude.ToXML Headers where
  toXML Headers' {..} =
    Lude.mconcat
      [ "Quantity" Lude.@= quantity,
        "Items" Lude.@= Lude.toXML (Lude.toXMLList "Name" Lude.<$> items)
      ]
