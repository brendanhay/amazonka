{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CustomHeaders
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CustomHeaders
  ( CustomHeaders (..),

    -- * Smart constructor
    mkCustomHeaders,

    -- * Lenses
    chQuantity,
    chItems,
  )
where

import Network.AWS.CloudFront.Types.OriginCustomHeader
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex type that contains the list of Custom Headers for each origin.
--
-- /See:/ 'mkCustomHeaders' smart constructor.
data CustomHeaders = CustomHeaders'
  { -- | The number of custom headers, if any, for this distribution.
    quantity :: Lude.Int,
    -- | __Optional__ : A list that contains one @OriginCustomHeader@ element for each custom header that you want CloudFront to forward to the origin. If Quantity is @0@ , omit @Items@ .
    items :: Lude.Maybe [OriginCustomHeader]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CustomHeaders' with the minimum fields required to make a request.
--
-- * 'quantity' - The number of custom headers, if any, for this distribution.
-- * 'items' - __Optional__ : A list that contains one @OriginCustomHeader@ element for each custom header that you want CloudFront to forward to the origin. If Quantity is @0@ , omit @Items@ .
mkCustomHeaders ::
  -- | 'quantity'
  Lude.Int ->
  CustomHeaders
mkCustomHeaders pQuantity_ =
  CustomHeaders' {quantity = pQuantity_, items = Lude.Nothing}

-- | The number of custom headers, if any, for this distribution.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chQuantity :: Lens.Lens' CustomHeaders Lude.Int
chQuantity = Lens.lens (quantity :: CustomHeaders -> Lude.Int) (\s a -> s {quantity = a} :: CustomHeaders)
{-# DEPRECATED chQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | __Optional__ : A list that contains one @OriginCustomHeader@ element for each custom header that you want CloudFront to forward to the origin. If Quantity is @0@ , omit @Items@ .
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chItems :: Lens.Lens' CustomHeaders (Lude.Maybe [OriginCustomHeader])
chItems = Lens.lens (items :: CustomHeaders -> Lude.Maybe [OriginCustomHeader]) (\s a -> s {items = a} :: CustomHeaders)
{-# DEPRECATED chItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Lude.FromXML CustomHeaders where
  parseXML x =
    CustomHeaders'
      Lude.<$> (x Lude..@ "Quantity")
      Lude.<*> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "OriginCustomHeader")
               )

instance Lude.ToXML CustomHeaders where
  toXML CustomHeaders' {..} =
    Lude.mconcat
      [ "Quantity" Lude.@= quantity,
        "Items"
          Lude.@= Lude.toXML (Lude.toXMLList "OriginCustomHeader" Lude.<$> items)
      ]
