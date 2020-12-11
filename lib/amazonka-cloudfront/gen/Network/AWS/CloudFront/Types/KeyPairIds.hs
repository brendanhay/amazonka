-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.KeyPairIds
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.KeyPairIds
  ( KeyPairIds (..),

    -- * Smart constructor
    mkKeyPairIds,

    -- * Lenses
    kpiItems,
    kpiQuantity,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of CloudFront key pair identifiers.
--
-- /See:/ 'mkKeyPairIds' smart constructor.
data KeyPairIds = KeyPairIds'
  { items :: Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'KeyPairIds' with the minimum fields required to make a request.
--
-- * 'items' - A list of CloudFront key pair identifiers.
-- * 'quantity' - The number of key pair identifiers in the list.
mkKeyPairIds ::
  -- | 'quantity'
  Lude.Int ->
  KeyPairIds
mkKeyPairIds pQuantity_ =
  KeyPairIds' {items = Lude.Nothing, quantity = pQuantity_}

-- | A list of CloudFront key pair identifiers.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpiItems :: Lens.Lens' KeyPairIds (Lude.Maybe [Lude.Text])
kpiItems = Lens.lens (items :: KeyPairIds -> Lude.Maybe [Lude.Text]) (\s a -> s {items = a} :: KeyPairIds)
{-# DEPRECATED kpiItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | The number of key pair identifiers in the list.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpiQuantity :: Lens.Lens' KeyPairIds Lude.Int
kpiQuantity = Lens.lens (quantity :: KeyPairIds -> Lude.Int) (\s a -> s {quantity = a} :: KeyPairIds)
{-# DEPRECATED kpiQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

instance Lude.FromXML KeyPairIds where
  parseXML x =
    KeyPairIds'
      Lude.<$> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "KeyPairId")
               )
      Lude.<*> (x Lude..@ "Quantity")
