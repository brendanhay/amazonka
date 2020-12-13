{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.StatusCodes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.StatusCodes
  ( StatusCodes (..),

    -- * Smart constructor
    mkStatusCodes,

    -- * Lenses
    scQuantity,
    scItems,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex data type for the status codes that you specify that, when returned by a primary origin, trigger CloudFront to failover to a second origin.
--
-- /See:/ 'mkStatusCodes' smart constructor.
data StatusCodes = StatusCodes'
  { -- | The number of status codes.
    quantity :: Lude.Int,
    -- | The items (status codes) for an origin group.
    items :: Lude.NonEmpty Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StatusCodes' with the minimum fields required to make a request.
--
-- * 'quantity' - The number of status codes.
-- * 'items' - The items (status codes) for an origin group.
mkStatusCodes ::
  -- | 'quantity'
  Lude.Int ->
  -- | 'items'
  Lude.NonEmpty Lude.Int ->
  StatusCodes
mkStatusCodes pQuantity_ pItems_ =
  StatusCodes' {quantity = pQuantity_, items = pItems_}

-- | The number of status codes.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scQuantity :: Lens.Lens' StatusCodes Lude.Int
scQuantity = Lens.lens (quantity :: StatusCodes -> Lude.Int) (\s a -> s {quantity = a} :: StatusCodes)
{-# DEPRECATED scQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | The items (status codes) for an origin group.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scItems :: Lens.Lens' StatusCodes (Lude.NonEmpty Lude.Int)
scItems = Lens.lens (items :: StatusCodes -> Lude.NonEmpty Lude.Int) (\s a -> s {items = a} :: StatusCodes)
{-# DEPRECATED scItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Lude.FromXML StatusCodes where
  parseXML x =
    StatusCodes'
      Lude.<$> (x Lude..@ "Quantity")
      Lude.<*> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.parseXMLNonEmpty "StatusCode"
               )

instance Lude.ToXML StatusCodes where
  toXML StatusCodes' {..} =
    Lude.mconcat
      [ "Quantity" Lude.@= quantity,
        "Items" Lude.@= Lude.toXMLList "StatusCode" items
      ]
