{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.FieldPatterns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldPatterns
  ( FieldPatterns (..),

    -- * Smart constructor
    mkFieldPatterns,

    -- * Lenses
    fpQuantity,
    fpItems,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex data type that includes the field patterns to match for field-level encryption.
--
-- /See:/ 'mkFieldPatterns' smart constructor.
data FieldPatterns = FieldPatterns'
  { -- | The number of field-level encryption field patterns.
    quantity :: Lude.Int,
    -- | An array of the field-level encryption field patterns.
    items :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FieldPatterns' with the minimum fields required to make a request.
--
-- * 'quantity' - The number of field-level encryption field patterns.
-- * 'items' - An array of the field-level encryption field patterns.
mkFieldPatterns ::
  -- | 'quantity'
  Lude.Int ->
  FieldPatterns
mkFieldPatterns pQuantity_ =
  FieldPatterns' {quantity = pQuantity_, items = Lude.Nothing}

-- | The number of field-level encryption field patterns.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fpQuantity :: Lens.Lens' FieldPatterns Lude.Int
fpQuantity = Lens.lens (quantity :: FieldPatterns -> Lude.Int) (\s a -> s {quantity = a} :: FieldPatterns)
{-# DEPRECATED fpQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | An array of the field-level encryption field patterns.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fpItems :: Lens.Lens' FieldPatterns (Lude.Maybe [Lude.Text])
fpItems = Lens.lens (items :: FieldPatterns -> Lude.Maybe [Lude.Text]) (\s a -> s {items = a} :: FieldPatterns)
{-# DEPRECATED fpItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Lude.FromXML FieldPatterns where
  parseXML x =
    FieldPatterns'
      Lude.<$> (x Lude..@ "Quantity")
      Lude.<*> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "FieldPattern")
               )

instance Lude.ToXML FieldPatterns where
  toXML FieldPatterns' {..} =
    Lude.mconcat
      [ "Quantity" Lude.@= quantity,
        "Items"
          Lude.@= Lude.toXML (Lude.toXMLList "FieldPattern" Lude.<$> items)
      ]
