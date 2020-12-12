{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryptionList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldLevelEncryptionList
  ( FieldLevelEncryptionList (..),

    -- * Smart constructor
    mkFieldLevelEncryptionList,

    -- * Lenses
    flelItems,
    flelNextMarker,
    flelMaxItems,
    flelQuantity,
  )
where

import Network.AWS.CloudFront.Types.FieldLevelEncryptionSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | List of field-level encrpytion configurations.
--
-- /See:/ 'mkFieldLevelEncryptionList' smart constructor.
data FieldLevelEncryptionList = FieldLevelEncryptionList'
  { items ::
      Lude.Maybe [FieldLevelEncryptionSummary],
    nextMarker :: Lude.Maybe Lude.Text,
    maxItems :: Lude.Int,
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

-- | Creates a value of 'FieldLevelEncryptionList' with the minimum fields required to make a request.
--
-- * 'items' - An array of field-level encryption items.
-- * 'maxItems' - The maximum number of elements you want in the response body.
-- * 'nextMarker' - If there are more elements to be listed, this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your configurations where you left off.
-- * 'quantity' - The number of field-level encryption items.
mkFieldLevelEncryptionList ::
  -- | 'maxItems'
  Lude.Int ->
  -- | 'quantity'
  Lude.Int ->
  FieldLevelEncryptionList
mkFieldLevelEncryptionList pMaxItems_ pQuantity_ =
  FieldLevelEncryptionList'
    { items = Lude.Nothing,
      nextMarker = Lude.Nothing,
      maxItems = pMaxItems_,
      quantity = pQuantity_
    }

-- | An array of field-level encryption items.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flelItems :: Lens.Lens' FieldLevelEncryptionList (Lude.Maybe [FieldLevelEncryptionSummary])
flelItems = Lens.lens (items :: FieldLevelEncryptionList -> Lude.Maybe [FieldLevelEncryptionSummary]) (\s a -> s {items = a} :: FieldLevelEncryptionList)
{-# DEPRECATED flelItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | If there are more elements to be listed, this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your configurations where you left off.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flelNextMarker :: Lens.Lens' FieldLevelEncryptionList (Lude.Maybe Lude.Text)
flelNextMarker = Lens.lens (nextMarker :: FieldLevelEncryptionList -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: FieldLevelEncryptionList)
{-# DEPRECATED flelNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The maximum number of elements you want in the response body.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flelMaxItems :: Lens.Lens' FieldLevelEncryptionList Lude.Int
flelMaxItems = Lens.lens (maxItems :: FieldLevelEncryptionList -> Lude.Int) (\s a -> s {maxItems = a} :: FieldLevelEncryptionList)
{-# DEPRECATED flelMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The number of field-level encryption items.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flelQuantity :: Lens.Lens' FieldLevelEncryptionList Lude.Int
flelQuantity = Lens.lens (quantity :: FieldLevelEncryptionList -> Lude.Int) (\s a -> s {quantity = a} :: FieldLevelEncryptionList)
{-# DEPRECATED flelQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

instance Lude.FromXML FieldLevelEncryptionList where
  parseXML x =
    FieldLevelEncryptionList'
      Lude.<$> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "FieldLevelEncryptionSummary")
               )
      Lude.<*> (x Lude..@? "NextMarker")
      Lude.<*> (x Lude..@ "MaxItems")
      Lude.<*> (x Lude..@ "Quantity")
