{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileList
  ( FieldLevelEncryptionProfileList (..),

    -- * Smart constructor
    mkFieldLevelEncryptionProfileList,

    -- * Lenses
    fleplQuantity,
    fleplItems,
    fleplMaxItems,
    fleplNextMarker,
  )
where

import Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | List of field-level encryption profiles.
--
-- /See:/ 'mkFieldLevelEncryptionProfileList' smart constructor.
data FieldLevelEncryptionProfileList = FieldLevelEncryptionProfileList'
  { -- | The number of field-level encryption profiles.
    quantity :: Lude.Int,
    -- | The field-level encryption profile items.
    items :: Lude.Maybe [FieldLevelEncryptionProfileSummary],
    -- | The maximum number of field-level encryption profiles you want in the response body.
    maxItems :: Lude.Int,
    -- | If there are more elements to be listed, this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your profiles where you left off.
    nextMarker :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FieldLevelEncryptionProfileList' with the minimum fields required to make a request.
--
-- * 'quantity' - The number of field-level encryption profiles.
-- * 'items' - The field-level encryption profile items.
-- * 'maxItems' - The maximum number of field-level encryption profiles you want in the response body.
-- * 'nextMarker' - If there are more elements to be listed, this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your profiles where you left off.
mkFieldLevelEncryptionProfileList ::
  -- | 'quantity'
  Lude.Int ->
  -- | 'maxItems'
  Lude.Int ->
  FieldLevelEncryptionProfileList
mkFieldLevelEncryptionProfileList pQuantity_ pMaxItems_ =
  FieldLevelEncryptionProfileList'
    { quantity = pQuantity_,
      items = Lude.Nothing,
      maxItems = pMaxItems_,
      nextMarker = Lude.Nothing
    }

-- | The number of field-level encryption profiles.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleplQuantity :: Lens.Lens' FieldLevelEncryptionProfileList Lude.Int
fleplQuantity = Lens.lens (quantity :: FieldLevelEncryptionProfileList -> Lude.Int) (\s a -> s {quantity = a} :: FieldLevelEncryptionProfileList)
{-# DEPRECATED fleplQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | The field-level encryption profile items.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleplItems :: Lens.Lens' FieldLevelEncryptionProfileList (Lude.Maybe [FieldLevelEncryptionProfileSummary])
fleplItems = Lens.lens (items :: FieldLevelEncryptionProfileList -> Lude.Maybe [FieldLevelEncryptionProfileSummary]) (\s a -> s {items = a} :: FieldLevelEncryptionProfileList)
{-# DEPRECATED fleplItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | The maximum number of field-level encryption profiles you want in the response body.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleplMaxItems :: Lens.Lens' FieldLevelEncryptionProfileList Lude.Int
fleplMaxItems = Lens.lens (maxItems :: FieldLevelEncryptionProfileList -> Lude.Int) (\s a -> s {maxItems = a} :: FieldLevelEncryptionProfileList)
{-# DEPRECATED fleplMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | If there are more elements to be listed, this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your profiles where you left off.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fleplNextMarker :: Lens.Lens' FieldLevelEncryptionProfileList (Lude.Maybe Lude.Text)
fleplNextMarker = Lens.lens (nextMarker :: FieldLevelEncryptionProfileList -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: FieldLevelEncryptionProfileList)
{-# DEPRECATED fleplNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

instance Lude.FromXML FieldLevelEncryptionProfileList where
  parseXML x =
    FieldLevelEncryptionProfileList'
      Lude.<$> (x Lude..@ "Quantity")
      Lude.<*> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "FieldLevelEncryptionProfileSummary")
               )
      Lude.<*> (x Lude..@ "MaxItems")
      Lude.<*> (x Lude..@? "NextMarker")
