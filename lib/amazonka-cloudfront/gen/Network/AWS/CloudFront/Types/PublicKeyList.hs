{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.PublicKeyList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.PublicKeyList
  ( PublicKeyList (..),

    -- * Smart constructor
    mkPublicKeyList,

    -- * Lenses
    pklItems,
    pklNextMarker,
    pklMaxItems,
    pklQuantity,
  )
where

import Network.AWS.CloudFront.Types.PublicKeySummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of public keys that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies> , or with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption> .
--
-- /See:/ 'mkPublicKeyList' smart constructor.
data PublicKeyList = PublicKeyList'
  { items ::
      Lude.Maybe [PublicKeySummary],
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

-- | Creates a value of 'PublicKeyList' with the minimum fields required to make a request.
--
-- * 'items' - A list of public keys.
-- * 'maxItems' - The maximum number of public keys you want in the response.
-- * 'nextMarker' - If there are more elements to be listed, this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your public keys where you left off.
-- * 'quantity' - The number of public keys in the list.
mkPublicKeyList ::
  -- | 'maxItems'
  Lude.Int ->
  -- | 'quantity'
  Lude.Int ->
  PublicKeyList
mkPublicKeyList pMaxItems_ pQuantity_ =
  PublicKeyList'
    { items = Lude.Nothing,
      nextMarker = Lude.Nothing,
      maxItems = pMaxItems_,
      quantity = pQuantity_
    }

-- | A list of public keys.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pklItems :: Lens.Lens' PublicKeyList (Lude.Maybe [PublicKeySummary])
pklItems = Lens.lens (items :: PublicKeyList -> Lude.Maybe [PublicKeySummary]) (\s a -> s {items = a} :: PublicKeyList)
{-# DEPRECATED pklItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | If there are more elements to be listed, this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your public keys where you left off.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pklNextMarker :: Lens.Lens' PublicKeyList (Lude.Maybe Lude.Text)
pklNextMarker = Lens.lens (nextMarker :: PublicKeyList -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: PublicKeyList)
{-# DEPRECATED pklNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The maximum number of public keys you want in the response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pklMaxItems :: Lens.Lens' PublicKeyList Lude.Int
pklMaxItems = Lens.lens (maxItems :: PublicKeyList -> Lude.Int) (\s a -> s {maxItems = a} :: PublicKeyList)
{-# DEPRECATED pklMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The number of public keys in the list.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pklQuantity :: Lens.Lens' PublicKeyList Lude.Int
pklQuantity = Lens.lens (quantity :: PublicKeyList -> Lude.Int) (\s a -> s {quantity = a} :: PublicKeyList)
{-# DEPRECATED pklQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

instance Lude.FromXML PublicKeyList where
  parseXML x =
    PublicKeyList'
      Lude.<$> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "PublicKeySummary")
               )
      Lude.<*> (x Lude..@? "NextMarker")
      Lude.<*> (x Lude..@ "MaxItems")
      Lude.<*> (x Lude..@ "Quantity")
