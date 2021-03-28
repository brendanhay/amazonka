{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.PublicKeyList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.PublicKeyList
  ( PublicKeyList (..)
  -- * Smart constructor
  , mkPublicKeyList
  -- * Lenses
  , pklMaxItems
  , pklQuantity
  , pklItems
  , pklNextMarker
  ) where

import qualified Network.AWS.CloudFront.Types.PublicKeySummary as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of public keys that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies> , or with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption> .
--
-- /See:/ 'mkPublicKeyList' smart constructor.
data PublicKeyList = PublicKeyList'
  { maxItems :: Core.Int
    -- ^ The maximum number of public keys you want in the response.
  , quantity :: Core.Int
    -- ^ The number of public keys in the list.
  , items :: Core.Maybe [Types.PublicKeySummary]
    -- ^ A list of public keys.
  , nextMarker :: Core.Maybe Core.Text
    -- ^ If there are more elements to be listed, this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your public keys where you left off.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PublicKeyList' value with any optional fields omitted.
mkPublicKeyList
    :: Core.Int -- ^ 'maxItems'
    -> Core.Int -- ^ 'quantity'
    -> PublicKeyList
mkPublicKeyList maxItems quantity
  = PublicKeyList'{maxItems, quantity, items = Core.Nothing,
                   nextMarker = Core.Nothing}

-- | The maximum number of public keys you want in the response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pklMaxItems :: Lens.Lens' PublicKeyList Core.Int
pklMaxItems = Lens.field @"maxItems"
{-# INLINEABLE pklMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | The number of public keys in the list.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pklQuantity :: Lens.Lens' PublicKeyList Core.Int
pklQuantity = Lens.field @"quantity"
{-# INLINEABLE pklQuantity #-}
{-# DEPRECATED quantity "Use generic-lens or generic-optics with 'quantity' instead"  #-}

-- | A list of public keys.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pklItems :: Lens.Lens' PublicKeyList (Core.Maybe [Types.PublicKeySummary])
pklItems = Lens.field @"items"
{-# INLINEABLE pklItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | If there are more elements to be listed, this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your public keys where you left off.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pklNextMarker :: Lens.Lens' PublicKeyList (Core.Maybe Core.Text)
pklNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE pklNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

instance Core.FromXML PublicKeyList where
        parseXML x
          = PublicKeyList' Core.<$>
              (x Core..@ "MaxItems") Core.<*> x Core..@ "Quantity" Core.<*>
                x Core..@? "Items" Core..<@> Core.parseXMLList "PublicKeySummary"
                Core.<*> x Core..@? "NextMarker"
