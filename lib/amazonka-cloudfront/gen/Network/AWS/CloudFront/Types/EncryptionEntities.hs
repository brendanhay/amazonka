{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.EncryptionEntities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.EncryptionEntities
  ( EncryptionEntities (..)
  -- * Smart constructor
  , mkEncryptionEntities
  -- * Lenses
  , eeQuantity
  , eeItems
  ) where

import qualified Network.AWS.CloudFront.Types.EncryptionEntity as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Complex data type for field-level encryption profiles that includes all of the encryption entities. 
--
-- /See:/ 'mkEncryptionEntities' smart constructor.
data EncryptionEntities = EncryptionEntities'
  { quantity :: Core.Int
    -- ^ Number of field pattern items in a field-level encryption content type-profile mapping. 
  , items :: Core.Maybe [Types.EncryptionEntity]
    -- ^ An array of field patterns in a field-level encryption content type-profile mapping. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EncryptionEntities' value with any optional fields omitted.
mkEncryptionEntities
    :: Core.Int -- ^ 'quantity'
    -> EncryptionEntities
mkEncryptionEntities quantity
  = EncryptionEntities'{quantity, items = Core.Nothing}

-- | Number of field pattern items in a field-level encryption content type-profile mapping. 
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eeQuantity :: Lens.Lens' EncryptionEntities Core.Int
eeQuantity = Lens.field @"quantity"
{-# INLINEABLE eeQuantity #-}
{-# DEPRECATED quantity "Use generic-lens or generic-optics with 'quantity' instead"  #-}

-- | An array of field patterns in a field-level encryption content type-profile mapping. 
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eeItems :: Lens.Lens' EncryptionEntities (Core.Maybe [Types.EncryptionEntity])
eeItems = Lens.field @"items"
{-# INLINEABLE eeItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

instance Core.ToXML EncryptionEntities where
        toXML EncryptionEntities{..}
          = Core.toXMLElement "Quantity" quantity Core.<>
              Core.toXMLElement "Items"
                (Core.maybe Core.mempty (Core.toXMLList "EncryptionEntity") items)

instance Core.FromXML EncryptionEntities where
        parseXML x
          = EncryptionEntities' Core.<$>
              (x Core..@ "Quantity") Core.<*>
                x Core..@? "Items" Core..<@> Core.parseXMLList "EncryptionEntity"
