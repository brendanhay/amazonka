{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.IndexAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.IndexAttachment
  ( IndexAttachment (..)
  -- * Smart constructor
  , mkIndexAttachment
  -- * Lenses
  , iaIndexedAttributes
  , iaObjectIdentifier
  ) where

import qualified Network.AWS.CloudDirectory.Types.AttributeKeyAndValue as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectIdentifier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents an index and an attached object.
--
-- /See:/ 'mkIndexAttachment' smart constructor.
data IndexAttachment = IndexAttachment'
  { indexedAttributes :: Core.Maybe [Types.AttributeKeyAndValue]
    -- ^ The indexed attribute values.
  , objectIdentifier :: Core.Maybe Types.ObjectIdentifier
    -- ^ In response to 'ListIndex' , the @ObjectIdentifier@ of the object attached to the index. In response to 'ListAttachedIndices' , the @ObjectIdentifier@ of the index attached to the object. This field will always contain the @ObjectIdentifier@ of the object on the opposite side of the attachment specified in the query.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'IndexAttachment' value with any optional fields omitted.
mkIndexAttachment
    :: IndexAttachment
mkIndexAttachment
  = IndexAttachment'{indexedAttributes = Core.Nothing,
                     objectIdentifier = Core.Nothing}

-- | The indexed attribute values.
--
-- /Note:/ Consider using 'indexedAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaIndexedAttributes :: Lens.Lens' IndexAttachment (Core.Maybe [Types.AttributeKeyAndValue])
iaIndexedAttributes = Lens.field @"indexedAttributes"
{-# INLINEABLE iaIndexedAttributes #-}
{-# DEPRECATED indexedAttributes "Use generic-lens or generic-optics with 'indexedAttributes' instead"  #-}

-- | In response to 'ListIndex' , the @ObjectIdentifier@ of the object attached to the index. In response to 'ListAttachedIndices' , the @ObjectIdentifier@ of the index attached to the object. This field will always contain the @ObjectIdentifier@ of the object on the opposite side of the attachment specified in the query.
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaObjectIdentifier :: Lens.Lens' IndexAttachment (Core.Maybe Types.ObjectIdentifier)
iaObjectIdentifier = Lens.field @"objectIdentifier"
{-# INLINEABLE iaObjectIdentifier #-}
{-# DEPRECATED objectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead"  #-}

instance Core.FromJSON IndexAttachment where
        parseJSON
          = Core.withObject "IndexAttachment" Core.$
              \ x ->
                IndexAttachment' Core.<$>
                  (x Core..:? "IndexedAttributes") Core.<*>
                    x Core..:? "ObjectIdentifier"
