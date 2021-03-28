{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.AttributeKeyAndValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.AttributeKeyAndValue
  ( AttributeKeyAndValue (..)
  -- * Smart constructor
  , mkAttributeKeyAndValue
  -- * Lenses
  , akavKey
  , akavValue
  ) where

import qualified Network.AWS.CloudDirectory.Types.AttributeKey as Types
import qualified Network.AWS.CloudDirectory.Types.TypedAttributeValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The combination of an attribute key and an attribute value.
--
-- /See:/ 'mkAttributeKeyAndValue' smart constructor.
data AttributeKeyAndValue = AttributeKeyAndValue'
  { key :: Types.AttributeKey
    -- ^ The key of the attribute.
  , value :: Types.TypedAttributeValue
    -- ^ The value of the attribute.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AttributeKeyAndValue' value with any optional fields omitted.
mkAttributeKeyAndValue
    :: Types.AttributeKey -- ^ 'key'
    -> Types.TypedAttributeValue -- ^ 'value'
    -> AttributeKeyAndValue
mkAttributeKeyAndValue key value
  = AttributeKeyAndValue'{key, value}

-- | The key of the attribute.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akavKey :: Lens.Lens' AttributeKeyAndValue Types.AttributeKey
akavKey = Lens.field @"key"
{-# INLINEABLE akavKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The value of the attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akavValue :: Lens.Lens' AttributeKeyAndValue Types.TypedAttributeValue
akavValue = Lens.field @"value"
{-# INLINEABLE akavValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON AttributeKeyAndValue where
        toJSON AttributeKeyAndValue{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Key" Core..= key), Core.Just ("Value" Core..= value)])

instance Core.FromJSON AttributeKeyAndValue where
        parseJSON
          = Core.withObject "AttributeKeyAndValue" Core.$
              \ x ->
                AttributeKeyAndValue' Core.<$>
                  (x Core..: "Key") Core.<*> x Core..: "Value"
