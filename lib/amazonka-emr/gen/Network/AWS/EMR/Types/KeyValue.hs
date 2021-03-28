{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.KeyValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.KeyValue
  ( KeyValue (..)
  -- * Smart constructor
  , mkKeyValue
  -- * Lenses
  , kvKey
  , kvValue
  ) where

import qualified Network.AWS.EMR.Types.XmlString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A key-value pair.
--
-- /See:/ 'mkKeyValue' smart constructor.
data KeyValue = KeyValue'
  { key :: Core.Maybe Types.XmlString
    -- ^ The unique identifier of a key-value pair.
  , value :: Core.Maybe Types.XmlString
    -- ^ The value part of the identified key.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KeyValue' value with any optional fields omitted.
mkKeyValue
    :: KeyValue
mkKeyValue = KeyValue'{key = Core.Nothing, value = Core.Nothing}

-- | The unique identifier of a key-value pair.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kvKey :: Lens.Lens' KeyValue (Core.Maybe Types.XmlString)
kvKey = Lens.field @"key"
{-# INLINEABLE kvKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The value part of the identified key.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kvValue :: Lens.Lens' KeyValue (Core.Maybe Types.XmlString)
kvValue = Lens.field @"value"
{-# INLINEABLE kvValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON KeyValue where
        toJSON KeyValue{..}
          = Core.object
              (Core.catMaybes
                 [("Key" Core..=) Core.<$> key, ("Value" Core..=) Core.<$> value])
