{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AssetPropertyVariant
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AssetPropertyVariant
  ( AssetPropertyVariant (..)
  -- * Smart constructor
  , mkAssetPropertyVariant
  -- * Lenses
  , apvBooleanValue
  , apvDoubleValue
  , apvIntegerValue
  , apvStringValue
  ) where

import qualified Network.AWS.IoT.Types.AssetPropertyDoubleValue as Types
import qualified Network.AWS.IoT.Types.AssetPropertyIntegerValue as Types
import qualified Network.AWS.IoT.Types.AssetPropertyStringValue as Types
import qualified Network.AWS.IoT.Types.BooleanValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains an asset property value (of a single type).
--
-- /See:/ 'mkAssetPropertyVariant' smart constructor.
data AssetPropertyVariant = AssetPropertyVariant'
  { booleanValue :: Core.Maybe Types.BooleanValue
    -- ^ Optional. A string that contains the boolean value (@true@ or @false@ ) of the value entry. Accepts substitution templates.
  , doubleValue :: Core.Maybe Types.AssetPropertyDoubleValue
    -- ^ Optional. A string that contains the double value of the value entry. Accepts substitution templates.
  , integerValue :: Core.Maybe Types.AssetPropertyIntegerValue
    -- ^ Optional. A string that contains the integer value of the value entry. Accepts substitution templates.
  , stringValue :: Core.Maybe Types.AssetPropertyStringValue
    -- ^ Optional. The string value of the value entry. Accepts substitution templates.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssetPropertyVariant' value with any optional fields omitted.
mkAssetPropertyVariant
    :: AssetPropertyVariant
mkAssetPropertyVariant
  = AssetPropertyVariant'{booleanValue = Core.Nothing,
                          doubleValue = Core.Nothing, integerValue = Core.Nothing,
                          stringValue = Core.Nothing}

-- | Optional. A string that contains the boolean value (@true@ or @false@ ) of the value entry. Accepts substitution templates.
--
-- /Note:/ Consider using 'booleanValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apvBooleanValue :: Lens.Lens' AssetPropertyVariant (Core.Maybe Types.BooleanValue)
apvBooleanValue = Lens.field @"booleanValue"
{-# INLINEABLE apvBooleanValue #-}
{-# DEPRECATED booleanValue "Use generic-lens or generic-optics with 'booleanValue' instead"  #-}

-- | Optional. A string that contains the double value of the value entry. Accepts substitution templates.
--
-- /Note:/ Consider using 'doubleValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apvDoubleValue :: Lens.Lens' AssetPropertyVariant (Core.Maybe Types.AssetPropertyDoubleValue)
apvDoubleValue = Lens.field @"doubleValue"
{-# INLINEABLE apvDoubleValue #-}
{-# DEPRECATED doubleValue "Use generic-lens or generic-optics with 'doubleValue' instead"  #-}

-- | Optional. A string that contains the integer value of the value entry. Accepts substitution templates.
--
-- /Note:/ Consider using 'integerValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apvIntegerValue :: Lens.Lens' AssetPropertyVariant (Core.Maybe Types.AssetPropertyIntegerValue)
apvIntegerValue = Lens.field @"integerValue"
{-# INLINEABLE apvIntegerValue #-}
{-# DEPRECATED integerValue "Use generic-lens or generic-optics with 'integerValue' instead"  #-}

-- | Optional. The string value of the value entry. Accepts substitution templates.
--
-- /Note:/ Consider using 'stringValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apvStringValue :: Lens.Lens' AssetPropertyVariant (Core.Maybe Types.AssetPropertyStringValue)
apvStringValue = Lens.field @"stringValue"
{-# INLINEABLE apvStringValue #-}
{-# DEPRECATED stringValue "Use generic-lens or generic-optics with 'stringValue' instead"  #-}

instance Core.FromJSON AssetPropertyVariant where
        toJSON AssetPropertyVariant{..}
          = Core.object
              (Core.catMaybes
                 [("booleanValue" Core..=) Core.<$> booleanValue,
                  ("doubleValue" Core..=) Core.<$> doubleValue,
                  ("integerValue" Core..=) Core.<$> integerValue,
                  ("stringValue" Core..=) Core.<$> stringValue])

instance Core.FromJSON AssetPropertyVariant where
        parseJSON
          = Core.withObject "AssetPropertyVariant" Core.$
              \ x ->
                AssetPropertyVariant' Core.<$>
                  (x Core..:? "booleanValue") Core.<*> x Core..:? "doubleValue"
                    Core.<*> x Core..:? "integerValue"
                    Core.<*> x Core..:? "stringValue"
