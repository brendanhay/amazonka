{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AssetPropertyValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AssetPropertyValue
  ( AssetPropertyValue (..)
  -- * Smart constructor
  , mkAssetPropertyValue
  -- * Lenses
  , apvValue
  , apvTimestamp
  , apvQuality
  ) where

import qualified Network.AWS.IoT.Types.AssetPropertyQuality as Types
import qualified Network.AWS.IoT.Types.AssetPropertyTimestamp as Types
import qualified Network.AWS.IoT.Types.AssetPropertyVariant as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An asset property value entry containing the following information.
--
-- /See:/ 'mkAssetPropertyValue' smart constructor.
data AssetPropertyValue = AssetPropertyValue'
  { value :: Types.AssetPropertyVariant
    -- ^ The value of the asset property.
  , timestamp :: Types.AssetPropertyTimestamp
    -- ^ The asset property value timestamp.
  , quality :: Core.Maybe Types.AssetPropertyQuality
    -- ^ Optional. A string that describes the quality of the value. Accepts substitution templates. Must be @GOOD@ , @BAD@ , or @UNCERTAIN@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssetPropertyValue' value with any optional fields omitted.
mkAssetPropertyValue
    :: Types.AssetPropertyVariant -- ^ 'value'
    -> Types.AssetPropertyTimestamp -- ^ 'timestamp'
    -> AssetPropertyValue
mkAssetPropertyValue value timestamp
  = AssetPropertyValue'{value, timestamp, quality = Core.Nothing}

-- | The value of the asset property.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apvValue :: Lens.Lens' AssetPropertyValue Types.AssetPropertyVariant
apvValue = Lens.field @"value"
{-# INLINEABLE apvValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

-- | The asset property value timestamp.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apvTimestamp :: Lens.Lens' AssetPropertyValue Types.AssetPropertyTimestamp
apvTimestamp = Lens.field @"timestamp"
{-# INLINEABLE apvTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

-- | Optional. A string that describes the quality of the value. Accepts substitution templates. Must be @GOOD@ , @BAD@ , or @UNCERTAIN@ .
--
-- /Note:/ Consider using 'quality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apvQuality :: Lens.Lens' AssetPropertyValue (Core.Maybe Types.AssetPropertyQuality)
apvQuality = Lens.field @"quality"
{-# INLINEABLE apvQuality #-}
{-# DEPRECATED quality "Use generic-lens or generic-optics with 'quality' instead"  #-}

instance Core.FromJSON AssetPropertyValue where
        toJSON AssetPropertyValue{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("value" Core..= value),
                  Core.Just ("timestamp" Core..= timestamp),
                  ("quality" Core..=) Core.<$> quality])

instance Core.FromJSON AssetPropertyValue where
        parseJSON
          = Core.withObject "AssetPropertyValue" Core.$
              \ x ->
                AssetPropertyValue' Core.<$>
                  (x Core..: "value") Core.<*> x Core..: "timestamp" Core.<*>
                    x Core..:? "quality"
