{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Sunglasses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.Sunglasses
  ( Sunglasses (..)
  -- * Smart constructor
  , mkSunglasses
  -- * Lenses
  , sfConfidence
  , sfValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Indicates whether or not the face is wearing sunglasses, and the confidence level in the determination.
--
-- /See:/ 'mkSunglasses' smart constructor.
data Sunglasses = Sunglasses'
  { confidence :: Core.Maybe Core.Double
    -- ^ Level of confidence in the determination.
  , value :: Core.Maybe Core.Bool
    -- ^ Boolean value that indicates whether the face is wearing sunglasses or not.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Sunglasses' value with any optional fields omitted.
mkSunglasses
    :: Sunglasses
mkSunglasses
  = Sunglasses'{confidence = Core.Nothing, value = Core.Nothing}

-- | Level of confidence in the determination.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfConfidence :: Lens.Lens' Sunglasses (Core.Maybe Core.Double)
sfConfidence = Lens.field @"confidence"
{-# INLINEABLE sfConfidence #-}
{-# DEPRECATED confidence "Use generic-lens or generic-optics with 'confidence' instead"  #-}

-- | Boolean value that indicates whether the face is wearing sunglasses or not.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfValue :: Lens.Lens' Sunglasses (Core.Maybe Core.Bool)
sfValue = Lens.field @"value"
{-# INLINEABLE sfValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON Sunglasses where
        parseJSON
          = Core.withObject "Sunglasses" Core.$
              \ x ->
                Sunglasses' Core.<$>
                  (x Core..:? "Confidence") Core.<*> x Core..:? "Value"
