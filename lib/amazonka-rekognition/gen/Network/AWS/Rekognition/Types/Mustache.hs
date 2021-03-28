{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Mustache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.Mustache
  ( Mustache (..)
  -- * Smart constructor
  , mkMustache
  -- * Lenses
  , mConfidence
  , mValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Indicates whether or not the face has a mustache, and the confidence level in the determination.
--
-- /See:/ 'mkMustache' smart constructor.
data Mustache = Mustache'
  { confidence :: Core.Maybe Core.Double
    -- ^ Level of confidence in the determination.
  , value :: Core.Maybe Core.Bool
    -- ^ Boolean value that indicates whether the face has mustache or not.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Mustache' value with any optional fields omitted.
mkMustache
    :: Mustache
mkMustache
  = Mustache'{confidence = Core.Nothing, value = Core.Nothing}

-- | Level of confidence in the determination.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mConfidence :: Lens.Lens' Mustache (Core.Maybe Core.Double)
mConfidence = Lens.field @"confidence"
{-# INLINEABLE mConfidence #-}
{-# DEPRECATED confidence "Use generic-lens or generic-optics with 'confidence' instead"  #-}

-- | Boolean value that indicates whether the face has mustache or not.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mValue :: Lens.Lens' Mustache (Core.Maybe Core.Bool)
mValue = Lens.field @"value"
{-# INLINEABLE mValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON Mustache where
        parseJSON
          = Core.withObject "Mustache" Core.$
              \ x ->
                Mustache' Core.<$>
                  (x Core..:? "Confidence") Core.<*> x Core..:? "Value"
