{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Smile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.Smile
  ( Smile (..)
  -- * Smart constructor
  , mkSmile
  -- * Lenses
  , sConfidence
  , sValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Indicates whether or not the face is smiling, and the confidence level in the determination.
--
-- /See:/ 'mkSmile' smart constructor.
data Smile = Smile'
  { confidence :: Core.Maybe Core.Double
    -- ^ Level of confidence in the determination.
  , value :: Core.Maybe Core.Bool
    -- ^ Boolean value that indicates whether the face is smiling or not.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Smile' value with any optional fields omitted.
mkSmile
    :: Smile
mkSmile = Smile'{confidence = Core.Nothing, value = Core.Nothing}

-- | Level of confidence in the determination.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sConfidence :: Lens.Lens' Smile (Core.Maybe Core.Double)
sConfidence = Lens.field @"confidence"
{-# INLINEABLE sConfidence #-}
{-# DEPRECATED confidence "Use generic-lens or generic-optics with 'confidence' instead"  #-}

-- | Boolean value that indicates whether the face is smiling or not.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sValue :: Lens.Lens' Smile (Core.Maybe Core.Bool)
sValue = Lens.field @"value"
{-# INLINEABLE sValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON Smile where
        parseJSON
          = Core.withObject "Smile" Core.$
              \ x ->
                Smile' Core.<$>
                  (x Core..:? "Confidence") Core.<*> x Core..:? "Value"
