{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.MouthOpen
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.MouthOpen
  ( MouthOpen (..)
  -- * Smart constructor
  , mkMouthOpen
  -- * Lenses
  , moConfidence
  , moValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Indicates whether or not the mouth on the face is open, and the confidence level in the determination.
--
-- /See:/ 'mkMouthOpen' smart constructor.
data MouthOpen = MouthOpen'
  { confidence :: Core.Maybe Core.Double
    -- ^ Level of confidence in the determination.
  , value :: Core.Maybe Core.Bool
    -- ^ Boolean value that indicates whether the mouth on the face is open or not.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MouthOpen' value with any optional fields omitted.
mkMouthOpen
    :: MouthOpen
mkMouthOpen
  = MouthOpen'{confidence = Core.Nothing, value = Core.Nothing}

-- | Level of confidence in the determination.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
moConfidence :: Lens.Lens' MouthOpen (Core.Maybe Core.Double)
moConfidence = Lens.field @"confidence"
{-# INLINEABLE moConfidence #-}
{-# DEPRECATED confidence "Use generic-lens or generic-optics with 'confidence' instead"  #-}

-- | Boolean value that indicates whether the mouth on the face is open or not.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
moValue :: Lens.Lens' MouthOpen (Core.Maybe Core.Bool)
moValue = Lens.field @"value"
{-# INLINEABLE moValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON MouthOpen where
        parseJSON
          = Core.withObject "MouthOpen" Core.$
              \ x ->
                MouthOpen' Core.<$>
                  (x Core..:? "Confidence") Core.<*> x Core..:? "Value"
