{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.EyeOpen
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.EyeOpen
  ( EyeOpen (..),

    -- * Smart constructor
    mkEyeOpen,

    -- * Lenses
    eoConfidence,
    eoValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Indicates whether or not the eyes on the face are open, and the confidence level in the determination.
--
-- /See:/ 'mkEyeOpen' smart constructor.
data EyeOpen = EyeOpen'
  { -- | Level of confidence in the determination.
    confidence :: Core.Maybe Core.Double,
    -- | Boolean value that indicates whether the eyes on the face are open.
    value :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EyeOpen' value with any optional fields omitted.
mkEyeOpen ::
  EyeOpen
mkEyeOpen =
  EyeOpen' {confidence = Core.Nothing, value = Core.Nothing}

-- | Level of confidence in the determination.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoConfidence :: Lens.Lens' EyeOpen (Core.Maybe Core.Double)
eoConfidence = Lens.field @"confidence"
{-# DEPRECATED eoConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | Boolean value that indicates whether the eyes on the face are open.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoValue :: Lens.Lens' EyeOpen (Core.Maybe Core.Bool)
eoValue = Lens.field @"value"
{-# DEPRECATED eoValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON EyeOpen where
  parseJSON =
    Core.withObject "EyeOpen" Core.$
      \x ->
        EyeOpen'
          Core.<$> (x Core..:? "Confidence") Core.<*> (x Core..:? "Value")
