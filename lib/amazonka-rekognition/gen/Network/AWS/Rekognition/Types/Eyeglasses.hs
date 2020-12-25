{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Eyeglasses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Eyeglasses
  ( Eyeglasses (..),

    -- * Smart constructor
    mkEyeglasses,

    -- * Lenses
    efConfidence,
    efValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Indicates whether or not the face is wearing eye glasses, and the confidence level in the determination.
--
-- /See:/ 'mkEyeglasses' smart constructor.
data Eyeglasses = Eyeglasses'
  { -- | Level of confidence in the determination.
    confidence :: Core.Maybe Core.Double,
    -- | Boolean value that indicates whether the face is wearing eye glasses or not.
    value :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Eyeglasses' value with any optional fields omitted.
mkEyeglasses ::
  Eyeglasses
mkEyeglasses =
  Eyeglasses' {confidence = Core.Nothing, value = Core.Nothing}

-- | Level of confidence in the determination.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efConfidence :: Lens.Lens' Eyeglasses (Core.Maybe Core.Double)
efConfidence = Lens.field @"confidence"
{-# DEPRECATED efConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | Boolean value that indicates whether the face is wearing eye glasses or not.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efValue :: Lens.Lens' Eyeglasses (Core.Maybe Core.Bool)
efValue = Lens.field @"value"
{-# DEPRECATED efValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON Eyeglasses where
  parseJSON =
    Core.withObject "Eyeglasses" Core.$
      \x ->
        Eyeglasses'
          Core.<$> (x Core..:? "Confidence") Core.<*> (x Core..:? "Value")
