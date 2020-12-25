{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Beard
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Beard
  ( Beard (..),

    -- * Smart constructor
    mkBeard,

    -- * Lenses
    bConfidence,
    bValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Indicates whether or not the face has a beard, and the confidence level in the determination.
--
-- /See:/ 'mkBeard' smart constructor.
data Beard = Beard'
  { -- | Level of confidence in the determination.
    confidence :: Core.Maybe Core.Double,
    -- | Boolean value that indicates whether the face has beard or not.
    value :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Beard' value with any optional fields omitted.
mkBeard ::
  Beard
mkBeard = Beard' {confidence = Core.Nothing, value = Core.Nothing}

-- | Level of confidence in the determination.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bConfidence :: Lens.Lens' Beard (Core.Maybe Core.Double)
bConfidence = Lens.field @"confidence"
{-# DEPRECATED bConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | Boolean value that indicates whether the face has beard or not.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bValue :: Lens.Lens' Beard (Core.Maybe Core.Bool)
bValue = Lens.field @"value"
{-# DEPRECATED bValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON Beard where
  parseJSON =
    Core.withObject "Beard" Core.$
      \x ->
        Beard'
          Core.<$> (x Core..:? "Confidence") Core.<*> (x Core..:? "Value")
