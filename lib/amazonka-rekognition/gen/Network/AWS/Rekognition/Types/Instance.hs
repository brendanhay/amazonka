{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Instance
  ( Instance (..),

    -- * Smart constructor
    mkInstance,

    -- * Lenses
    iBoundingBox,
    iConfidence,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.BoundingBox as Types

-- | An instance of a label returned by Amazon Rekognition Image ('DetectLabels' ) or by Amazon Rekognition Video ('GetLabelDetection' ).
--
-- /See:/ 'mkInstance' smart constructor.
data Instance = Instance'
  { -- | The position of the label instance on the image.
    boundingBox :: Core.Maybe Types.BoundingBox,
    -- | The confidence that Amazon Rekognition has in the accuracy of the bounding box.
    confidence :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Instance' value with any optional fields omitted.
mkInstance ::
  Instance
mkInstance =
  Instance' {boundingBox = Core.Nothing, confidence = Core.Nothing}

-- | The position of the label instance on the image.
--
-- /Note:/ Consider using 'boundingBox' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iBoundingBox :: Lens.Lens' Instance (Core.Maybe Types.BoundingBox)
iBoundingBox = Lens.field @"boundingBox"
{-# DEPRECATED iBoundingBox "Use generic-lens or generic-optics with 'boundingBox' instead." #-}

-- | The confidence that Amazon Rekognition has in the accuracy of the bounding box.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iConfidence :: Lens.Lens' Instance (Core.Maybe Core.Double)
iConfidence = Lens.field @"confidence"
{-# DEPRECATED iConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

instance Core.FromJSON Instance where
  parseJSON =
    Core.withObject "Instance" Core.$
      \x ->
        Instance'
          Core.<$> (x Core..:? "BoundingBox") Core.<*> (x Core..:? "Confidence")
