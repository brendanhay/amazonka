{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.TextDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.TextDetection
  ( TextDetection (..),

    -- * Smart constructor
    mkTextDetection,

    -- * Lenses
    tdConfidence,
    tdDetectedText,
    tdGeometry,
    tdId,
    tdParentId,
    tdType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.Geometry as Types
import qualified Network.AWS.Rekognition.Types.String as Types
import qualified Network.AWS.Rekognition.Types.TextTypes as Types

-- | Information about a word or line of text detected by 'DetectText' .
--
-- The @DetectedText@ field contains the text that Amazon Rekognition detected in the image.
-- Every word and line has an identifier (@Id@ ). Each word belongs to a line and has a parent identifier (@ParentId@ ) that identifies the line of text in which the word appears. The word @Id@ is also an index for the word within a line of words.
-- For more information, see Detecting Text in the Amazon Rekognition Developer Guide.
--
-- /See:/ 'mkTextDetection' smart constructor.
data TextDetection = TextDetection'
  { -- | The confidence that Amazon Rekognition has in the accuracy of the detected text and the accuracy of the geometry points around the detected text.
    confidence :: Core.Maybe Core.Double,
    -- | The word or line of text recognized by Amazon Rekognition.
    detectedText :: Core.Maybe Types.String,
    -- | The location of the detected text on the image. Includes an axis aligned coarse bounding box surrounding the text and a finer grain polygon for more accurate spatial information.
    geometry :: Core.Maybe Types.Geometry,
    -- | The identifier for the detected text. The identifier is only unique for a single call to @DetectText@ .
    id :: Core.Maybe Core.Natural,
    -- | The Parent identifier for the detected text identified by the value of @ID@ . If the type of detected text is @LINE@ , the value of @ParentId@ is @Null@ .
    parentId :: Core.Maybe Core.Natural,
    -- | The type of text that was detected.
    type' :: Core.Maybe Types.TextTypes
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TextDetection' value with any optional fields omitted.
mkTextDetection ::
  TextDetection
mkTextDetection =
  TextDetection'
    { confidence = Core.Nothing,
      detectedText = Core.Nothing,
      geometry = Core.Nothing,
      id = Core.Nothing,
      parentId = Core.Nothing,
      type' = Core.Nothing
    }

-- | The confidence that Amazon Rekognition has in the accuracy of the detected text and the accuracy of the geometry points around the detected text.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdConfidence :: Lens.Lens' TextDetection (Core.Maybe Core.Double)
tdConfidence = Lens.field @"confidence"
{-# DEPRECATED tdConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | The word or line of text recognized by Amazon Rekognition.
--
-- /Note:/ Consider using 'detectedText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdDetectedText :: Lens.Lens' TextDetection (Core.Maybe Types.String)
tdDetectedText = Lens.field @"detectedText"
{-# DEPRECATED tdDetectedText "Use generic-lens or generic-optics with 'detectedText' instead." #-}

-- | The location of the detected text on the image. Includes an axis aligned coarse bounding box surrounding the text and a finer grain polygon for more accurate spatial information.
--
-- /Note:/ Consider using 'geometry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdGeometry :: Lens.Lens' TextDetection (Core.Maybe Types.Geometry)
tdGeometry = Lens.field @"geometry"
{-# DEPRECATED tdGeometry "Use generic-lens or generic-optics with 'geometry' instead." #-}

-- | The identifier for the detected text. The identifier is only unique for a single call to @DetectText@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdId :: Lens.Lens' TextDetection (Core.Maybe Core.Natural)
tdId = Lens.field @"id"
{-# DEPRECATED tdId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The Parent identifier for the detected text identified by the value of @ID@ . If the type of detected text is @LINE@ , the value of @ParentId@ is @Null@ .
--
-- /Note:/ Consider using 'parentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdParentId :: Lens.Lens' TextDetection (Core.Maybe Core.Natural)
tdParentId = Lens.field @"parentId"
{-# DEPRECATED tdParentId "Use generic-lens or generic-optics with 'parentId' instead." #-}

-- | The type of text that was detected.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdType :: Lens.Lens' TextDetection (Core.Maybe Types.TextTypes)
tdType = Lens.field @"type'"
{-# DEPRECATED tdType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON TextDetection where
  parseJSON =
    Core.withObject "TextDetection" Core.$
      \x ->
        TextDetection'
          Core.<$> (x Core..:? "Confidence")
          Core.<*> (x Core..:? "DetectedText")
          Core.<*> (x Core..:? "Geometry")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "ParentId")
          Core.<*> (x Core..:? "Type")
