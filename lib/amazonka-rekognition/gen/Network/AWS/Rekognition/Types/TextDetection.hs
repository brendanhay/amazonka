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
    tdDetectedText,
    tdConfidence,
    tdGeometry,
    tdId,
    tdType,
    tdParentId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.Geometry
import Network.AWS.Rekognition.Types.TextTypes

-- | Information about a word or line of text detected by 'DetectText' .
--
-- The @DetectedText@ field contains the text that Amazon Rekognition detected in the image.
-- Every word and line has an identifier (@Id@ ). Each word belongs to a line and has a parent identifier (@ParentId@ ) that identifies the line of text in which the word appears. The word @Id@ is also an index for the word within a line of words.
-- For more information, see Detecting Text in the Amazon Rekognition Developer Guide.
--
-- /See:/ 'mkTextDetection' smart constructor.
data TextDetection = TextDetection'
  { detectedText ::
      Lude.Maybe Lude.Text,
    confidence :: Lude.Maybe Lude.Double,
    geometry :: Lude.Maybe Geometry,
    id :: Lude.Maybe Lude.Natural,
    type' :: Lude.Maybe TextTypes,
    parentId :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TextDetection' with the minimum fields required to make a request.
--
-- * 'confidence' - The confidence that Amazon Rekognition has in the accuracy of the detected text and the accuracy of the geometry points around the detected text.
-- * 'detectedText' - The word or line of text recognized by Amazon Rekognition.
-- * 'geometry' - The location of the detected text on the image. Includes an axis aligned coarse bounding box surrounding the text and a finer grain polygon for more accurate spatial information.
-- * 'id' - The identifier for the detected text. The identifier is only unique for a single call to @DetectText@ .
-- * 'parentId' - The Parent identifier for the detected text identified by the value of @ID@ . If the type of detected text is @LINE@ , the value of @ParentId@ is @Null@ .
-- * 'type'' - The type of text that was detected.
mkTextDetection ::
  TextDetection
mkTextDetection =
  TextDetection'
    { detectedText = Lude.Nothing,
      confidence = Lude.Nothing,
      geometry = Lude.Nothing,
      id = Lude.Nothing,
      type' = Lude.Nothing,
      parentId = Lude.Nothing
    }

-- | The word or line of text recognized by Amazon Rekognition.
--
-- /Note:/ Consider using 'detectedText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdDetectedText :: Lens.Lens' TextDetection (Lude.Maybe Lude.Text)
tdDetectedText = Lens.lens (detectedText :: TextDetection -> Lude.Maybe Lude.Text) (\s a -> s {detectedText = a} :: TextDetection)
{-# DEPRECATED tdDetectedText "Use generic-lens or generic-optics with 'detectedText' instead." #-}

-- | The confidence that Amazon Rekognition has in the accuracy of the detected text and the accuracy of the geometry points around the detected text.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdConfidence :: Lens.Lens' TextDetection (Lude.Maybe Lude.Double)
tdConfidence = Lens.lens (confidence :: TextDetection -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: TextDetection)
{-# DEPRECATED tdConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | The location of the detected text on the image. Includes an axis aligned coarse bounding box surrounding the text and a finer grain polygon for more accurate spatial information.
--
-- /Note:/ Consider using 'geometry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdGeometry :: Lens.Lens' TextDetection (Lude.Maybe Geometry)
tdGeometry = Lens.lens (geometry :: TextDetection -> Lude.Maybe Geometry) (\s a -> s {geometry = a} :: TextDetection)
{-# DEPRECATED tdGeometry "Use generic-lens or generic-optics with 'geometry' instead." #-}

-- | The identifier for the detected text. The identifier is only unique for a single call to @DetectText@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdId :: Lens.Lens' TextDetection (Lude.Maybe Lude.Natural)
tdId = Lens.lens (id :: TextDetection -> Lude.Maybe Lude.Natural) (\s a -> s {id = a} :: TextDetection)
{-# DEPRECATED tdId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of text that was detected.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdType :: Lens.Lens' TextDetection (Lude.Maybe TextTypes)
tdType = Lens.lens (type' :: TextDetection -> Lude.Maybe TextTypes) (\s a -> s {type' = a} :: TextDetection)
{-# DEPRECATED tdType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The Parent identifier for the detected text identified by the value of @ID@ . If the type of detected text is @LINE@ , the value of @ParentId@ is @Null@ .
--
-- /Note:/ Consider using 'parentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdParentId :: Lens.Lens' TextDetection (Lude.Maybe Lude.Natural)
tdParentId = Lens.lens (parentId :: TextDetection -> Lude.Maybe Lude.Natural) (\s a -> s {parentId = a} :: TextDetection)
{-# DEPRECATED tdParentId "Use generic-lens or generic-optics with 'parentId' instead." #-}

instance Lude.FromJSON TextDetection where
  parseJSON =
    Lude.withObject
      "TextDetection"
      ( \x ->
          TextDetection'
            Lude.<$> (x Lude..:? "DetectedText")
            Lude.<*> (x Lude..:? "Confidence")
            Lude.<*> (x Lude..:? "Geometry")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "ParentId")
      )
