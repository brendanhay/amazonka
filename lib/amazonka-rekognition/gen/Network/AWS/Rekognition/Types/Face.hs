-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Face
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Face
  ( Face (..),

    -- * Smart constructor
    mkFace,

    -- * Lenses
    fFaceId,
    fBoundingBox,
    fExternalImageId,
    fConfidence,
    fImageId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.BoundingBox

-- | Describes the face properties such as the bounding box, face ID, image ID of the input image, and external image ID that you assigned.
--
-- /See:/ 'mkFace' smart constructor.
data Face = Face'
  { faceId :: Lude.Maybe Lude.Text,
    boundingBox :: Lude.Maybe BoundingBox,
    externalImageId :: Lude.Maybe Lude.Text,
    confidence :: Lude.Maybe Lude.Double,
    imageId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Face' with the minimum fields required to make a request.
--
-- * 'boundingBox' - Bounding box of the face.
-- * 'confidence' - Confidence level that the bounding box contains a face (and not a different object such as a tree).
-- * 'externalImageId' - Identifier that you assign to all the faces in the input image.
-- * 'faceId' - Unique identifier that Amazon Rekognition assigns to the face.
-- * 'imageId' - Unique identifier that Amazon Rekognition assigns to the input image.
mkFace ::
  Face
mkFace =
  Face'
    { faceId = Lude.Nothing,
      boundingBox = Lude.Nothing,
      externalImageId = Lude.Nothing,
      confidence = Lude.Nothing,
      imageId = Lude.Nothing
    }

-- | Unique identifier that Amazon Rekognition assigns to the face.
--
-- /Note:/ Consider using 'faceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fFaceId :: Lens.Lens' Face (Lude.Maybe Lude.Text)
fFaceId = Lens.lens (faceId :: Face -> Lude.Maybe Lude.Text) (\s a -> s {faceId = a} :: Face)
{-# DEPRECATED fFaceId "Use generic-lens or generic-optics with 'faceId' instead." #-}

-- | Bounding box of the face.
--
-- /Note:/ Consider using 'boundingBox' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fBoundingBox :: Lens.Lens' Face (Lude.Maybe BoundingBox)
fBoundingBox = Lens.lens (boundingBox :: Face -> Lude.Maybe BoundingBox) (\s a -> s {boundingBox = a} :: Face)
{-# DEPRECATED fBoundingBox "Use generic-lens or generic-optics with 'boundingBox' instead." #-}

-- | Identifier that you assign to all the faces in the input image.
--
-- /Note:/ Consider using 'externalImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fExternalImageId :: Lens.Lens' Face (Lude.Maybe Lude.Text)
fExternalImageId = Lens.lens (externalImageId :: Face -> Lude.Maybe Lude.Text) (\s a -> s {externalImageId = a} :: Face)
{-# DEPRECATED fExternalImageId "Use generic-lens or generic-optics with 'externalImageId' instead." #-}

-- | Confidence level that the bounding box contains a face (and not a different object such as a tree).
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fConfidence :: Lens.Lens' Face (Lude.Maybe Lude.Double)
fConfidence = Lens.lens (confidence :: Face -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: Face)
{-# DEPRECATED fConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | Unique identifier that Amazon Rekognition assigns to the input image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fImageId :: Lens.Lens' Face (Lude.Maybe Lude.Text)
fImageId = Lens.lens (imageId :: Face -> Lude.Maybe Lude.Text) (\s a -> s {imageId = a} :: Face)
{-# DEPRECATED fImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

instance Lude.FromJSON Face where
  parseJSON =
    Lude.withObject
      "Face"
      ( \x ->
          Face'
            Lude.<$> (x Lude..:? "FaceId")
            Lude.<*> (x Lude..:? "BoundingBox")
            Lude.<*> (x Lude..:? "ExternalImageId")
            Lude.<*> (x Lude..:? "Confidence")
            Lude.<*> (x Lude..:? "ImageId")
      )
