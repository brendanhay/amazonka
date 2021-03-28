{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Face
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.Face
  ( Face (..)
  -- * Smart constructor
  , mkFace
  -- * Lenses
  , fBoundingBox
  , fConfidence
  , fExternalImageId
  , fFaceId
  , fImageId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.BoundingBox as Types
import qualified Network.AWS.Rekognition.Types.ExternalImageId as Types
import qualified Network.AWS.Rekognition.Types.FaceId as Types
import qualified Network.AWS.Rekognition.Types.ImageId as Types

-- | Describes the face properties such as the bounding box, face ID, image ID of the input image, and external image ID that you assigned. 
--
-- /See:/ 'mkFace' smart constructor.
data Face = Face'
  { boundingBox :: Core.Maybe Types.BoundingBox
    -- ^ Bounding box of the face.
  , confidence :: Core.Maybe Core.Double
    -- ^ Confidence level that the bounding box contains a face (and not a different object such as a tree).
  , externalImageId :: Core.Maybe Types.ExternalImageId
    -- ^ Identifier that you assign to all the faces in the input image.
  , faceId :: Core.Maybe Types.FaceId
    -- ^ Unique identifier that Amazon Rekognition assigns to the face.
  , imageId :: Core.Maybe Types.ImageId
    -- ^ Unique identifier that Amazon Rekognition assigns to the input image.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Face' value with any optional fields omitted.
mkFace
    :: Face
mkFace
  = Face'{boundingBox = Core.Nothing, confidence = Core.Nothing,
          externalImageId = Core.Nothing, faceId = Core.Nothing,
          imageId = Core.Nothing}

-- | Bounding box of the face.
--
-- /Note:/ Consider using 'boundingBox' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fBoundingBox :: Lens.Lens' Face (Core.Maybe Types.BoundingBox)
fBoundingBox = Lens.field @"boundingBox"
{-# INLINEABLE fBoundingBox #-}
{-# DEPRECATED boundingBox "Use generic-lens or generic-optics with 'boundingBox' instead"  #-}

-- | Confidence level that the bounding box contains a face (and not a different object such as a tree).
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fConfidence :: Lens.Lens' Face (Core.Maybe Core.Double)
fConfidence = Lens.field @"confidence"
{-# INLINEABLE fConfidence #-}
{-# DEPRECATED confidence "Use generic-lens or generic-optics with 'confidence' instead"  #-}

-- | Identifier that you assign to all the faces in the input image.
--
-- /Note:/ Consider using 'externalImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fExternalImageId :: Lens.Lens' Face (Core.Maybe Types.ExternalImageId)
fExternalImageId = Lens.field @"externalImageId"
{-# INLINEABLE fExternalImageId #-}
{-# DEPRECATED externalImageId "Use generic-lens or generic-optics with 'externalImageId' instead"  #-}

-- | Unique identifier that Amazon Rekognition assigns to the face.
--
-- /Note:/ Consider using 'faceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fFaceId :: Lens.Lens' Face (Core.Maybe Types.FaceId)
fFaceId = Lens.field @"faceId"
{-# INLINEABLE fFaceId #-}
{-# DEPRECATED faceId "Use generic-lens or generic-optics with 'faceId' instead"  #-}

-- | Unique identifier that Amazon Rekognition assigns to the input image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fImageId :: Lens.Lens' Face (Core.Maybe Types.ImageId)
fImageId = Lens.field @"imageId"
{-# INLINEABLE fImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

instance Core.FromJSON Face where
        parseJSON
          = Core.withObject "Face" Core.$
              \ x ->
                Face' Core.<$>
                  (x Core..:? "BoundingBox") Core.<*> x Core..:? "Confidence"
                    Core.<*> x Core..:? "ExternalImageId"
                    Core.<*> x Core..:? "FaceId"
                    Core.<*> x Core..:? "ImageId"
