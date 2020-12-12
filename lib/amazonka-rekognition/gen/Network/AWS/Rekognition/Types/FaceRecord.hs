{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.FaceRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.FaceRecord
  ( FaceRecord (..),

    -- * Smart constructor
    mkFaceRecord,

    -- * Lenses
    frFaceDetail,
    frFace,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.Face
import Network.AWS.Rekognition.Types.FaceDetail

-- | Object containing both the face metadata (stored in the backend database), and facial attributes that are detected but aren't stored in the database.
--
-- /See:/ 'mkFaceRecord' smart constructor.
data FaceRecord = FaceRecord'
  { faceDetail :: Lude.Maybe FaceDetail,
    face :: Lude.Maybe Face
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FaceRecord' with the minimum fields required to make a request.
--
-- * 'face' - Describes the face properties such as the bounding box, face ID, image ID of the input image, and external image ID that you assigned.
-- * 'faceDetail' - Structure containing attributes of the face that the algorithm detected.
mkFaceRecord ::
  FaceRecord
mkFaceRecord =
  FaceRecord' {faceDetail = Lude.Nothing, face = Lude.Nothing}

-- | Structure containing attributes of the face that the algorithm detected.
--
-- /Note:/ Consider using 'faceDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frFaceDetail :: Lens.Lens' FaceRecord (Lude.Maybe FaceDetail)
frFaceDetail = Lens.lens (faceDetail :: FaceRecord -> Lude.Maybe FaceDetail) (\s a -> s {faceDetail = a} :: FaceRecord)
{-# DEPRECATED frFaceDetail "Use generic-lens or generic-optics with 'faceDetail' instead." #-}

-- | Describes the face properties such as the bounding box, face ID, image ID of the input image, and external image ID that you assigned.
--
-- /Note:/ Consider using 'face' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frFace :: Lens.Lens' FaceRecord (Lude.Maybe Face)
frFace = Lens.lens (face :: FaceRecord -> Lude.Maybe Face) (\s a -> s {face = a} :: FaceRecord)
{-# DEPRECATED frFace "Use generic-lens or generic-optics with 'face' instead." #-}

instance Lude.FromJSON FaceRecord where
  parseJSON =
    Lude.withObject
      "FaceRecord"
      ( \x ->
          FaceRecord'
            Lude.<$> (x Lude..:? "FaceDetail") Lude.<*> (x Lude..:? "Face")
      )
