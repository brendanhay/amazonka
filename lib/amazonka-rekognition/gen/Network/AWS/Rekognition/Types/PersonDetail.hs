{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.PersonDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.PersonDetail
  ( PersonDetail (..),

    -- * Smart constructor
    mkPersonDetail,

    -- * Lenses
    pdBoundingBox,
    pdIndex,
    pdFace,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.BoundingBox
import Network.AWS.Rekognition.Types.FaceDetail

-- | Details about a person detected in a video analysis request.
--
-- /See:/ 'mkPersonDetail' smart constructor.
data PersonDetail = PersonDetail'
  { boundingBox ::
      Lude.Maybe BoundingBox,
    index :: Lude.Maybe Lude.Integer,
    face :: Lude.Maybe FaceDetail
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PersonDetail' with the minimum fields required to make a request.
--
-- * 'boundingBox' - Bounding box around the detected person.
-- * 'face' - Face details for the detected person.
-- * 'index' - Identifier for the person detected person within a video. Use to keep track of the person throughout the video. The identifier is not stored by Amazon Rekognition.
mkPersonDetail ::
  PersonDetail
mkPersonDetail =
  PersonDetail'
    { boundingBox = Lude.Nothing,
      index = Lude.Nothing,
      face = Lude.Nothing
    }

-- | Bounding box around the detected person.
--
-- /Note:/ Consider using 'boundingBox' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdBoundingBox :: Lens.Lens' PersonDetail (Lude.Maybe BoundingBox)
pdBoundingBox = Lens.lens (boundingBox :: PersonDetail -> Lude.Maybe BoundingBox) (\s a -> s {boundingBox = a} :: PersonDetail)
{-# DEPRECATED pdBoundingBox "Use generic-lens or generic-optics with 'boundingBox' instead." #-}

-- | Identifier for the person detected person within a video. Use to keep track of the person throughout the video. The identifier is not stored by Amazon Rekognition.
--
-- /Note:/ Consider using 'index' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdIndex :: Lens.Lens' PersonDetail (Lude.Maybe Lude.Integer)
pdIndex = Lens.lens (index :: PersonDetail -> Lude.Maybe Lude.Integer) (\s a -> s {index = a} :: PersonDetail)
{-# DEPRECATED pdIndex "Use generic-lens or generic-optics with 'index' instead." #-}

-- | Face details for the detected person.
--
-- /Note:/ Consider using 'face' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdFace :: Lens.Lens' PersonDetail (Lude.Maybe FaceDetail)
pdFace = Lens.lens (face :: PersonDetail -> Lude.Maybe FaceDetail) (\s a -> s {face = a} :: PersonDetail)
{-# DEPRECATED pdFace "Use generic-lens or generic-optics with 'face' instead." #-}

instance Lude.FromJSON PersonDetail where
  parseJSON =
    Lude.withObject
      "PersonDetail"
      ( \x ->
          PersonDetail'
            Lude.<$> (x Lude..:? "BoundingBox")
            Lude.<*> (x Lude..:? "Index")
            Lude.<*> (x Lude..:? "Face")
      )
