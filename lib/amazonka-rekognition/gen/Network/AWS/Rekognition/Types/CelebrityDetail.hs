-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.CelebrityDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.CelebrityDetail
  ( CelebrityDetail (..),

    -- * Smart constructor
    mkCelebrityDetail,

    -- * Lenses
    cdBoundingBox,
    cdURLs,
    cdConfidence,
    cdName,
    cdId,
    cdFace,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.BoundingBox
import Network.AWS.Rekognition.Types.FaceDetail

-- | Information about a recognized celebrity.
--
-- /See:/ 'mkCelebrityDetail' smart constructor.
data CelebrityDetail = CelebrityDetail'
  { boundingBox ::
      Lude.Maybe BoundingBox,
    urls :: Lude.Maybe [Lude.Text],
    confidence :: Lude.Maybe Lude.Double,
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CelebrityDetail' with the minimum fields required to make a request.
--
-- * 'boundingBox' - Bounding box around the body of a celebrity.
-- * 'confidence' - The confidence, in percentage, that Amazon Rekognition has that the recognized face is the celebrity.
-- * 'face' - Face details for the recognized celebrity.
-- * 'id' - The unique identifier for the celebrity.
-- * 'name' - The name of the celebrity.
-- * 'urls' - An array of URLs pointing to additional celebrity information.
mkCelebrityDetail ::
  CelebrityDetail
mkCelebrityDetail =
  CelebrityDetail'
    { boundingBox = Lude.Nothing,
      urls = Lude.Nothing,
      confidence = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      face = Lude.Nothing
    }

-- | Bounding box around the body of a celebrity.
--
-- /Note:/ Consider using 'boundingBox' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdBoundingBox :: Lens.Lens' CelebrityDetail (Lude.Maybe BoundingBox)
cdBoundingBox = Lens.lens (boundingBox :: CelebrityDetail -> Lude.Maybe BoundingBox) (\s a -> s {boundingBox = a} :: CelebrityDetail)
{-# DEPRECATED cdBoundingBox "Use generic-lens or generic-optics with 'boundingBox' instead." #-}

-- | An array of URLs pointing to additional celebrity information.
--
-- /Note:/ Consider using 'urls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdURLs :: Lens.Lens' CelebrityDetail (Lude.Maybe [Lude.Text])
cdURLs = Lens.lens (urls :: CelebrityDetail -> Lude.Maybe [Lude.Text]) (\s a -> s {urls = a} :: CelebrityDetail)
{-# DEPRECATED cdURLs "Use generic-lens or generic-optics with 'urls' instead." #-}

-- | The confidence, in percentage, that Amazon Rekognition has that the recognized face is the celebrity.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdConfidence :: Lens.Lens' CelebrityDetail (Lude.Maybe Lude.Double)
cdConfidence = Lens.lens (confidence :: CelebrityDetail -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: CelebrityDetail)
{-# DEPRECATED cdConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | The name of the celebrity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdName :: Lens.Lens' CelebrityDetail (Lude.Maybe Lude.Text)
cdName = Lens.lens (name :: CelebrityDetail -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CelebrityDetail)
{-# DEPRECATED cdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique identifier for the celebrity.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdId :: Lens.Lens' CelebrityDetail (Lude.Maybe Lude.Text)
cdId = Lens.lens (id :: CelebrityDetail -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CelebrityDetail)
{-# DEPRECATED cdId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Face details for the recognized celebrity.
--
-- /Note:/ Consider using 'face' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdFace :: Lens.Lens' CelebrityDetail (Lude.Maybe FaceDetail)
cdFace = Lens.lens (face :: CelebrityDetail -> Lude.Maybe FaceDetail) (\s a -> s {face = a} :: CelebrityDetail)
{-# DEPRECATED cdFace "Use generic-lens or generic-optics with 'face' instead." #-}

instance Lude.FromJSON CelebrityDetail where
  parseJSON =
    Lude.withObject
      "CelebrityDetail"
      ( \x ->
          CelebrityDetail'
            Lude.<$> (x Lude..:? "BoundingBox")
            Lude.<*> (x Lude..:? "Urls" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Confidence")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Face")
      )
