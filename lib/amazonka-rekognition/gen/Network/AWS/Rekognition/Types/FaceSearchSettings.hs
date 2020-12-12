{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.FaceSearchSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.FaceSearchSettings
  ( FaceSearchSettings (..),

    -- * Smart constructor
    mkFaceSearchSettings,

    -- * Lenses
    fssFaceMatchThreshold,
    fssCollectionId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Input face recognition parameters for an Amazon Rekognition stream processor. @FaceRecognitionSettings@ is a request parameter for 'CreateStreamProcessor' .
--
-- /See:/ 'mkFaceSearchSettings' smart constructor.
data FaceSearchSettings = FaceSearchSettings'
  { faceMatchThreshold ::
      Lude.Maybe Lude.Double,
    collectionId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FaceSearchSettings' with the minimum fields required to make a request.
--
-- * 'collectionId' - The ID of a collection that contains faces that you want to search for.
-- * 'faceMatchThreshold' - Minimum face match confidence score that must be met to return a result for a recognized face. Default is 80. 0 is the lowest confidence. 100 is the highest confidence.
mkFaceSearchSettings ::
  FaceSearchSettings
mkFaceSearchSettings =
  FaceSearchSettings'
    { faceMatchThreshold = Lude.Nothing,
      collectionId = Lude.Nothing
    }

-- | Minimum face match confidence score that must be met to return a result for a recognized face. Default is 80. 0 is the lowest confidence. 100 is the highest confidence.
--
-- /Note:/ Consider using 'faceMatchThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fssFaceMatchThreshold :: Lens.Lens' FaceSearchSettings (Lude.Maybe Lude.Double)
fssFaceMatchThreshold = Lens.lens (faceMatchThreshold :: FaceSearchSettings -> Lude.Maybe Lude.Double) (\s a -> s {faceMatchThreshold = a} :: FaceSearchSettings)
{-# DEPRECATED fssFaceMatchThreshold "Use generic-lens or generic-optics with 'faceMatchThreshold' instead." #-}

-- | The ID of a collection that contains faces that you want to search for.
--
-- /Note:/ Consider using 'collectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fssCollectionId :: Lens.Lens' FaceSearchSettings (Lude.Maybe Lude.Text)
fssCollectionId = Lens.lens (collectionId :: FaceSearchSettings -> Lude.Maybe Lude.Text) (\s a -> s {collectionId = a} :: FaceSearchSettings)
{-# DEPRECATED fssCollectionId "Use generic-lens or generic-optics with 'collectionId' instead." #-}

instance Lude.FromJSON FaceSearchSettings where
  parseJSON =
    Lude.withObject
      "FaceSearchSettings"
      ( \x ->
          FaceSearchSettings'
            Lude.<$> (x Lude..:? "FaceMatchThreshold")
            Lude.<*> (x Lude..:? "CollectionId")
      )

instance Lude.ToJSON FaceSearchSettings where
  toJSON FaceSearchSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("FaceMatchThreshold" Lude..=) Lude.<$> faceMatchThreshold,
            ("CollectionId" Lude..=) Lude.<$> collectionId
          ]
      )
