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
    fssCollectionId,
    fssFaceMatchThreshold,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.CollectionId as Types

-- | Input face recognition parameters for an Amazon Rekognition stream processor. @FaceRecognitionSettings@ is a request parameter for 'CreateStreamProcessor' .
--
-- /See:/ 'mkFaceSearchSettings' smart constructor.
data FaceSearchSettings = FaceSearchSettings'
  { -- | The ID of a collection that contains faces that you want to search for.
    collectionId :: Core.Maybe Types.CollectionId,
    -- | Minimum face match confidence score that must be met to return a result for a recognized face. Default is 80. 0 is the lowest confidence. 100 is the highest confidence.
    faceMatchThreshold :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FaceSearchSettings' value with any optional fields omitted.
mkFaceSearchSettings ::
  FaceSearchSettings
mkFaceSearchSettings =
  FaceSearchSettings'
    { collectionId = Core.Nothing,
      faceMatchThreshold = Core.Nothing
    }

-- | The ID of a collection that contains faces that you want to search for.
--
-- /Note:/ Consider using 'collectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fssCollectionId :: Lens.Lens' FaceSearchSettings (Core.Maybe Types.CollectionId)
fssCollectionId = Lens.field @"collectionId"
{-# DEPRECATED fssCollectionId "Use generic-lens or generic-optics with 'collectionId' instead." #-}

-- | Minimum face match confidence score that must be met to return a result for a recognized face. Default is 80. 0 is the lowest confidence. 100 is the highest confidence.
--
-- /Note:/ Consider using 'faceMatchThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fssFaceMatchThreshold :: Lens.Lens' FaceSearchSettings (Core.Maybe Core.Double)
fssFaceMatchThreshold = Lens.field @"faceMatchThreshold"
{-# DEPRECATED fssFaceMatchThreshold "Use generic-lens or generic-optics with 'faceMatchThreshold' instead." #-}

instance Core.FromJSON FaceSearchSettings where
  toJSON FaceSearchSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("CollectionId" Core..=) Core.<$> collectionId,
            ("FaceMatchThreshold" Core..=) Core.<$> faceMatchThreshold
          ]
      )

instance Core.FromJSON FaceSearchSettings where
  parseJSON =
    Core.withObject "FaceSearchSettings" Core.$
      \x ->
        FaceSearchSettings'
          Core.<$> (x Core..:? "CollectionId")
          Core.<*> (x Core..:? "FaceMatchThreshold")
