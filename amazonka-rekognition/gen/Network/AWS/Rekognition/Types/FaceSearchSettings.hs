{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.FaceSearchSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.FaceSearchSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Input face recognition parameters for an Amazon Rekognition stream
-- processor. @FaceRecognitionSettings@ is a request parameter for
-- CreateStreamProcessor.
--
-- /See:/ 'newFaceSearchSettings' smart constructor.
data FaceSearchSettings = FaceSearchSettings'
  { -- | The ID of a collection that contains faces that you want to search for.
    collectionId :: Core.Maybe Core.Text,
    -- | Minimum face match confidence score that must be met to return a result
    -- for a recognized face. Default is 80. 0 is the lowest confidence. 100 is
    -- the highest confidence.
    faceMatchThreshold :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FaceSearchSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectionId', 'faceSearchSettings_collectionId' - The ID of a collection that contains faces that you want to search for.
--
-- 'faceMatchThreshold', 'faceSearchSettings_faceMatchThreshold' - Minimum face match confidence score that must be met to return a result
-- for a recognized face. Default is 80. 0 is the lowest confidence. 100 is
-- the highest confidence.
newFaceSearchSettings ::
  FaceSearchSettings
newFaceSearchSettings =
  FaceSearchSettings'
    { collectionId = Core.Nothing,
      faceMatchThreshold = Core.Nothing
    }

-- | The ID of a collection that contains faces that you want to search for.
faceSearchSettings_collectionId :: Lens.Lens' FaceSearchSettings (Core.Maybe Core.Text)
faceSearchSettings_collectionId = Lens.lens (\FaceSearchSettings' {collectionId} -> collectionId) (\s@FaceSearchSettings' {} a -> s {collectionId = a} :: FaceSearchSettings)

-- | Minimum face match confidence score that must be met to return a result
-- for a recognized face. Default is 80. 0 is the lowest confidence. 100 is
-- the highest confidence.
faceSearchSettings_faceMatchThreshold :: Lens.Lens' FaceSearchSettings (Core.Maybe Core.Double)
faceSearchSettings_faceMatchThreshold = Lens.lens (\FaceSearchSettings' {faceMatchThreshold} -> faceMatchThreshold) (\s@FaceSearchSettings' {} a -> s {faceMatchThreshold = a} :: FaceSearchSettings)

instance Core.FromJSON FaceSearchSettings where
  parseJSON =
    Core.withObject
      "FaceSearchSettings"
      ( \x ->
          FaceSearchSettings'
            Core.<$> (x Core..:? "CollectionId")
            Core.<*> (x Core..:? "FaceMatchThreshold")
      )

instance Core.Hashable FaceSearchSettings

instance Core.NFData FaceSearchSettings

instance Core.ToJSON FaceSearchSettings where
  toJSON FaceSearchSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CollectionId" Core..=) Core.<$> collectionId,
            ("FaceMatchThreshold" Core..=)
              Core.<$> faceMatchThreshold
          ]
      )
