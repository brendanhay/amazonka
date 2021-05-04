{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Input face recognition parameters for an Amazon Rekognition stream
-- processor. @FaceRecognitionSettings@ is a request parameter for
-- CreateStreamProcessor.
--
-- /See:/ 'newFaceSearchSettings' smart constructor.
data FaceSearchSettings = FaceSearchSettings'
  { -- | The ID of a collection that contains faces that you want to search for.
    collectionId :: Prelude.Maybe Prelude.Text,
    -- | Minimum face match confidence score that must be met to return a result
    -- for a recognized face. Default is 80. 0 is the lowest confidence. 100 is
    -- the highest confidence.
    faceMatchThreshold :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { collectionId = Prelude.Nothing,
      faceMatchThreshold = Prelude.Nothing
    }

-- | The ID of a collection that contains faces that you want to search for.
faceSearchSettings_collectionId :: Lens.Lens' FaceSearchSettings (Prelude.Maybe Prelude.Text)
faceSearchSettings_collectionId = Lens.lens (\FaceSearchSettings' {collectionId} -> collectionId) (\s@FaceSearchSettings' {} a -> s {collectionId = a} :: FaceSearchSettings)

-- | Minimum face match confidence score that must be met to return a result
-- for a recognized face. Default is 80. 0 is the lowest confidence. 100 is
-- the highest confidence.
faceSearchSettings_faceMatchThreshold :: Lens.Lens' FaceSearchSettings (Prelude.Maybe Prelude.Double)
faceSearchSettings_faceMatchThreshold = Lens.lens (\FaceSearchSettings' {faceMatchThreshold} -> faceMatchThreshold) (\s@FaceSearchSettings' {} a -> s {faceMatchThreshold = a} :: FaceSearchSettings)

instance Prelude.FromJSON FaceSearchSettings where
  parseJSON =
    Prelude.withObject
      "FaceSearchSettings"
      ( \x ->
          FaceSearchSettings'
            Prelude.<$> (x Prelude..:? "CollectionId")
            Prelude.<*> (x Prelude..:? "FaceMatchThreshold")
      )

instance Prelude.Hashable FaceSearchSettings

instance Prelude.NFData FaceSearchSettings

instance Prelude.ToJSON FaceSearchSettings where
  toJSON FaceSearchSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CollectionId" Prelude..=)
              Prelude.<$> collectionId,
            ("FaceMatchThreshold" Prelude..=)
              Prelude.<$> faceMatchThreshold
          ]
      )
