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
-- Module      : Amazonka.Rekognition.Types.FaceSearchSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.FaceSearchSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Input face recognition parameters for an Amazon Rekognition stream
-- processor. Includes the collection to use for face recognition and the
-- face attributes to detect. Defining the settings is required in the
-- request parameter for CreateStreamProcessor.
--
-- /See:/ 'newFaceSearchSettings' smart constructor.
data FaceSearchSettings = FaceSearchSettings'
  { -- | Minimum face match confidence score that must be met to return a result
    -- for a recognized face. The default is 80. 0 is the lowest confidence.
    -- 100 is the highest confidence. Values between 0 and 100 are accepted,
    -- and values lower than 80 are set to 80.
    faceMatchThreshold :: Prelude.Maybe Prelude.Double,
    -- | The ID of a collection that contains faces that you want to search for.
    collectionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FaceSearchSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceMatchThreshold', 'faceSearchSettings_faceMatchThreshold' - Minimum face match confidence score that must be met to return a result
-- for a recognized face. The default is 80. 0 is the lowest confidence.
-- 100 is the highest confidence. Values between 0 and 100 are accepted,
-- and values lower than 80 are set to 80.
--
-- 'collectionId', 'faceSearchSettings_collectionId' - The ID of a collection that contains faces that you want to search for.
newFaceSearchSettings ::
  FaceSearchSettings
newFaceSearchSettings =
  FaceSearchSettings'
    { faceMatchThreshold =
        Prelude.Nothing,
      collectionId = Prelude.Nothing
    }

-- | Minimum face match confidence score that must be met to return a result
-- for a recognized face. The default is 80. 0 is the lowest confidence.
-- 100 is the highest confidence. Values between 0 and 100 are accepted,
-- and values lower than 80 are set to 80.
faceSearchSettings_faceMatchThreshold :: Lens.Lens' FaceSearchSettings (Prelude.Maybe Prelude.Double)
faceSearchSettings_faceMatchThreshold = Lens.lens (\FaceSearchSettings' {faceMatchThreshold} -> faceMatchThreshold) (\s@FaceSearchSettings' {} a -> s {faceMatchThreshold = a} :: FaceSearchSettings)

-- | The ID of a collection that contains faces that you want to search for.
faceSearchSettings_collectionId :: Lens.Lens' FaceSearchSettings (Prelude.Maybe Prelude.Text)
faceSearchSettings_collectionId = Lens.lens (\FaceSearchSettings' {collectionId} -> collectionId) (\s@FaceSearchSettings' {} a -> s {collectionId = a} :: FaceSearchSettings)

instance Data.FromJSON FaceSearchSettings where
  parseJSON =
    Data.withObject
      "FaceSearchSettings"
      ( \x ->
          FaceSearchSettings'
            Prelude.<$> (x Data..:? "FaceMatchThreshold")
            Prelude.<*> (x Data..:? "CollectionId")
      )

instance Prelude.Hashable FaceSearchSettings where
  hashWithSalt _salt FaceSearchSettings' {..} =
    _salt `Prelude.hashWithSalt` faceMatchThreshold
      `Prelude.hashWithSalt` collectionId

instance Prelude.NFData FaceSearchSettings where
  rnf FaceSearchSettings' {..} =
    Prelude.rnf faceMatchThreshold
      `Prelude.seq` Prelude.rnf collectionId

instance Data.ToJSON FaceSearchSettings where
  toJSON FaceSearchSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FaceMatchThreshold" Data..=)
              Prelude.<$> faceMatchThreshold,
            ("CollectionId" Data..=) Prelude.<$> collectionId
          ]
      )
