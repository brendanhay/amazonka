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
-- Module      : Amazonka.Rekognition.Types.FaceRecord
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.FaceRecord where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.Face
import Amazonka.Rekognition.Types.FaceDetail

-- | Object containing both the face metadata (stored in the backend
-- database), and facial attributes that are detected but aren\'t stored in
-- the database.
--
-- /See:/ 'newFaceRecord' smart constructor.
data FaceRecord = FaceRecord'
  { -- | Describes the face properties such as the bounding box, face ID, image
    -- ID of the input image, and external image ID that you assigned.
    face :: Prelude.Maybe Face,
    -- | Structure containing attributes of the face that the algorithm detected.
    faceDetail :: Prelude.Maybe FaceDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FaceRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'face', 'faceRecord_face' - Describes the face properties such as the bounding box, face ID, image
-- ID of the input image, and external image ID that you assigned.
--
-- 'faceDetail', 'faceRecord_faceDetail' - Structure containing attributes of the face that the algorithm detected.
newFaceRecord ::
  FaceRecord
newFaceRecord =
  FaceRecord'
    { face = Prelude.Nothing,
      faceDetail = Prelude.Nothing
    }

-- | Describes the face properties such as the bounding box, face ID, image
-- ID of the input image, and external image ID that you assigned.
faceRecord_face :: Lens.Lens' FaceRecord (Prelude.Maybe Face)
faceRecord_face = Lens.lens (\FaceRecord' {face} -> face) (\s@FaceRecord' {} a -> s {face = a} :: FaceRecord)

-- | Structure containing attributes of the face that the algorithm detected.
faceRecord_faceDetail :: Lens.Lens' FaceRecord (Prelude.Maybe FaceDetail)
faceRecord_faceDetail = Lens.lens (\FaceRecord' {faceDetail} -> faceDetail) (\s@FaceRecord' {} a -> s {faceDetail = a} :: FaceRecord)

instance Data.FromJSON FaceRecord where
  parseJSON =
    Data.withObject
      "FaceRecord"
      ( \x ->
          FaceRecord'
            Prelude.<$> (x Data..:? "Face")
            Prelude.<*> (x Data..:? "FaceDetail")
      )

instance Prelude.Hashable FaceRecord where
  hashWithSalt _salt FaceRecord' {..} =
    _salt
      `Prelude.hashWithSalt` face
      `Prelude.hashWithSalt` faceDetail

instance Prelude.NFData FaceRecord where
  rnf FaceRecord' {..} =
    Prelude.rnf face
      `Prelude.seq` Prelude.rnf faceDetail
