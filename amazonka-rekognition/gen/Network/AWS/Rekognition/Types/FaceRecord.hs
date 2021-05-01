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
-- Module      : Network.AWS.Rekognition.Types.FaceRecord
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.FaceRecord where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.Face
import Network.AWS.Rekognition.Types.FaceDetail

-- | Object containing both the face metadata (stored in the backend
-- database), and facial attributes that are detected but aren\'t stored in
-- the database.
--
-- /See:/ 'newFaceRecord' smart constructor.
data FaceRecord = FaceRecord'
  { -- | Structure containing attributes of the face that the algorithm detected.
    faceDetail :: Prelude.Maybe FaceDetail,
    -- | Describes the face properties such as the bounding box, face ID, image
    -- ID of the input image, and external image ID that you assigned.
    face :: Prelude.Maybe Face
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FaceRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceDetail', 'faceRecord_faceDetail' - Structure containing attributes of the face that the algorithm detected.
--
-- 'face', 'faceRecord_face' - Describes the face properties such as the bounding box, face ID, image
-- ID of the input image, and external image ID that you assigned.
newFaceRecord ::
  FaceRecord
newFaceRecord =
  FaceRecord'
    { faceDetail = Prelude.Nothing,
      face = Prelude.Nothing
    }

-- | Structure containing attributes of the face that the algorithm detected.
faceRecord_faceDetail :: Lens.Lens' FaceRecord (Prelude.Maybe FaceDetail)
faceRecord_faceDetail = Lens.lens (\FaceRecord' {faceDetail} -> faceDetail) (\s@FaceRecord' {} a -> s {faceDetail = a} :: FaceRecord)

-- | Describes the face properties such as the bounding box, face ID, image
-- ID of the input image, and external image ID that you assigned.
faceRecord_face :: Lens.Lens' FaceRecord (Prelude.Maybe Face)
faceRecord_face = Lens.lens (\FaceRecord' {face} -> face) (\s@FaceRecord' {} a -> s {face = a} :: FaceRecord)

instance Prelude.FromJSON FaceRecord where
  parseJSON =
    Prelude.withObject
      "FaceRecord"
      ( \x ->
          FaceRecord'
            Prelude.<$> (x Prelude..:? "FaceDetail")
            Prelude.<*> (x Prelude..:? "Face")
      )

instance Prelude.Hashable FaceRecord

instance Prelude.NFData FaceRecord
