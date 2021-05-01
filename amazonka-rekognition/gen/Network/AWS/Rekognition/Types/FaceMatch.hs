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
-- Module      : Network.AWS.Rekognition.Types.FaceMatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.FaceMatch where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.Face

-- | Provides face metadata. In addition, it also provides the confidence in
-- the match of this face with the input face.
--
-- /See:/ 'newFaceMatch' smart constructor.
data FaceMatch = FaceMatch'
  { -- | Confidence in the match of this face with the input face.
    similarity :: Prelude.Maybe Prelude.Double,
    -- | Describes the face properties such as the bounding box, face ID, image
    -- ID of the source image, and external image ID that you assigned.
    face :: Prelude.Maybe Face
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FaceMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'similarity', 'faceMatch_similarity' - Confidence in the match of this face with the input face.
--
-- 'face', 'faceMatch_face' - Describes the face properties such as the bounding box, face ID, image
-- ID of the source image, and external image ID that you assigned.
newFaceMatch ::
  FaceMatch
newFaceMatch =
  FaceMatch'
    { similarity = Prelude.Nothing,
      face = Prelude.Nothing
    }

-- | Confidence in the match of this face with the input face.
faceMatch_similarity :: Lens.Lens' FaceMatch (Prelude.Maybe Prelude.Double)
faceMatch_similarity = Lens.lens (\FaceMatch' {similarity} -> similarity) (\s@FaceMatch' {} a -> s {similarity = a} :: FaceMatch)

-- | Describes the face properties such as the bounding box, face ID, image
-- ID of the source image, and external image ID that you assigned.
faceMatch_face :: Lens.Lens' FaceMatch (Prelude.Maybe Face)
faceMatch_face = Lens.lens (\FaceMatch' {face} -> face) (\s@FaceMatch' {} a -> s {face = a} :: FaceMatch)

instance Prelude.FromJSON FaceMatch where
  parseJSON =
    Prelude.withObject
      "FaceMatch"
      ( \x ->
          FaceMatch'
            Prelude.<$> (x Prelude..:? "Similarity")
            Prelude.<*> (x Prelude..:? "Face")
      )

instance Prelude.Hashable FaceMatch

instance Prelude.NFData FaceMatch
