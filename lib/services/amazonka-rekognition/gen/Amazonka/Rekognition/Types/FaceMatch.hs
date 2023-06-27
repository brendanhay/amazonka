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
-- Module      : Amazonka.Rekognition.Types.FaceMatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.FaceMatch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.Face

-- | Provides face metadata. In addition, it also provides the confidence in
-- the match of this face with the input face.
--
-- /See:/ 'newFaceMatch' smart constructor.
data FaceMatch = FaceMatch'
  { -- | Describes the face properties such as the bounding box, face ID, image
    -- ID of the source image, and external image ID that you assigned.
    face :: Prelude.Maybe Face,
    -- | Confidence in the match of this face with the input face.
    similarity :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FaceMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'face', 'faceMatch_face' - Describes the face properties such as the bounding box, face ID, image
-- ID of the source image, and external image ID that you assigned.
--
-- 'similarity', 'faceMatch_similarity' - Confidence in the match of this face with the input face.
newFaceMatch ::
  FaceMatch
newFaceMatch =
  FaceMatch'
    { face = Prelude.Nothing,
      similarity = Prelude.Nothing
    }

-- | Describes the face properties such as the bounding box, face ID, image
-- ID of the source image, and external image ID that you assigned.
faceMatch_face :: Lens.Lens' FaceMatch (Prelude.Maybe Face)
faceMatch_face = Lens.lens (\FaceMatch' {face} -> face) (\s@FaceMatch' {} a -> s {face = a} :: FaceMatch)

-- | Confidence in the match of this face with the input face.
faceMatch_similarity :: Lens.Lens' FaceMatch (Prelude.Maybe Prelude.Double)
faceMatch_similarity = Lens.lens (\FaceMatch' {similarity} -> similarity) (\s@FaceMatch' {} a -> s {similarity = a} :: FaceMatch)

instance Data.FromJSON FaceMatch where
  parseJSON =
    Data.withObject
      "FaceMatch"
      ( \x ->
          FaceMatch'
            Prelude.<$> (x Data..:? "Face")
            Prelude.<*> (x Data..:? "Similarity")
      )

instance Prelude.Hashable FaceMatch where
  hashWithSalt _salt FaceMatch' {..} =
    _salt
      `Prelude.hashWithSalt` face
      `Prelude.hashWithSalt` similarity

instance Prelude.NFData FaceMatch where
  rnf FaceMatch' {..} =
    Prelude.rnf face
      `Prelude.seq` Prelude.rnf similarity
