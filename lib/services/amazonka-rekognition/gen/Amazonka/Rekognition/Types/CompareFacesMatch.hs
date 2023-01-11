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
-- Module      : Amazonka.Rekognition.Types.CompareFacesMatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.CompareFacesMatch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.ComparedFace

-- | Provides information about a face in a target image that matches the
-- source image face analyzed by @CompareFaces@. The @Face@ property
-- contains the bounding box of the face in the target image. The
-- @Similarity@ property is the confidence that the source image face
-- matches the face in the bounding box.
--
-- /See:/ 'newCompareFacesMatch' smart constructor.
data CompareFacesMatch = CompareFacesMatch'
  { -- | Provides face metadata (bounding box and confidence that the bounding
    -- box actually contains a face).
    face :: Prelude.Maybe ComparedFace,
    -- | Level of confidence that the faces match.
    similarity :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompareFacesMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'face', 'compareFacesMatch_face' - Provides face metadata (bounding box and confidence that the bounding
-- box actually contains a face).
--
-- 'similarity', 'compareFacesMatch_similarity' - Level of confidence that the faces match.
newCompareFacesMatch ::
  CompareFacesMatch
newCompareFacesMatch =
  CompareFacesMatch'
    { face = Prelude.Nothing,
      similarity = Prelude.Nothing
    }

-- | Provides face metadata (bounding box and confidence that the bounding
-- box actually contains a face).
compareFacesMatch_face :: Lens.Lens' CompareFacesMatch (Prelude.Maybe ComparedFace)
compareFacesMatch_face = Lens.lens (\CompareFacesMatch' {face} -> face) (\s@CompareFacesMatch' {} a -> s {face = a} :: CompareFacesMatch)

-- | Level of confidence that the faces match.
compareFacesMatch_similarity :: Lens.Lens' CompareFacesMatch (Prelude.Maybe Prelude.Double)
compareFacesMatch_similarity = Lens.lens (\CompareFacesMatch' {similarity} -> similarity) (\s@CompareFacesMatch' {} a -> s {similarity = a} :: CompareFacesMatch)

instance Data.FromJSON CompareFacesMatch where
  parseJSON =
    Data.withObject
      "CompareFacesMatch"
      ( \x ->
          CompareFacesMatch'
            Prelude.<$> (x Data..:? "Face")
            Prelude.<*> (x Data..:? "Similarity")
      )

instance Prelude.Hashable CompareFacesMatch where
  hashWithSalt _salt CompareFacesMatch' {..} =
    _salt `Prelude.hashWithSalt` face
      `Prelude.hashWithSalt` similarity

instance Prelude.NFData CompareFacesMatch where
  rnf CompareFacesMatch' {..} =
    Prelude.rnf face
      `Prelude.seq` Prelude.rnf similarity
