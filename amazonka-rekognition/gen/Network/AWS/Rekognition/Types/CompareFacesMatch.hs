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
-- Module      : Network.AWS.Rekognition.Types.CompareFacesMatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.CompareFacesMatch where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.ComparedFace

-- | Provides information about a face in a target image that matches the
-- source image face analyzed by @CompareFaces@. The @Face@ property
-- contains the bounding box of the face in the target image. The
-- @Similarity@ property is the confidence that the source image face
-- matches the face in the bounding box.
--
-- /See:/ 'newCompareFacesMatch' smart constructor.
data CompareFacesMatch = CompareFacesMatch'
  { -- | Level of confidence that the faces match.
    similarity :: Prelude.Maybe Prelude.Double,
    -- | Provides face metadata (bounding box and confidence that the bounding
    -- box actually contains a face).
    face :: Prelude.Maybe ComparedFace
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CompareFacesMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'similarity', 'compareFacesMatch_similarity' - Level of confidence that the faces match.
--
-- 'face', 'compareFacesMatch_face' - Provides face metadata (bounding box and confidence that the bounding
-- box actually contains a face).
newCompareFacesMatch ::
  CompareFacesMatch
newCompareFacesMatch =
  CompareFacesMatch'
    { similarity = Prelude.Nothing,
      face = Prelude.Nothing
    }

-- | Level of confidence that the faces match.
compareFacesMatch_similarity :: Lens.Lens' CompareFacesMatch (Prelude.Maybe Prelude.Double)
compareFacesMatch_similarity = Lens.lens (\CompareFacesMatch' {similarity} -> similarity) (\s@CompareFacesMatch' {} a -> s {similarity = a} :: CompareFacesMatch)

-- | Provides face metadata (bounding box and confidence that the bounding
-- box actually contains a face).
compareFacesMatch_face :: Lens.Lens' CompareFacesMatch (Prelude.Maybe ComparedFace)
compareFacesMatch_face = Lens.lens (\CompareFacesMatch' {face} -> face) (\s@CompareFacesMatch' {} a -> s {face = a} :: CompareFacesMatch)

instance Prelude.FromJSON CompareFacesMatch where
  parseJSON =
    Prelude.withObject
      "CompareFacesMatch"
      ( \x ->
          CompareFacesMatch'
            Prelude.<$> (x Prelude..:? "Similarity")
            Prelude.<*> (x Prelude..:? "Face")
      )

instance Prelude.Hashable CompareFacesMatch

instance Prelude.NFData CompareFacesMatch
