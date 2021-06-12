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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
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
    similarity :: Core.Maybe Core.Double,
    -- | Provides face metadata (bounding box and confidence that the bounding
    -- box actually contains a face).
    face :: Core.Maybe ComparedFace
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { similarity = Core.Nothing,
      face = Core.Nothing
    }

-- | Level of confidence that the faces match.
compareFacesMatch_similarity :: Lens.Lens' CompareFacesMatch (Core.Maybe Core.Double)
compareFacesMatch_similarity = Lens.lens (\CompareFacesMatch' {similarity} -> similarity) (\s@CompareFacesMatch' {} a -> s {similarity = a} :: CompareFacesMatch)

-- | Provides face metadata (bounding box and confidence that the bounding
-- box actually contains a face).
compareFacesMatch_face :: Lens.Lens' CompareFacesMatch (Core.Maybe ComparedFace)
compareFacesMatch_face = Lens.lens (\CompareFacesMatch' {face} -> face) (\s@CompareFacesMatch' {} a -> s {face = a} :: CompareFacesMatch)

instance Core.FromJSON CompareFacesMatch where
  parseJSON =
    Core.withObject
      "CompareFacesMatch"
      ( \x ->
          CompareFacesMatch'
            Core.<$> (x Core..:? "Similarity")
            Core.<*> (x Core..:? "Face")
      )

instance Core.Hashable CompareFacesMatch

instance Core.NFData CompareFacesMatch
