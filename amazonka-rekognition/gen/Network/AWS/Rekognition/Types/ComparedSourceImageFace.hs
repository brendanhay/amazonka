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
-- Module      : Network.AWS.Rekognition.Types.ComparedSourceImageFace
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ComparedSourceImageFace where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.BoundingBox

-- | Type that describes the face Amazon Rekognition chose to compare with
-- the faces in the target. This contains a bounding box for the selected
-- face and confidence level that the bounding box contains a face. Note
-- that Amazon Rekognition selects the largest face in the source image for
-- this comparison.
--
-- /See:/ 'newComparedSourceImageFace' smart constructor.
data ComparedSourceImageFace = ComparedSourceImageFace'
  { -- | Bounding box of the face.
    boundingBox :: Prelude.Maybe BoundingBox,
    -- | Confidence level that the selected bounding box contains a face.
    confidence :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ComparedSourceImageFace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'boundingBox', 'comparedSourceImageFace_boundingBox' - Bounding box of the face.
--
-- 'confidence', 'comparedSourceImageFace_confidence' - Confidence level that the selected bounding box contains a face.
newComparedSourceImageFace ::
  ComparedSourceImageFace
newComparedSourceImageFace =
  ComparedSourceImageFace'
    { boundingBox =
        Prelude.Nothing,
      confidence = Prelude.Nothing
    }

-- | Bounding box of the face.
comparedSourceImageFace_boundingBox :: Lens.Lens' ComparedSourceImageFace (Prelude.Maybe BoundingBox)
comparedSourceImageFace_boundingBox = Lens.lens (\ComparedSourceImageFace' {boundingBox} -> boundingBox) (\s@ComparedSourceImageFace' {} a -> s {boundingBox = a} :: ComparedSourceImageFace)

-- | Confidence level that the selected bounding box contains a face.
comparedSourceImageFace_confidence :: Lens.Lens' ComparedSourceImageFace (Prelude.Maybe Prelude.Double)
comparedSourceImageFace_confidence = Lens.lens (\ComparedSourceImageFace' {confidence} -> confidence) (\s@ComparedSourceImageFace' {} a -> s {confidence = a} :: ComparedSourceImageFace)

instance Prelude.FromJSON ComparedSourceImageFace where
  parseJSON =
    Prelude.withObject
      "ComparedSourceImageFace"
      ( \x ->
          ComparedSourceImageFace'
            Prelude.<$> (x Prelude..:? "BoundingBox")
            Prelude.<*> (x Prelude..:? "Confidence")
      )

instance Prelude.Hashable ComparedSourceImageFace

instance Prelude.NFData ComparedSourceImageFace
