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
-- Module      : Amazonka.Rekognition.Types.FaceOccluded
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.FaceOccluded where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | @FaceOccluded@ should return \"true\" with a high confidence score if a
-- detected face’s eyes, nose, and mouth are partially captured or if they
-- are covered by masks, dark sunglasses, cell phones, hands, or other
-- objects. @FaceOccluded@ should return \"false\" with a high confidence
-- score if common occurrences that do not impact face verification are
-- detected, such as eye glasses, lightly tinted sunglasses, strands of
-- hair, and others.
--
-- You can use @FaceOccluded@ to determine if an obstruction on a face
-- negatively impacts using the image for face matching.
--
-- /See:/ 'newFaceOccluded' smart constructor.
data FaceOccluded = FaceOccluded'
  { -- | The confidence that the service has detected the presence of a face
    -- occlusion.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | True if a detected face’s eyes, nose, and mouth are partially captured
    -- or if they are covered by masks, dark sunglasses, cell phones, hands, or
    -- other objects. False if common occurrences that do not impact face
    -- verification are detected, such as eye glasses, lightly tinted
    -- sunglasses, strands of hair, and others.
    value :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FaceOccluded' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidence', 'faceOccluded_confidence' - The confidence that the service has detected the presence of a face
-- occlusion.
--
-- 'value', 'faceOccluded_value' - True if a detected face’s eyes, nose, and mouth are partially captured
-- or if they are covered by masks, dark sunglasses, cell phones, hands, or
-- other objects. False if common occurrences that do not impact face
-- verification are detected, such as eye glasses, lightly tinted
-- sunglasses, strands of hair, and others.
newFaceOccluded ::
  FaceOccluded
newFaceOccluded =
  FaceOccluded'
    { confidence = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The confidence that the service has detected the presence of a face
-- occlusion.
faceOccluded_confidence :: Lens.Lens' FaceOccluded (Prelude.Maybe Prelude.Double)
faceOccluded_confidence = Lens.lens (\FaceOccluded' {confidence} -> confidence) (\s@FaceOccluded' {} a -> s {confidence = a} :: FaceOccluded)

-- | True if a detected face’s eyes, nose, and mouth are partially captured
-- or if they are covered by masks, dark sunglasses, cell phones, hands, or
-- other objects. False if common occurrences that do not impact face
-- verification are detected, such as eye glasses, lightly tinted
-- sunglasses, strands of hair, and others.
faceOccluded_value :: Lens.Lens' FaceOccluded (Prelude.Maybe Prelude.Bool)
faceOccluded_value = Lens.lens (\FaceOccluded' {value} -> value) (\s@FaceOccluded' {} a -> s {value = a} :: FaceOccluded)

instance Data.FromJSON FaceOccluded where
  parseJSON =
    Data.withObject
      "FaceOccluded"
      ( \x ->
          FaceOccluded'
            Prelude.<$> (x Data..:? "Confidence")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable FaceOccluded where
  hashWithSalt _salt FaceOccluded' {..} =
    _salt
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` value

instance Prelude.NFData FaceOccluded where
  rnf FaceOccluded' {..} =
    Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf value
