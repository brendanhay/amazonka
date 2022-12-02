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
-- Module      : Amazonka.Rekognition.Types.Instance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.Instance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.BoundingBox
import Amazonka.Rekognition.Types.DominantColor

-- | An instance of a label returned by Amazon Rekognition Image
-- (DetectLabels) or by Amazon Rekognition Video (GetLabelDetection).
--
-- /See:/ 'newInstance' smart constructor.
data Instance = Instance'
  { -- | The confidence that Amazon Rekognition has in the accuracy of the
    -- bounding box.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | The position of the label instance on the image.
    boundingBox :: Prelude.Maybe BoundingBox,
    -- | The dominant colors found in an individual instance of a label.
    dominantColors :: Prelude.Maybe [DominantColor]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Instance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidence', 'instance_confidence' - The confidence that Amazon Rekognition has in the accuracy of the
-- bounding box.
--
-- 'boundingBox', 'instance_boundingBox' - The position of the label instance on the image.
--
-- 'dominantColors', 'instance_dominantColors' - The dominant colors found in an individual instance of a label.
newInstance ::
  Instance
newInstance =
  Instance'
    { confidence = Prelude.Nothing,
      boundingBox = Prelude.Nothing,
      dominantColors = Prelude.Nothing
    }

-- | The confidence that Amazon Rekognition has in the accuracy of the
-- bounding box.
instance_confidence :: Lens.Lens' Instance (Prelude.Maybe Prelude.Double)
instance_confidence = Lens.lens (\Instance' {confidence} -> confidence) (\s@Instance' {} a -> s {confidence = a} :: Instance)

-- | The position of the label instance on the image.
instance_boundingBox :: Lens.Lens' Instance (Prelude.Maybe BoundingBox)
instance_boundingBox = Lens.lens (\Instance' {boundingBox} -> boundingBox) (\s@Instance' {} a -> s {boundingBox = a} :: Instance)

-- | The dominant colors found in an individual instance of a label.
instance_dominantColors :: Lens.Lens' Instance (Prelude.Maybe [DominantColor])
instance_dominantColors = Lens.lens (\Instance' {dominantColors} -> dominantColors) (\s@Instance' {} a -> s {dominantColors = a} :: Instance) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Instance where
  parseJSON =
    Data.withObject
      "Instance"
      ( \x ->
          Instance'
            Prelude.<$> (x Data..:? "Confidence")
            Prelude.<*> (x Data..:? "BoundingBox")
            Prelude.<*> ( x Data..:? "DominantColors"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Instance where
  hashWithSalt _salt Instance' {..} =
    _salt `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` boundingBox
      `Prelude.hashWithSalt` dominantColors

instance Prelude.NFData Instance where
  rnf Instance' {..} =
    Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf boundingBox
      `Prelude.seq` Prelude.rnf dominantColors
