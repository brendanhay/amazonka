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
-- Module      : Amazonka.Rekognition.Types.CustomLabel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.CustomLabel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.Geometry

-- | A custom label detected in an image by a call to DetectCustomLabels.
--
-- /See:/ 'newCustomLabel' smart constructor.
data CustomLabel = CustomLabel'
  { -- | The confidence that the model has in the detection of the custom label.
    -- The range is 0-100. A higher value indicates a higher confidence.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | The location of the detected object on the image that corresponds to the
    -- custom label. Includes an axis aligned coarse bounding box surrounding
    -- the object and a finer grain polygon for more accurate spatial
    -- information.
    geometry :: Prelude.Maybe Geometry,
    -- | The name of the custom label.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomLabel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidence', 'customLabel_confidence' - The confidence that the model has in the detection of the custom label.
-- The range is 0-100. A higher value indicates a higher confidence.
--
-- 'geometry', 'customLabel_geometry' - The location of the detected object on the image that corresponds to the
-- custom label. Includes an axis aligned coarse bounding box surrounding
-- the object and a finer grain polygon for more accurate spatial
-- information.
--
-- 'name', 'customLabel_name' - The name of the custom label.
newCustomLabel ::
  CustomLabel
newCustomLabel =
  CustomLabel'
    { confidence = Prelude.Nothing,
      geometry = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The confidence that the model has in the detection of the custom label.
-- The range is 0-100. A higher value indicates a higher confidence.
customLabel_confidence :: Lens.Lens' CustomLabel (Prelude.Maybe Prelude.Double)
customLabel_confidence = Lens.lens (\CustomLabel' {confidence} -> confidence) (\s@CustomLabel' {} a -> s {confidence = a} :: CustomLabel)

-- | The location of the detected object on the image that corresponds to the
-- custom label. Includes an axis aligned coarse bounding box surrounding
-- the object and a finer grain polygon for more accurate spatial
-- information.
customLabel_geometry :: Lens.Lens' CustomLabel (Prelude.Maybe Geometry)
customLabel_geometry = Lens.lens (\CustomLabel' {geometry} -> geometry) (\s@CustomLabel' {} a -> s {geometry = a} :: CustomLabel)

-- | The name of the custom label.
customLabel_name :: Lens.Lens' CustomLabel (Prelude.Maybe Prelude.Text)
customLabel_name = Lens.lens (\CustomLabel' {name} -> name) (\s@CustomLabel' {} a -> s {name = a} :: CustomLabel)

instance Data.FromJSON CustomLabel where
  parseJSON =
    Data.withObject
      "CustomLabel"
      ( \x ->
          CustomLabel'
            Prelude.<$> (x Data..:? "Confidence")
            Prelude.<*> (x Data..:? "Geometry")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable CustomLabel where
  hashWithSalt _salt CustomLabel' {..} =
    _salt
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` geometry
      `Prelude.hashWithSalt` name

instance Prelude.NFData CustomLabel where
  rnf CustomLabel' {..} =
    Prelude.rnf confidence `Prelude.seq`
      Prelude.rnf geometry `Prelude.seq`
        Prelude.rnf name
