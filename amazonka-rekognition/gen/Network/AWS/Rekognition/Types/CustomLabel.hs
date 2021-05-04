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
-- Module      : Network.AWS.Rekognition.Types.CustomLabel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.CustomLabel where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.Geometry

-- | A custom label detected in an image by a call to DetectCustomLabels.
--
-- /See:/ 'newCustomLabel' smart constructor.
data CustomLabel = CustomLabel'
  { -- | The name of the custom label.
    name :: Prelude.Maybe Prelude.Text,
    -- | The confidence that the model has in the detection of the custom label.
    -- The range is 0-100. A higher value indicates a higher confidence.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | The location of the detected object on the image that corresponds to the
    -- custom label. Includes an axis aligned coarse bounding box surrounding
    -- the object and a finer grain polygon for more accurate spatial
    -- information.
    geometry :: Prelude.Maybe Geometry
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CustomLabel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'customLabel_name' - The name of the custom label.
--
-- 'confidence', 'customLabel_confidence' - The confidence that the model has in the detection of the custom label.
-- The range is 0-100. A higher value indicates a higher confidence.
--
-- 'geometry', 'customLabel_geometry' - The location of the detected object on the image that corresponds to the
-- custom label. Includes an axis aligned coarse bounding box surrounding
-- the object and a finer grain polygon for more accurate spatial
-- information.
newCustomLabel ::
  CustomLabel
newCustomLabel =
  CustomLabel'
    { name = Prelude.Nothing,
      confidence = Prelude.Nothing,
      geometry = Prelude.Nothing
    }

-- | The name of the custom label.
customLabel_name :: Lens.Lens' CustomLabel (Prelude.Maybe Prelude.Text)
customLabel_name = Lens.lens (\CustomLabel' {name} -> name) (\s@CustomLabel' {} a -> s {name = a} :: CustomLabel)

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

instance Prelude.FromJSON CustomLabel where
  parseJSON =
    Prelude.withObject
      "CustomLabel"
      ( \x ->
          CustomLabel'
            Prelude.<$> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Confidence")
            Prelude.<*> (x Prelude..:? "Geometry")
      )

instance Prelude.Hashable CustomLabel

instance Prelude.NFData CustomLabel
