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
-- Module      : Amazonka.LookoutVision.Types.PixelAnomaly
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.PixelAnomaly where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the pixels in an anomaly mask. For more information,
-- see Anomaly. @PixelAnomaly@ is only returned by image segmentation
-- models.
--
-- /See:/ 'newPixelAnomaly' smart constructor.
data PixelAnomaly = PixelAnomaly'
  { -- | A hex color value for the mask that covers an anomaly type. Each anomaly
    -- type has a different mask color. The color maps to the color of the
    -- anomaly type used in the training dataset.
    color :: Prelude.Maybe Prelude.Text,
    -- | The percentage area of the image that the anomaly type covers.
    totalPercentageArea :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PixelAnomaly' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'color', 'pixelAnomaly_color' - A hex color value for the mask that covers an anomaly type. Each anomaly
-- type has a different mask color. The color maps to the color of the
-- anomaly type used in the training dataset.
--
-- 'totalPercentageArea', 'pixelAnomaly_totalPercentageArea' - The percentage area of the image that the anomaly type covers.
newPixelAnomaly ::
  PixelAnomaly
newPixelAnomaly =
  PixelAnomaly'
    { color = Prelude.Nothing,
      totalPercentageArea = Prelude.Nothing
    }

-- | A hex color value for the mask that covers an anomaly type. Each anomaly
-- type has a different mask color. The color maps to the color of the
-- anomaly type used in the training dataset.
pixelAnomaly_color :: Lens.Lens' PixelAnomaly (Prelude.Maybe Prelude.Text)
pixelAnomaly_color = Lens.lens (\PixelAnomaly' {color} -> color) (\s@PixelAnomaly' {} a -> s {color = a} :: PixelAnomaly)

-- | The percentage area of the image that the anomaly type covers.
pixelAnomaly_totalPercentageArea :: Lens.Lens' PixelAnomaly (Prelude.Maybe Prelude.Double)
pixelAnomaly_totalPercentageArea = Lens.lens (\PixelAnomaly' {totalPercentageArea} -> totalPercentageArea) (\s@PixelAnomaly' {} a -> s {totalPercentageArea = a} :: PixelAnomaly)

instance Data.FromJSON PixelAnomaly where
  parseJSON =
    Data.withObject
      "PixelAnomaly"
      ( \x ->
          PixelAnomaly'
            Prelude.<$> (x Data..:? "Color")
            Prelude.<*> (x Data..:? "TotalPercentageArea")
      )

instance Prelude.Hashable PixelAnomaly where
  hashWithSalt _salt PixelAnomaly' {..} =
    _salt `Prelude.hashWithSalt` color
      `Prelude.hashWithSalt` totalPercentageArea

instance Prelude.NFData PixelAnomaly where
  rnf PixelAnomaly' {..} =
    Prelude.rnf color
      `Prelude.seq` Prelude.rnf totalPercentageArea
