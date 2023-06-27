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
-- Module      : Amazonka.MediaConvert.Types.ClipLimits
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.ClipLimits where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify YUV limits and RGB tolerances when you set Sample range
-- conversion to Limited range clip.
--
-- /See:/ 'newClipLimits' smart constructor.
data ClipLimits = ClipLimits'
  { -- | Specify the Maximum RGB color sample range tolerance for your output.
    -- MediaConvert corrects any YUV values that, when converted to RGB, would
    -- be outside the upper tolerance that you specify. Enter an integer from
    -- 90 to 105 as an offset percentage to the maximum possible value. Leave
    -- blank to use the default value 100. When you specify a value for Maximum
    -- RGB tolerance, you must set Sample range conversion to Limited range
    -- clip.
    maximumRGBTolerance :: Prelude.Maybe Prelude.Natural,
    -- | Specify the Maximum YUV color sample limit. MediaConvert conforms any
    -- pixels in your input above the value that you specify to typical limited
    -- range bounds. Enter an integer from 920 to 1023. Leave blank to use the
    -- default value 940. The value that you enter applies to 10-bit ranges.
    -- For 8-bit ranges, MediaConvert automatically scales this value down.
    -- When you specify a value for Maximum YUV, you must set Sample range
    -- conversion to Limited range clip.
    maximumYUV :: Prelude.Maybe Prelude.Natural,
    -- | Specify the Minimum RGB color sample range tolerance for your output.
    -- MediaConvert corrects any YUV values that, when converted to RGB, would
    -- be outside the lower tolerance that you specify. Enter an integer from
    -- -5 to 10 as an offset percentage to the minimum possible value. Leave
    -- blank to use the default value 0. When you specify a value for Minimum
    -- RGB tolerance, you must set Sample range conversion to Limited range
    -- clip.
    minimumRGBTolerance :: Prelude.Maybe Prelude.Int,
    -- | Specify the Minimum YUV color sample limit. MediaConvert conforms any
    -- pixels in your input below the value that you specify to typical limited
    -- range bounds. Enter an integer from 0 to 128. Leave blank to use the
    -- default value 64. The value that you enter applies to 10-bit ranges. For
    -- 8-bit ranges, MediaConvert automatically scales this value down. When
    -- you specify a value for Minumum YUV, you must set Sample range
    -- conversion to Limited range clip.
    minimumYUV :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClipLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumRGBTolerance', 'clipLimits_maximumRGBTolerance' - Specify the Maximum RGB color sample range tolerance for your output.
-- MediaConvert corrects any YUV values that, when converted to RGB, would
-- be outside the upper tolerance that you specify. Enter an integer from
-- 90 to 105 as an offset percentage to the maximum possible value. Leave
-- blank to use the default value 100. When you specify a value for Maximum
-- RGB tolerance, you must set Sample range conversion to Limited range
-- clip.
--
-- 'maximumYUV', 'clipLimits_maximumYUV' - Specify the Maximum YUV color sample limit. MediaConvert conforms any
-- pixels in your input above the value that you specify to typical limited
-- range bounds. Enter an integer from 920 to 1023. Leave blank to use the
-- default value 940. The value that you enter applies to 10-bit ranges.
-- For 8-bit ranges, MediaConvert automatically scales this value down.
-- When you specify a value for Maximum YUV, you must set Sample range
-- conversion to Limited range clip.
--
-- 'minimumRGBTolerance', 'clipLimits_minimumRGBTolerance' - Specify the Minimum RGB color sample range tolerance for your output.
-- MediaConvert corrects any YUV values that, when converted to RGB, would
-- be outside the lower tolerance that you specify. Enter an integer from
-- -5 to 10 as an offset percentage to the minimum possible value. Leave
-- blank to use the default value 0. When you specify a value for Minimum
-- RGB tolerance, you must set Sample range conversion to Limited range
-- clip.
--
-- 'minimumYUV', 'clipLimits_minimumYUV' - Specify the Minimum YUV color sample limit. MediaConvert conforms any
-- pixels in your input below the value that you specify to typical limited
-- range bounds. Enter an integer from 0 to 128. Leave blank to use the
-- default value 64. The value that you enter applies to 10-bit ranges. For
-- 8-bit ranges, MediaConvert automatically scales this value down. When
-- you specify a value for Minumum YUV, you must set Sample range
-- conversion to Limited range clip.
newClipLimits ::
  ClipLimits
newClipLimits =
  ClipLimits'
    { maximumRGBTolerance = Prelude.Nothing,
      maximumYUV = Prelude.Nothing,
      minimumRGBTolerance = Prelude.Nothing,
      minimumYUV = Prelude.Nothing
    }

-- | Specify the Maximum RGB color sample range tolerance for your output.
-- MediaConvert corrects any YUV values that, when converted to RGB, would
-- be outside the upper tolerance that you specify. Enter an integer from
-- 90 to 105 as an offset percentage to the maximum possible value. Leave
-- blank to use the default value 100. When you specify a value for Maximum
-- RGB tolerance, you must set Sample range conversion to Limited range
-- clip.
clipLimits_maximumRGBTolerance :: Lens.Lens' ClipLimits (Prelude.Maybe Prelude.Natural)
clipLimits_maximumRGBTolerance = Lens.lens (\ClipLimits' {maximumRGBTolerance} -> maximumRGBTolerance) (\s@ClipLimits' {} a -> s {maximumRGBTolerance = a} :: ClipLimits)

-- | Specify the Maximum YUV color sample limit. MediaConvert conforms any
-- pixels in your input above the value that you specify to typical limited
-- range bounds. Enter an integer from 920 to 1023. Leave blank to use the
-- default value 940. The value that you enter applies to 10-bit ranges.
-- For 8-bit ranges, MediaConvert automatically scales this value down.
-- When you specify a value for Maximum YUV, you must set Sample range
-- conversion to Limited range clip.
clipLimits_maximumYUV :: Lens.Lens' ClipLimits (Prelude.Maybe Prelude.Natural)
clipLimits_maximumYUV = Lens.lens (\ClipLimits' {maximumYUV} -> maximumYUV) (\s@ClipLimits' {} a -> s {maximumYUV = a} :: ClipLimits)

-- | Specify the Minimum RGB color sample range tolerance for your output.
-- MediaConvert corrects any YUV values that, when converted to RGB, would
-- be outside the lower tolerance that you specify. Enter an integer from
-- -5 to 10 as an offset percentage to the minimum possible value. Leave
-- blank to use the default value 0. When you specify a value for Minimum
-- RGB tolerance, you must set Sample range conversion to Limited range
-- clip.
clipLimits_minimumRGBTolerance :: Lens.Lens' ClipLimits (Prelude.Maybe Prelude.Int)
clipLimits_minimumRGBTolerance = Lens.lens (\ClipLimits' {minimumRGBTolerance} -> minimumRGBTolerance) (\s@ClipLimits' {} a -> s {minimumRGBTolerance = a} :: ClipLimits)

-- | Specify the Minimum YUV color sample limit. MediaConvert conforms any
-- pixels in your input below the value that you specify to typical limited
-- range bounds. Enter an integer from 0 to 128. Leave blank to use the
-- default value 64. The value that you enter applies to 10-bit ranges. For
-- 8-bit ranges, MediaConvert automatically scales this value down. When
-- you specify a value for Minumum YUV, you must set Sample range
-- conversion to Limited range clip.
clipLimits_minimumYUV :: Lens.Lens' ClipLimits (Prelude.Maybe Prelude.Natural)
clipLimits_minimumYUV = Lens.lens (\ClipLimits' {minimumYUV} -> minimumYUV) (\s@ClipLimits' {} a -> s {minimumYUV = a} :: ClipLimits)

instance Data.FromJSON ClipLimits where
  parseJSON =
    Data.withObject
      "ClipLimits"
      ( \x ->
          ClipLimits'
            Prelude.<$> (x Data..:? "maximumRGBTolerance")
            Prelude.<*> (x Data..:? "maximumYUV")
            Prelude.<*> (x Data..:? "minimumRGBTolerance")
            Prelude.<*> (x Data..:? "minimumYUV")
      )

instance Prelude.Hashable ClipLimits where
  hashWithSalt _salt ClipLimits' {..} =
    _salt
      `Prelude.hashWithSalt` maximumRGBTolerance
      `Prelude.hashWithSalt` maximumYUV
      `Prelude.hashWithSalt` minimumRGBTolerance
      `Prelude.hashWithSalt` minimumYUV

instance Prelude.NFData ClipLimits where
  rnf ClipLimits' {..} =
    Prelude.rnf maximumRGBTolerance
      `Prelude.seq` Prelude.rnf maximumYUV
      `Prelude.seq` Prelude.rnf minimumRGBTolerance
      `Prelude.seq` Prelude.rnf minimumYUV

instance Data.ToJSON ClipLimits where
  toJSON ClipLimits' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maximumRGBTolerance" Data..=)
              Prelude.<$> maximumRGBTolerance,
            ("maximumYUV" Data..=) Prelude.<$> maximumYUV,
            ("minimumRGBTolerance" Data..=)
              Prelude.<$> minimumRGBTolerance,
            ("minimumYUV" Data..=) Prelude.<$> minimumYUV
          ]
      )
