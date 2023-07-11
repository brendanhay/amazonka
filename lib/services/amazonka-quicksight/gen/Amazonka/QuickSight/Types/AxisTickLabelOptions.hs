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
-- Module      : Amazonka.QuickSight.Types.AxisTickLabelOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AxisTickLabelOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.LabelOptions

-- | The tick label options of an axis.
--
-- /See:/ 'newAxisTickLabelOptions' smart constructor.
data AxisTickLabelOptions = AxisTickLabelOptions'
  { -- | Determines whether or not the axis ticks are visible.
    labelOptions :: Prelude.Maybe LabelOptions,
    -- | The rotation angle of the axis tick labels.
    rotationAngle :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AxisTickLabelOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelOptions', 'axisTickLabelOptions_labelOptions' - Determines whether or not the axis ticks are visible.
--
-- 'rotationAngle', 'axisTickLabelOptions_rotationAngle' - The rotation angle of the axis tick labels.
newAxisTickLabelOptions ::
  AxisTickLabelOptions
newAxisTickLabelOptions =
  AxisTickLabelOptions'
    { labelOptions =
        Prelude.Nothing,
      rotationAngle = Prelude.Nothing
    }

-- | Determines whether or not the axis ticks are visible.
axisTickLabelOptions_labelOptions :: Lens.Lens' AxisTickLabelOptions (Prelude.Maybe LabelOptions)
axisTickLabelOptions_labelOptions = Lens.lens (\AxisTickLabelOptions' {labelOptions} -> labelOptions) (\s@AxisTickLabelOptions' {} a -> s {labelOptions = a} :: AxisTickLabelOptions)

-- | The rotation angle of the axis tick labels.
axisTickLabelOptions_rotationAngle :: Lens.Lens' AxisTickLabelOptions (Prelude.Maybe Prelude.Double)
axisTickLabelOptions_rotationAngle = Lens.lens (\AxisTickLabelOptions' {rotationAngle} -> rotationAngle) (\s@AxisTickLabelOptions' {} a -> s {rotationAngle = a} :: AxisTickLabelOptions)

instance Data.FromJSON AxisTickLabelOptions where
  parseJSON =
    Data.withObject
      "AxisTickLabelOptions"
      ( \x ->
          AxisTickLabelOptions'
            Prelude.<$> (x Data..:? "LabelOptions")
            Prelude.<*> (x Data..:? "RotationAngle")
      )

instance Prelude.Hashable AxisTickLabelOptions where
  hashWithSalt _salt AxisTickLabelOptions' {..} =
    _salt
      `Prelude.hashWithSalt` labelOptions
      `Prelude.hashWithSalt` rotationAngle

instance Prelude.NFData AxisTickLabelOptions where
  rnf AxisTickLabelOptions' {..} =
    Prelude.rnf labelOptions
      `Prelude.seq` Prelude.rnf rotationAngle

instance Data.ToJSON AxisTickLabelOptions where
  toJSON AxisTickLabelOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LabelOptions" Data..=) Prelude.<$> labelOptions,
            ("RotationAngle" Data..=) Prelude.<$> rotationAngle
          ]
      )
