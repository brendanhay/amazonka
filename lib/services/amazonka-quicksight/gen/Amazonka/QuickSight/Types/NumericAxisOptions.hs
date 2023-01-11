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
-- Module      : Amazonka.QuickSight.Types.NumericAxisOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NumericAxisOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AxisDisplayRange
import Amazonka.QuickSight.Types.AxisScale

-- | The options for an axis with a numeric field.
--
-- /See:/ 'newNumericAxisOptions' smart constructor.
data NumericAxisOptions = NumericAxisOptions'
  { -- | The range setup of a numeric axis.
    range :: Prelude.Maybe AxisDisplayRange,
    -- | The scale setup of a numeric axis.
    scale :: Prelude.Maybe AxisScale
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NumericAxisOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'range', 'numericAxisOptions_range' - The range setup of a numeric axis.
--
-- 'scale', 'numericAxisOptions_scale' - The scale setup of a numeric axis.
newNumericAxisOptions ::
  NumericAxisOptions
newNumericAxisOptions =
  NumericAxisOptions'
    { range = Prelude.Nothing,
      scale = Prelude.Nothing
    }

-- | The range setup of a numeric axis.
numericAxisOptions_range :: Lens.Lens' NumericAxisOptions (Prelude.Maybe AxisDisplayRange)
numericAxisOptions_range = Lens.lens (\NumericAxisOptions' {range} -> range) (\s@NumericAxisOptions' {} a -> s {range = a} :: NumericAxisOptions)

-- | The scale setup of a numeric axis.
numericAxisOptions_scale :: Lens.Lens' NumericAxisOptions (Prelude.Maybe AxisScale)
numericAxisOptions_scale = Lens.lens (\NumericAxisOptions' {scale} -> scale) (\s@NumericAxisOptions' {} a -> s {scale = a} :: NumericAxisOptions)

instance Data.FromJSON NumericAxisOptions where
  parseJSON =
    Data.withObject
      "NumericAxisOptions"
      ( \x ->
          NumericAxisOptions'
            Prelude.<$> (x Data..:? "Range")
            Prelude.<*> (x Data..:? "Scale")
      )

instance Prelude.Hashable NumericAxisOptions where
  hashWithSalt _salt NumericAxisOptions' {..} =
    _salt `Prelude.hashWithSalt` range
      `Prelude.hashWithSalt` scale

instance Prelude.NFData NumericAxisOptions where
  rnf NumericAxisOptions' {..} =
    Prelude.rnf range `Prelude.seq` Prelude.rnf scale

instance Data.ToJSON NumericAxisOptions where
  toJSON NumericAxisOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Range" Data..=) Prelude.<$> range,
            ("Scale" Data..=) Prelude.<$> scale
          ]
      )
