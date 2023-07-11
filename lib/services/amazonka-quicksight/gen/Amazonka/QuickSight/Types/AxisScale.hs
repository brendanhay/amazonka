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
-- Module      : Amazonka.QuickSight.Types.AxisScale
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AxisScale where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AxisLinearScale
import Amazonka.QuickSight.Types.AxisLogarithmicScale

-- | The scale setup options for a numeric axis display.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newAxisScale' smart constructor.
data AxisScale = AxisScale'
  { -- | The linear axis scale setup.
    linear :: Prelude.Maybe AxisLinearScale,
    -- | The logarithmic axis scale setup.
    logarithmic :: Prelude.Maybe AxisLogarithmicScale
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AxisScale' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'linear', 'axisScale_linear' - The linear axis scale setup.
--
-- 'logarithmic', 'axisScale_logarithmic' - The logarithmic axis scale setup.
newAxisScale ::
  AxisScale
newAxisScale =
  AxisScale'
    { linear = Prelude.Nothing,
      logarithmic = Prelude.Nothing
    }

-- | The linear axis scale setup.
axisScale_linear :: Lens.Lens' AxisScale (Prelude.Maybe AxisLinearScale)
axisScale_linear = Lens.lens (\AxisScale' {linear} -> linear) (\s@AxisScale' {} a -> s {linear = a} :: AxisScale)

-- | The logarithmic axis scale setup.
axisScale_logarithmic :: Lens.Lens' AxisScale (Prelude.Maybe AxisLogarithmicScale)
axisScale_logarithmic = Lens.lens (\AxisScale' {logarithmic} -> logarithmic) (\s@AxisScale' {} a -> s {logarithmic = a} :: AxisScale)

instance Data.FromJSON AxisScale where
  parseJSON =
    Data.withObject
      "AxisScale"
      ( \x ->
          AxisScale'
            Prelude.<$> (x Data..:? "Linear")
            Prelude.<*> (x Data..:? "Logarithmic")
      )

instance Prelude.Hashable AxisScale where
  hashWithSalt _salt AxisScale' {..} =
    _salt
      `Prelude.hashWithSalt` linear
      `Prelude.hashWithSalt` logarithmic

instance Prelude.NFData AxisScale where
  rnf AxisScale' {..} =
    Prelude.rnf linear
      `Prelude.seq` Prelude.rnf logarithmic

instance Data.ToJSON AxisScale where
  toJSON AxisScale' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Linear" Data..=) Prelude.<$> linear,
            ("Logarithmic" Data..=) Prelude.<$> logarithmic
          ]
      )
