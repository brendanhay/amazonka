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
-- Module      : Amazonka.QuickSight.Types.AxisLinearScale
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AxisLinearScale where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The liner axis scale setup.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newAxisLinearScale' smart constructor.
data AxisLinearScale = AxisLinearScale'
  { -- | The step count setup of a linear axis.
    stepCount :: Prelude.Maybe Prelude.Int,
    -- | The step size setup of a linear axis.
    stepSize :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AxisLinearScale' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stepCount', 'axisLinearScale_stepCount' - The step count setup of a linear axis.
--
-- 'stepSize', 'axisLinearScale_stepSize' - The step size setup of a linear axis.
newAxisLinearScale ::
  AxisLinearScale
newAxisLinearScale =
  AxisLinearScale'
    { stepCount = Prelude.Nothing,
      stepSize = Prelude.Nothing
    }

-- | The step count setup of a linear axis.
axisLinearScale_stepCount :: Lens.Lens' AxisLinearScale (Prelude.Maybe Prelude.Int)
axisLinearScale_stepCount = Lens.lens (\AxisLinearScale' {stepCount} -> stepCount) (\s@AxisLinearScale' {} a -> s {stepCount = a} :: AxisLinearScale)

-- | The step size setup of a linear axis.
axisLinearScale_stepSize :: Lens.Lens' AxisLinearScale (Prelude.Maybe Prelude.Double)
axisLinearScale_stepSize = Lens.lens (\AxisLinearScale' {stepSize} -> stepSize) (\s@AxisLinearScale' {} a -> s {stepSize = a} :: AxisLinearScale)

instance Data.FromJSON AxisLinearScale where
  parseJSON =
    Data.withObject
      "AxisLinearScale"
      ( \x ->
          AxisLinearScale'
            Prelude.<$> (x Data..:? "StepCount")
            Prelude.<*> (x Data..:? "StepSize")
      )

instance Prelude.Hashable AxisLinearScale where
  hashWithSalt _salt AxisLinearScale' {..} =
    _salt
      `Prelude.hashWithSalt` stepCount
      `Prelude.hashWithSalt` stepSize

instance Prelude.NFData AxisLinearScale where
  rnf AxisLinearScale' {..} =
    Prelude.rnf stepCount `Prelude.seq`
      Prelude.rnf stepSize

instance Data.ToJSON AxisLinearScale where
  toJSON AxisLinearScale' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StepCount" Data..=) Prelude.<$> stepCount,
            ("StepSize" Data..=) Prelude.<$> stepSize
          ]
      )
