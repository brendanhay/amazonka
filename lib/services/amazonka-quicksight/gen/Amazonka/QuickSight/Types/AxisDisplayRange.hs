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
-- Module      : Amazonka.QuickSight.Types.AxisDisplayRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AxisDisplayRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AxisDisplayDataDrivenRange
import Amazonka.QuickSight.Types.AxisDisplayMinMaxRange

-- | The range setup of a numeric axis display range.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newAxisDisplayRange' smart constructor.
data AxisDisplayRange = AxisDisplayRange'
  { -- | The data-driven setup of an axis display range.
    dataDriven :: Prelude.Maybe AxisDisplayDataDrivenRange,
    -- | The minimum and maximum setup of an axis display range.
    minMax :: Prelude.Maybe AxisDisplayMinMaxRange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AxisDisplayRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataDriven', 'axisDisplayRange_dataDriven' - The data-driven setup of an axis display range.
--
-- 'minMax', 'axisDisplayRange_minMax' - The minimum and maximum setup of an axis display range.
newAxisDisplayRange ::
  AxisDisplayRange
newAxisDisplayRange =
  AxisDisplayRange'
    { dataDriven = Prelude.Nothing,
      minMax = Prelude.Nothing
    }

-- | The data-driven setup of an axis display range.
axisDisplayRange_dataDriven :: Lens.Lens' AxisDisplayRange (Prelude.Maybe AxisDisplayDataDrivenRange)
axisDisplayRange_dataDriven = Lens.lens (\AxisDisplayRange' {dataDriven} -> dataDriven) (\s@AxisDisplayRange' {} a -> s {dataDriven = a} :: AxisDisplayRange)

-- | The minimum and maximum setup of an axis display range.
axisDisplayRange_minMax :: Lens.Lens' AxisDisplayRange (Prelude.Maybe AxisDisplayMinMaxRange)
axisDisplayRange_minMax = Lens.lens (\AxisDisplayRange' {minMax} -> minMax) (\s@AxisDisplayRange' {} a -> s {minMax = a} :: AxisDisplayRange)

instance Data.FromJSON AxisDisplayRange where
  parseJSON =
    Data.withObject
      "AxisDisplayRange"
      ( \x ->
          AxisDisplayRange'
            Prelude.<$> (x Data..:? "DataDriven")
            Prelude.<*> (x Data..:? "MinMax")
      )

instance Prelude.Hashable AxisDisplayRange where
  hashWithSalt _salt AxisDisplayRange' {..} =
    _salt `Prelude.hashWithSalt` dataDriven
      `Prelude.hashWithSalt` minMax

instance Prelude.NFData AxisDisplayRange where
  rnf AxisDisplayRange' {..} =
    Prelude.rnf dataDriven
      `Prelude.seq` Prelude.rnf minMax

instance Data.ToJSON AxisDisplayRange where
  toJSON AxisDisplayRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataDriven" Data..=) Prelude.<$> dataDriven,
            ("MinMax" Data..=) Prelude.<$> minMax
          ]
      )
