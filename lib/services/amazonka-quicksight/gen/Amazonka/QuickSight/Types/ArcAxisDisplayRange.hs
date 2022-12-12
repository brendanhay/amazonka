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
-- Module      : Amazonka.QuickSight.Types.ArcAxisDisplayRange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ArcAxisDisplayRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The arc axis range of a @GaugeChartVisual@.
--
-- /See:/ 'newArcAxisDisplayRange' smart constructor.
data ArcAxisDisplayRange = ArcAxisDisplayRange'
  { -- | The maximum value of the arc axis range.
    max :: Prelude.Maybe Prelude.Double,
    -- | The minimum value of the arc axis range.
    min :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArcAxisDisplayRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'arcAxisDisplayRange_max' - The maximum value of the arc axis range.
--
-- 'min', 'arcAxisDisplayRange_min' - The minimum value of the arc axis range.
newArcAxisDisplayRange ::
  ArcAxisDisplayRange
newArcAxisDisplayRange =
  ArcAxisDisplayRange'
    { max = Prelude.Nothing,
      min = Prelude.Nothing
    }

-- | The maximum value of the arc axis range.
arcAxisDisplayRange_max :: Lens.Lens' ArcAxisDisplayRange (Prelude.Maybe Prelude.Double)
arcAxisDisplayRange_max = Lens.lens (\ArcAxisDisplayRange' {max} -> max) (\s@ArcAxisDisplayRange' {} a -> s {max = a} :: ArcAxisDisplayRange)

-- | The minimum value of the arc axis range.
arcAxisDisplayRange_min :: Lens.Lens' ArcAxisDisplayRange (Prelude.Maybe Prelude.Double)
arcAxisDisplayRange_min = Lens.lens (\ArcAxisDisplayRange' {min} -> min) (\s@ArcAxisDisplayRange' {} a -> s {min = a} :: ArcAxisDisplayRange)

instance Data.FromJSON ArcAxisDisplayRange where
  parseJSON =
    Data.withObject
      "ArcAxisDisplayRange"
      ( \x ->
          ArcAxisDisplayRange'
            Prelude.<$> (x Data..:? "Max") Prelude.<*> (x Data..:? "Min")
      )

instance Prelude.Hashable ArcAxisDisplayRange where
  hashWithSalt _salt ArcAxisDisplayRange' {..} =
    _salt `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` min

instance Prelude.NFData ArcAxisDisplayRange where
  rnf ArcAxisDisplayRange' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance Data.ToJSON ArcAxisDisplayRange where
  toJSON ArcAxisDisplayRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Max" Data..=) Prelude.<$> max,
            ("Min" Data..=) Prelude.<$> min
          ]
      )
