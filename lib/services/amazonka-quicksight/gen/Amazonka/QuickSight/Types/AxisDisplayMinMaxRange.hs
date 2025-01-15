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
-- Module      : Amazonka.QuickSight.Types.AxisDisplayMinMaxRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AxisDisplayMinMaxRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The minimum and maximum setup for an axis display range.
--
-- /See:/ 'newAxisDisplayMinMaxRange' smart constructor.
data AxisDisplayMinMaxRange = AxisDisplayMinMaxRange'
  { -- | The maximum setup for an axis display range.
    maximum :: Prelude.Maybe Prelude.Double,
    -- | The minimum setup for an axis display range.
    minimum :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AxisDisplayMinMaxRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximum', 'axisDisplayMinMaxRange_maximum' - The maximum setup for an axis display range.
--
-- 'minimum', 'axisDisplayMinMaxRange_minimum' - The minimum setup for an axis display range.
newAxisDisplayMinMaxRange ::
  AxisDisplayMinMaxRange
newAxisDisplayMinMaxRange =
  AxisDisplayMinMaxRange'
    { maximum = Prelude.Nothing,
      minimum = Prelude.Nothing
    }

-- | The maximum setup for an axis display range.
axisDisplayMinMaxRange_maximum :: Lens.Lens' AxisDisplayMinMaxRange (Prelude.Maybe Prelude.Double)
axisDisplayMinMaxRange_maximum = Lens.lens (\AxisDisplayMinMaxRange' {maximum} -> maximum) (\s@AxisDisplayMinMaxRange' {} a -> s {maximum = a} :: AxisDisplayMinMaxRange)

-- | The minimum setup for an axis display range.
axisDisplayMinMaxRange_minimum :: Lens.Lens' AxisDisplayMinMaxRange (Prelude.Maybe Prelude.Double)
axisDisplayMinMaxRange_minimum = Lens.lens (\AxisDisplayMinMaxRange' {minimum} -> minimum) (\s@AxisDisplayMinMaxRange' {} a -> s {minimum = a} :: AxisDisplayMinMaxRange)

instance Data.FromJSON AxisDisplayMinMaxRange where
  parseJSON =
    Data.withObject
      "AxisDisplayMinMaxRange"
      ( \x ->
          AxisDisplayMinMaxRange'
            Prelude.<$> (x Data..:? "Maximum")
            Prelude.<*> (x Data..:? "Minimum")
      )

instance Prelude.Hashable AxisDisplayMinMaxRange where
  hashWithSalt _salt AxisDisplayMinMaxRange' {..} =
    _salt
      `Prelude.hashWithSalt` maximum
      `Prelude.hashWithSalt` minimum

instance Prelude.NFData AxisDisplayMinMaxRange where
  rnf AxisDisplayMinMaxRange' {..} =
    Prelude.rnf maximum `Prelude.seq`
      Prelude.rnf minimum

instance Data.ToJSON AxisDisplayMinMaxRange where
  toJSON AxisDisplayMinMaxRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Maximum" Data..=) Prelude.<$> maximum,
            ("Minimum" Data..=) Prelude.<$> minimum
          ]
      )
