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
-- Module      : Amazonka.QuickSight.Types.AxisDataOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AxisDataOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DateAxisOptions
import Amazonka.QuickSight.Types.NumericAxisOptions

-- | The data options for an axis.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newAxisDataOptions' smart constructor.
data AxisDataOptions = AxisDataOptions'
  { -- | The options for an axis with a date field.
    dateAxisOptions :: Prelude.Maybe DateAxisOptions,
    -- | The options for an axis with a numeric field.
    numericAxisOptions :: Prelude.Maybe NumericAxisOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AxisDataOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dateAxisOptions', 'axisDataOptions_dateAxisOptions' - The options for an axis with a date field.
--
-- 'numericAxisOptions', 'axisDataOptions_numericAxisOptions' - The options for an axis with a numeric field.
newAxisDataOptions ::
  AxisDataOptions
newAxisDataOptions =
  AxisDataOptions'
    { dateAxisOptions = Prelude.Nothing,
      numericAxisOptions = Prelude.Nothing
    }

-- | The options for an axis with a date field.
axisDataOptions_dateAxisOptions :: Lens.Lens' AxisDataOptions (Prelude.Maybe DateAxisOptions)
axisDataOptions_dateAxisOptions = Lens.lens (\AxisDataOptions' {dateAxisOptions} -> dateAxisOptions) (\s@AxisDataOptions' {} a -> s {dateAxisOptions = a} :: AxisDataOptions)

-- | The options for an axis with a numeric field.
axisDataOptions_numericAxisOptions :: Lens.Lens' AxisDataOptions (Prelude.Maybe NumericAxisOptions)
axisDataOptions_numericAxisOptions = Lens.lens (\AxisDataOptions' {numericAxisOptions} -> numericAxisOptions) (\s@AxisDataOptions' {} a -> s {numericAxisOptions = a} :: AxisDataOptions)

instance Data.FromJSON AxisDataOptions where
  parseJSON =
    Data.withObject
      "AxisDataOptions"
      ( \x ->
          AxisDataOptions'
            Prelude.<$> (x Data..:? "DateAxisOptions")
            Prelude.<*> (x Data..:? "NumericAxisOptions")
      )

instance Prelude.Hashable AxisDataOptions where
  hashWithSalt _salt AxisDataOptions' {..} =
    _salt `Prelude.hashWithSalt` dateAxisOptions
      `Prelude.hashWithSalt` numericAxisOptions

instance Prelude.NFData AxisDataOptions where
  rnf AxisDataOptions' {..} =
    Prelude.rnf dateAxisOptions
      `Prelude.seq` Prelude.rnf numericAxisOptions

instance Data.ToJSON AxisDataOptions where
  toJSON AxisDataOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DateAxisOptions" Data..=)
              Prelude.<$> dateAxisOptions,
            ("NumericAxisOptions" Data..=)
              Prelude.<$> numericAxisOptions
          ]
      )
