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
-- Module      : Amazonka.QuickSight.Types.VisibleRangeOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.VisibleRangeOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.PercentVisibleRange

-- | The range options for the data zoom scroll bar.
--
-- /See:/ 'newVisibleRangeOptions' smart constructor.
data VisibleRangeOptions = VisibleRangeOptions'
  { -- | The percent range in the visible range.
    percentRange :: Prelude.Maybe PercentVisibleRange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VisibleRangeOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'percentRange', 'visibleRangeOptions_percentRange' - The percent range in the visible range.
newVisibleRangeOptions ::
  VisibleRangeOptions
newVisibleRangeOptions =
  VisibleRangeOptions'
    { percentRange =
        Prelude.Nothing
    }

-- | The percent range in the visible range.
visibleRangeOptions_percentRange :: Lens.Lens' VisibleRangeOptions (Prelude.Maybe PercentVisibleRange)
visibleRangeOptions_percentRange = Lens.lens (\VisibleRangeOptions' {percentRange} -> percentRange) (\s@VisibleRangeOptions' {} a -> s {percentRange = a} :: VisibleRangeOptions)

instance Data.FromJSON VisibleRangeOptions where
  parseJSON =
    Data.withObject
      "VisibleRangeOptions"
      ( \x ->
          VisibleRangeOptions'
            Prelude.<$> (x Data..:? "PercentRange")
      )

instance Prelude.Hashable VisibleRangeOptions where
  hashWithSalt _salt VisibleRangeOptions' {..} =
    _salt `Prelude.hashWithSalt` percentRange

instance Prelude.NFData VisibleRangeOptions where
  rnf VisibleRangeOptions' {..} =
    Prelude.rnf percentRange

instance Data.ToJSON VisibleRangeOptions where
  toJSON VisibleRangeOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [("PercentRange" Data..=) Prelude.<$> percentRange]
      )
