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
-- Module      : Amazonka.QuickSight.Types.HistogramBinOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.HistogramBinOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.BinCountOptions
import Amazonka.QuickSight.Types.BinWidthOptions
import Amazonka.QuickSight.Types.HistogramBinType

-- | The options that determine the presentation of histogram bins.
--
-- /See:/ 'newHistogramBinOptions' smart constructor.
data HistogramBinOptions = HistogramBinOptions'
  { -- | The options that determine the bin count of a histogram.
    binCount :: Prelude.Maybe BinCountOptions,
    -- | The options that determine the bin width of a histogram.
    binWidth :: Prelude.Maybe BinWidthOptions,
    -- | The options that determine the selected bin type.
    selectedBinType :: Prelude.Maybe HistogramBinType,
    -- | The options that determine the bin start value.
    startValue :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HistogramBinOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'binCount', 'histogramBinOptions_binCount' - The options that determine the bin count of a histogram.
--
-- 'binWidth', 'histogramBinOptions_binWidth' - The options that determine the bin width of a histogram.
--
-- 'selectedBinType', 'histogramBinOptions_selectedBinType' - The options that determine the selected bin type.
--
-- 'startValue', 'histogramBinOptions_startValue' - The options that determine the bin start value.
newHistogramBinOptions ::
  HistogramBinOptions
newHistogramBinOptions =
  HistogramBinOptions'
    { binCount = Prelude.Nothing,
      binWidth = Prelude.Nothing,
      selectedBinType = Prelude.Nothing,
      startValue = Prelude.Nothing
    }

-- | The options that determine the bin count of a histogram.
histogramBinOptions_binCount :: Lens.Lens' HistogramBinOptions (Prelude.Maybe BinCountOptions)
histogramBinOptions_binCount = Lens.lens (\HistogramBinOptions' {binCount} -> binCount) (\s@HistogramBinOptions' {} a -> s {binCount = a} :: HistogramBinOptions)

-- | The options that determine the bin width of a histogram.
histogramBinOptions_binWidth :: Lens.Lens' HistogramBinOptions (Prelude.Maybe BinWidthOptions)
histogramBinOptions_binWidth = Lens.lens (\HistogramBinOptions' {binWidth} -> binWidth) (\s@HistogramBinOptions' {} a -> s {binWidth = a} :: HistogramBinOptions)

-- | The options that determine the selected bin type.
histogramBinOptions_selectedBinType :: Lens.Lens' HistogramBinOptions (Prelude.Maybe HistogramBinType)
histogramBinOptions_selectedBinType = Lens.lens (\HistogramBinOptions' {selectedBinType} -> selectedBinType) (\s@HistogramBinOptions' {} a -> s {selectedBinType = a} :: HistogramBinOptions)

-- | The options that determine the bin start value.
histogramBinOptions_startValue :: Lens.Lens' HistogramBinOptions (Prelude.Maybe Prelude.Double)
histogramBinOptions_startValue = Lens.lens (\HistogramBinOptions' {startValue} -> startValue) (\s@HistogramBinOptions' {} a -> s {startValue = a} :: HistogramBinOptions)

instance Data.FromJSON HistogramBinOptions where
  parseJSON =
    Data.withObject
      "HistogramBinOptions"
      ( \x ->
          HistogramBinOptions'
            Prelude.<$> (x Data..:? "BinCount")
            Prelude.<*> (x Data..:? "BinWidth")
            Prelude.<*> (x Data..:? "SelectedBinType")
            Prelude.<*> (x Data..:? "StartValue")
      )

instance Prelude.Hashable HistogramBinOptions where
  hashWithSalt _salt HistogramBinOptions' {..} =
    _salt
      `Prelude.hashWithSalt` binCount
      `Prelude.hashWithSalt` binWidth
      `Prelude.hashWithSalt` selectedBinType
      `Prelude.hashWithSalt` startValue

instance Prelude.NFData HistogramBinOptions where
  rnf HistogramBinOptions' {..} =
    Prelude.rnf binCount `Prelude.seq`
      Prelude.rnf binWidth `Prelude.seq`
        Prelude.rnf selectedBinType `Prelude.seq`
          Prelude.rnf startValue

instance Data.ToJSON HistogramBinOptions where
  toJSON HistogramBinOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BinCount" Data..=) Prelude.<$> binCount,
            ("BinWidth" Data..=) Prelude.<$> binWidth,
            ("SelectedBinType" Data..=)
              Prelude.<$> selectedBinType,
            ("StartValue" Data..=) Prelude.<$> startValue
          ]
      )
