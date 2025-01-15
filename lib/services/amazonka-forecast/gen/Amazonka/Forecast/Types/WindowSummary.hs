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
-- Module      : Amazonka.Forecast.Types.WindowSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.WindowSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types.EvaluationType
import Amazonka.Forecast.Types.Metrics
import qualified Amazonka.Prelude as Prelude

-- | The metrics for a time range within the evaluation portion of a dataset.
-- This object is part of the EvaluationResult object.
--
-- The @TestWindowStart@ and @TestWindowEnd@ parameters are determined by
-- the @BackTestWindowOffset@ parameter of the EvaluationParameters object.
--
-- /See:/ 'newWindowSummary' smart constructor.
data WindowSummary = WindowSummary'
  { -- | The type of evaluation.
    --
    -- -   @SUMMARY@ - The average metrics across all windows.
    --
    -- -   @COMPUTED@ - The metrics for the specified window.
    evaluationType :: Prelude.Maybe EvaluationType,
    -- | The number of data points within the window.
    itemCount :: Prelude.Maybe Prelude.Int,
    -- | Provides metrics used to evaluate the performance of a predictor.
    metrics :: Prelude.Maybe Metrics,
    -- | The timestamp that defines the end of the window.
    testWindowEnd :: Prelude.Maybe Data.POSIX,
    -- | The timestamp that defines the start of the window.
    testWindowStart :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WindowSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluationType', 'windowSummary_evaluationType' - The type of evaluation.
--
-- -   @SUMMARY@ - The average metrics across all windows.
--
-- -   @COMPUTED@ - The metrics for the specified window.
--
-- 'itemCount', 'windowSummary_itemCount' - The number of data points within the window.
--
-- 'metrics', 'windowSummary_metrics' - Provides metrics used to evaluate the performance of a predictor.
--
-- 'testWindowEnd', 'windowSummary_testWindowEnd' - The timestamp that defines the end of the window.
--
-- 'testWindowStart', 'windowSummary_testWindowStart' - The timestamp that defines the start of the window.
newWindowSummary ::
  WindowSummary
newWindowSummary =
  WindowSummary'
    { evaluationType = Prelude.Nothing,
      itemCount = Prelude.Nothing,
      metrics = Prelude.Nothing,
      testWindowEnd = Prelude.Nothing,
      testWindowStart = Prelude.Nothing
    }

-- | The type of evaluation.
--
-- -   @SUMMARY@ - The average metrics across all windows.
--
-- -   @COMPUTED@ - The metrics for the specified window.
windowSummary_evaluationType :: Lens.Lens' WindowSummary (Prelude.Maybe EvaluationType)
windowSummary_evaluationType = Lens.lens (\WindowSummary' {evaluationType} -> evaluationType) (\s@WindowSummary' {} a -> s {evaluationType = a} :: WindowSummary)

-- | The number of data points within the window.
windowSummary_itemCount :: Lens.Lens' WindowSummary (Prelude.Maybe Prelude.Int)
windowSummary_itemCount = Lens.lens (\WindowSummary' {itemCount} -> itemCount) (\s@WindowSummary' {} a -> s {itemCount = a} :: WindowSummary)

-- | Provides metrics used to evaluate the performance of a predictor.
windowSummary_metrics :: Lens.Lens' WindowSummary (Prelude.Maybe Metrics)
windowSummary_metrics = Lens.lens (\WindowSummary' {metrics} -> metrics) (\s@WindowSummary' {} a -> s {metrics = a} :: WindowSummary)

-- | The timestamp that defines the end of the window.
windowSummary_testWindowEnd :: Lens.Lens' WindowSummary (Prelude.Maybe Prelude.UTCTime)
windowSummary_testWindowEnd = Lens.lens (\WindowSummary' {testWindowEnd} -> testWindowEnd) (\s@WindowSummary' {} a -> s {testWindowEnd = a} :: WindowSummary) Prelude.. Lens.mapping Data._Time

-- | The timestamp that defines the start of the window.
windowSummary_testWindowStart :: Lens.Lens' WindowSummary (Prelude.Maybe Prelude.UTCTime)
windowSummary_testWindowStart = Lens.lens (\WindowSummary' {testWindowStart} -> testWindowStart) (\s@WindowSummary' {} a -> s {testWindowStart = a} :: WindowSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON WindowSummary where
  parseJSON =
    Data.withObject
      "WindowSummary"
      ( \x ->
          WindowSummary'
            Prelude.<$> (x Data..:? "EvaluationType")
            Prelude.<*> (x Data..:? "ItemCount")
            Prelude.<*> (x Data..:? "Metrics")
            Prelude.<*> (x Data..:? "TestWindowEnd")
            Prelude.<*> (x Data..:? "TestWindowStart")
      )

instance Prelude.Hashable WindowSummary where
  hashWithSalt _salt WindowSummary' {..} =
    _salt
      `Prelude.hashWithSalt` evaluationType
      `Prelude.hashWithSalt` itemCount
      `Prelude.hashWithSalt` metrics
      `Prelude.hashWithSalt` testWindowEnd
      `Prelude.hashWithSalt` testWindowStart

instance Prelude.NFData WindowSummary where
  rnf WindowSummary' {..} =
    Prelude.rnf evaluationType `Prelude.seq`
      Prelude.rnf itemCount `Prelude.seq`
        Prelude.rnf metrics `Prelude.seq`
          Prelude.rnf testWindowEnd `Prelude.seq`
            Prelude.rnf testWindowStart
